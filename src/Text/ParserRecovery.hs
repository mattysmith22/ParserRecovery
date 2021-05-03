{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Text.pRecovery
Description : Monadic combinator implementation that allows for recovery
Copyright   : (c) Matthew Smith, 2020
License     : BSD
Maintainer  : mattysmith22@googlemail.com
Stability   : experimental
Portability : POSIX

Monadic combinator implementation that allows for recovery
-}
module Text.ParserRecovery (ConsumeRecoverOpt(..), RecoveryParserT, endRecover, midRecover, endRecoverMaybe, betweenSync, endBy1Sync, endBySync, sepBy1Sync, sepBySync, findSyncToken, runRecoveryParser, runRecoveryParser') where

import           Control.Applicative      (Alternative)
import           Control.Monad.Reader
import           Text.Megaparsec
import           Text.Megaparsec.Internal
import Unsafe.Coerce

data ConsumeRecoverOpt = ForceRecovery | AlternativeSafe
    deriving Eq

type RecoverEnv e s m = [ParseError s e -> RecoveryParserT e s m ()]

newtype RecoveryParserT e s m a = RecoveryParserT {
    unRecoveryParser :: ParsecT e s (ReaderT (RecoverEnv e s m) m) a
}

instance (Ord e, Stream s, Monad m) => Functor (RecoveryParserT e s m) where
    fmap f x = RecoveryParserT (f <$> unRecoveryParser x)

instance (Ord e, Stream s, Monad m) => Applicative (RecoveryParserT e s m) where
    l <*> r = RecoveryParserT (unRecoveryParser l <*> unRecoveryParser r)
    pure x = RecoveryParserT $ pure x

instance (Ord e, Stream s, Monad m) => Monad (RecoveryParserT e s m) where
    x >>= f = RecoveryParserT (unRecoveryParser x >>= (unRecoveryParser . f))

instance (Ord e, Stream s, Monad m) => Alternative (RecoveryParserT e s m) where
    l <|> r = RecoveryParserT (unRecoveryParser l <|> unRecoveryParser r)
    empty = RecoveryParserT empty

instance (Ord e, Stream s, Monad m) => MonadPlus (RecoveryParserT e s m) where
   mzero = RecoveryParserT mzero
   mplus l r = RecoveryParserT $ mplus (unRecoveryParser l) (unRecoveryParser r)

instance (Ord e, Stream s, Monad m) => MonadFail (RecoveryParserT e s m) where
    fail s = RecoveryParserT $ fail s

instance (Ord e, Stream s, Monad m) => MonadParsec e s (RecoveryParserT e s m) where
    parseError = RecoveryParserT . parseError
    label str pX = RecoveryParserT $ label str $ unRecoveryParser pX
    hidden pX = RecoveryParserT $ hidden $ unRecoveryParser pX
    try pX = RecoveryParserT $ try $ unRecoveryParser pX
    lookAhead pX = RecoveryParserT $ lookAhead $ unRecoveryParser pX
    notFollowedBy pX = RecoveryParserT $ notFollowedBy $ unRecoveryParser pX
    withRecovery recF pX = RecoveryParserT $ withRecovery (unRecoveryParser . recF) (unRecoveryParser pX)
    observing pX = RecoveryParserT $ observing $ unRecoveryParser pX
    eof = RecoveryParserT eof
    token tF n = RecoveryParserT $ token tF n
    tokens eqF toks = RecoveryParserT $ tokens eqF toks
    takeWhileP n tF = RecoveryParserT $ takeWhileP n tF
    takeWhile1P n tF = RecoveryParserT $ takeWhile1P n tF
    takeP n c = RecoveryParserT $ takeP n c
    getParserState = RecoveryParserT getParserState
    updateParserState f = RecoveryParserT $ updateParserState f

class MonadParsec e s m => MonadRecover e s m where
    -- | Parses the first parser, and then the second synchronisation parser. If the first parser fails to parse, tokens from the input stream will be dropped until second parser succeeds.
    -- | ConsumeRecoverOpt can be used to specify whether if, upon failure when no tokens have been consumed, the parser should try and recover. If it is being used within an Alternative, it should not as otherwise it will not try other alternates, and instead try to force usage of the given one.
    endRecover :: ConsumeRecoverOpt -> m a -> m sync -> a -> m a

    -- | Parses the first parser. If the first parser fails to parse, tokens from the input stream will be dropped until second parser succeeds.
    -- | ConsumeRecoverOpt can be used to specify whether if, upon failure when no tokens have been consumed, the parser should try and recover. If it is being used within an Alternative, it should not as otherwise it will not try other alternates, and instead try to force usage of the given one.
    midRecover :: ConsumeRecoverOpt -> m a -> m sync -> m a


buildJump :: Monad m => ((a -> RecoveryParserT e s m b) -> RecoveryParserT e s m a) -> RecoveryParserT e s m a
buildJump x = RecoveryParserT $ ParsecT $ \s cok cerr eok eerr -> let
        cont ret = RecoveryParserT (ParsecT $ \s' _ _ _ _ -> unsafeCoerce cok ret s' [])
    in
        unParser (unRecoveryParser $ x cont) s cok cerr eok eerr

runRecoveryParser :: Monad m => RecoveryParserT e s m a -> String -> s -> m (Either (ParseErrorBundle s e) a)
runRecoveryParser pX f s= runReaderT (runParserT (unRecoveryParser pX) f s) []

runRecoveryParser' :: Monad m => RecoveryParserT e s m a -> State s e -> m (State s e, Either (ParseErrorBundle s e) a)
runRecoveryParser' pX s = runReaderT (runParserT' (unRecoveryParser pX) s) []

-- | Searches for the given synchronisation token. If it finds the token somewhere in the input stream then it will return True, and drop until after the found token. If it doesn't exist, it will return false at the point it finished searching.
findSyncToken :: (Ord e, Stream s, Monad m) => ParseError s e -> RecoveryParserT e s m a
findSyncToken err = do
    syncToks <- RecoveryParserT $ lift ask
    choice (fmap (\sync -> undefined <$ sync err) syncToks)
        <|> (anySingle *> findSyncToken err)
        <|> parseError err

withRecovery' :: Stream s
    => (ParseError s e -> ParsecT e s m a)
    -> (ParseError s e -> ParsecT e s m a)
    -> ParsecT e s m a
    -> ParsecT e s m a
withRecovery' cRecF eRecF p = ParsecT $ \s cok cerr eok eerr ->
    let mcerr err ms =
            let rcok x s' _ = cok x s' mempty
                rcerr _ _ = cerr err ms
                reok x s' _ = eok x s' (toHints (stateOffset  s') err)
                reerr _ _ = cerr err ms
            in unParser (cRecF err) ms rcok rcerr reok reerr
        meerr err ms =
            let rcok x s' _ = cok x s' (toHints (stateOffset s') err)
                rcerr _ _ = eerr err ms
                reok x s' _ = eok x s' (toHints (stateOffset s') err)
                reerr _ _ = eerr err ms
            in unParser (eRecF err) ms rcok rcerr reok reerr
    in unParser p s cok mcerr eok meerr

instance (Eq s, Ord e, Monad m, Stream s) => MonadRecover e s (RecoveryParserT e s m) where
    endRecover consumeOpt pX pSync def = buildJump $ \contF -> RecoveryParserT $
            local ((\err -> pSync >> registerParseError err >> contF def):) $
                withRecovery' recoveryFunc eRecoveryFunc (unRecoveryParser pX <* unRecoveryParser pSync)
        where
            eRecoveryFunc = case consumeOpt of
                AlternativeSafe -> parseError
                ForceRecovery   -> recoveryFunc
            recoveryFunc = unRecoveryParser . findSyncToken

    midRecover consumeOpt pX pSync = buildJump $ \contF -> RecoveryParserT $
            local ((\err -> pSync >> registerParseError err >> pX >>= contF):) $
                withRecovery' recoveryFunc eRecoveryFunc (unRecoveryParser pX)
        where
            eRecoveryFunc = case consumeOpt of
                AlternativeSafe -> parseError
                ForceRecovery   -> recoveryFunc
            recoveryFunc = unRecoveryParser . findSyncToken

-- | Parses the first parser. If the first parser fails to parse, tokens from the input stream will be dropped until second parser succeeds, and then the first parser will be parsed again.
-- | ConsumeRecoverOpt can be used to specify whether if, upon failure when no tokens have been consumed, the parser should try and recover. If it is being used within an Alternative, it should not as otherwise it will not try other alternates, and instead try to force usage of the given one.
endRecoverMaybe :: MonadRecover e s m => ConsumeRecoverOpt -> m a -> m sync -> m (Maybe a)
endRecoverMaybe consumeOpt pX pSync = endRecover consumeOpt  (Just <$> pX) pSync Nothing

-- Parses the open parser, the middle parser and then the end parser. if the middle parser fails, then the recovery will be attempted using the end parser to synchronise.
betweenSync :: MonadRecover e s m => m open -> m close -> m a -> m (Maybe a)
betweenSync pOpen pClose pX = pOpen *> endRecoverMaybe ForceRecovery pX (void pClose)

-- Parses 0 or more occurences of the first parser, then the separator parser. If the first parser fails
endBySync :: MonadRecover e s m => m a -> m sync -> m [Maybe a]
endBySync pX pSep = many (endRecoverMaybe AlternativeSafe pX (void pSep))

endBy1Sync :: MonadRecover e s m => m a -> m sync -> m [Maybe a]
endBy1Sync pX pSep = some (endRecoverMaybe AlternativeSafe pX pSep)

sepBy1Sync :: MonadRecover e s m => m a -> m sync -> m [a]
sepBy1Sync pX pSep = (:) <$> midRecover AlternativeSafe pX pSep <*> many (pSep *> midRecover AlternativeSafe pX pSep)

sepBySync :: MonadRecover e s m => m a -> m sync -> m [a]
sepBySync pX pSep = sepBy1Sync pX pSep <|> pure [] --This requires only doing recovery when info has been consumed
