{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

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
module Text.ParserRecovery (ConsumeRecoverOpt(..), RecoveryParserT, endRecover, midRecover, endRecoverMaybe, betweenSync, endBy1Sync, endBySync, sepBy1Sync, sepBySync, findSyncToken, runRecoveryParser) where

import           Control.Applicative  (Alternative)
import           Control.Monad.Reader
import           Data.Either
import           Debug.Trace
import           Text.Megaparsec

data ConsumeRecoverOpt = ForceRecovery | AlternativeSafe
    deriving Eq
type RecoverEnv e m = [RecoveryParserT e m ()]

newtype RecoveryParserT e m a = RecoveryParserT {
    unRecoveryParser :: ReaderT (RecoverEnv e m) m a
}

instance MonadParsec e s m => Functor (RecoveryParserT e m) where
    fmap f x = RecoveryParserT (f <$> unRecoveryParser x)

instance MonadParsec e s m => Applicative (RecoveryParserT e m) where
    l <*> r = RecoveryParserT (unRecoveryParser l <*> unRecoveryParser r)
    pure x = RecoveryParserT $ pure x

instance MonadParsec e s m => Monad (RecoveryParserT e m) where
    x >>= f = RecoveryParserT (unRecoveryParser x >>= (unRecoveryParser . f))

instance MonadParsec e s m => Alternative (RecoveryParserT e m) where
    l <|> r = RecoveryParserT (unRecoveryParser l <|> unRecoveryParser r)
    empty = RecoveryParserT empty

instance MonadParsec e s m => MonadPlus (RecoveryParserT e m) where
   mzero = RecoveryParserT mzero
   mplus l r = RecoveryParserT $ mplus (unRecoveryParser l) (unRecoveryParser r)

instance (MonadFail m, MonadParsec e s m) => MonadFail (RecoveryParserT e m) where
    fail s = RecoveryParserT $ fail s

instance MonadParsec e s m => MonadParsec e s (RecoveryParserT e m) where
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

instance MonadParsec e s m => MonadReader (RecoverEnv e m) (RecoveryParserT e m) where
    ask = RecoveryParserT ask
    local f pX = RecoveryParserT $ local f $ unRecoveryParser pX
    reader f = RecoveryParserT $ reader f

class MonadParsec e s m => MonadRecover e s m where
    -- | Parses the first parser, and then the second synchronisation parser. If the first parser fails to parse, tokens from the input stream will be dropped until second parser succeeds.
    -- | ConsumeRecoverOpt can be used to specify whether if, upon failure when no tokens have been consumed, the parser should try and recover. If it is being used within an Alternative, it should not as otherwise it will not try other alternates, and instead try to force usage of the given one.
    endRecover :: ConsumeRecoverOpt -> m a -> m sync -> a -> m a

    -- | Parses the first parser. If the first parser fails to parse, tokens from the input stream will be dropped until second parser succeeds.
    -- | ConsumeRecoverOpt can be used to specify whether if, upon failure when no tokens have been consumed, the parser should try and recover. If it is being used within an Alternative, it should not as otherwise it will not try other alternates, and instead try to force usage of the given one.
    midRecover :: ConsumeRecoverOpt -> m a -> m sync -> m a

runRecoveryParser :: MonadParsec e s m => RecoveryParserT e m a -> m a
runRecoveryParser pX = runReaderT (unRecoveryParser pX) []

-- | Searches for the given synchronisation token. If it finds the token somewhere in the input stream then it will return True, and drop until after the found token. If it doesn't exist, it will return false at the point it finished searching.
findSyncToken :: MonadParsec e s m
    => RecoveryParserT e m (Maybe Int)
findSyncToken = do
    syncToks <- ask
    res <- choice (zipWith (<$) (fmap Just [0..]) (fmap lookAhead $ syncToks) ++ [anySingle *> findSyncToken, pure Nothing])
    return res

instance (Eq s, Ord e, MonadParsec e s m) => MonadRecover e s (RecoveryParserT e m) where
    endRecover consumeOpt pX pSync def = do
        stateBeforeParse <- getParserState
        local (void pSync:) $
            withRecovery (recoveryFunc stateBeforeParse) (pX <* pSync)
        where
            recoveryFunc stateBeforeParse err = do
                stateAfterParse <- getParserState
                if stateBeforeParse == stateAfterParse && consumeOpt == AlternativeSafe then --TODO: try to find a way to check if tokens consumed that is more efficient.
                    parseError err
                else do
                    foundSync <- findSyncToken
                    case foundSync of
                        Nothing -> parseError err
                        Just 0 -> do
                            registerParseError err
                            def <$ pSync
                        Just x -> parseError err

    midRecover consumeOpt pX pSync = do
        stateBeforeParse <- getParserState
        local (void pSync:) $
            withRecovery (recoveryFunc stateBeforeParse) pX
        where
            recoveryFunc stateBeforeParse err = do
                stateAfterParse <- getParserState
                if stateBeforeParse == stateAfterParse && consumeOpt == AlternativeSafe then
                    parseError err
                else do
                    foundSync <- findSyncToken
                    case foundSync of
                        Nothing  -> parseError err
                        Just 0 -> do
                            registerParseError err
                            pSync
                            stateBeforeParse <- getParserState
                            withRecovery (recoveryFunc stateBeforeParse) pX
                        Just _ -> parseError err

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
