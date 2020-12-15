{-# LANGUAGE FlexibleContexts #-}

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
module Text.ParserRecovery (ConsumeRecoverOpt(..), endRecover, midRecover, endRecoverMaybe, betweenSync, endBy1Sync, endBySync, sepBy1Sync, sepBySync, findSyncToken) where

import           Data.Maybe
import           Debug.Trace
import           Text.Megaparsec

data ConsumeRecoverOpt = ForceRecovery | AlternativeSafe
    deriving Eq

-- | Searches for the given synchronisation token. If it finds the token somewhere in the input stream then it will return True, and drop until after the found token. If it doesn't exist, it will return false at the point it finished searching.
findSyncToken :: (Ord e, Stream s, MonadParsec e s p)
    => p sync -- ^ A parser that searches for the given synchronisation token. Must either parse it consuming input, or fail to parse and parse NO input.
    -> p Bool
findSyncToken pSync = do
    res <- observing pSync
    case res of
        (Right _) -> return True
        (Left _) -> do
            res' <- observing anySingle
            case res' of
                (Right _) -> findSyncToken pSync
                (Left _)  -> return False

-- | Parses the first parser, and then the second synchronisation parser. If the first parser fails to parse, tokens from the input stream will be dropped until second parser succeeds.
-- | ConsumeRecoverOpt can be used to specify whether if, upon failure when no tokens have been consumed, the parser should try and recover. If it is being used within an Alternative, it should not as otherwise it will not try other alternates, and instead try to force usage of the given one.
endRecover :: (Show (Token s), Show e, Show s, Eq s, Ord e, Stream s, MonadParsec e s p) => ConsumeRecoverOpt -> p a -> p sync -> a -> p a
endRecover consumeOpt pX pSync def = do
    stateBeforeParse <- getParserState
    withRecovery (recoveryFunc stateBeforeParse) (pX <* pSync)
    where
        recoveryFunc stateBeforeParse err = do
            stateAfterParse <- getParserState
            if stateBeforeParse == stateAfterParse && consumeOpt == AlternativeSafe then --TODO: try to find a way to check if tokens consumed that is more efficient.
                parseError err
            else do
                foundSync <- findSyncToken pSync
                if foundSync then do
                    registerParseError err
                    return def
                else parseError err

-- | Parses the first parser. If the first parser fails to parse, tokens from the input stream will be dropped until second parser succeeds.
-- | ConsumeRecoverOpt can be used to specify whether if, upon failure when no tokens have been consumed, the parser should try and recover. If it is being used within an Alternative, it should not as otherwise it will not try other alternates, and instead try to force usage of the given one.
midRecover :: (Show (Token s), Show e, Show s, Eq s, Ord e, Stream s, MonadParsec e s p) => ConsumeRecoverOpt -> p a -> p sync -> p a
midRecover consumeOpt pX pSync = do
    stateBeforeParse <- getParserState
    withRecovery (recoveryFunc stateBeforeParse) pX
    where
        recoveryFunc stateBeforeParse err = do
            stateAfterParse <- getParserState
            if stateBeforeParse == stateAfterParse && consumeOpt == AlternativeSafe then
                parseError err
            else do
                foundSync <- findSyncToken pSync
                if foundSync then do
                    registerParseError err
                    midRecover consumeOpt pX pSync
                else parseError err

-- | Parses the first parser. If the first parser fails to parse, tokens from the input stream will be dropped until second parser succeeds, and then the first parser will be parsed again.
-- | ConsumeRecoverOpt can be used to specify whether if, upon failure when no tokens have been consumed, the parser should try and recover. If it is being used within an Alternative, it should not as otherwise it will not try other alternates, and instead try to force usage of the given one.
endRecoverMaybe :: (Show (Token s), Show e, Show s, Eq s, Ord e, Stream s, MonadParsec e s p) => ConsumeRecoverOpt -> p a -> p sync -> p (Maybe a)
endRecoverMaybe consumeOpt pX pSync = endRecover consumeOpt  (Just <$> pX) pSync Nothing

-- Parses the open parser, the middle parser and then the end parser. if the middle parser fails, then the recovery will be attempted using the end parser to synchronise.
betweenSync :: (Show (Token s), Show e, Show s, Eq s, Ord e, Stream s, MonadParsec e s p) => p open -> p close -> p a -> p (Maybe a)
betweenSync pOpen pClose pX = pOpen *> endRecoverMaybe ForceRecovery pX pClose

-- Parses 0 or more occurences of the first parser, then the separator parser. If the first parser fails
endBySync :: (Show (Token s), Show e, Show s, Eq s, Ord e, Stream s, MonadParsec e s p) => p a -> p sep -> p [Maybe a]
endBySync pX pSep = many (endRecoverMaybe AlternativeSafe pX pSep)

endBy1Sync :: (Show (Token s), Show e, Show s, Eq s, Ord e, Stream s, MonadParsec e s p) => p a -> p sep -> p [Maybe a]
endBy1Sync pX pSep = some (endRecoverMaybe AlternativeSafe pX pSep)

sepBy1Sync :: (Show (Token s), Show e, Show s, Eq s, Ord e, Stream s, MonadParsec e s p)=> p a -> p sync -> p [a]
sepBy1Sync pX pSep = (:) <$> midRecover AlternativeSafe pX pSep <*> many (pSep *> midRecover AlternativeSafe pX pSep)

sepBySync :: (Show (Token s), Show e, Show s, Eq s, Ord e, Stream s, MonadParsec e s p) => p a -> p sync -> p [a]
sepBySync pX pSep = sepBy1Sync pX pSep <|> pure [] --This requires only doing recovery when info has been consumed
