{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Text.Megaparsec

data ConsumeRecoverOpt = ForceRecovery | AlternativeSafe
    deriving Eq

findSyncToken :: (Ord e, Stream s, MonadParsec e s p) => p sync -> p Bool
findSyncToken pSync = do
    res <- observing pSync
    case res of
        (Right _) -> return True
        (Left _) -> do
            res' <- observing anySingle
            case res' of
                (Right _) -> findSyncToken pSync
                (Left _)  -> return False

endRecover :: (Eq s, Ord e, Stream s, MonadParsec e s p) => ConsumeRecoverOpt -> p a -> p sync -> a -> p a
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
                else
                    parseError err

midRecover :: (Eq s, Ord e, Stream s, MonadParsec e s p) => ConsumeRecoverOpt -> p a -> p sync -> p a
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
                else
                    parseError err

endRecoverMaybe :: (Eq s, Ord e, Stream s, MonadParsec e s p) => ConsumeRecoverOpt -> p a -> p sync -> p (Maybe a)
endRecoverMaybe consumeOpt pX pSync = endRecover consumeOpt  (Just <$> pX) pSync Nothing

betweenSync :: (Eq s, Ord e, Stream s, MonadParsec e s p) => p open -> p close -> p a -> p (Maybe a)
betweenSync pOpen pClose pX = pOpen *> endRecoverMaybe ForceRecovery pX pClose

endBySync :: (Eq s, Ord e, Stream s, MonadParsec e s p) => p a -> p sep -> p [Maybe a]
endBySync pX pSep = many (endRecoverMaybe AlternativeSafe pX pSep)

endBy1Sync :: (Eq s, Ord e, Stream s, MonadParsec e s p) => p a -> p sep -> p [Maybe a]
endBy1Sync pX pSep = some (endRecoverMaybe AlternativeSafe pX pSep)

sepBy1Sync :: (Eq s, Ord e, Stream s, MonadParsec e s p) => p a -> p sync -> p [a]
sepBy1Sync pX pSep = (:) <$> midRecover AlternativeSafe pX pSep <*> many (pSep *> midRecover AlternativeSafe pX pSep)

sepBySync :: (Eq s, Ord e, Stream s, MonadParsec e s p) => p a -> p sync -> p [a]
sepBySync pX pSep = sepBy1Sync pX pSep <|> pure [] --This requires only doing recovery when info has been consumed
