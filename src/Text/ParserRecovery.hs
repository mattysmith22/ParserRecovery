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
module Text.ParserRecovery (endRecover, midRecover, endRecoverMaybe, betweenSync, endBySync, sepBy1Sync, sepBySync, findSyncToken) where

import           Data.Maybe
import qualified Data.Set        as Set
import           Text.Megaparsec

findSyncToken :: (Ord e, Stream s, MonadParsec e s p) => p sync -> p Bool
findSyncToken pSync = do
    res <- observing pSync
    case res of
        (Right _) -> return True
        (Left _) -> do
            res' <- observing anyTok
            case res' of
                (Right _) -> findSyncToken pSync
                (Left _)  -> return False
    where
        anyTok = token (const $ Just ()) Set.empty

endRecover :: (Eq s, Ord e, Stream s, MonadParsec e s p) => p a -> p sync -> a -> p a
endRecover pX pSync def = do
    stateBeforeParse <- getParserState
    withRecovery (recoveryFunc stateBeforeParse) (pX <* pSync)
    where
        recoveryFunc stateBeforeParse err = do
            stateAfterParse <- getParserState
            if stateBeforeParse == stateAfterParse then --TODO: try to find a way to check if tokens consumed that is more efficient.
                parseError err
            else do
                foundSync <- findSyncToken pSync
                if foundSync then
                    return def
                else
                    parseError err

midRecover :: (Eq s, Ord e, Stream s, MonadParsec e s p) => p a -> p sync -> p a
midRecover pX pSync = do
    stateBeforeParse <- getParserState
    withRecovery (recoveryFunc stateBeforeParse) pX
    where
        recoveryFunc stateBeforeParse err = do
            stateAfterParse <- getParserState
            if stateBeforeParse == stateAfterParse then
                parseError err
            else do
                foundSync <- findSyncToken pSync
                if foundSync then
                    midRecover pX pSync
                else
                    parseError err

endRecoverMaybe :: (Eq s, Ord e, Stream s, MonadParsec e s p) => p a -> p sync -> p (Maybe a)
endRecoverMaybe pX pSync = endRecover (Just <$> pX) pSync Nothing

betweenSync :: (Eq s, Ord e, Stream s, MonadParsec e s p) => p open -> p close -> p a -> p (Maybe a)
betweenSync pOpen pClose pX = pOpen *> endRecoverMaybe pX pClose

endBySync :: (Eq s, Ord e, Stream s, MonadParsec e s p) => p a -> p sep -> p [a]
endBySync pX pSep = catMaybes <$> many (endRecoverMaybe pX pSep)

sepBy1Sync :: (Eq s, Ord e, Stream s, MonadParsec e s p) => p a -> p sync -> p [a]
sepBy1Sync pX pSep = (:) <$> midRecover pX pSep <*> many (pSep *> midRecover pX pSep)

sepBySync :: (Eq s, Ord e, Stream s, MonadParsec e s p) => p a -> p sync -> p [a]
sepBySync pX pSep = sepBy1Sync pX pSep <|> pure [] --This requires only doing recovery when info has been consumed
