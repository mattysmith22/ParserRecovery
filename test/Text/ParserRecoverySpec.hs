module Text.ParserRecoverySpec (spec) where

import qualified Data.List.NonEmpty    as NE
import           Data.Maybe
import qualified Data.Set              as Set
import           Data.Void
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.ParserRecovery

-- As a standard for all these tests, `s` is a synchronisation token and "ab" is a valide parse. 'a' and 'b' are parsed separately to allow for the differentiation between consuming input and not.
-- Since the parse error is only deferred, and still returns a failure eventually when evaluated with `runParser`, to check that a parser has recovered we check that a parser afterwards runs (by seeing it consumes input)

runParserRec p = runParser (runRecoveryParser p)
runParserRec' p = runParser' (runRecoveryParser p)

pVal :: RecoveryParserT Void (Parsec Void String) Char
pVal = char 'a' *> char 'b'

pSync :: RecoveryParserT Void (Parsec Void String) Char
pSync = char 's'

pAlt :: RecoveryParserT Void (Parsec Void String) [Char]
pAlt = (:[]) <$> char 'c'

pAltM :: RecoveryParserT Void (Parsec Void String) [Maybe Char]
pAltM = (:[]) . Just <$> char 'c'

pOpen :: RecoveryParserT Void (Parsec Void String) Char
pOpen = char '('

pClose :: RecoveryParserT Void (Parsec Void String) Char
pClose = char ')'

pOpen' :: RecoveryParserT Void (Parsec Void String) Char
pOpen' = char '['

pClose' :: RecoveryParserT Void (Parsec Void String) Char
pClose' = char ']'

endRecoverSpec :: Spec
endRecoverSpec = do
    it "Should parse the input parser, then the synchronisation token" $ do
        runParserRec (endRecover AlternativeSafe pVal pSync '?') "" "absab" `shouldParse` 'b'
        runParserRec' (endRecover AlternativeSafe pVal pSync '?') (initialState "absab") `succeedsLeaving` "ab"
    it "Should be compatible with the alternative syntax (if input parser fails without consuming input continue failure)" $ do
        runParserRec (endRecover AlternativeSafe pVal pSync '?' <|> char 'c') "" "cbsab" `shouldParse` 'c'
        runParserRec' (endRecover AlternativeSafe pVal pSync '?' <|> char 'c') (initialState "cbsab") `succeedsLeaving` "bsab"
    it "Should the parser fail, should find a the synchronisation token, stop at that point and return the default" $ do
        runParserRec (endRecover AlternativeSafe pVal pSync '?' *> pVal) "" `shouldFailOn` "acsab"
        runParserRec' (endRecover AlternativeSafe pVal pSync '?' *> pVal) (initialState "acsab") `failsLeaving` ""
    it "Should the parser fail and there is no synchronisation token, just return the original error message." $ do
        runParserRec (endRecover AlternativeSafe pVal pSync '?') "" `shouldFailOn` "acabad"
        --Currently leaving leftover state undefined

midRecoverSpec :: Spec
midRecoverSpec = do
    it "Should parse the input parser" $ do
        runParserRec (midRecover AlternativeSafe pVal pSync) "" "absab" `shouldParse` 'b'
        runParserRec' (midRecover AlternativeSafe pVal pSync) (initialState "absab") `succeedsLeaving` "sab"
    it "Should be compatible with the alternative syntax (if input parser fails without consuming input continue failure)" $ do
        runParserRec (endRecover AlternativeSafe pVal pSync '?' <|> char 'c') "" "cbsab" `shouldParse` 'c'
        runParserRec' (endRecover AlternativeSafe pVal pSync '?' <|> char 'c') (initialState "cbsab") `succeedsLeaving` "bsab"
    it "Should the parser fail, should find a the synchronisation token, stop at that point and try and parse again" $ do
        runParserRec (midRecover AlternativeSafe pVal pSync *> pVal) "" `shouldFailOn` "acsab"
        runParserRec' (midRecover AlternativeSafe pVal pSync *> pVal) (initialState "acsab") `failsLeaving` ""
    it "Should the parser fail and there is no synchronisation token, just return the original error message." $ do
        runParserRec (endRecover AlternativeSafe pVal pSync '?') "" `shouldFailOn` "acabc"
        --Currently leaving leftover state undefined

endRecoverMaybeSpec :: Spec
endRecoverMaybeSpec = do
    it "Should parse the input parser, then the synchronisation token" $ do
        runParserRec (endRecoverMaybe AlternativeSafe pVal pSync) "" "absab" `shouldParse` Just 'b'
        runParserRec' (endRecoverMaybe AlternativeSafe pVal pSync) (initialState "absab") `succeedsLeaving` "ab"
    it "Should be compatible with the alternative syntax (if input parser fails without consuming input continue failure) if in AlternativeSafe mode" $ do
        runParserRec (endRecoverMaybe AlternativeSafe pVal pSync <|> (Just <$> char 'c')) "" "cbsab" `shouldParse` Just 'c'
        runParserRec' (endRecoverMaybe AlternativeSafe pVal pSync <|> (Just <$> char 'c')) (initialState "cbsab") `succeedsLeaving` "bsab"
    it "Should not be compatible with the alternative syntax (if input parser fails without consuming input then recover) if in ForceRecovery mode" $ do
        runParserRec (endRecoverMaybe ForceRecovery pVal pSync <|> (Just <$> char 'c')) "" `shouldFailOn` "cbsab"
        runParserRec' (endRecoverMaybe ForceRecovery pVal pSync <|> (Just <$> char 'c')) (initialState "cbsabe") `failsLeaving` "abe"
    it "Should the parser fail, should find a the synchronisation token, stop at that point and return the default" $ do
        runParserRec (endRecoverMaybe AlternativeSafe pVal pSync *> pVal) "" `shouldFailOn` "acsab"
        runParserRec' (endRecoverMaybe AlternativeSafe pVal pSync *> pVal) (initialState "acsab") `failsLeaving` ""
    it "Should the parser fail and there is no synchronisation token, just return the original error message." $ do
        runParserRec (endRecoverMaybe AlternativeSafe pVal pSync) "" `shouldFailOn` "acabad"
        --Currently leaving leftover state undefined

betweenSyncSpec :: Spec
betweenSyncSpec = do
    it "Should corectly parse correct input" $ do
        runParserRec (betweenSync pOpen pClose pVal) "" "(ab)e" `shouldParse` Just 'b'
        runParserRec' (betweenSync pOpen pClose pVal) (initialState "(ab)e") `succeedsLeaving` "e"
    it "Should be compatible with the alternative syntax" $ do
        runParserRec (betweenSync pOpen pClose pVal <|> (Just <$> char 'c')) "" "ce" `shouldParse` Just 'c'
        runParserRec' (betweenSync pOpen pClose pVal <|> (Just <$> char 'c')) (initialState "ce") `succeedsLeaving` "e"
    it "Should recover no input between pOpen and pClose" $ do
        runParserRec (betweenSync pOpen pClose pVal *> pVal) "()ab" `shouldFailOn` "absab"
        runParserRec' (betweenSync pOpen pClose pVal *> pVal) (initialState "()abe") `failsLeaving` "e"
    it "Should recover with a fail parse with no consumption between pOpen and pClose" $ do
        runParserRec (betweenSync pOpen pClose pVal *> pVal) "(c)ab" `shouldFailOn` "absab"
        runParserRec' (betweenSync pOpen pClose pVal *> pVal) (initialState "(c)abe") `failsLeaving` "e"
    it "Should recover with a fail parse with consumption between pOpen and pClose" $ do
        runParserRec (betweenSync pOpen pClose pVal *> pVal) "(ac)ab" `shouldFailOn` "absab"
        runParserRec' (betweenSync pOpen pClose pVal *> pVal) (initialState "(ac)abe") `failsLeaving` "e"

endBySyncSpec :: Spec
endBySyncSpec = do
    it "Should correctly parse empty input" $ do
        runParserRec (endBySync pVal pSync) "" "e" `shouldParse` []
        runParserRec' (endBySync pVal pSync) (initialState "e") `succeedsLeaving` "e"
    it "Should correctly parse correct input" $ do
        runParserRec (endBySync pVal pSync) "" "abse" `shouldParse` [Just 'b']
        runParserRec' (endBySync pVal pSync) (initialState "abse") `succeedsLeaving` "e"
        runParserRec (endBySync pVal pSync) "" "absabse" `shouldParse` [Just 'b', Just 'b']
        runParserRec' (endBySync pVal pSync) (initialState "absabse") `succeedsLeaving` "e"
    it "Should recover a fail parse with consumption between two parsers" $ do
        runParserRec (endBySync pVal pSync <* pAlt) "" `shouldFailOn` "absacsce"
        runParserRec' (endBySync pVal pSync <* pAlt) (initialState "absacsce") `failsLeaving` "e"

endBy1SyncSpec :: Spec
endBy1SyncSpec = do
    it "Should allow for alternative syntax" $ do
        runParserRec (endBy1Sync pVal pSync <|> pAltM) "" "ce" `shouldParse` [Just 'c']
        runParserRec' (endBy1Sync pVal pSync <|> pAltM) (initialState "ce") `succeedsLeaving` "e"
    it "Should correctly parse correct input" $ do
        runParserRec (endBy1Sync pVal pSync) "" "abse" `shouldParse` [Just 'b']
        runParserRec' (endBy1Sync pVal pSync) (initialState "abse") `succeedsLeaving` "e"
        runParserRec (endBy1Sync pVal pSync) "" "absabse" `shouldParse` [Just 'b', Just 'b']
        runParserRec' (endBy1Sync pVal pSync) (initialState "absabse") `succeedsLeaving` "e"
    it "Should recover a fail parse with consumption between two parsers" $ do
        runParserRec (endBy1Sync pVal pSync <* pAlt) "" `shouldFailOn` "absacsce"
        runParserRec' (endBy1Sync pVal pSync <* pAlt) (initialState "absacsce") `failsLeaving` "e"

sepBy1SyncSpec :: Spec
sepBy1SyncSpec = do
    it "Should allow for alternative syntax" $ do
        runParserRec (sepBy1Sync pVal pSync <|> pAlt) "" "ce" `shouldParse` ['c']
        runParserRec' (sepBy1Sync pVal pSync <|> pAlt) (initialState "ce") `succeedsLeaving` "e"
    it "Should correctly parse correct input" $ do
        runParserRec (sepBy1Sync pVal pSync) "" "abe" `shouldParse` ['b']
        runParserRec' (sepBy1Sync pVal pSync) (initialState "abe") `succeedsLeaving` "e"
        runParserRec (sepBy1Sync pVal pSync) "" "absabe" `shouldParse` ['b', 'b']
        runParserRec' (sepBy1Sync pVal pSync) (initialState "absabe") `succeedsLeaving` "e"
    it "Should recover a fail parse with consumption between two parsers" $ do
        runParserRec (sepBy1Sync pVal pSync <* pAlt) "" `shouldFailOn` "absacsabce"
        runParserRec' (sepBy1Sync pVal pSync <* pAlt) (initialState "absacsabce") `failsLeaving` "e"

sepBySyncSpec :: Spec
sepBySyncSpec = do
    it "Should correctly parse empty input" $ do
        runParserRec (sepBySync pVal pSync) "" "e" `shouldParse` []
        runParserRec' (sepBySync pVal pSync) (initialState "e") `succeedsLeaving` "e"
    it "Should correctly parse correct input" $ do
        runParserRec (sepBy1Sync pVal pSync) "" "abe" `shouldParse` ['b']
        runParserRec' (sepBy1Sync pVal pSync) (initialState "abe") `succeedsLeaving` "e"
        runParserRec (sepBy1Sync pVal pSync) "" "absabe" `shouldParse` ['b', 'b']
        runParserRec' (sepBy1Sync pVal pSync) (initialState "absabe") `succeedsLeaving` "e"
    it "Should recover a fail parse with consumption between two parsers" $ do
        runParserRec (sepBy1Sync pVal pSync <* pAlt) "" `shouldFailOn` "absacsabce"
        runParserRec' (sepBy1Sync pVal pSync <* pAlt) (initialState "absacsabce") `failsLeaving` "e"

multipleRecoverySpec = do
    it "Should be able to handle nested recovery (endRecover)" $ do
        runParserRec' (endRecoverMaybe AlternativeSafe
            (endRecoverMaybe AlternativeSafe pVal pClose)
            pClose' >> pVal) (initialState "ac]ace") `failsLeaving` "ce"
        runParserRec (endRecoverMaybe AlternativeSafe
            (endRecoverMaybe AlternativeSafe pVal pClose)
            pClose' >> pVal) "" "ac]ace" `shouldFailWithM`
            [
                TrivialError 1 (Just $ Tokens (NE.fromList "c")) (Set.fromList [Tokens (NE.fromList "b")]),
                TrivialError 4 (Just $ Tokens (NE.fromList "c")) (Set.fromList [Tokens (NE.fromList "b")])
            ]
    it "Should be able to handle nested recovery (midRecover)" $ do
        runParserRec (midRecover AlternativeSafe
            (midRecover AlternativeSafe pVal pClose)
            pClose' >> pVal) "" "ac]abac" `shouldFailWithM` [
                TrivialError 1 (Just $ Tokens (NE.fromList "c")) (Set.fromList [Tokens (NE.fromList "b")]),
                TrivialError 6 (Just $ Tokens (NE.fromList "c")) (Set.fromList [Tokens (NE.fromList "b")])
            ]

spec :: Spec
spec = do
    describe "endRecover" $ do
        endRecoverSpec
    describe "midRecover" $ do
        midRecoverSpec
    describe "endRecoverMaybe" $ do
        endRecoverMaybeSpec
    describe "betweenSync" $ do
        betweenSyncSpec
    describe "endBySync" $ do
        endBySyncSpec
    describe "endBy1Sync" $ do
        endBy1SyncSpec
    describe "sepBy1Spec" $ do
        sepBy1SyncSpec
    describe "sepBySync" $ do
        sepBySyncSpec
    describe "multipleRecovery" $ do
        multipleRecoverySpec
