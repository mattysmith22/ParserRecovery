module Text.ParserRecoverySpec (spec) where

import           Data.Void
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.ParserRecovery

-- As a standard for all these tests, `s` is a synchronisation token and "ab" is a valide parse. 'a' and 'b' are parsed separately to allow for the differentiation between consuming input and not.
-- Since the parse error is only deferred, and still returns a failure eventually when evaluated with `runParser`, to check that a parser has recovered we check that a parser afterwards runs (by seeing it consumes input)

findSyncTokenSpec :: Spec
findSyncTokenSpec = do
    it "Should parse an initial sync token, with parse state after sync" $ do
        runParser (findSyncToken pSync) "" "sab" `shouldParse` True
        runParser' (findSyncToken pSync) (initialState "sab") `succeedsLeaving` "ab"
    it "Should parse a sync token after failures, with parse state after sync" $ do
        runParser (findSyncToken pSync) "" "asab" `shouldParse` True
        runParser' (findSyncToken pSync) (initialState "asab") `succeedsLeaving` "ab"
    it "Should not parse if there is no synchronisation token" $ do
        runParser (findSyncToken pSync) "" "aab" `shouldParse` False
    where
        pSync = char 's' :: Parsec Void String Char

endRecoverSpec :: Spec
endRecoverSpec = do
    it "Should parse the input parser, then the synchronisation token" $ do
        runParser (endRecover AlternativeSafe pVal pSync '?') "" "absab" `shouldParse` 'b'
        runParser' (endRecover AlternativeSafe pVal pSync '?') (initialState "absab") `succeedsLeaving` "ab"
    it "Should be compatible with the alternative syntax (if input parser fails without consuming input continue failure)" $ do
        runParser (endRecover AlternativeSafe pVal pSync '?' <|> char 'c') "" "cbsab" `shouldParse` 'c'
        runParser' (endRecover AlternativeSafe pVal pSync '?' <|> char 'c') (initialState "cbsab") `succeedsLeaving` "bsab"
    it "Should the parser fail, should find a the synchronisation token, stop at that point and return the default" $ do
        runParser (endRecover AlternativeSafe pVal pSync '?' *> pVal) "" `shouldFailOn` "acsab"
        runParser' (endRecover AlternativeSafe pVal pSync '?' *> pVal) (initialState "acsab") `failsLeaving` ""
    it "Should the parser fail and there is no synchronisation token, just return the original error message." $ do
        runParser (endRecover AlternativeSafe pVal pSync '?') "" `shouldFailOn` "acabad"
        --Currently leaving leftover state undefined
    where
        pVal = (char 'a' *> char 'b') :: Parsec Void String Char
        pSync = char 's' :: Parsec Void String Char

midRecoverSpec :: Spec
midRecoverSpec = do
    it "Should parse the input parser" $ do
        runParser (midRecover AlternativeSafe pVal pSync) "" "absab" `shouldParse` 'b'
        runParser' (midRecover AlternativeSafe pVal pSync) (initialState "absab") `succeedsLeaving` "sab"
    it "Should be compatible with the alternative syntax (if input parser fails without consuming input continue failure)" $ do
        runParser (endRecover AlternativeSafe pVal pSync '?' <|> char 'c') "" "cbsab" `shouldParse` 'c'
        runParser' (endRecover AlternativeSafe pVal pSync '?' <|> char 'c') (initialState "cbsab") `succeedsLeaving` "bsab"
    it "Should the parser fail, should find a the synchronisation token, stop at that point and try and parse again" $ do
        runParser (midRecover AlternativeSafe pVal pSync *> pVal) "" `shouldFailOn` "acsab"
        runParser' (midRecover AlternativeSafe pVal pSync *> pVal) (initialState "acsab") `failsLeaving` ""
    it "Should the parser fail and there is no synchronisation token, just return the original error message." $ do
        runParser (endRecover AlternativeSafe pVal pSync '?') "" `shouldFailOn` "acabc"
        --Currently leaving leftover state undefined
    where
        pVal = (char 'a' *> char 'b') :: Parsec Void String Char
        pSync = char 's' :: Parsec Void String Char

endRecoverMaybeSpec :: Spec
endRecoverMaybeSpec = do
    it "Should parse the input parser, then the synchronisation token" $ do
        runParser (endRecoverMaybe AlternativeSafe pVal pSync) "" "absab" `shouldParse` Just 'b'
        runParser' (endRecoverMaybe AlternativeSafe pVal pSync) (initialState "absab") `succeedsLeaving` "ab"
    it "Should be compatible with the alternative syntax (if input parser fails without consuming input continue failure) if in AlternativeSafe mode" $ do
        runParser (endRecoverMaybe AlternativeSafe pVal pSync <|> (Just <$> char 'c')) "" "cbsab" `shouldParse` Just 'c'
        runParser' (endRecoverMaybe AlternativeSafe pVal pSync <|> (Just <$> char 'c')) (initialState "cbsab") `succeedsLeaving` "bsab"
    it "Should not be compatible with the alternative syntax (if input parser fails without consuming input then recover) if in ForceRecovery mode" $ do
        runParser (endRecoverMaybe ForceRecovery pVal pSync <|> (Just <$> char 'c')) "" `shouldFailOn` "cbsab"
        runParser' (endRecoverMaybe ForceRecovery pVal pSync <|> (Just <$> char 'c')) (initialState "cbsabe") `failsLeaving` "abe"
    it "Should the parser fail, should find a the synchronisation token, stop at that point and return the default" $ do
        runParser (endRecoverMaybe AlternativeSafe pVal pSync *> pVal) "" `shouldFailOn` "acsab"
        runParser' (endRecoverMaybe AlternativeSafe pVal pSync *> pVal) (initialState "acsab") `failsLeaving` ""
    it "Should the parser fail and there is no synchronisation token, just return the original error message." $ do
        runParser (endRecoverMaybe AlternativeSafe pVal pSync) "" `shouldFailOn` "acabad"
        --Currently leaving leftover state undefined
    where
        pVal = (char 'a' *> char 'b') :: Parsec Void String Char
        pSync = char 's' :: Parsec Void String Char

betweenSyncSpec :: Spec
betweenSyncSpec = do
    it "Should corectly parse correct input" $ do
        runParser (betweenSync pOpen pClose pVal) "" "(ab)e" `shouldParse` Just 'b'
        runParser' (betweenSync pOpen pClose pVal) (initialState "(ab)e") `succeedsLeaving` "e"
    it "Should be compatible with the alternative syntax" $ do
        runParser (betweenSync pOpen pClose pVal <|> (Just <$> char 'c')) "" "ce" `shouldParse` Just 'c'
        runParser' (betweenSync pOpen pClose pVal <|> (Just <$> char 'c')) (initialState "ce") `succeedsLeaving` "e"
    it "Should recover no input between pOpen and pClose" $ do
        runParser (betweenSync pOpen pClose pVal *> pVal) "()ab" `shouldFailOn` "absab"
        runParser' (betweenSync pOpen pClose pVal *> pVal) (initialState "()abe") `failsLeaving` "e"
    it "Should recover with a fail parse with no consumption between pOpen and pClose" $ do
        runParser (betweenSync pOpen pClose pVal *> pVal) "(c)ab" `shouldFailOn` "absab"
        runParser' (betweenSync pOpen pClose pVal *> pVal) (initialState "(c)abe") `failsLeaving` "e"
    it "Should recover with a fail parse with consumption between pOpen and pClose" $ do
        runParser (betweenSync pOpen pClose pVal *> pVal) "(ac)ab" `shouldFailOn` "absab"
        runParser' (betweenSync pOpen pClose pVal *> pVal) (initialState "(ac)abe") `failsLeaving` "e"
    where
        pVal = (char 'a' *> char 'b') :: Parsec Void String Char
        pOpen = char '(' :: Parsec Void String Char
        pClose = char ')' :: Parsec Void String Char

endBySyncSpec :: Spec
endBySyncSpec = do
    it "Should correctly parse empty input" $ do
        runParser (endBySync pVal pSync) "" "e" `shouldParse` []
        runParser' (endBySync pVal pSync) (initialState "e") `succeedsLeaving` "e"
    it "Should correctly parse correct input" $ do
        runParser (endBySync pVal pSync) "" "abse" `shouldParse` [Just 'b']
        runParser' (endBySync pVal pSync) (initialState "abse") `succeedsLeaving` "e"
        runParser (endBySync pVal pSync) "" "absabse" `shouldParse` [Just 'b', Just 'b']
        runParser' (endBySync pVal pSync) (initialState "absabse") `succeedsLeaving` "e"
    it "Should recover a fail parse with consumption between two parsers" $ do
        runParser (endBySync pVal pSync <* pAlt) "" `shouldFailOn` "absacsce"
        runParser' (endBySync pVal pSync <* pAlt) (initialState "absacsce") `failsLeaving` "e"
    where
        pVal = (char 'a' *> char 'b') :: Parsec Void String Char
        pSync = char 's' :: Parsec Void String Char
        pAlt = ((:[]) <$> char 'c') :: Parsec Void String [Char]

endBy1SyncSpec :: Spec
endBy1SyncSpec = do
    it "Should allow for alternative syntax" $ do
        runParser (endBy1Sync pVal pSync <|> pAlt) "" "ce" `shouldParse` [Just 'c']
        runParser' (endBy1Sync pVal pSync <|> pAlt) (initialState "ce") `succeedsLeaving` "e"
    it "Should correctly parse correct input" $ do
        runParser (endBy1Sync pVal pSync) "" "abse" `shouldParse` [Just 'b']
        runParser' (endBy1Sync pVal pSync) (initialState "abse") `succeedsLeaving` "e"
        runParser (endBy1Sync pVal pSync) "" "absabse" `shouldParse` [Just 'b', Just 'b']
        runParser' (endBy1Sync pVal pSync) (initialState "absabse") `succeedsLeaving` "e"
    it "Should recover a fail parse with consumption between two parsers" $ do
        runParser (endBy1Sync pVal pSync <* pAlt) "" `shouldFailOn` "absacsce"
        runParser' (endBy1Sync pVal pSync <* pAlt) (initialState "absacsce") `failsLeaving` "e"
    where
        pVal = (char 'a' *> char 'b') :: Parsec Void String Char
        pSync = char 's' :: Parsec Void String Char
        pAlt = (((:[]) . Just) <$> char 'c') :: Parsec Void String [Maybe Char]

sepBy1SyncSpec :: Spec
sepBy1SyncSpec = do
    it "Should allow for alternative syntax" $ do
        runParser (sepBy1Sync pVal pSync <|> pAlt) "" "ce" `shouldParse` ['c']
        runParser' (sepBy1Sync pVal pSync <|> pAlt) (initialState "ce") `succeedsLeaving` "e"
    it "Should correctly parse correct input" $ do
        runParser (sepBy1Sync pVal pSync) "" "abe" `shouldParse` ['b']
        runParser' (sepBy1Sync pVal pSync) (initialState "abe") `succeedsLeaving` "e"
        runParser (sepBy1Sync pVal pSync) "" "absabe" `shouldParse` ['b', 'b']
        runParser' (sepBy1Sync pVal pSync) (initialState "absabe") `succeedsLeaving` "e"
    it "Should recover a fail parse with consumption between two parsers" $ do
        runParser (sepBy1Sync pVal pSync <* pAlt) "" `shouldFailOn` "absacsabce"
        runParser' (sepBy1Sync pVal pSync <* pAlt) (initialState "absacsabce") `failsLeaving` "e"
    where
        pVal = (char 'a' *> char 'b') :: Parsec Void String Char
        pSync = char 's' :: Parsec Void String Char
        pAlt = ((:[]) <$> char 'c') :: Parsec Void String [Char]

sepBySyncSpec :: Spec
sepBySyncSpec = do
    it "Should correctly parse empty input" $ do
        runParser (sepBySync pVal pSync) "" "e" `shouldParse` []
        runParser' (sepBySync pVal pSync) (initialState "e") `succeedsLeaving` "e"
    it "Should correctly parse correct input" $ do
        runParser (sepBy1Sync pVal pSync) "" "abe" `shouldParse` ['b']
        runParser' (sepBy1Sync pVal pSync) (initialState "abe") `succeedsLeaving` "e"
        runParser (sepBy1Sync pVal pSync) "" "absabe" `shouldParse` ['b', 'b']
        runParser' (sepBy1Sync pVal pSync) (initialState "absabe") `succeedsLeaving` "e"
    it "Should recover a fail parse with consumption between two parsers" $ do
        runParser (sepBy1Sync pVal pSync <* pAlt) "" `shouldFailOn` "absacsabce"
        runParser' (sepBy1Sync pVal pSync <* pAlt) (initialState "absacsabce") `failsLeaving` "e"
    where
        pVal = (char 'a' *> char 'b') :: Parsec Void String Char
        pSync = char 's' :: Parsec Void String Char
        pAlt = ((:[]) <$> char 'c') :: Parsec Void String [Char]


spec :: Spec
spec = do
    describe "findSyncToken" $ do
        findSyncTokenSpec
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
