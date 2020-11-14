import           Test.Hspec
import qualified Text.ParserRecoverySpec

main :: IO ()
main = hspec $ do
    describe "Text" $ do
        describe "ParserRecovery" $ do
            Text.ParserRecoverySpec.spec
