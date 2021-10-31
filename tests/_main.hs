import qualified ExprParserTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  ExprParserTest.tests
