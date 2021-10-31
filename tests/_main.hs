import qualified ExprParserTest
import Test.Hspec
import qualified TypeTest

main :: IO ()
main = hspec $ do
  ExprParserTest.tests
  TypeTest.tests
