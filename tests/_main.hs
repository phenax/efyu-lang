import qualified BlockParserTest
import qualified ExprParserTest
import Test.Hspec
import qualified TypeTest

main :: IO ()
main = hspec $ do
  BlockParserTest.tests
  ExprParserTest.tests
  TypeTest.tests
