import qualified BlockParserTest
import Efyu.Utils (debugM)
import qualified ExprParserTest
import Test.Hspec
import qualified TypeTest

main :: IO ()
main = hspec $ do
  debugM "init"
  BlockParserTest.tests
  ExprParserTest.tests
  TypeTest.tests
