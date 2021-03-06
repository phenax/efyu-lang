import qualified BlockParserTest
import qualified CodegenTest
import qualified CustomTypeTest
import Efyu.Utils (debugM)
import qualified ExprParserTest
import qualified FuFilesTest
import qualified PatternMatchingTest
import Test.Hspec
import qualified TypeTest

main :: IO ()
main = hspec $ do
  debugM "init"
  BlockParserTest.tests
  ExprParserTest.tests
  TypeTest.tests
  CustomTypeTest.tests
  FuFilesTest.tests
  CodegenTest.tests
  PatternMatchingTest.tests
