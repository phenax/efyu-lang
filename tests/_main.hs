import qualified EvalTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  EvalTest.tests
