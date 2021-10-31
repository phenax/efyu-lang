module TypeTest where

import qualified Data.Map as Map
import Efyu.Syntax.Syntax
import Efyu.Types.Types
import Test.Hspec

tests =
  describe "Type inference" $ do
    let infer = runTI . inferType Map.empty
    it "should infer literal types" $ do
      infer (Literal $ LiteralInt 20) `shouldReturn` Right (Map.empty, TInt)
      infer (Literal $ LiteralString "") `shouldReturn` Right (Map.empty, TString)
      infer (Literal $ LiteralFloat 20.0) `shouldReturn` Right (Map.empty, TFloat)
      infer (Literal $ LiteralBool True) `shouldReturn` Right (Map.empty, TBool)
    it "should infer lambda types" $ do
      infer (Lambda "x" (Literal $ LiteralFloat 0.0)) `shouldReturn` Right (Map.empty, TLambda (TVar "a0") TFloat)
    it "should infer function application" $ do
      infer (Apply (Lambda "x" (Literal $ LiteralInt 3)) (Literal $ LiteralFloat 0.0))
        `shouldReturn` Right
          ( Map.fromList
              [ ("a0", TInt),
                ("a1", TFloat)
              ],
            TInt
          )
