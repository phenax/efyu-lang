module TypeTest where

import qualified Data.Map as Map
import Efyu.Syntax.Syntax
import Efyu.Types.Infer
import Efyu.Types.Types
import Test.Hspec

tests =
  describe "Type inference" $ do
    let infer = runTI . inferType Map.empty

    it "should infer literal types" $ do
      infer (Literal $ LiteralInt 20) `shouldReturn` Right TInt
      infer (Literal $ LiteralString "") `shouldReturn` Right TString
      infer (Literal $ LiteralFloat 20.0) `shouldReturn` Right TFloat
      infer (Literal $ LiteralBool True) `shouldReturn` Right TBool
    it "should infer lambda types" $ do
      infer (Lambda "x" (Literal $ LiteralFloat 0.0))
        `shouldReturn` Right (TLambda (TVar "a0") TFloat)
    it "should infer function application" $ do
      infer (Apply (Lambda "x" (Literal $ LiteralInt 3)) (Literal $ LiteralFloat 0.0))
        `shouldReturn` Right TInt
      infer (Apply (Lambda "x" (Var "x")) (Literal $ LiteralFloat 0.0))
        `shouldReturn` Right TFloat
      infer (Apply (Lambda "x" (Lambda "y" (Var "y"))) (Literal $ LiteralFloat 0.0))
        `shouldReturn` Right (TLambda (TVar "a2") (TVar "a2"))
      infer
        ( Let
            [ ("x", Literal . LiteralInt $ 200),
              ("id", Lambda "x" $ Var "x")
            ]
            (Apply (Var "id") (Var "x"))
        )
        `shouldReturn` Right TInt
    it "should error out for invalid variables" $ do
      infer (Var "foobar")
        `shouldReturn` Left "Unbound variable foobar"
      infer (Lambda "x" (Var "x1"))
        `shouldReturn` Left "Unbound variable x1"
    it "should infer types from let bindings" $ do
      infer (Let [("x", Literal . LiteralInt $ 200)] (Var "x"))
        `shouldReturn` Right TInt
      infer
        ( Let
            [ ("x", Literal . LiteralInt $ 200),
              ("id", Lambda "x" $ Var "x")
            ]
            (Apply (Var "id") (Var "x"))
        )
        `shouldReturn` Right TInt

---
