module TypeTest where

import qualified Data.Map as Map
import Efyu.Syntax.Syntax
import Efyu.Types.Checker
import Efyu.Types.Infer
import Efyu.Types.Types
import Test.Hspec
import TestHelpers

tests =
  describe "Type checker" $ do
    describe "Checker" $ do
      let check e = runTI . checkType Map.empty e

      it "should check for invalid type annotations" $ do
        check (int 5) TInt `shouldReturn` Right (int 5)
        check (int 5) TString
          `shouldReturn` Left (unificationErrorMessage TString TInt)
        check
          ("x" *->> "f" *->> Var "f" `call` Var "x")
          (TVar "a" `tlam` (TVar "a" `tlam` TVar "b") `tlam` TVar "b")
          `shouldReturn` Right ("x" *->> "f" *->> Var "f" `call` Var "x")
        check
          ("x" *->> "f" *->> Var "f" `call` Var "x")
          (TInt `tlam` (TInt `tlam` TString) `tlam` TString)
          `shouldReturn` Right ("x" *->> "f" *->> Var "f" `call` Var "x")
        check
          ("x" *->> "f" *->> Var "f" `call` Var "x")
          (TString `tlam` (TInt `tlam` TString) `tlam` TString)
          `shouldReturn` Left (unificationErrorMessage TInt TString)

      it "should use inferred type when explicit type is unknown" $ do
        check (int 5) TUnknown `shouldReturn` Right (int 5)
        check (str "x") TUnknown `shouldReturn` Right (str "x")
        check ("x" *->> "f" *->> Var "f" `call` Var "x") TUnknown
          `shouldReturn` Right ("x" *->> "f" *->> Var "f" `call` Var "x")

    describe "Inference" $ do
      let infer = runTI . inferType Map.empty

      describe "literals" $ do
        it "should infer literal types" $ do
          infer (Literal $ LiteralInt 20) `shouldReturn` Right TInt
          infer (Literal $ LiteralString "") `shouldReturn` Right TString
          infer (Literal $ LiteralFloat 20.0) `shouldReturn` Right TFloat
          infer (Literal $ LiteralBool True) `shouldReturn` Right TBool

      describe "functions" $ do
        it "should infer lambda types" $ do
          infer ("x" *->> Literal (LiteralFloat 0.0))
            `shouldReturn` Right (TLambda (TVar "a0") TFloat)
          infer ("x" *->> "y" *->> Literal (LiteralFloat 0.0))
            `shouldReturn` Right (TVar "a0" `tlam` TVar "a1" `tlam` TFloat)
          infer ("x" *->> "y" *->> Var "y")
            `shouldReturn` Right (TVar "a0" `tlam` TVar "a1" `tlam` TVar "a1")
          infer ("x" *->> "y" *->> Var "x")
            `shouldReturn` Right (TVar "a0" `tlam` TVar "a1" `tlam` TVar "a0")

        it "should infer function application" $ do
          infer (("x" *->> Literal (LiteralInt 3)) `call` Literal (LiteralFloat 0.0))
            `shouldReturn` Right TInt
          infer (("x" *->> Var "x") `call` Literal (LiteralFloat 0.0))
            `shouldReturn` Right TFloat
          infer (("x" *->> "y" *->> Var "y") `call` Literal (LiteralFloat 0.0))
            `shouldReturn` Right (TLambda (TVar "a2") (TVar "a2"))
          infer
            ( Let
                [ ("x", Literal . LiteralInt $ 200),
                  ("id", "x" *->> Var "x")
                ]
                (Var "id" `call` Var "x")
            )
            `shouldReturn` Right TInt
          infer ("fn" *->> "x" *->> Var "fn" `call` Var "x")
            `shouldReturn` Right ((TVar "a1" `tlam` TVar "a2") `tlam` TVar "a1" `tlam` TVar "a2")
          infer
            ("fn" *->> "pair" *->> Var "pair" `call` (Var "fn" `call` str "val") `call` (Var "fn" `call` int 5))
            `shouldReturn` Left "unable to unify types: TString and TInt"

      it "should error out for invalid variables" $ do
        infer (Var "foobar")
          `shouldReturn` Left "Unbound variable foobar"
        infer ("x" *->> Var "x1")
          `shouldReturn` Left "Unbound variable x1"

      it "should infer types from let bindings" $ do
        infer (Let [("x", Literal . LiteralInt $ 200)] (Var "x"))
          `shouldReturn` Right TInt
        infer
          ( Let
              [ ("x", Literal . LiteralInt $ 200),
                ("id", "x" *->> Var "x")
              ]
              (Var "id" `call` Var "x")
          )
          `shouldReturn` Right TInt

      it "should create substitutions from type annotations" $ do
        -- infer
        --   ( Let
        --       [ ("_", TypeAnnotation "name" TString),
        --         ("name", int 5)
        --       ]
        --       (Var "name")
        --   )
        --   `shouldReturn` Right TString
        infer (TypeAnnotation "name" TInt) `shouldReturn` Right TInt

---
