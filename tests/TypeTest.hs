module TypeTest where

import Efyu.Errors
import Efyu.TypeChecker.Infer
import Efyu.Types
import Test.Hspec
import TestHelpers

tests =
  describe "Type checker" $ do
    describe "Checker" $ do
      let check e = runTI . fmap fst . checkExpressionType e

      it "should check for invalid type annotations" $ do
        check (int 5) TInt `shouldReturn` Right TInt
        check (int 5) TString
          `shouldReturn` Left (TypeUnificationError TString TInt)
        check
          ("x" *->> "f" *->> var "f" `call` var "x")
          (tvar "a" `tlam` (tvar "a" `tlam` tvar "b") `tlam` tvar "b")
          `shouldReturn` Right (tvar "a" `tlam` (tvar "a" `tlam` tvar "b") `tlam` tvar "b")
        check
          ("x" *->> "f" *->> var "f" `call` var "x")
          (TInt `tlam` (TInt `tlam` TString) `tlam` TString)
          `shouldReturn` Right (TInt `tlam` (TInt `tlam` TString) `tlam` TString)
        check
          ("x" *->> "f" *->> var "f" `call` var "x")
          (TString `tlam` (TInt `tlam` TString) `tlam` TString)
          `shouldReturn` Left (TypeUnificationError TInt TString)

      it "should use inferred type when explicit type is unknown" $ do
        check (int 5) TUnknown `shouldReturn` Right TInt
        check (str "x") TUnknown `shouldReturn` Right TString
        check ("x" *->> "f" *->> var "f" `call` var "x") TUnknown
          `shouldReturn` Right (tvar "'a0" `tlam` (tvar "'a0" `tlam` tvar "'a2") `tlam` tvar "'a2")

      it "should check types for if-else" $ do
        check (IfElse (bool True) (int 5) (int 6)) TUnknown `shouldReturn` Right TInt
        check (IfElse (bool True) (float 5.0) (float 6.0)) TUnknown `shouldReturn` Right TFloat
        check (IfElse (bool True) (float 5.0) (int 6)) TUnknown
          `shouldReturn` Left (TypeUnificationError TFloat TInt)
        check (IfElse (int 3) (int 1) (int 1)) TUnknown
          `shouldReturn` Left (TypeUnificationError TInt TBool)

      describe "recursive functions" $ do
        it "should infer types of recursive functions" $ do
          let recursiveExpr =
                Let
                  [ defSig "gte" (TInt `tlam` TInt `tlam` TBool),
                    defSig "sub" (TInt `tlam` TInt `tlam` TInt),
                    defVal "fact" $
                      "x"
                        *->> IfElse
                          (var "gte" `call` var "x" `call` int 1)
                          (var "fact" `call` (var "sub" `call` var "x" `call` int 1))
                          (int 1)
                  ]
                  (var "fact")
          check recursiveExpr (TInt `tlam` TInt) `shouldReturn` Right (TInt `tlam` TInt)
          check recursiveExpr (TInt `tlam` TString) `shouldReturn` Left (TypeUnificationError TString TInt)

    describe "Inference" $ do
      let inferTy = runTI . inferExpressionType

      describe "literals" $ do
        it "should inferTy literal types" $ do
          inferTy (Literal $ LiteralInt 20) `shouldReturn` Right TInt
          inferTy (Literal $ LiteralString "") `shouldReturn` Right TString
          inferTy (Literal $ LiteralFloat 20.0) `shouldReturn` Right TFloat
          inferTy (Literal $ LiteralBool True) `shouldReturn` Right TBool
        it "should infer list literals" $ do
          inferTy (Literal $ LiteralList [int 5]) `shouldReturn` Right (TList TInt)
          inferTy (Literal $ LiteralList [float 5.0, float 5.0]) `shouldReturn` Right (TList TFloat)
          inferTy (Literal $ LiteralList [int 5, str "wow"])
            `shouldReturn` Left (TypeUnificationError TInt TString)
          inferTy (Literal $ LiteralList [str "wow", float 3.0])
            `shouldReturn` Left (TypeUnificationError TString TFloat)
        it "should infer tuple literals" $ do
          inferTy (Literal $ LiteralTuple []) `shouldReturn` Right TUnknown
          inferTy (Literal $ LiteralTuple [int 5]) `shouldReturn` Right (TTuple [TInt])
          inferTy (Literal $ LiteralTuple [float 5.0, float 5.0]) `shouldReturn` Right (TTuple [TFloat, TFloat])
          inferTy (Literal $ LiteralTuple [int 5, float 5.0]) `shouldReturn` Right (TTuple [TInt, TFloat])
          inferTy (Literal $ LiteralTuple [int 5, str "wow", float 5.0])
            `shouldReturn` Right (TTuple [TInt, TString, TFloat])

      describe "functions" $ do
        it "should infer lambda types" $ do
          inferTy ("x" *->> Literal (LiteralFloat 0.0))
            `shouldReturn` Right (TLambda (tvar "'a0") TFloat)
          inferTy ("x" *->> "y" *->> Literal (LiteralFloat 0.0))
            `shouldReturn` Right (tvar "'a0" `tlam` tvar "'a1" `tlam` TFloat)
          inferTy ("x" *->> "y" *->> var "y")
            `shouldReturn` Right (tvar "'a0" `tlam` tvar "'a1" `tlam` tvar "'a1")
          inferTy ("x" *->> "y" *->> var "x")
            `shouldReturn` Right (tvar "'a0" `tlam` tvar "'a1" `tlam` tvar "'a0")

        it "should infer function application" $ do
          inferTy (("x" *->> Literal (LiteralInt 3)) `call` Literal (LiteralFloat 0.0))
            `shouldReturn` Right TInt
          inferTy (("x" *->> var "x") `call` Literal (LiteralFloat 0.0))
            `shouldReturn` Right TFloat
          inferTy (("x" *->> "y" *->> var "y") `call` Literal (LiteralFloat 0.0))
            `shouldReturn` Right (TLambda (tvar "'a2") (tvar "'a2"))
          inferTy
            ( Let
                [ defVal "x" (Literal . LiteralInt $ 200),
                  defVal "id" ("x" *->> var "x")
                ]
                (var "id" `call` var "x")
            )
            `shouldReturn` Right TInt
          inferTy ("fn" *->> "x" *->> var "fn" `call` var "x")
            `shouldReturn` Right ((tvar "'a1" `tlam` tvar "'a2") `tlam` tvar "'a1" `tlam` tvar "'a2")
        it "should fail for polymorphic usage of lambda parameters" $ do
          inferTy
            ("fn" *->> "pair" *->> var "pair" `call` (var "fn" `call` str "val") `call` (var "fn" `call` int 5))
            `shouldReturn` Left (TypeUnificationError TString TInt)
          inferTy
            ("fn" *->> tuple [var "fn" `call` str "val", var "fn" `call` int 5])
            `shouldReturn` Left (TypeUnificationError TString TInt)
          inferTy
            ("fn" *->> list [var "fn" `call` str "val", var "fn" `call` int 5])
            `shouldReturn` Left (TypeUnificationError TString TInt)

      -- TODO: Fix type inference for lambda parameter
      -- describe "recursive functions" $ do
      --   it "should infer types of recursive functions" $ do
      --     let recursiveExpr =
      --           Let
      --             [ defSig "gte" (TInt `tlam` TInt `tlam` TBool),
      --               defSig "sub" (TInt `tlam` TInt `tlam` TInt),
      --               defVal "fact" $
      --                 "x"
      --                   *->> IfElse
      --                     (var "gte" `call` var "x" `call` int 1)
      --                     (var "fact" `call` (var "sub" `call` var "x" `call` int 1))
      --                     (int 1)
      --             ]
      --             (var "fact")
      --     inferTy recursiveExpr `shouldReturn` Right TInt

      describe "ifElse conditions" $ do
        it "should inferTy types for if-else" $ do
          inferTy (IfElse (bool True) (int 5) (int 6)) `shouldReturn` Right TInt
          inferTy (IfElse (bool True) (float 5) (float 6)) `shouldReturn` Right TFloat

      it "should error out for invalid variables" $ do
        inferTy (var "foobar")
          `shouldReturn` Left (UnboundVariableError . IdentifierName $ "foobar")
        inferTy ("x" *->> var "x1")
          `shouldReturn` Left (UnboundVariableError . IdentifierName $ "x1")

      it "should infer types from let bindings" $ do
        inferTy (Let [defVal "x" (Literal . LiteralInt $ 200)] (var "x"))
          `shouldReturn` Right TInt
        inferTy
          ( Let
              [ defVal "x" (Literal . LiteralInt $ 200),
                defVal "id" ("x" *->> var "x")
              ]
              (var "id" `call` var "x")
          )
          `shouldReturn` Right TInt

      describe "type signature" $ do
        it "should verify type signatures match inferred types" $ do
          inferTy
            ( Let
                [ defSig "id" $ TInt `tlam` TInt,
                  defVal "id" ("x" *->> var "x")
                ]
                (var "id")
            )
            `shouldReturn` Right (TInt `tlam` TInt)
          inferTy
            ( Let
                [ defVal "id" ("x" *->> var "x"),
                  defSig "id" $ TInt `tlam` TInt
                ]
                (var "id")
            )
            `shouldReturn` Right (TInt `tlam` TInt)
          inferTy
            ( Let
                [ defSig "tup" $ TTuple [TInt, TString],
                  defVal "tup" (tuple [int 1, float 3.0])
                ]
                (var "tup")
            )
            `shouldReturn` Left (TypeUnificationError TString TFloat)
          inferTy
            ( Let
                [ defSig "tup" $ TTuple [TInt, TFloat, TString],
                  defVal "tup" (tuple [int 1, float 3.0])
                ]
                (var "tup")
            )
            `shouldReturn` Left (TypeUnificationError (TTuple [TInt, TFloat, TString]) (TTuple [TInt, TFloat]))
          inferTy
            ( Let
                [ defSig "tup" $ TTuple [TInt, TFloat, TString],
                  defVal "tup" (tuple [int 1, float 3.0, str "x"])
                ]
                (var "tup")
            )
            `shouldReturn` Right (TTuple [TInt, TFloat, TString])
        it "should error out if type signature doesn't match inferred type" $ do
          inferTy
            ( Let
                [ defVal "id" ("x" *->> var "x"),
                  defSig "id" $ TInt `tlam` TString
                ]
                (var "id")
            )
            `shouldReturn` Left (TypeUnificationError TString TInt)

---
