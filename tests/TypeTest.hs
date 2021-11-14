module TypeTest where

import qualified Data.Map as Map
import Efyu.TypeChecker.Infer
import Efyu.Types
import Test.Hspec
import TestHelpers

tests =
  describe "Type checker" $ do
    describe "Checker" $ do
      let check e = runTI . fmap fst . checkExpressionType Map.empty e

      it "should check for invalid type annotations" $ do
        check (int 5) TInt `shouldReturn` Right TInt
        check (int 5) TString
          `shouldReturn` Left (unificationErrorMessage TString TInt)
        check
          ("x" *->> "f" *->> Var "f" `call` Var "x")
          (TVar "a" `tlam` (TVar "a" `tlam` TVar "b") `tlam` TVar "b")
          `shouldReturn` Right (TVar "a" `tlam` (TVar "a" `tlam` TVar "b") `tlam` TVar "b")
        check
          ("x" *->> "f" *->> Var "f" `call` Var "x")
          (TInt `tlam` (TInt `tlam` TString) `tlam` TString)
          `shouldReturn` Right (TInt `tlam` (TInt `tlam` TString) `tlam` TString)
        check
          ("x" *->> "f" *->> Var "f" `call` Var "x")
          (TString `tlam` (TInt `tlam` TString) `tlam` TString)
          `shouldReturn` Left (unificationErrorMessage TInt TString)

      it "should use inferred type when explicit type is unknown" $ do
        check (int 5) TUnknown `shouldReturn` Right TInt
        check (str "x") TUnknown `shouldReturn` Right TString
        check ("x" *->> "f" *->> Var "f" `call` Var "x") TUnknown
          `shouldReturn` Right (TVar "a0" `tlam` (TVar "a0" `tlam` TVar "a2") `tlam` TVar "a2")

      it "should check types for if-else" $ do
        check (IfElse (bool True) (int 5) (int 6)) TUnknown `shouldReturn` Right TInt
        check (IfElse (bool True) (float 5.0) (float 6.0)) TUnknown `shouldReturn` Right TFloat
        check (IfElse (bool True) (float 5.0) (int 6)) TUnknown
          `shouldReturn` Left (unificationErrorMessage TFloat TInt)
        check (IfElse (int 3) (int 1) (int 1)) TUnknown
          `shouldReturn` Left (unificationErrorMessage TInt TBool)

      describe "recursive functions" $ do
        it "should infer types of recursive functions" $ do
          let recursiveExpr =
                Let
                  [ DefSignature "gte" (TInt `tlam` TInt `tlam` TBool),
                    DefSignature "sub" (TInt `tlam` TInt `tlam` TInt),
                    DefValue "fact" $
                      "x"
                        *->> IfElse
                          (Var "gte" `call` Var "x" `call` int 1)
                          (Var "fact" `call` (Var "sub" `call` Var "x" `call` int 1))
                          (int 1)
                  ]
                  (Var "fact" `call` int 5)
          check recursiveExpr TInt `shouldReturn` Right TInt
          check recursiveExpr TString `shouldReturn` Left (unificationErrorMessage TString TInt)

    describe "Inference" $ do
      let infer = runTI . inferExpressionType Map.empty

      describe "literals" $ do
        it "should infer literal types" $ do
          infer (Literal $ LiteralInt 20) `shouldReturn` Right TInt
          infer (Literal $ LiteralString "") `shouldReturn` Right TString
          infer (Literal $ LiteralFloat 20.0) `shouldReturn` Right TFloat
          infer (Literal $ LiteralBool True) `shouldReturn` Right TBool
        it "should infer list literals" $ do
          infer (Literal $ LiteralList [int 5]) `shouldReturn` Right (TList TInt)
          infer (Literal $ LiteralList [float 5.0, float 5.0]) `shouldReturn` Right (TList TFloat)
          infer (Literal $ LiteralList [int 5, str "wow"])
            `shouldReturn` Left (unificationErrorMessage TInt TString)
          infer (Literal $ LiteralList [str "wow", float 3.0])
            `shouldReturn` Left (unificationErrorMessage TString TFloat)
        it "should infer tuple literals" $ do
          infer (Literal $ LiteralTuple []) `shouldReturn` Right TUnknown
          infer (Literal $ LiteralTuple [int 5]) `shouldReturn` Right (TTuple [TInt])
          infer (Literal $ LiteralTuple [float 5.0, float 5.0]) `shouldReturn` Right (TTuple [TFloat, TFloat])
          infer (Literal $ LiteralTuple [int 5, float 5.0]) `shouldReturn` Right (TTuple [TInt, TFloat])
          infer (Literal $ LiteralTuple [int 5, str "wow", float 5.0])
            `shouldReturn` Right (TTuple [TInt, TString, TFloat])

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
                [ DefValue "x" (Literal . LiteralInt $ 200),
                  DefValue "id" ("x" *->> Var "x")
                ]
                (Var "id" `call` Var "x")
            )
            `shouldReturn` Right TInt
          infer ("fn" *->> "x" *->> Var "fn" `call` Var "x")
            `shouldReturn` Right ((TVar "a1" `tlam` TVar "a2") `tlam` TVar "a1" `tlam` TVar "a2")
          infer
            ("fn" *->> "pair" *->> Var "pair" `call` (Var "fn" `call` str "val") `call` (Var "fn" `call` int 5))
            `shouldReturn` Left "unable to unify types: TString and TInt"

      describe "ifElse conditions" $ do
        it "should infer types for if-else" $ do
          infer (IfElse (bool True) (int 5) (int 6)) `shouldReturn` Right TInt
          infer (IfElse (bool True) (float 5) (float 6)) `shouldReturn` Right TFloat

      it "should error out for invalid variables" $ do
        infer (Var "foobar")
          `shouldReturn` Left (unboundVarErrorMessage "foobar")
        infer ("x" *->> Var "x1")
          `shouldReturn` Left (unboundVarErrorMessage "x1")

      it "should infer types from let bindings" $ do
        infer (Let [DefValue "x" (Literal . LiteralInt $ 200)] (Var "x"))
          `shouldReturn` Right TInt
        infer
          ( Let
              [ DefValue "x" (Literal . LiteralInt $ 200),
                DefValue "id" ("x" *->> Var "x")
              ]
              (Var "id" `call` Var "x")
          )
          `shouldReturn` Right TInt

      describe "type signature" $ do
        it "should verify type signatures match inferred types" $ do
          infer
            ( Let
                [ DefSignature "id" $ TInt `tlam` TInt,
                  DefValue "id" ("x" *->> Var "x")
                ]
                (Var "id")
            )
            `shouldReturn` Right (TInt `tlam` TInt)
          infer
            ( Let
                [ DefValue "id" ("x" *->> Var "x"),
                  DefSignature "id" $ TInt `tlam` TInt
                ]
                (Var "id")
            )
            `shouldReturn` Right (TInt `tlam` TInt)
          infer
            ( Let
                [ DefSignature "tup" $ TTuple [TInt, TString],
                  DefValue "tup" (tuple [int 1, float 3.0])
                ]
                (Var "tup")
            )
            `shouldReturn` Left (unificationErrorMessage TString TFloat)
          infer
            ( Let
                [ DefSignature "tup" $ TTuple [TInt, TFloat, TString],
                  DefValue "tup" (tuple [int 1, float 3.0])
                ]
                (Var "tup")
            )
            `shouldReturn` Left (unificationErrorMessage (TTuple [TInt, TFloat, TString]) (TTuple [TInt, TFloat]))
          infer
            ( Let
                [ DefSignature "tup" $ TTuple [TInt, TFloat, TString],
                  DefValue "tup" (tuple [int 1, float 3.0, str "x"])
                ]
                (Var "tup")
            )
            `shouldReturn` Right (TTuple [TInt, TFloat, TString])
        it "should error out if type signature doesn't match inferred type" $ do
          infer
            ( Let
                [ DefValue "id" ("x" *->> Var "x"),
                  DefSignature "id" $ TInt `tlam` TString
                ]
                (Var "id")
            )
            `shouldReturn` Left (unificationErrorMessage TString TInt)

---
