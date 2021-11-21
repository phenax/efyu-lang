module PatternMatchingTest where

import Efyu.TypeChecker.Infer
import Efyu.Types
import Test.Hspec
import TestHelpers

tests = do
  let inferTy = runTI . inferExpressionType

  describe "Pattern matching > type inference" $ do
    it "should infer lambda type correctly" $ do
      inferTy
        ( "x"
            *->> CaseOf
              (var "x")
              [ CaseItem (PatLiteral $ LiteralInt 5) defaultGuard (str "somethin"),
                CaseItem PatWildcard defaultGuard (str "none")
              ]
        )
        `shouldReturn` Right (TInt `tlam` TString)

    it "should infer input type from guard" $ do
      inferTy
        ( Let [defSig "gt0" $ TInt `tlam` TBool] $
            "x"
              *->> CaseOf
                (var "x")
                [ CaseItem PatWildcard (var "gt0" `call` var "x") (str "somethin")
                ]
        )
        `shouldReturn` Right (TInt `tlam` TString)
      inferTy
        ( Let [defSig "gt0" $ TInt `tlam` TBool] $
            "x"
              *->> CaseOf
                (var "x")
                [ CaseItem (PatVar $ ident "x1") (var "gt0" `call` var "x1") (var "x1")
                ]
        )
        `shouldReturn` Right (TInt `tlam` TInt)

    it "should infer input type from pattern variable" $ do
      inferTy
        ( "x"
            *->> CaseOf
              (var "x")
              [ CaseItem (PatVar (ident "ret")) defaultGuard (var "ret")
              ]
        )
        `shouldReturn` Right (tvar "'a0" `tlam` tvar "'a0")

    it "should infer input type from pattern variables nested inside tuples" $ do
      inferTy
        ( "x"
            *->> CaseOf
              (var "x")
              [ CaseItem
                  (PatLiteral $ LiteralTuple [PatLiteral $ LiteralInt 5, PatVar $ ident "ret"])
                  defaultGuard
                  (var "ret")
              ]
        )
        `shouldReturn` Right (TTuple [TInt, tvar "'p3"] `tlam` tvar "'p3")
