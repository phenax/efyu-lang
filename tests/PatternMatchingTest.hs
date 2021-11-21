module PatternMatchingTest where

import Efyu.TypeChecker.Infer
import Efyu.Types
import Test.Hspec
import TestHelpers

tests = do
  let check = runTI . inferExpressionType

  describe "Pattern matching > type inference" $ do
    it "should infer lambda type correctly" $ do
      check
        ( "x"
            *->> CaseOf
              (var "x")
              [ CaseItem (PatLiteral $ LiteralInt 5) defaultGuard (str "somethin"),
                CaseItem PatWildcard defaultGuard (str "none")
              ]
        )
        `shouldReturn` Right (TInt `tlam` TString)

    it "should infer input type from guard" $ do
      check
        ( Let [defSig "gt0" $ TInt `tlam` TBool] $
            "x"
              *->> CaseOf
                (var "x")
                [ CaseItem PatWildcard (var "gt0" `call` var "x") (str "somethin")
                ]
        )
        `shouldReturn` Right (TInt `tlam` TString)

    it "should infer input type from pattern variable" $ do
      check
        ( "x"
            *->> CaseOf
              (var "x")
              [ CaseItem (PatVar (ident "ret")) defaultGuard (var "ret")
              ]
        )
        `shouldReturn` Right (tvar "'a0" `tlam` tvar "'a0")
