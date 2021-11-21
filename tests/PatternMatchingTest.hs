module PatternMatchingTest where

import Efyu.TypeChecker.Infer
import Efyu.Types
import Test.Hspec
import TestHelpers

tests = do
  let check = runTI . inferExpressionType

  describe "Pattern matching > type inference" $ do
    it "should do stuff" $ do
      check
        ( "x"
            *->> CaseOf
              (var "x")
              [ CaseItem (PatLiteral $ LiteralInt 5) defaultGuard (str "somethin"),
                CaseItem PatWildcard defaultGuard (str "none")
              ]
        )
        `shouldReturn` Right TString
