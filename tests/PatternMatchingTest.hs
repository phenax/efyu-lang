module PatternMatchingTest where

import Efyu.Syntax.Expression
import Efyu.TypeChecker.Env
import Efyu.TypeChecker.Infer
import Efyu.Types
import Test.Hspec
import Test.Hspec.Megaparsec
import TestHelpers
import qualified Text.Megaparsec as MP
import Text.RawString.QQ (r)

tests = do
  let inferTyExpr = runTI . inferExpressionType
  let getValType name m = runTI $ do
        checkModuleWithEnv m
        lookupValue (ident name) >>= \case
          Just r' -> instantiate r'
          Nothing -> pure TUnknown
  let p = MP.parse (expressionP <* MP.eof) "testfile.fu"

  describe "Pattern matching > parsing" $ do
    it "should parse out case of syntax with simple patterns" $ do
      p
        [r|
          case x of
            2 -> "its 2"
            "wow" -> "wow"
            x -> fn x
            _ -> "some other shit"
        |]
        `shouldParse` CaseOf
          (var "x")
          [ CaseItem (PatLiteral $ LiteralInt 2) Nothing (str "its 2"),
            CaseItem (PatLiteral $ LiteralString "wow") Nothing (str "wow"),
            CaseItem (PatVar (ident "x")) Nothing (var "fn" `call` var "x"),
            CaseItem PatWildcard Nothing (str "some other shit")
          ]
    it "should parse out case of for tuple and list patterns" $ do
      p
        [r|
             case x of
               (2, x) -> x
               [1, 2, x] -> x
               _ -> 20
           |]
        `shouldParse` CaseOf
          (var "x")
          [ CaseItem (pattuple [patint 2, patvar "x"]) Nothing (var "x"),
            CaseItem (patlist [patint 1, patint 2, patvar "x"]) Nothing (var "x"),
            CaseItem PatWildcard Nothing (int 20)
          ]
    it "should parse out constructor patterns and nestings" $ do
      p
        [r|
             case x of
               Just x -> x
               Just (Just x) -> x
               Just (x, 3) -> x
               (Just x, Right y) -> x
               _ -> 20
           |]
        `shouldParse` CaseOf
          (var "x")
          [ CaseItem (patctor "Just" [patvar "x"]) Nothing (var "x"),
            CaseItem (patctor "Just" [patctor "Just" [patvar "x"]]) Nothing (var "x"),
            CaseItem (patctor "Just" [pattuple [patvar "x", patint 3]]) Nothing (var "x"),
            CaseItem (pattuple [patctor "Just" [patvar "x"], patctor "Right" [patvar "y"]]) Nothing (var "x"),
            CaseItem PatWildcard Nothing (int 20)
          ]

  describe "Pattern matching > type inference" $ do
    it "should infer lambda type correctly" $ do
      inferTyExpr
        ( "x"
            *->> CaseOf
              (var "x")
              [ CaseItem (PatLiteral $ LiteralInt 5) Nothing (str "somethin"),
                CaseItem PatWildcard Nothing (str "none")
              ]
        )
        `shouldReturn` Right (TInt `tlam` TString)

    it "should infer input type from guard" $ do
      inferTyExpr
        ( Let [defSig "gt0" $ TInt `tlam` TBool] $
            "x"
              *->> CaseOf
                (var "x")
                [ CaseItem PatWildcard (Just $ var "gt0" `call` var "x") (str "somethin")
                ]
        )
        `shouldReturn` Right (TInt `tlam` TString)
      inferTyExpr
        ( Let [defSig "gt0" $ TInt `tlam` TBool] $
            "x"
              *->> CaseOf
                (var "x")
                [ CaseItem (PatVar $ ident "x1") (Just $ var "gt0" `call` var "x1") (var "x1")
                ]
        )
        `shouldReturn` Right (TInt `tlam` TInt)

    it "should infer input type from pattern variable" $ do
      inferTyExpr
        ( "x"
            *->> CaseOf
              (var "x")
              [ CaseItem (PatVar (ident "ret")) Nothing (var "ret")
              ]
        )
        `shouldReturn` Right (tvar "'a0" `tlam` tvar "'a0")

    it "should infer input type from pattern variables nested inside tuples" $ do
      inferTyExpr
        ( "x"
            *->> CaseOf
              (var "x")
              [ CaseItem
                  (PatLiteral $ LiteralTuple [PatLiteral $ LiteralInt 5, PatVar $ ident "ret"])
                  Nothing
                  (var "ret")
              ]
        )
        `shouldReturn` Right (TTuple [TInt, tvar "'p3"] `tlam` tvar "'p3")

    it "should infer input type from constructors" $ do
      getValType
        "result"
        ( Module
            "Main"
            [ TypeDef (ident "Pair") $ TCtors [Constructor TUnknown (ident "Pair") [TInt, TString]],
              Def . defVal "result" $
                "x"
                  *->> CaseOf
                    (var "x")
                    [ CaseItem
                        (PatCtor (ident "Pair") [PatLiteral $ LiteralInt 5, PatVar $ ident "y"])
                        Nothing
                        (var "y")
                    ]
            ]
        )
        `shouldReturn` Right (tname "Pair" `TLambda` TString)

---
