module CustomTypeTest where

import Efyu.Errors
import Efyu.Syntax.Block
import Efyu.TypeChecker.Infer
import Efyu.Types
import Test.Hspec
import TestHelpers

tests =
  describe "Type checker" $ do
    let checkM = runTI . checkBlockType
    -- let getType m = runTI (envTypes <$> checkModuleWithEnv m)
    -- let getDebug m = runTI (envDebugEnvs <$> checkModuleWithEnv m)
    it "should check modules" $ do
      checkM
        ( Module
            "Hello"
            [ TypeAliasDef (ident "Pair") $ TScope (ident "a") (TScope (ident "b") $ TTuple [tvar "a", tvar "b"]),
              Def . DefSignature (ident "foobar") $ TName (ident "Pair") `TApply` TInt `TApply` TString,
              Def . DefValue (ident "foobar") $ tuple [int 5, str "wow"]
            ]
        )
        `shouldReturn` Right ()
      checkM
        ( Module
            "Hello"
            [ TypeAliasDef (ident "DummyInt") TInt,
              Def . DefSignature (ident "foobar") $ TName (ident "DummyInt"),
              Def . DefValue (ident "foobar") $ int 5
            ]
        )
        `shouldReturn` Right ()

      checkM
        ( Module
            "Hello"
            [ Def . DefSignature (ident "foobar") $ TName (ident "TestType"),
              Def . DefValue (ident "foobar") $ int 5
            ]
        )
        `shouldReturn` Left (UnboundTypeError . ident $ "TestType")

      checkM
        ( Module
            "Hello"
            [ TypeAliasDef (ident "DummyStr") TString,
              Def . DefSignature (ident "foobar") $ TName (ident "DummyStr"),
              Def . DefValue (ident "foobar") $ int 5
            ]
        )
        `shouldReturn` Left (TypeUnificationError TInt TString)
      checkM
        ( Module
            "Hello"
            [ Def . DefSignature (ident "foobar") $ TInt `TApply` TInt,
              Def . DefValue (ident "foobar") $ int 5
            ]
        )
        `shouldReturn` Left (KindMismatchError TInt TInt)
      checkM
        ( Module
            "Hello"
            [ TypeAliasDef (ident "Pair") TString,
              Def . DefSignature (ident "foobar") $ TName (ident "Pair") `TApply` TInt,
              Def . DefValue (ident "foobar") $ tuple [int 5, str "wow"]
            ]
        )
        `shouldReturn` Left (KindMismatchError TString TInt)
      checkM
        ( Module
            "Hello"
            [ TypeAliasDef (ident "Pair") $ TScope (ident "a") (TTuple [tvar "a", TFloat]),
              Def . DefSignature (ident "foobar") $ TName (ident "Pair") `TApply` TInt `TApply` TString,
              Def . DefValue (ident "foobar") $ tuple [int 5, str "wow"]
            ]
        )
        `shouldReturn` Left (KindMismatchError (TTuple [TInt, TFloat]) TString)
      checkM
        ( Module
            "Hello"
            [ Def . DefSignature (ident "foobar") $ TName (ident "Pair") `TApply` TInt `TApply` TString,
              Def . DefValue (ident "foobar") $ tuple [int 5, str "wow"]
            ]
        )
        `shouldReturn` Left (UnboundTypeError . ident $ "Pair")
