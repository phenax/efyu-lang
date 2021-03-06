module CustomTypeTest where

import Efyu.Errors
import Efyu.TypeChecker.Env
import Efyu.TypeChecker.Infer
import Efyu.Types
import Test.Hspec
import TestHelpers

tests =
  describe "Type checker" $ do
    let checkM = runTI . checkModule
    let getValType name m = runTI (checkModuleWithEnv m >> lookupValue (ident name))
    -- let getDebug m = runTI (envDebugEnvs <$> checkModuleWithEnv m)
    it "should check module" $ do
      checkM
        ( Module
            "Hello"
            [ TypeDef (ident "Pair") $ TScope (ident "a") (TScope (ident "b") $ TTuple [tvar "a", tvar "b"]),
              Def . DefSignature (ident "foobar") $ TName (ident "Pair") `TApply` TInt `TApply` TString,
              Def . DefValue (ident "foobar") $ tuple [int 5, str "wow"]
            ]
        )
        `shouldReturn` Right ()
      checkM
        ( Module
            "Hello"
            [ TypeDef (ident "DummyInt") TInt,
              Def . DefSignature (ident "foobar") $ TName (ident "DummyInt"),
              Def . DefValue (ident "foobar") $ int 5
            ]
        )
        `shouldReturn` Right ()

      checkM
        ( Module
            "Hello"
            [ TypeDef (ident "Dummy") $ ident "a" `TScope` (ident "b" `TScope` (ident "c" `TScope` TTuple [tvar "b", tvar "a", tvar "c"])),
              Def . DefSignature (ident "foobar") $ TName (ident "Dummy") `TApply` TInt `TApply` TString `TApply` TFloat,
              Def . DefValue (ident "foobar") $ tuple [str "x", int 5, float 2.0]
            ]
        )
        `shouldReturn` Right ()

    it "should error out for unbound type vars in alias definitions" $ do
      checkM
        ( Module
            "Hello"
            [ TypeDef (ident "Pair") $ TScope (ident "a") (TTuple [tvar "a", tvar "b"])
            ]
        )
        `shouldReturn` Left (UnboundPolyTypeError $ ident "b")

    it "should error out for invalid type definitions" $ do
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
            [ TypeDef (ident "DummyStr") TString,
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
            [ TypeDef (ident "Pair") TString,
              Def . DefSignature (ident "foobar") $ TName (ident "Pair") `TApply` TInt,
              Def . DefValue (ident "foobar") $ tuple [int 5, str "wow"]
            ]
        )
        `shouldReturn` Left (KindMismatchError TString TInt)

      checkM
        ( Module
            "Hello"
            [ TypeDef (ident "Pair") $ TScope (ident "a") (TTuple [tvar "a", TFloat]),
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

    it "should error out for incomplete kind signatures" $ do
      checkM
        ( Module
            "Hello"
            [ TypeDef (ident "Fancy") $ TScope (ident "a") (TTuple [tvar "a", TFloat]),
              Def . defSig "a" $ tname "Fancy",
              Def . defVal "a" $ tuple [int 2, float 2.0]
            ]
        )
        `shouldReturn` Left (IllegalKindError (TScope (ident "a") (TTuple [tvar "a", TFloat])))
      checkM
        ( Module
            "Hello"
            [ TypeDef (ident "Fancy") $ TScope (ident "a") (TTuple [tvar "a", TFloat]),
              Def . defSig "a" $ TName (ident "Fancy")
            ]
        )
        `shouldReturn` Left (IllegalKindError (TScope (ident "a") (TTuple [tvar "a", TFloat])))

    it "should unify type correctly for nested type with alias" $ do
      getValType
        "result"
        ( Module
            "Hello"
            [ TypeDef (ident "WithName") $ TScope (ident "a") (TTuple [TString, tvar "a"]),
              Def . defSig "map" $ (tvar "a" `tlam` tvar "b") `tlam` TList (tvar "a") `tlam` TList (tvar "b"),
              Def . defSig "result" $ TList (tname "WithName" `TApply` TInt),
              Def . defVal "result" $
                var "map" `call` ("x" *->> tuple [str "wow", var "x"]) `call` list [int 5, int 3]
            ]
        )
        `shouldReturn` Right (Just . TypeScheme [] $ TList (tname "WithName" `TApply` TInt))

    describe "type constructor" $ do
      let maybeT =
            TypeDef
              (ident "Maybe")
              $ TScope (ident "a") $
                TCtors
                  [ Constructor TUnknown (ident "Just") [tvar "a"],
                    Constructor TUnknown (ident "Nothing") []
                  ]
      let maybeForIntT = TypeDef (ident "MaybeInt") $ tname "Maybe" `TApply` TInt
      let maybeIntT =
            TypeDef
              (ident "MaybeInt")
              $ TCtors
                [ Constructor TUnknown (ident "Just") [TInt],
                  Constructor TUnknown (ident "Nothing") []
                ]

      it "should infer types generated by constructor call" $ do
        getValType
          "a"
          ( Module
              "Hello"
              [ maybeIntT,
                Def . defVal "a" $ Ctor (ident "Just") `call` int 5
              ]
          )
          `shouldReturn` Right (Just . TypeScheme [] $ tname "MaybeInt")
        getValType
          "a"
          ( Module
              "Hello"
              [ maybeIntT,
                Def . defVal "a" $ Ctor (ident "Nothing")
              ]
          )
          `shouldReturn` Right (Just . TypeScheme [] $ tname "MaybeInt")
        getValType
          "a"
          ( Module
              "Hello"
              [ maybeIntT,
                Def . defVal "a" $ Ctor (ident "Just")
              ]
          )
          `shouldReturn` Right (Just . TypeScheme [] $ TInt `TLambda` tname "MaybeInt")

      it "should infer correct type for Maybe" $ do
        getValType
          "a"
          ( Module
              "Hello"
              [ maybeT,
                Def . defVal "a" $ Ctor (ident "Just") `call` int 5
              ]
          )
          `shouldReturn` Right (Just . TypeScheme [] $ tname "Maybe" `TApply` TInt)
        getValType
          "a"
          ( Module
              "Hello"
              [ maybeT,
                maybeForIntT,
                Def . defSig "a" $ tname "MaybeInt",
                Def . defVal "a" $ Ctor (ident "Just") `call` int 5
              ]
          )
          `shouldReturn` Right (Just . TypeScheme [] $ tname "MaybeInt")

---
