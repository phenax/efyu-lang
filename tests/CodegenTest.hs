module CodegenTest where

import Efyu.Codegen.Js.Js
import Efyu.Codegen.Js.Printer
import Efyu.Types
import Efyu.Utils (printStr)
import Test.Hspec
import TestHelpers

-- import Text.RawString.QQ (r)

tests = do
  describe "CodeGen > JS" $ do
    xit "should do it" $ do
      let ast =
            jsModule $
              Module
                "Main"
                [ Def . defVal "map" $
                    "fn" *->> "ls"
                      *->> IfElse
                        (var "null" `call` var "ls")
                        (list [])
                        ( var "prepend"
                            `call` (var "fn" `call` (var "head" `call` var "ls"))
                            `call` (var "map" `call` var "fn" `call` (var "tail" `call` var "ls"))
                        ),
                  Def . defVal "list" $
                    list [int 4, int 5, int 6, int 21],
                  Def . defVal "main" $
                    var "print" `call` (var "map" `call` (var "add" `call` int 1) `call` var "list")
                ]
      -- debugM ast
      printStr $ compileJsModuleToString ast
      1 `shouldBe` 2
    xit "should do it" $ do
      let ast =
            jsModule $
              Module
                "Main"
                [ Def . defVal "foo" $
                    "fn"
                      *->> "wow"
                      *->> "test"
                      *->> Let
                        [ defVal "x" $ str "foobar",
                          defVal "y" $ var "globalVar"
                        ]
                        (var "fn" `call` str "val" `call` var "x" `call` var "y")
                ]
      -- debugM ast
      printStr $ compileJsModuleToString ast
      1 `shouldBe` 2
