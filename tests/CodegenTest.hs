module CodegenTest where

import Efyu.Codegen.Js.Js
import Efyu.Codegen.Js.Printer
import Efyu.Types
import Efyu.Utils (debugM, printStr)
import Test.Hspec
import TestHelpers
import Text.RawString.QQ (r)

tests = do
  describe "CodeGen > JS" $ do
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
      debugM ast
      printStr $ compileJsModuleToString ast
      1 `shouldBe` 2
