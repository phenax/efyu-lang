module Efyu.Codegen.Js.Printer where

import Efyu.Codegen.Js.Types
import Efyu.Codegen.Utils
import Efyu.Types
import Text.RawString.QQ (r)

helperUtils =
  [r|
const call = (fn, ...args) => fn.length >= args.length
  ? fn.apply(null, args)
  : fn.bind(null, ...args)

///////

|]

instance CodeGenerator (IdentifierName a) where
  codegen (IdentifierName n) = n

instance CodeGenerator JsModule where
  codegen (JsModule _ ms) = unlines . map codegen $ ms

instance CodeGenerator JsBlock where
  codegen (JsBlock stmts) = braces $ "\n" ++ indent (map codegen stmts)

instance CodeGenerator JsModuleItem where
  codegen (JsExport st) = "export " ++ codegen st
  codegen JsIgnoreM = "<<ignored>>"
  codegen _ = "<<pending>>"

instance CodeGenerator JsStatement where
  codegen (JsConstVar (IdentifierName name) val) = "const " ++ name ++ " = " ++ codegen val ++ ";"
  codegen (JsReturn val) = "return " ++ codegen val
  codegen JsIgnoreS = "<<ignored>>"

instance CodeGenerator JsExpr where
  codegen (JsVar name) = codegen name
  codegen (JsFunction args block) = parens (commaList args) ++ " => " ++ codegen block
  codegen (JsLitString x) = show x
  codegen (JsLitNumber x) = show x
  codegen (JsLitBool x) = if x then "true" else "false"
  codegen (JsLitList ls) = brackets . commaList $ ls
  codegen (JsCall fn params) = "call" ++ parens (commaList (fn : params))
  codegen (JsTernary condE ifE elseE) = codegen condE ++ " ? " ++ codegen ifE ++ " : " ++ codegen elseE
  codegen JsIgnoreE = "<<ignored>>"
  codegen _ = "<<pending>>"

compileJsModuleToString :: JsModule -> String
compileJsModuleToString = (helperUtils ++) . codegen
