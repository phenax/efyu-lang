module Efyu.Codegen.Js.Printer where

import Data.List (intercalate)
import Efyu.Codegen.Js.Types
import Efyu.Types
import Text.RawString.QQ (r)

indent = unlines . map (ind ++)
  where
    ind = replicate 2 ' '

between s e x = s ++ x ++ e

parens = between "(" ")"

braces = between "{" "}"

brackets = between "[" "]"

printCommaList print' = intercalate "," . map print'

printIdent (IdentifierName n) = n

helperUtils =
  [r|
const call = (fn, ...args) => fn.length >= args.length
  ? fn.apply(null, args)
  : fn.bind(null, ...args)

///////

|]

compileJsModuleToString :: JsModule -> String
compileJsModuleToString (JsModule _ ms) = (helperUtils ++) . unlines . map printModuleItem $ ms

printBlock :: JsBlock -> String
printBlock (JsBlock stmts) = braces $ "\n" ++ indent (map printStatement stmts)

printModuleItem :: JsModuleItem -> String
printModuleItem (JsExport st) = "export " ++ printStatement st
printModuleItem JsIgnoreM = "<<ignored>>"
printModuleItem _ = "<<pending>>"

printStatement :: JsStatement -> String
printStatement (JsConstVar (IdentifierName name) val) = "const " ++ name ++ " = " ++ printExpression val ++ ";"
printStatement (JsReturn val) = "return " ++ printExpression val
printStatement JsIgnoreS = "<<ignored>>"
printStatement _ = "<<pending>>"

printExpression :: JsExpr -> String
printExpression (JsVar name) = printIdent name
printExpression (JsFunction args block) = parens (printCommaList printIdent args) ++ " => " ++ printBlock block
printExpression (JsLitString x) = show x
printExpression (JsLitNumber x) = show x
printExpression (JsLitBool x) = if x then "true" else "false"
printExpression (JsLitList ls) = brackets . printCommaList printExpression $ ls
printExpression (JsCall fn params) = "call" ++ parens (printCommaList printExpression (fn : params))
printExpression (JsTernary condE ifE elseE) = printExpression condE ++ " ? " ++ printExpression ifE ++ " : " ++ printExpression elseE
printExpression JsIgnoreE = "<<ignored>>"
printExpression _ = "<<pending>>"
