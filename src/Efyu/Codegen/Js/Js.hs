module Efyu.Codegen.Js.Js where

import Efyu.Codegen.Js.Printer
import Efyu.Codegen.Js.Types
import Efyu.Types

jsIife :: [JsStatement] -> JsExpr
jsIife = flip JsCall [] . JsFunction [] . JsBlock

toConstDeclr :: Definition -> JsStatement
toConstDeclr (DefValue name expr) = JsConstVar name (jsExpr expr)
toConstDeclr _ = JsIgnoreS

--jsBlock :: Expression -> JsBlock
--jsBlock = _

flattenApply :: Expression -> Expression -> (Expression, [Expression])
flattenApply (Apply f p) p' = (f', ps ++ [p'])
  where
    (f', ps) = flattenApply f p
flattenApply f p = (f, [p])

flattenLambda :: IdentifierName 'VarName -> Expression -> ([IdentifierName 'VarName], Expression)
flattenLambda a (Lambda a' b) = (a : args, body)
  where
    (args, body) = flattenLambda a' b
flattenLambda a b = ([a], b)

jsExpr :: Expression -> JsExpr
jsExpr (Let declrs body) = jsIife $ map toConstDeclr declrs ++ [JsReturn (jsExpr body)]
jsExpr (Lambda arg body) = JsFunction args . JsBlock $ [JsReturn (jsExpr body')]
  where
    (args, body') = flattenLambda arg body
jsExpr (Apply fn p) = JsCall (jsExpr fn') $ map jsExpr params
  where
    (fn', params) = flattenApply fn p
jsExpr (Var name) = JsVar name
jsExpr (Ctor (IdentifierName name)) = JsVar (IdentifierName name)
jsExpr (Literal lit) = case lit of
  LiteralString s -> JsLitString s
  LiteralInt n -> JsLitNumber (fromInteger n)
  LiteralFloat n -> JsLitNumber n
  LiteralBool b -> JsLitBool b
  LiteralList ls -> JsLitList . map jsExpr $ ls
  LiteralTuple ls -> JsLitList . map jsExpr $ ls
jsExpr (IfElse condE ifE elseE) =
  JsTernary (jsExpr condE) (jsExpr ifE) (jsExpr elseE)
jsExpr _ = JsIgnoreE

jsModule :: Module -> JsModule
jsModule (Module name blocks) = JsModule name . map jsModuleItem $ blocks
  where
    jsModuleItem (Def (DefValue name' res)) = JsExport . JsConstVar name' $ jsExpr res
    jsModuleItem _ = JsIgnoreM

compileToString :: Module -> String
compileToString = compileJsModuleToString . jsModule
