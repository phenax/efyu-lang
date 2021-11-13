module Efyu.Compiler where

import Data.List (intercalate)
import Efyu.Syntax.Block
import Efyu.Types

between cs ce x = cs ++ x ++ ce

parens = between "(" ")"

braces = between "{" "}"

quoted :: String -> String
quoted = show

iife s = parens ("() => " ++ braces s) ++ "()"

compileLiteral :: Literal -> String
compileLiteral = \case
  LiteralString s -> show s
  LiteralInt n -> show n
  LiteralFloat n -> show n
  LiteralBool b -> if b then "true" else "false"
  LiteralList exprs -> between "[" "]" . intercalate "," . map compileExpression $ exprs

compileExpression :: Expression -> String
compileExpression = \case
  Literal l -> compileLiteral l
  Apply fn p ->
    let fnStr = parens . compileExpression $ fn
        paramStr = compileExpression p
     in fnStr ++ parens paramStr
  Lambda param expr ->
    param ++ " => " ++ compileExpression expr
  Var x -> x
  -- Let bindings body ->
  --   let toVar (name, val) = "const " ++ name ++ " = " ++ compileExpression val ++ ";"
  --       vars = map toVar . filter ((/= "_") . fst) $ bindings
  --       block = unlines vars ++ "\n return " ++ compileExpression body
  --    in iife block
  IfElse cond ifE elseE ->
    let condStr = parens . compileExpression $ cond
        bodyS = braces . ("return " ++) . compileExpression
     in iife $ "if " ++ condStr ++ "\n" ++ bodyS ifE ++ "\n else " ++ bodyS elseE
  _ -> "<pending>"

compileBlock :: Block -> String
compileBlock = \case
  Module name blocks ->
    let children = unlines . map compileBlock $ blocks
        toName _ = ""
        moduleStr = children ++ "; return " ++ (braces . intercalate "," . map toName $ blocks)
     in "export const " ++ name ++ " = " ++ iife moduleStr
  _ -> "<pending>"

-- Def name expr ->
--   let value = compileExpression expr
--    in "const " ++ name ++ " =  " ++ value ++ ";"
