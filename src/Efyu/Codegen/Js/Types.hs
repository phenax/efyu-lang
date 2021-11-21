module Efyu.Codegen.Js.Types where

import Efyu.Types

data JsUnaryOp
  = JsOpBoolNot
  | JsOpPositive
  | JsOpNegative
  deriving (Show, Eq)

data JsBinaryOp
  = JsOpBoolAnd
  | JsOpBoolOr
  | JsOpAdd
  | JsOpSub
  | JsOpMul
  | JsOpDiv
  deriving (Show, Eq)

newtype JsBlock = JsBlock [JsStatement]
  deriving (Show, Eq)

data JsModule = JsModule String [JsModuleItem]
  deriving (Show, Eq)

data JsModuleItem
  = JsExport JsStatement
  | JsImport [IdentifierName 'VarName] String
  | JsIgnoreM
  deriving (Show, Eq)

-- JsIfChain [(JsExpr, JsBlock)] (Maybe JsBlock)
-- JsWhile JsExpr JsBlock
data JsStatement
  = JsConstVar (IdentifierName 'VarName) JsExpr
  | JsReturn JsExpr
  | JsIgnoreS
  deriving (Show, Eq)

data JsExpr
  = JsTernary JsExpr JsExpr JsExpr
  | JsLitString String
  | JsLitNumber Double
  | JsLitBool Bool
  | JsLitList [JsExpr]
  | JsVar (IdentifierName 'VarName)
  | JsCall JsExpr [JsExpr]
  | JsProperty JsExpr (IdentifierName 'VarName)
  | JsBinaryOp JsBinaryOp JsExpr JsExpr
  | JsUnaryOp JsUnaryOp JsExpr
  | JsFunction [IdentifierName 'VarName] JsBlock
  | JsIgnoreE
  deriving (Show, Eq)
