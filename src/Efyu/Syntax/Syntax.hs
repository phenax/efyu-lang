module Efyu.Syntax.Syntax where

data Literal
  = LiteralString String
  | LiteralInt Integer
  | LiteralFloat Float
  | LiteralBool Bool
  deriving (Show, Eq)

type Identifier = String

data Expression
  = Literal Literal
  | Let [(Identifier, Expression)] Expression
  | Var String
  | Apply Expression Expression
  | Lambda Identifier Expression
  deriving (Show, Eq)
