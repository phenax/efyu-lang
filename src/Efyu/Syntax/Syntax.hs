module Efyu.Syntax.Syntax where

import Efyu.Types.Types (Type)

data Literal
  = LiteralString String
  | LiteralInt Integer
  | LiteralFloat Double
  | LiteralBool Bool
  deriving (Show, Eq)

type Identifier = String

data Expression
  = Literal Literal
  | Let [(Identifier, Expression)] Expression
  | Var String
  | Apply Expression Expression
  | Lambda Identifier Expression
  | TypeAnnotation Identifier Type
  deriving (Show, Eq)
