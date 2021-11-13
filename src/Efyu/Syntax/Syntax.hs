module Efyu.Syntax.Syntax where

import Efyu.Types.Types (Type)

data Literal
  = LiteralString String
  | LiteralInt Integer
  | LiteralFloat Double
  | LiteralBool Bool
  deriving (Show, Eq)

type Identifier = String

data Definition
  = DefValue Identifier Expression
  | DefSignature Identifier Type
  deriving (Show, Eq)

data Expression
  = Literal Literal
  | Let [Definition] Expression
  | Var String
  | Apply Expression Expression
  | Lambda Identifier Expression
  | IfElse Expression Expression Expression
  deriving (Show, Eq)
