module Efyu.Types where

data Literal
  = LiteralString String
  | LiteralInt Integer
  | LiteralFloat Double
  | LiteralBool Bool
  | LiteralList [Expression]
  | LiteralTuple [Expression]
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

data Type
  = TLambda Type Type
  | TInt
  | TString
  | TFloat
  | TBool
  | TVar String
  | TList Type
  | TTuple [Type]
  | TUnknown
  deriving (Show, Eq)
