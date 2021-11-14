module Efyu.Types where

data Literal
  = LiteralString String
  | LiteralInt Integer
  | LiteralFloat Double
  | LiteralBool Bool
  | LiteralList [Expression]
  | LiteralTuple [Expression]
  deriving (Show, Eq)

-- | Set of identifier types
data IdentifierType = VarName | PolyTypeName | TypeName | ContructorName

-- | Identifier constructor
newtype IdentifierName (t :: IdentifierType) = IdentifierName {getIdentifier :: String}
  deriving (Show, Eq, Ord)

data Definition
  = DefValue (IdentifierName VarName) Expression
  | DefSignature (IdentifierName VarName) Type
  deriving (Show, Eq)

data Expression
  = Literal Literal
  | Let [Definition] Expression
  | Var (IdentifierName VarName)
  | Apply Expression Expression
  | Lambda (IdentifierName 'VarName) Expression
  | IfElse Expression Expression Expression
  deriving (Show, Eq)

data Type
  = TLambda Type Type
  | TInt
  | TString
  | TFloat
  | TBool
  | TVar (IdentifierName 'PolyTypeName)
  | TList Type
  | TTuple [Type]
  | TUnknown
  deriving (Show, Eq)
