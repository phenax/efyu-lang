module Efyu.Types where

data Module = Module String [Block]
  deriving (Show, Eq)

data Block
  = Def Definition
  | TypeDef (IdentifierName 'TypeName) Type
  deriving (Show, Eq)

data Literal
  = LiteralString String
  | LiteralInt Integer
  | LiteralFloat Double
  | LiteralBool Bool
  | LiteralList [Expression]
  | LiteralTuple [Expression]
  deriving (Show, Eq)

-- | Set of identifier types
data IdentifierType = VarName | PolyTypeName | TypeName | ConstructorName

-- | Identifier constructor
newtype IdentifierName (t :: IdentifierType) = IdentifierName {getIdentifier :: String}
  deriving (Eq, Ord)

instance Show (IdentifierName x) where
  show (IdentifierName s) = show s

data Definition
  = DefValue (IdentifierName 'VarName) Expression
  | DefSignature (IdentifierName 'VarName) Type
  deriving (Show, Eq)

data Expression
  = Literal Literal
  | Let [Definition] Expression
  | Var (IdentifierName 'VarName)
  | Apply Expression Expression
  | Lambda (IdentifierName 'VarName) Expression
  | IfElse Expression Expression Expression
  | Ctor (IdentifierName 'ConstructorName)
  deriving (Show, Eq)

-- | Polymorphic set of vars (forall a, b, c. Type)
data TypeScheme
  = TypeScheme [IdentifierName PolyTypeName] Type
  deriving (Show, Eq)

-- | Constructor
data Constructor = Constructor Type (IdentifierName 'ConstructorName) [Type]
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
  | TName (IdentifierName 'TypeName)
  | TApply Type Type
  | TScope (IdentifierName 'PolyTypeName) Type
  | TCtors [Constructor]
  | TUnknown
  deriving (Show, Eq)
