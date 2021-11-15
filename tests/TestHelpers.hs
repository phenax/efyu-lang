module TestHelpers where

import Efyu.Syntax.Block
import Efyu.Types

(*->>) = Lambda . IdentifierName

tlam = TLambda

infixr 5 `tlam`

infixr 5 *->>

call = Apply

int = Literal . LiteralInt

str = Literal . LiteralString

float = Literal . LiteralFloat

bool = Literal . LiteralBool

tuple = Literal . LiteralTuple

list = Literal . LiteralList

var = Var . IdentifierName

tvar = TVar . IdentifierName

defVal = DefValue . IdentifierName

defSig = DefSignature . IdentifierName

typeAlias = TypeAliasDef . IdentifierName

tname = TName . IdentifierName
