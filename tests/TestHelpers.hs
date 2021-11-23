module TestHelpers where

import Efyu.Types

ident = IdentifierName

(*->>) = Lambda . ident

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

patint = PatLiteral . LiteralInt

patstr = PatLiteral . LiteralString

patfloat = PatLiteral . LiteralFloat

patbool = PatLiteral . LiteralBool

pattuple = PatLiteral . LiteralTuple

patlist = PatLiteral . LiteralList

var = Var . ident

patvar = PatVar . ident

tvar = TVar . ident

defVal = DefValue . ident

defSig = DefSignature . ident

typeAlias = TypeDef . ident

tname = TName . ident
