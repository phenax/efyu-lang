module TestHelpers where

import Efyu.Syntax.Syntax

(*->>) = Lambda

infixr 5 *->>

call = Apply

int = Literal . LiteralInt

str = Literal . LiteralString

float = Literal . LiteralFloat
