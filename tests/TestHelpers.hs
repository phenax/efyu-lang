module TestHelpers where

import Efyu.Syntax.Syntax
import Efyu.Types.Types

(*->>) = Lambda

tlam = TLambda

infixr 5 `tlam`

infixr 5 *->>

call = Apply

int = Literal . LiteralInt

str = Literal . LiteralString

float = Literal . LiteralFloat

bool = Literal . LiteralBool
