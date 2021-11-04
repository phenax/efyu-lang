module Scratchpad where


-- |||||||||||| Nested modules

module Test where
  gam = 200
  add5 = add 5

-- |||||||||||| pattern matching + recursion
fibo : Int -> Int
fibo x = match x {
  | 0 | 1 -> 1
  | n -> add (fibo (sub n 1)) (fibo (sub n 2))
}

-- |||||||||||| infered type
fibo = match {
  | 0 | 1 -> 1
  | n -> add (fibo (sub n 1)) (fibo (sub n 2))
}

-- |||||||||||| variants
type Maybe a = Just a | Nothing

-- |||||||||||| recursive definition
type List a = Nil | Cons a (List a)

-- |||||||||||| typeclass - instance
class Mappable (m a) = {
	map : (a -> b) -> m a -> m b
}

type Maybe a = Just a | Nothing

instance Mappable (Maybe a) = {
	map fn = match {
	| Just a -> Just (fn a)
  | Nothing -> Nothing
	}
}




type List a = Nil | Cons a (List a)

add5 = add 5

struct User = {
  name: String,
  age: Maybe (Mutable a),
} deriving (Show)

main = {
  print (add5 20)

  let
    user = User { name: "My name", age: Just 20 }
  in
    print user
}

-- Traits + Structs
-- Mutable type
trait Mutable a = {
	get: a
	set: a -> unit
}
-- type Mutable a = Mutable a

-- FFI
foreign foobar: Int -> Int = "foobar(%1)"

-- Effect system
effect State a =
  | Get -> a
  | Set a -> unit

effectful : Int -> Int -> Effect Int
effectful n max = {
  let x = State.Get
  State.Set (x + n)

  ifElse (x < max) (effectful x) x
}

usage = {
  let x = Mutable(1);
  Effect.run (effectful n) (match {
    | Get resume -> resume(x.get())
    | Set value resume -> {
      x.set(value)
      resume(unit)
    }
  })
}


