# TODO
  - [X] Allow single parenthesis around expressions
  - [X] Lambda parser
  - [X] Use megaparsec?
  - [X] Manage indents
  - [X] Function application parser
  - [X] Module parser
  - [X] Function application parser (without @)
  - [X] Test with hspec-megaparsec
  - [X] Try hpack
  - [X] Type annotations parsing
  - [X] Type checker
  - [X] If expression
  - [X] Start with js compiler
  - [X] Implement recursion for type inference
  - [X] Type checking inside block
  - [ ] Use type annotation in type checker
  - [ ] List literal + type
  - [ ] Tuple literal + type
  - [ ] Effects system
  - [ ] Nested module parser
  - [ ] Types for modules?
  - [ ] Stdlib

## Bugs
  - [ ] :error-message: Better type error messages with loc
  - [ ] :indent: indentation issues (indent from reference expression not reference source character)
  - [ ] :indent: Type annotation indentation issue

## Custom types
  - [ ] aliases
  - [ ] adt (constructors)
  - [ ] type classes/instances?
  - [ ] records (define as product types with get/set)

## Compiler
  - [ ] Transform ast to target friendly ast
  - [ ] Compile to js
  - [ ] Compile to wasm
  - [ ] optimizer

## Infix operators
  - [ ] infix operators for arithmetics
  - [ ] infix operators for lists
  - [ ] infix operators for functions
  - [ ] Custom infix operators?

## Pattern matching
  - [ ] Look into pattern matching
  - [ ] Type inference from patterns
  - [ ] Replace let, lambda parameter parser with patterns
  - [ ] Compiler runtime for patterns

