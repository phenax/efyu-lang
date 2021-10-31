module Efyu.Types.Types where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Efyu.Syntax.Syntax

data Type
  = TFunc Type Type
  | TInt
  | TString
  | TFloat
  | TBool
  | TVar String
  deriving (Show, Eq)

-- type variable names
type TypeVars = Set.Set String

-- substitutions
type TypeSubst = Map.Map String Type

-- forall (a, b). Int -> a -> b -> String -> b
--   [String] -> Polymorphic labels
--   Type -> Type signature consuming the polymorphic labels
data TypeScheme = Scheme [String] Type

-- type definitions map (name -> scheme)
type TypeEnv = Map.Map String TypeScheme

class FreeTypeVar a where
  freeTypeVars :: a -> TypeVars
  apply :: TypeSubst -> a -> a

instance FreeTypeVar Type where
  freeTypeVars = \case
    TVar n -> Set.singleton n
    TFunc p r -> Set.union (freeTypeVars p) (freeTypeVars r)
    _ -> Set.empty
  apply sub = \case
    TFunc p r -> TFunc (apply sub p) (apply sub r)
    TVar n -> case Map.lookup n sub of
      Just t -> t
      Nothing -> TVar n
    t -> t

-----------------

-- literalType :: Literal -> Type
-- literalType = \case
--   LiteralInt _ -> TInt
--   LiteralString _ -> TString
--   LiteralBool _ -> TBool
--   LiteralFloat _ -> TFloat
--
-- inferType :: Expression -> Maybe Type
-- inferType = \case
--   Literal lit -> Just $ literalType lit
--   Lambda _p body -> do
--     paramT <- Nothing -- TODO: infer lambda param
--     TFunc paramT <$> inferType body
--   Apply fn _ -> case inferType fn of
--     Just (TFunc _ ret) -> Just ret
--     _ -> Nothing
--   Var _name -> Nothing -- TODO: Lookup in scope
--   Let _bindings _expr -> Nothing -- TODO: Use bindings in expr to get result type
--
