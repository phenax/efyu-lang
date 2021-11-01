module Efyu.Types.Types where

import qualified Data.Map as Map
import qualified Data.Set as Set

data Type
  = TLambda Type Type
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

composeSubst :: TypeSubst -> TypeSubst -> TypeSubst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- Polymorphic set of vars (forall a, b, c. Type)
data TypeScheme = TypeScheme [String] Type

-- type definitions map (name -> scheme)
type TypeEnv = Map.Map String TypeScheme

class FreeTypeVar a where
  -- | Get a set of all free type variables
  freeTypeVars :: a -> TypeVars

  -- | Apply a substitution on a type and get a new type
  apply :: TypeSubst -> a -> a

instance FreeTypeVar Type where
  freeTypeVars = \case
    TVar name -> Set.singleton name
    TLambda p r -> Set.union (freeTypeVars p) (freeTypeVars r)
    _ -> Set.empty
  apply sub = \case
    TLambda p r -> TLambda (apply sub p) (apply sub r)
    TVar n -> case Map.lookup n sub of
      Just t -> t
      Nothing -> TVar n
    t -> t

mapDeleteKeys :: (Ord k) => [k] -> Map.Map k a -> Map.Map k a
mapDeleteKeys keys map = foldr Map.delete map keys

instance FreeTypeVar TypeScheme where
  freeTypeVars (TypeScheme vars t) = Set.difference (freeTypeVars t) (Set.fromList vars)
  apply s (TypeScheme vars t) = TypeScheme vars (apply (mapDeleteKeys vars s) t)

instance FreeTypeVar a => FreeTypeVar [a] where
  apply = map . apply
  freeTypeVars = foldr (Set.union . freeTypeVars) Set.empty

instance FreeTypeVar TypeEnv where
  freeTypeVars env = freeTypeVars (Map.elems env)
  apply = Map.map . apply

-----------------
