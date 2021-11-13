module Efyu.Types.Types where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Efyu.Utils (mapDeleteKeys)

data Type
  = TLambda Type Type
  | TInt
  | TString
  | TFloat
  | TBool
  | TVar String
  | TUnknown
  deriving (Show, Eq)

-- type variable names
type TypeVars = Set.Set String

-- substitutions
type TypeSubst = Map.Map String Type

composeSubst :: TypeSubst -> TypeSubst -> TypeSubst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- TODO: Recursive for nested types
-- TODO: Prioritize concrete types over polymorphic types
specificity :: Type -> Type -> Ordering
specificity TUnknown _ = LT
specificity _ TUnknown = GT
specificity _ _ = EQ

higherSp :: Type -> Type -> Type
higherSp t1 t2 = case specificity t1 t2 of LT -> t2; _ -> t1

-- Polymorphic set of vars (forall a, b, c. Type)
data TypeScheme = TypeScheme [String] Type deriving (Show)

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
    TVar n -> fromMaybe (TVar n) $ Map.lookup n sub
    t -> t

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
