module Efyu.TypeChecker.FreeTypeVars where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Efyu.Types

-- substitutions
type TypeSubst = Map.Map (IdentifierName PolyTypeName) Type

-- type variable names
type TypeVars = Set.Set (IdentifierName 'PolyTypeName)

class FreeTypeVar a where
  -- | Get a set of all free type variables
  freeTypeVars :: a -> TypeVars

  -- | Apply a substitution on a type and get a new type
  apply :: TypeSubst -> a -> a

instance FreeTypeVar a => FreeTypeVar [a] where
  apply = map . apply
  freeTypeVars = foldr (Set.union . freeTypeVars) Set.empty
