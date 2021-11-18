module Efyu.TypeChecker.FreeTypeVars where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Efyu.Types
import Efyu.Utils (mapDeleteKeys)

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

instance FreeTypeVar Type where
  freeTypeVars = \case
    TVar name -> Set.singleton name
    TLambda p r -> freeTypeVars p `Set.union` freeTypeVars r
    TTuple tys -> foldl' Set.union Set.empty . map freeTypeVars $ tys
    TList ty -> freeTypeVars ty
    TApply ty ty' -> freeTypeVars ty `Set.union` freeTypeVars ty'
    TScope _ ty -> freeTypeVars ty
    _ -> Set.empty
  apply st = \case
    TLambda p r -> TLambda (apply st p) (apply st r)
    TVar n -> fromMaybe (TVar n) $ Map.lookup n st
    TTuple tys -> TTuple . map (apply st) $ tys
    TList ty -> TList $ apply st ty
    TApply ty ty' -> TApply (apply st ty) (apply st ty')
    -- TScope var ty -> TScope var $ apply st ty
    t -> t

instance FreeTypeVar TypeScheme where
  freeTypeVars (TypeScheme vars t) = Set.difference (freeTypeVars t) (Set.fromList vars)
  apply s (TypeScheme vars t) = TypeScheme vars (apply (mapDeleteKeys vars s) t)
