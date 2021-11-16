module Efyu.TypeChecker.Env where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Efyu.TypeChecker.FreeTypeVars
import Efyu.Types
import Efyu.Utils (mapDeleteKeys)

composeSubst :: TypeSubst -> TypeSubst -> TypeSubst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- | Polymorphic set of vars (forall a, b, c. Type)
data TypeScheme = TypeScheme [IdentifierName PolyTypeName] Type deriving (Show)

type ValueMap = Map.Map (IdentifierName 'VarName) TypeScheme

type TypeMap = Map.Map (IdentifierName 'TypeName) TypeScheme

-- | Environment state for type inference
data TypeEnv = TypeEnv
  { -- | index for generating new poly type names from
    envPolyTypeIndex :: Int,
    -- | values in scope
    envValues :: ValueMap,
    -- | type names defined in the environment
    envTypes :: TypeMap,
    -- | contructors for types
    envConstructors :: Map.Map (IdentifierName 'ContructorName) TypeScheme
  }

type WithEnv = StateT TypeEnv

getEnv :: (MonadIO m) => WithEnv m TypeEnv
getEnv = get

modifyEnv :: (MonadIO m) => (TypeEnv -> TypeEnv) -> WithEnv m ()
modifyEnv = modify

emptyEnv :: TypeEnv
emptyEnv =
  TypeEnv
    { envPolyTypeIndex = 0,
      envValues = Map.empty,
      envTypes = Map.empty,
      envConstructors = Map.empty
    }

runWithEnv :: (MonadIO m) => WithEnv m a -> m a
runWithEnv = fmap fst . flip runStateT emptyEnv

-- | Create a new type variable
newTypeVar :: (MonadIO m) => String -> WithEnv m Type
newTypeVar prefix = do
  index <- envPolyTypeIndex <$> get
  modify $ \env -> env {envPolyTypeIndex = envPolyTypeIndex env + 1}
  pure . TVar . IdentifierName $ "'" ++ prefix ++ show index

-- | Update value map in a given environment and return a new env
updateValues :: (ValueMap -> ValueMap) -> TypeEnv -> TypeEnv
updateValues fn env = env {envValues = fn . envValues $ env}

-- | Lookup a value in the environment
lookupValue :: (MonadIO m) => IdentifierName 'VarName -> WithEnv m (Maybe TypeScheme)
lookupValue name = Map.lookup name . envValues <$> getEnv

-- | Create a scope for binding value types
withValues :: (MonadIO m) => ValueMap -> WithEnv m a -> WithEnv m a
withValues names blockM = do
  oldEnv <- get
  modify $ updateValues (`Map.union` names)
  res <- blockM
  modify $ updateValues (const $ envValues oldEnv)
  pure res

defineTypeAliases :: (MonadIO m) => TypeMap -> WithEnv m ()
defineTypeAliases tyMap =
  modifyEnv $ \env -> env {envTypes = envTypes env `Map.union` tyMap}

lookupType :: (MonadIO m) => IdentifierName 'TypeName -> WithEnv m (Maybe TypeScheme)
lookupType name = Map.lookup name . envTypes <$> getEnv

instance FreeTypeVar Type where
  freeTypeVars = \case
    TVar name -> Set.singleton name
    TLambda p r -> freeTypeVars p `Set.union` freeTypeVars r
    TTuple tys -> foldl' Set.union Set.empty . map freeTypeVars $ tys
    TList ty -> freeTypeVars ty
    _ -> Set.empty
  apply sub = \case
    TLambda p r -> TLambda (apply sub p) (apply sub r)
    TVar n -> fromMaybe (TVar n) $ Map.lookup n sub
    TTuple tys -> TTuple . map (apply sub) $ tys
    TList ty -> TList $ apply sub ty
    t -> t

instance FreeTypeVar TypeScheme where
  freeTypeVars (TypeScheme vars t) = Set.difference (freeTypeVars t) (Set.fromList vars)
  apply s (TypeScheme vars t) = TypeScheme vars (apply (mapDeleteKeys vars s) t)

instance FreeTypeVar TypeEnv where
  freeTypeVars = freeTypeVars . Map.elems . envValues
  apply st = updateValues $ Map.map (apply st)

--
