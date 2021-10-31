module Efyu.Types.Types where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Efyu.Syntax.Syntax

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
    TLambda p r -> Set.union (freeTypeVars p) (freeTypeVars r)
    _ -> Set.empty
  apply sub = \case
    TLambda p r -> TLambda (apply sub p) (apply sub r)
    TVar n -> case Map.lookup n sub of
      Just t -> t
      Nothing -> TVar n
    t -> t

instance FreeTypeVar TypeScheme where
  freeTypeVars (Scheme vars t) = freeTypeVars t `Set.difference` Set.fromList vars
  apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

instance FreeTypeVar a => FreeTypeVar [a] where
  apply = map . apply
  freeTypeVars = foldr (Set.union . freeTypeVars) Set.empty

instance FreeTypeVar TypeEnv where
  freeTypeVars env = freeTypeVars (Map.elems env)
  apply = Map.map . apply

data TIEnv = TIEnv {}

data TIState = TIState {tiSupply :: Int, tiSubst :: TypeSubst}

type TI = StateT TIState (ExceptT String (ReaderT TIEnv IO))

runTI :: TI a -> IO (Either String a)
runTI t = do
  res <- run t
  pure $ fst <$> res
  where
    run =
      flip runReaderT initTIEnv
        . runExceptT
        . flip runStateT initTIState
    initTIState = TIState {tiSupply = 0, tiSubst = Map.empty}
    initTIEnv = TIEnv {}

newTyVar :: Identifier -> TI Type
newTyVar prefix = do
  s <- get
  put (s {tiSupply = 1 + tiSupply s})
  pure . TVar $ prefix ++ show (tiSupply s)

literalType :: Literal -> Type
literalType = \case
  LiteralInt _ -> TInt
  LiteralString _ -> TString
  LiteralBool _ -> TBool
  LiteralFloat _ -> TFloat

unify :: Type -> Type -> TI TypeSubst
unify a b = case (a, b) of
  (t, TVar n) -> varBind n t
  (TVar n, t) -> varBind n t
  (t, t') | t == t' -> pure Map.empty
  (TLambda p r, TLambda p' r') -> do
    st <- unify p p'
    st' <- unify (apply st r) (apply st r')
    pure $ composeSubst st st'
  (t, t') -> lift . throwE $ "unable to unify types: " ++ show t ++ " and " ++ show t'
  where
    varBind n t = case t of
      TVar n' | n' == n -> pure Map.empty
      _ | Set.member n (freeTypeVars t) -> lift $ throwE "foobaroty"
      _ -> pure $ Map.singleton n t

inferType :: TypeEnv -> Expression -> TI (TypeSubst, Type)
inferType env = \case
  Literal lit -> pure (Map.empty, literalType lit)
  Lambda p r -> do
    tv <- newTyVar "a"
    let env' = Map.insert p (Scheme [] tv) env
    (s1, t1) <- inferType env' r
    pure (s1, TLambda (apply s1 tv) t1)
  Apply fn param -> do
    tv <- newTyVar "a"
    (s1, t1) <- inferType env fn
    (s2, t2) <- inferType (apply s1 env) param
    s3 <- unify (apply s2 t1) (TLambda t2 tv)
    pure (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
  _ -> pure (Map.empty, TInt)

-----------------
