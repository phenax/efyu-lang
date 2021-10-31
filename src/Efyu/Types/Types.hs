module Efyu.Types.Types where

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

-- type TIState ty = Either String (TypeSubst, ty)

data TIEnv = TIEnv {}

data TIState = TIState {tiSupply :: Int, tiSubst :: TypeSubst}

type TI a = StateT TIState (ReaderT TIEnv (ExceptT String IO)) a

runTI :: TI a -> IO (Either String a)
runTI t = do
  res <- runExceptT (runReaderT (runStateT t initTIState) initTIEnv)
  pure $ fst <$> res
  where
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

inferType :: TypeEnv -> Expression -> TI (TypeSubst, Type)
inferType env = \case
  Literal lit -> pure (Map.empty, literalType lit)
  Lambda p r -> do
    tv <- newTyVar "a"
    let env' = Map.insert p (Scheme [] tv) env
    (s1, t1) <- inferType env' r
    pure (s1, TLambda (apply s1 tv) t1)
  --Apply fn param -> do
  _ -> pure (Map.empty, TInt)

-----------------
