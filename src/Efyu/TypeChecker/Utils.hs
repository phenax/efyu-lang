module Efyu.TypeChecker.Utils where

import Efyu.Types

specificity :: Type -> Type -> Ordering
specificity TUnknown _ = LT
specificity _ TUnknown = GT
specificity _ _ = EQ

higherSp :: Type -> Type -> Type
higherSp t1 t2 = case specificity t1 t2 of LT -> t2; _ -> t1

-----------------
