module Efyu.Utils where

import qualified Data.Map as Map
import Debug.Trace (trace)

debugM :: (Monad m, Show a) => a -> m ()
debugM = flip trace (pure ()) . ("::::LOG:::: " ++) . show

peekM :: (Monad m, Show a, Show b) => b -> m a -> m a
peekM prefix m = do
  a <- m
  a <$ debugM (prefix, a)

-- | Delete a set of keys from a Map
mapDeleteKeys :: (Ord k) => [k] -> Map.Map k a -> Map.Map k a
mapDeleteKeys keys map = foldr Map.delete map keys
