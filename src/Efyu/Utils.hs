module Efyu.Utils where

import qualified Data.Map as Map

-- | Delete a set of keys from a Map
mapDeleteKeys :: (Ord k) => [k] -> Map.Map k a -> Map.Map k a
mapDeleteKeys keys map = foldr Map.delete map keys
