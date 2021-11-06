module Efyu.Utils where

import qualified Data.Map as Map
import qualified Data.Text.Lazy as T
import Debug.Trace (trace)
import qualified Text.Pretty.Simple as Pretty

pShow :: (Show a) => a -> String
pShow = T.unpack . Pretty.pShow

debugM :: (Monad m, Show a) => a -> m ()
debugM = flip trace (pure ()) . ("::::LOG:::: " ++) . pShow

peekM :: (Monad m, Show a, Show b) => b -> m a -> m a
peekM prefix m = do
  a <- m
  a <$ debugM (prefix, a)

-- | Delete a set of keys from a Map
mapDeleteKeys :: (Ord k) => [k] -> Map.Map k a -> Map.Map k a
mapDeleteKeys keys m = foldr Map.delete m keys
