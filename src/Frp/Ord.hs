module Frp.Ord where

import           ClassyPrelude

clamp :: Ord a => a -> a -> a -> a
clamp minV maxV v = min maxV (max minV v)
