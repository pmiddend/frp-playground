module Frp.List where

import           ClassyPrelude

deleteNth :: Int -> [a] -> [a]
deleteNth n = uncurry (++) . second unsafeTail . splitAt n

