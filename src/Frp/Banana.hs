module Frp.Banana where

import           ClassyPrelude               hiding (union)
import           Reactive.Banana.Combinators

merge :: (Semigroup (f (Either t1 t2)),Functor f) => f t1 -> f t2 -> (t1 -> x) -> (t2 -> x) -> f x
merge e1 e2 f1 f2 = either f1 f2 <$> ((Left <$> e1) <> (Right <$> e2))

instance Semigroup (Event t a) where
  (<>) = union

