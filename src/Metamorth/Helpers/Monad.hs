module Metamorth.Helpers.Monad
  ( forFoldM
  , mapMaybeM
  , forMaybeM
  ) where

import Control.Monad

-- | `foldM` with different order of arguments.
forFoldM :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
forFoldM acc xs f = foldM f acc xs

-- | Applicative version of mapMaybe
--   taken from "GHC.Utils.Monad".
mapMaybeM :: Applicative m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = foldr g (pure [])
  where g a = liftA2 (maybe id (:)) (f a)

-- | `mapMaybeM` with different order of arguments.
forMaybeM :: Applicative m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM xs f = mapMaybeM f xs
