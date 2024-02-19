module Metamorth.Helpers.Map
  ( forWithKey
  , fromSelfList
  ) where

import Data.Map.Strict qualified as M

-- | Like `M.traverseWithKey`, but with the
--   opposite argument order. Makes it easier
--   to do things like
-- 
--   > forWithKey myMap $ \key val -> do ...
-- 
forWithKey :: Applicative t => M.Map k a -> (k -> a -> t b) -> t (M.Map k b)
forWithKey mp f = M.traverseWithKey f mp

-- | Create a `M.Map` from a simple list,
--   where every element is used as both
--   the key and the value.
fromSelfList :: Ord a => [a] -> M.Map a a
fromSelfList = M.fromList . (map (\x -> (x,x)))