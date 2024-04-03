module Metamorth.Helpers.Maybe
  ( partitionMaybe
  ) where

-- | Variant of `Data.Maybe.mapMaybe` that partitions
--   the values that fail the check in a separate list.
partitionMaybe          :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe _ []     = ([], [])
partitionMaybe f (x:xs) =
 let rs = partitionMaybe f xs in
 case f x of
  Nothing -> x >: rs
  Just r  -> r <: rs
{-# NOINLINE [1] partitionMaybe #-}

(<:) :: a -> ([a],[b]) -> ([a],[b])
x <: (xs,ys) = (x:xs,ys)

(>:) :: b -> ([a],[b]) -> ([a],[b])
y >: (xs,ys) = (xs,y:ys)

infixr 5 <:
infixr 5 >:
