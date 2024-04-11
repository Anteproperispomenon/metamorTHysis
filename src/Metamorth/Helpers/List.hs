{-|
Module      : Metamorth.Helpers.List
Description : Various List Functions
Copyright   : (c) David Wilson, 2024
License     : BSD-3

This module mostly contains some helper
functions for lists, along with some 
functions with re-ordered arguments to
make them easier to use in some situations.

It also includes `nubSort`, a combination
of `L.sort` and `L.nub` that has improved
performance because it `L.nub`s the list
as it performs the merge sort.

-}

module Metamorth.Helpers.List
  -- * Major Function(s)
  ( nubSort 
  -- * Simple Helpers
  , allUnique
  , firstJust
  -- * Re-Ordered Functions
  , withZip
  , withZipM
  -- * Re-Exports
  , liftEitherList
  ) where

import Control.Monad (zipWithM)

import Data.List qualified as L

import Metamorth.Helpers.Either (liftEitherList)

-- | A combination of `L.sort` and `L.nub`. This should
--   be faster than @`L.nub` . `L.sort`@ and possibly
--   faster than just `L.sort` on its own. This is
--   because `nubSort` removes excess copies of
--   elements throughout the sorting process,
--   which reduces the lengths of runs created
--   and merged, and even allows the creation
--   of longer descending runs. This is because
--   descending runs can work on monotonically 
--   decreasing runs, instead of just strictly
--   descreasing runs.
--
--   i.e. In a normal merge sort, using the 
--   following as a run
--
--   @ [...5,4,3,2,2,1...] ==> [...[1,2,2,3,4,5]...] @
--
--   wouldn't work, since it would break the
--   stable-sorting guarantee. On the other hand,
--   since @nubSort@ only keeps the first instance
--   of each element around, it would create the
--   following run instead:
--
--   @ [...5,4,3,3,2,1...] ==> [...[1,2,3,4,5]...] @
--
nubSort :: (Ord a) => [a] -> [a]
nubSort = mergeAll . getRuns

data Run a
  = ReverseRun [a]
  | ForwardRun ([a] -> [a])
  -- deriving (Show, Eq)

revRun2 :: a -> a -> Run a
revRun2 x y = ReverseRun [y,x]

forwRun2 :: a -> a -> Run a
forwRun2 x y = ForwardRun ((x:) . (y:))

addToRun :: a -> Run a -> Run a
addToRun x (ReverseRun xs) = ReverseRun (x:xs)
addToRun x (ForwardRun fs) = ForwardRun $ fs . (x:)

getRun :: Run a -> [a]
getRun (ReverseRun xs) = xs
getRun (ForwardRun fs) = fs []

getRuns :: (Ord a) => [a] -> [[a]]
getRuns = (map getRun) . makeRuns

makeRuns :: (Ord a) => [a] -> [Run a]
makeRuns []  = []
makeRuns [x] = [ForwardRun (x:)]
makeRuns (x:y:xs)
  | x <= y    = makeRuns' (forwRun2 x y) y xs
  | otherwise = makeRuns' (revRun2  x y) y xs

-- This nubs the runs as it goes.
makeRuns' :: (Ord a) => Run a -> a -> [a] -> [Run a]
makeRuns' rn _ [] = [rn]
makeRuns' rn@(ForwardRun _) lst (x:xs)
  | lst == x  = makeRuns' rn x xs
  | lst <  x  = makeRuns' (addToRun x rn) x xs
  | otherwise = rn : (makeRuns (x:xs))
makeRuns' rn@(ReverseRun _) lst (x:xs)
  | lst == x  = makeRuns' rn x xs
  | lst >  x  = makeRuns' (addToRun x rn) x xs
  | otherwise = rn : (makeRuns (x:xs))

mergeRuns :: (Ord a) => [a] -> [a] -> [a]
mergeRuns [] ys = ys
mergeRuns xs [] = xs
mergeRuns (x:xs) (y:ys)
  | x == y    = x : (mergeRuns xs ys) -- this should work, since the run step nubs the sublists.
  | x <  y    = x : (mergeRuns xs (y:ys))
  | otherwise = y : (mergeRuns (x:xs) ys)

mergeAll :: (Ord a) => [[a]] -> [a]
mergeAll [] = [] -- need this for safety
mergeAll [xs] = xs
mergeAll xss = mergeAll (pairwiseMerge xss)

pairwiseMerge :: (Ord a) => [[a]] -> [[a]]
pairwiseMerge [] = []
pairwiseMerge [xs] = [xs]
pairwiseMerge (xs:ys:xss) = (mergeRuns xs ys) : pairwiseMerge xss

-- | Like `zipWith`, but with a different argument order.
withZip :: [a] -> [b] -> (a -> b -> c) -> [c]
withZip xs ys f = zipWith f xs ys

-- | Like `zipWithM`, but with a different argument order.
withZipM :: (Applicative m) => [a] -> [b] -> (a -> b -> m c) -> m [c]
withZipM xs ys f = zipWithM f xs ys

-- | Check that all members of a list are unique
allUnique :: (Eq a) => [a] -> Bool
allUnique xs = xs == (L.nub xs)

-- | Return the first non-`Nothing` value
--   in a list of @`Maybe` a@.
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust ((Just x ):_) = Just x
firstJust (Nothing:xs) = firstJust xs
