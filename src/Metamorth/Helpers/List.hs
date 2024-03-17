module Metamorth.Helpers.List
  ( nubSort 
  , liftEitherList
  , withZip
  , withZipM
  , allUnique
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
--   of longer descending runs (since this allows
--   descending runs to work on monotonically 
--   decreasing runs, instead of just strictly
--   descreasing runs).
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

-- | Like zipWith, but with a different argument order.
withZip :: [a] -> [b] -> (a -> b -> c) -> [c]
withZip xs ys f = zipWith f xs ys

-- | Like zipWithM, but with a different argument order.
withZipM :: (Applicative m) => [a] -> [b] -> (a -> b -> m c) -> m [c]
withZipM xs ys f = zipWithM f xs ys

-- | Check that all members of a list are unique
allUnique :: (Eq a) => [a] -> Bool
allUnique xs = xs == (L.nub xs)
