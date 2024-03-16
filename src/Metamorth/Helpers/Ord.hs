{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Metamorth.Helpers.Ord
  ( SizeOrdList(..)
  , compareLen
  ) where

import Data.Foldable qualified as Fold

-- | A @newtype@ over lists that orders elements
--   by length first, then by the elements within
--   them. Note that this works even if one of the
--   the lists is infinite, since the comparing 
--   function doesn't actually count the length
--   of the longer list. It will fail, however,
--   if both of the lists are infinite.
newtype SizeOrdList a = SizeOrdList { getRegularList :: [a] }
  deriving stock   (Show, Eq)
  deriving newtype (Functor, Applicative, Monad, Semigroup, Monoid, Foldable)

instance (Ord a) => Ord (SizeOrdList a) where
  compare (SizeOrdList xs) (SizeOrdList ys) = compareLen xs ys

-- | Compare two lists, sorting by length
--   first, then elements. This function
--   works when at most one of the two lists
--   is infinite, since it doesn't actually
--   calculate the length of the longer list.
compareLen :: (Ord a) => [a] -> [a] -> Ordering
compareLen = compareLen' EQ

compareLen' :: (Ord a) => Ordering -> [a] -> [a] -> Ordering
compareLen' ordr [] [] = ordr
compareLen' _ [] ys = LT
compareLen' _ xs [] = GT
compareLen' EQ   (x:xs) (y:ys) = compareLen' (compare x y) xs ys
compareLen' ordr (x:xs) (y:ys) = compareLen' ordr xs ys



