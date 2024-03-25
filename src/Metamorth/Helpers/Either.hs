module Metamorth.Helpers.Either 
  ( liftEitherList
  , liftEitherNonEmpty
  , eitherMaybe
  , eitherMaybe'
  , partitionWith
  ) where

import Data.Either
import Data.List qualified as L

import Data.List.NonEmpty qualified as NE

-- | Take a list of @`Either` a b@, returning
--   only the lefts if there are any lefts,
--   otherwise return the rights.
liftEitherList :: [Either a b] -> Either [a] [b]
liftEitherList xs
  | (L.null ls) = Right rs
  | otherwise   = Left  ls
  where 
    (ls,rs) = partitionEithers xs

-- | Same as `liftEitherList`, but over `NE.NonEmpty`.
liftEitherNonEmpty :: NE.NonEmpty (Either a b) -> Either (NE.NonEmpty a) (NE.NonEmpty b)
liftEitherNonEmpty xs
  | (Just ls') <- NE.nonEmpty ls
  = Left ls'
  | otherwise = Right $ NE.fromList rs
  where
    (ls, rs) = partitionEithers $ NE.toList xs


-- | Turn a `Maybe` into an `Either` by
--   supplying it with a default `Left`
--   value. Useful for lifting operations
--   that might fail in a function that
--   uses `Either` to report errors.
eitherMaybe :: a -> Maybe b -> Either a b
eitherMaybe _ (Just x) = Right x
eitherMaybe x Nothing  = Left  x

-- | The same as `eitherMaybe`, but with
--   the opposite argument order
eitherMaybe' :: Maybe b -> a -> Either a b
eitherMaybe' (Just x) _  = Right x
eitherMaybe' Nothing  x  = Left  x

-- | Uses a function to determine which of two output lists an input element should join.
--
--   Taken from `GHC.Utils.Misc`.
partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith _ [] = ([],[])
partitionWith f (x:xs) = case f x of
                         Left  b -> (b:bs, cs)
                         Right c -> (bs, c:cs)
    where (bs,cs) = partitionWith f xs
