module Metamorth.Helpers.Map
  ( forWithKey
  , forWithKey_
  , fromSelfList
  , forMapWithKey
  , forMaybeMap
  , forMaybeMapWithKey
  , forIntersectionWithKey
  , lookupE
  , forMapFromSet
  , forMapFromSetM
  , mapKeysMaybe
  ) where

import Data.Functor (($>))
import Data.Functor.Identity

import Data.Maybe
import Data.Map.Strict qualified as M

import Data.Set qualified as S

-- | Like `M.traverseWithKey`, but with the
--   opposite argument order. Makes it easier
--   to do things like
-- 
--   > forWithKey myMap $ \key val -> do ...
-- 
forWithKey :: Applicative t => M.Map k a -> (k -> a -> t b) -> t (M.Map k b)
forWithKey mp f = M.traverseWithKey f mp
{-# INLINE forWithKey #-}

forWithKey_ :: Applicative t => M.Map k a -> (k -> a -> t b) -> t ()
forWithKey_ mp f = (M.traverseWithKey f mp) $> ()

-- | Create a `M.Map` from a simple list,
--   where every element is used as both
--   the key and the value.
fromSelfList :: Ord a => [a] -> M.Map a a
fromSelfList = M.fromList . (map (\x -> (x,x)))

-- | Like `M.MapWithKey`, but with the arguments
--   reversed.
forMapWithKey :: M.Map k a -> (k -> a -> b) -> M.Map k b
forMapWithKey mp f = M.mapWithKey f mp
{-# INLINE forMapWithKey #-}

forIntersectionWithKey :: (Ord k) => M.Map k a -> M.Map k b -> (k -> a -> b -> c) -> M.Map k c
forIntersectionWithKey mp1 mp2 f = M.intersectionWithKey f mp1 mp2
{-# INLINE forIntersectionWithKey #-}

forMaybeMap :: M.Map k a -> (a -> Maybe b) -> M.Map k b
forMaybeMap mp f = M.mapMaybe f mp

forMaybeMapWithKey :: M.Map k a -> (k -> a -> Maybe b) -> M.Map k b
forMaybeMapWithKey mp f = M.mapMaybeWithKey f mp

-- | Lookup an item, returning @Left $ show key@ if the
--   key isn't present.
lookupE :: (Ord k, Show k) => k -> M.Map k a -> Either String a
lookupE k mp = case (M.lookup k mp) of
  (Nothing) -> Left $ show k
  (Just  x) -> Right x

-- | Convert a `S.Set` to a `M.Map` using
--   an applicative action on each element.
forMapFromSet :: (Applicative m) => S.Set a -> (a -> m b) -> m (M.Map a b)
forMapFromSet st f = sequenceA $ M.fromSet f st

-- | Convert a `S.Set` to a `M.Map` using
--   a monadic action on each element.
forMapFromSetM :: (Monad m) => S.Set a -> (a -> m b) -> m (M.Map a b)
forMapFromSetM st f = sequence $ M.fromSet f st

mapKeysMaybe :: (Ord k, Ord k') => (k -> Maybe k') -> M.Map k a -> M.Map k' a
mapKeysMaybe f = M.fromList . M.foldrWithKey fr []
  where
    fr k x xs = case (f k) of
      Nothing   -> xs
      (Just k') -> (k', x) : xs

{-
mapKeys :: Ord k2 => (k1->k2) -> Map k1 a -> Map k2 a
mapKeys f = fromList . foldrWithKey (\k x xs -> (f k, x) : xs) []
-}

{- whoops
mapMapMaybeWithKey :: (k -> a -> Maybe b) -> M.Map k a -> M.Map k b
mapMapMaybeWithKey f mp = runIdentity $ M.traverseMaybeWithKey (\k a -> pure $ f k a)
-}