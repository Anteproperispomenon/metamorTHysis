{-|
Module      : Metamorth.Interpretation.Parser.Parsing.Boolean
Description : Boolean Trees
Copyright   : (c) David Wilson, 2024
License     : BSD-3

This is a module where I create a type that
represents a boolean expression. The reason
to do it expressly here is so that it can
be made an instance of `Traversable`, so
we can easily lift existing code over a full
expression.

-}

module Metamorth.Interpretation.Parser.Parsing.Boolean
  -- * Binary And/Or Only
  ( Boolean2(..)
  , evaluate2
  , leftmostB2
  , showBoolTree2
  , showBoolTree2'
  -- * Arbitrary length And/Or
  , BooleanA(..)
  , evaluateA
  ) where

import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty(..))

import Data.Semigroup

-- | A boolean expression tree tree that
--   only allows two sub-expressions for 
--   "AND" and "OR".
data Boolean2 a
  = PlainB2 a
  | NotB2 (Boolean2 a)
  | AndB2 (Boolean2 a) (Boolean2 a)
  | OrB2  (Boolean2 a) (Boolean2 a)
  deriving (Show, Eq, Ord)

instance Functor Boolean2 where
  fmap f (PlainB2 a) = PlainB2 (f a)
  fmap f (NotB2 x)   = NotB2 (fmap f x)
  fmap f (AndB2 x y) = AndB2 (fmap f x) (fmap f y)
  fmap f (OrB2  x y) = OrB2  (fmap f x) (fmap f y)

instance Foldable Boolean2 where
  foldMap f (PlainB2 x) = f x
  foldMap f (NotB2   x) = foldMap f x
  foldMap f (AndB2 x y) = (foldMap f x) <> (foldMap f y)
  foldMap f (OrB2  x y) = (foldMap f x) <> (foldMap f y)

  -- foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr ap acc (PlainB2 x) = ap x acc
  foldr ap acc (NotB2   x) = foldr ap acc x 
  foldr ap acc (AndB2 x y) = foldr ap (foldr ap acc y) x 
  foldr ap acc (OrB2  x y) = foldr ap (foldr ap acc y) x 

  -- foldl :: (b -> a -> b) -> b -> [a] -> b
  foldl ap acc (PlainB2 x) = ap acc x
  foldl ap acc (NotB2   x) = foldl ap acc x
  foldl ap acc (AndB2 x y) = foldl ap (foldl ap acc x) y
  foldl ap acc (OrB2  x y) = foldl ap (foldl ap acc x) y

-- Now, the hard one...
instance Traversable Boolean2 where
  -- sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA (PlainB2    act1) = PlainB2 <$> act1
  sequenceA (NotB2      act1) = NotB2   <$> sequenceA act1
  sequenceA (AndB2 act1 act2) = AndB2   <$> sequenceA act1 <*> sequenceA act2
  sequenceA (OrB2  act1 act2) = OrB2    <$> sequenceA act1 <*> sequenceA act2

  -- traverse :: Applicative f (a -> f b) -> t a -> f (t b)
  traverse f (PlainB2 x) = PlainB2 <$> f x
  traverse f (NotB2   x) = NotB2   <$> traverse f x
  traverse f (AndB2 x y) = AndB2   <$> traverse f x <*> traverse f y
  traverse f (OrB2  x y) = OrB2    <$> traverse f x <*> traverse f y

-- | Evaluate a `Boolean2` structure by
--   applying a predicate to every entry
--   in the tree.
evaluate2 :: (a -> Bool) -> Boolean2 a -> Bool
evaluate2 f (PlainB2 x) = f x
evaluate2 f (NotB2   x) = not (evaluate2 f x)
evaluate2 f (AndB2 x y) = (evaluate2 f x) && (evaluate2 f y)
evaluate2 f (OrB2  x y) = (evaluate2 f x) || (evaluate2 f y)

-- | Extract the "leftmost" element from
--   a `Boolean2` tree.
leftmostB2 :: Boolean2 a -> a
leftmostB2 (PlainB2 x) = x
leftmostB2 (NotB2   x) = leftmostB2 x
leftmostB2 (AndB2 x _) = leftmostB2 x
leftmostB2 (OrB2  x _) = leftmostB2 x

showBoolTree2 :: (Show a) => Boolean2 a -> String
showBoolTree2 (PlainB2 x) = show x
showBoolTree2 (NotB2   x) = '~' : (showBoolTree2 x)
showBoolTree2 (AndB2 x y) = '(' : (showBoolTree2 x) ++ '&' : (showBoolTree2 y) ++ ")"
showBoolTree2 (OrB2  x y) = '(' : (showBoolTree2 x) ++ '|' : (showBoolTree2 y) ++ ")"

showBoolTree2' :: (a -> String) -> Boolean2 a -> String
showBoolTree2' f (PlainB2 x) = f x
showBoolTree2' f (NotB2   x) = '~' : (showBoolTree2' f x)
showBoolTree2' f (AndB2 x y) = '(' : (showBoolTree2' f x) ++ '&' : (showBoolTree2' f y) ++ ")"
showBoolTree2' f (OrB2  x y) = '(' : (showBoolTree2' f x) ++ '|' : (showBoolTree2' f y) ++ ")"


-- | A boolean expression tree tree that
--   arbitrarily many sub-expressions for
--   "AND" and "OR".
data BooleanA a
  = PlainBA a
  | NotBA (BooleanA a)
  | AndBA (NonEmpty (BooleanA a))
  | OrBA  (NonEmpty (BooleanA a))
  deriving (Show, Eq, Ord)

instance Functor BooleanA where
  fmap f (PlainBA a) = PlainBA (f a)
  fmap f (NotBA   x) = NotBA   (fmap f x)
  fmap f (AndBA  xs) = AndBA   (fmap (fmap f) xs)
  fmap f (OrBA   xs) = OrBA    (fmap (fmap f) xs)

instance Foldable BooleanA where
  foldMap f (PlainBA x) = f x
  foldMap f (NotBA   x) = foldMap f x
  foldMap f (AndBA  xs) = sconcat (fmap (foldMap f) xs)
  foldMap f (OrBA   xs) = sconcat (fmap (foldMap f) xs)

  -- foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr ap acc (PlainBA x) = ap x acc
  foldr ap acc (NotBA   x) = foldr ap acc x 
  foldr ap acc (AndBA  xs) = foldr (\x y -> foldr ap y x) acc xs
  foldr ap acc (OrBA   xs) = foldr (\x y -> foldr ap y x) acc xs

  -- foldl :: (b -> a -> b) -> b -> [a] -> b
  foldl ap acc (PlainBA x) = ap acc x
  foldl ap acc (NotBA   x) = foldl ap acc x
  foldl ap acc (AndBA  xs) = foldl (foldl ap) acc xs
  foldl ap acc (OrBA   xs) = foldl (foldl ap) acc xs

-- Now, the hard one...
instance Traversable BooleanA where
  -- sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA (PlainBA action) = PlainBA <$> action
  sequenceA (NotBA   action) = NotBA   <$> sequenceA action
  sequenceA (AndBA  actions) = AndBA   <$> traverse sequenceA actions
  sequenceA (OrBA   actions) = OrBA    <$> traverse sequenceA actions

  -- traverse :: Applicative f (a -> f b) -> t a -> f (t b)
  traverse f (PlainBA x) = PlainBA <$> f x
  traverse f (NotBA   x) = NotBA   <$> traverse f x
  traverse f (AndBA  xs) = AndBA   <$> traverse (traverse f) xs
  traverse f (OrBA   xs) = OrBA    <$> traverse (traverse f) xs
  -- xs :: (NonEmpty (BooleanA a))

-- | Evaluate a `BooleanA` structure by
--   applying a predicate to every entry
--   in the tree.
evaluateA :: (a -> Bool) -> BooleanA a -> Bool
evaluateA f (PlainBA x) = f x
evaluateA f (NotBA   x) = not (evaluateA f x)
evaluateA f (AndBA  xs) = all (evaluateA f) xs
evaluateA f (OrBA   xs) = any (evaluateA f) xs

