{-|
Module      : Metamorth.ForOutput.Functor.Cased
Description : A simple Functor with a boolean annotation
Copyright   : (c) David Wilson, 2024
License     : BSD-3

This module includes functions and types 
that will be used by the generated code.

This way, the code generator doesn't
have to generate static code that will
be the same regardless of the input files.

In the future, these modules may be moved
to a separate package.

This module provides a simple Functor,
akin to `Data.Functor.Identity.Identity`,
but with two constructors. The main use of
this is to provide an easy way to store
extra case data about a certain value.

-}


module Metamorth.ForOutput.Functor.Cased
  ( CasedValue(..)
  , extractValue
  , extractCased
  , extractCase
  , liftPred
  ) where

import Data.Foldable

-- | A simple `Functor` with two options.
--   Therefore, @CasedValue a@ is equivalent
--   to @(Bool, a)@.
data CasedValue a
  = MinVal a
  | MajVal a
  deriving (Eq, Ord)

extractValue :: CasedValue a -> a
extractValue = extractCased
{-# INLINE extractValue #-}

extractCased :: CasedValue a -> a
extractCased (MinVal x) = x
extractCased (MajVal x) = x

extractCase :: CasedValue a -> Bool
extractCase (MinVal _) = False
extractCase (MajVal _) = True

-- | Lift a predicate to work on any
--   instance of `Foldable`. This is
--   just `any`, but with a different
--   intent/purpose.
liftPred :: (Foldable f) => (a -> Bool) -> f a -> Bool
liftPred = any

-- Not a proper "Show" instance, but it makes output
-- easier to read.
instance (Show a) => Show (CasedValue a) where
  show (MinVal x) = "-" ++ show x
  show (MajVal x) = "+" ++ show x

instance Functor CasedValue where
  fmap f (MinVal x) = MinVal $ f x
  fmap f (MajVal x) = MajVal $ f x

-- Can't be an instance of Applicative,
-- since there's no default choice for
-- min/maj.

-- | Mostly used so that `any` can be used
--   to lift predicates over `CasedValue`.
instance Foldable CasedValue where
  foldMap f (MinVal x) = f x
  foldMap f (MajVal x) = f x
  foldr f y (MinVal x) = f x y
  foldr f y (MajVal x) = f x y
  foldl f x (MinVal y) = f x y
  foldl f x (MajVal y) = f x y

  -- Probably the most useful function.
  elem x y = x == extractCased y

  -- Functions that just extract
  -- the inner value
  foldr1 _ = extractCased
  foldl1 _ = extractCased
  maximum  = extractCased
  minimum  = extractCased
  product  = extractCased
  sum      = extractCased

  toList (MinVal x) = [x]
  toList (MajVal x) = [x]

  -- Instantly knowable functions
  length _ = 1
  null   _ = False

  -- Strict variants
  foldMap' f (MinVal x) = f $! x
  foldMap' f (MajVal x) = f $! x
  foldr' f y (MinVal x) = (f $! x) $! y
  foldr' f y (MajVal x) = (f $! x) $! y
  foldl' f x (MinVal y) = (f $! x) $! y
  foldl' f x (MajVal y) = (f $! x) $! y
