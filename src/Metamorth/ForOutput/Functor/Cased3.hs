{-|
Module      : Metamorth.ForOutput.Functor.Cased3
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
but with three constructors. The main use of
this is to provide an easy way to store
extra case data about a certain value.

Unlike "Metamorth.ForOutput.Functor.Cased",
this provides an extra Constructor for a
\"null\" case, since not all phonemes may
have a definite case.

NOTE: The function names, Constructors, and
type names overlap with those from 
"Metamorth.ForOutput.Functor.Cased", so don't
import them both unqualified in the same module.

-}


module Metamorth.ForOutput.Functor.Cased3
  ( CasedValue(..)
  , extractValue
  , extractCased
  , extractCase
  , fromCased2
  , liftPred
  ) where

import Data.Foldable

import Metamorth.ForOutput.Char (CharCase(..))

import Metamorth.ForOutput.Functor.Cased qualified as C2

-- | A simple `Functor` with two options.
--   Therefore, @CasedValue a@ is equivalent
--   to @(Bool, a)@.
data CasedValue a
  = NulVal a
  | MinVal a
  | MajVal a
  deriving (Eq, Ord)

extractValue :: CasedValue a -> a
extractValue = extractCased
{-# INLINE extractValue #-}

extractCased :: CasedValue a -> a
extractCased (NulVal x) = x
extractCased (MinVal x) = x
extractCased (MajVal x) = x

extractCase :: CasedValue a -> CharCase
extractCase (NulVal _) = NonCased
extractCase (MinVal _) = LowerCase
extractCase (MajVal _) = UpperCase

fromCased2 :: C2.CasedValue a -> CasedValue a
fromCased2 (C2.MinVal x) = MinVal x
fromCased2 (C2.MajVal x) = MajVal x

-- | Lift a predicate to work on any
--   instance of `Foldable`. This is
--   just `any`, but with a different
--   intent/purpose.
liftPred :: (Foldable f) => (a -> Bool) -> f a -> Bool
liftPred = any

-- Not a proper "Show" instance, but it makes output
-- easier to read.
instance (Show a) => Show (CasedValue a) where
  show (NulVal x) = "x" ++ show x
  show (MinVal x) = "-" ++ show x
  show (MajVal x) = "+" ++ show x

instance Functor CasedValue where
  fmap f (NulVal x) = NulVal $ f x
  fmap f (MinVal x) = MinVal $ f x
  fmap f (MajVal x) = MajVal $ f x

-- Can't be an instance of Applicative,
-- since there's no default choice for
-- min/maj.

-- | Mostly used so that `any` can be used
--   to lift predicates over `CasedValue`.
instance Foldable CasedValue where
  foldMap f (NulVal x) = f x
  foldMap f (MinVal x) = f x
  foldMap f (MajVal x) = f x
  
  foldr f y (NulVal x) = f x y
  foldr f y (MinVal x) = f x y
  foldr f y (MajVal x) = f x y
  
  foldl f x (NulVal y) = f x y
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

  toList (NulVal x) = [x]
  toList (MinVal x) = [x]
  toList (MajVal x) = [x]

  -- Instantly knowable functions
  length _ = 1
  null   _ = False

  -- Strict variants
  foldMap' f (NulVal x) = f $! x
  foldMap' f (MinVal x) = f $! x
  foldMap' f (MajVal x) = f $! x

  foldr' f y (NulVal x) = (f $! x) $! y
  foldr' f y (MinVal x) = (f $! x) $! y
  foldr' f y (MajVal x) = (f $! x) $! y
  
  foldl' f x (NulVal y) = (f $! x) $! y
  foldl' f x (MinVal y) = (f $! x) $! y
  foldl' f x (MajVal y) = (f $! x) $! y
