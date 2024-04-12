{-# LANGUAGE TupleSections #-}

{-|
Module      : Metamorth.ForOutput.Monad.Matcher.Stateful.Result
Description : Helper for Stateful MatcherT
Copyright   : (c) David Wilson, 2024
License     : BSD-3

This module includes functions and types 
that will be used by the generated code.

This way, the code generator doesn't
have to generate static code that will
be the same regardless of the input files.

In the future, these modules may be moved
to a separate package.

This particular module defines a simple
type to be used to define matchers/parsers
for MatcherT. This particular version
is meant to be used with the Stateful
version of `Metamorth.ForOutput.Monad.Matcher.Stateful.MatcherT`.

-}


module Metamorth.ForOutput.Monad.Matcher.Stateful.Result
  ( MatchResult(..)
  , MatchReturn(..)
  , checkStateReturn
  , checkConditionalStateReturn
  ) where

import Control.Arrow (first)

-- | How to match lists in a `Metamorth.ForOutput.Monad.Matcher.MatcherT`.
--   Usually called as @(i -> MatchResult m i v r)@, where @i@ is an input
--   item from the input buffer\/stream\/list. e.g.
--
--   @
--   data Example = A | B | C | D deriving (Show, Eq, Ord)
--
--   plainReturn :: r -> MatchReturn m i v s r
--   plainReturn x = PlainReturn $ \_ _ -> x
--
--   myMatch :: Example -> MatchResult Maybe Example () () String
--   myMatch A = MatchReturn $ plainReturn "(a)"
--   myMatch B = MatchContinue myMatch2
--   myMatch C = MatchOptions (plainReturn "(c)") myMatch3
--   myMatch D = MatchReturn $ plainReturn "(d)"
--
--   myMatch2 :: Example -> MatchResult Maybe Example () () String
--   myMatch2 A = MatchReturn $ plainReturn "(ba)"
--   myMatch2 B = MatchReturn $ plainReturn "(bb)"
--   myMatch2 C = MatchReturn $ plainReturn "(bc)"
--   myMatch2 D = MatchOptions (plainReturn "(bd)") myMatch4
--
--   myMatch3 :: Example -> MatchResult Maybe Example () () String
--   myMatch3 A = MatchFail Nothing
--   myMatch3 B = MatchReturn $ plainReturn "(cb)"
--   myMatch3 C = ('c':) <$> MatchContinue myMatch4
--   myMatch3 D = MatchReturn $ plainReturn "(cd)"
--
--   myMatch4 :: Example -> MatchResult Maybe Example () () String
--   myMatch4 A = MatchReturn $ plainReturn "[bda]"
--   myMatch4 B = MatchFail Nothing
--   myMatch4 C = MatchReturn $ plainReturn "[bdc]"
--   myMatch4 D = MatchReturn $ plainReturn "[bdd]"
--   @   
--   
--   This could then be run with `Metamorth.ForOutput.Monad.Matcher.Stateful.matches`
--   to produce a `MatcherT` action that would continually 
--   run the matcher until the input stream is empty.
data MatchResult m i v s r
  -- | There is only one option, which
  --   is to consume nothing and produce
  --   a value.
  = MatchReturn (MatchReturn m i v s r)
  -- | Continue matching the input using this
  --   function next. Typically run in the same
  --   way as the initial step (see above).
  | MatchContinue (i -> MatchResult m i v s r)
  -- | The `MatcherT` can either return here,
  --   or continue matching data from the input
  --   buffer. i.e. if the "Continue path" leads
  --   to a `MatchFail` result, then the input
  --   will be rewound, and the return value
  --   here will be run instead.
  | MatchOptions (MatchReturn m i v s r) (i -> MatchResult m i v s r)
  -- | Used for paths that aren't a valid match. 
  --   When writing a `MatchContinue` function, 
  --   you should use this together with a wildcard,
  --   e.g. 
  --
  --   @
  --   ... MatchContinue $ \case
  --     A -> ...
  --     ...
  --     _ -> MatchFail "Bad Path"
  --   @
  --
  --   So that the function isn't partial.
  | MatchFail String

instance (Functor m) => Functor (MatchResult m i v s) where
  -- fmap f (MatchReturn rets)       = MatchReturn (map (fmap f) rets)
  fmap f (MatchReturn rets)       = MatchReturn (fmap f rets)
  fmap f (MatchContinue cont)     = MatchContinue $ \inp -> f <$> cont inp
  fmap f (MatchOptions rets cont) = MatchOptions (fmap f rets) (\inp -> f <$> cont inp)
  -- fmap f (MatchOptions rets cont) = MatchOptions (map (fmap f) rets) (\inp -> f <$> cont inp)
  fmap _ (MatchFail x) = MatchFail x

-- | Matching on return values. This is to
--   provide more functionaliy without having
--   to create multiple Constructors in `MatchResult`
--   for each kind of Return.
data MatchReturn m i v s r
  -- | The usual return. The @v@ in the
  --   input a `Monoid` collected from 
  --   the input of the items parsed
  --   so far. If you want to ignore it,
  --   just use @PlainReturn (\_ -> pure x)@.
  = PlainReturn (v -> m r)
  | StateReturn (v -> s -> m (r,s))
  -- | Returning the results, but using
  --   the next value in the stream to
  --   determine what exactly should be
  --   returned. 
  | ConditionalReturn (v -> Maybe i -> m r)
  | ConditionalStateReturn (v -> Maybe i -> s -> m (r,s))

-- | A simpler version of `StateReturn` that doesn't
--   modify the state.
checkStateReturn :: (Functor m) => (v -> s -> m r) -> MatchReturn m i v s r
checkStateReturn f = StateReturn $ \v s -> (,s) <$> f v s

checkConditionalStateReturn :: (Functor m) => (v -> Maybe i -> s -> m r) -> MatchReturn m i v s r
checkConditionalStateReturn f = ConditionalStateReturn $ \v i s -> (,s) <$> f v i s

instance (Functor m) => Functor (MatchReturn m i v s) where
  fmap f (PlainReturn ret) = PlainReturn $ \v -> f <$> ret v
  fmap f (StateReturn ret) = StateReturn $ \v s -> (first f) <$> ret v s
  fmap f (ConditionalReturn ret) 
    = ConditionalReturn $ \v i -> f <$> ret v i
  fmap f (ConditionalStateReturn ret)
    = ConditionalStateReturn $ \v i s -> (first f) <$> ret v i s
  
