{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : Metamorth.ForOutput.Monad.Matcher.Stateful.Extra
Description : Custom Monad type used by Generated Code
Copyright   : (c) David Wilson, 2024
License     : BSD-3

This module includes functions and types 
that will be used by the generated code.

This way, the code generator doesn't
have to generate static code that will
be the same regardless of the input files.

In the future, these modules may be moved
to a separate package.

This module is an add-on to "Metamorth.ForOutput.Monad.Matcher.Stateful"
that allows pre-processing of 

-}

module Metamorth.ForOutput.Monad.Matcher.Stateful.Extra
  -- * Primary Operations
  ( matchX
  , matchesX
  , matchesLX
  , matchesLX'
  , matchesDefLX
  , matchesRX
  , matchesRX'
  -- ** Automatic MonadFail Versions
  , matchFX
  , matchesFX
  , matchesLFX
  , matchesLFX'
  , matchesDefLFX
  , matchesRFX
  , matchesRFX'
  -- ** Simple Matchers
  , matchSimpleX
  , matchesSimpleX
  , matchesSimpleDefX
  ) where

import Control.Applicative

import Control.Monad
import Control.Monad.Trans.Class

import Metamorth.ForOutput.Monad.Matcher.Stateful.Result

import Metamorth.ForOutput.Monad.Matcher.Stateful

-- | Match on the input stream by applying a `MatchResult` action
--   to the next value in the stream, and then either:
--
--     - Returning, if the the action returns a `MatchReturn`.
--     - Throwing an error, if the action returns a `MatchFail`.
--     - Running `match` on the next value in the stream, if the action returns a `MatchContinue` or `MatchOptions`.
matchX :: (MonadPlus m, Monoid v) => (i -> j) -> (String -> m r) -> (j -> MatchResult m j v s r) -> MatcherT i v s m r
matchX prp err f = do
  mybX <- proceed' prp
  case mybX of
    Nothing  -> lift $ err "Not Enough Input."
    (Just x) -> do
      case (f x) of
        -- MatchReturn rets -> msum $ map matchReturn rets
        MatchReturn ret  -> matchReturnX prp ret
        MatchContinue mc -> matchX prp err mc
        MatchFail fstr   -> lift $ err (fstr x)
        MatchOptions ret cont
          -> matchX prp err cont <|> matchReturnX prp ret
          -- -> match err cont <|> msum (map matchReturn ret)

-- matchReturn :: (MonadPlus m, Monoid v) => (String -> m r) -> MatchReturn m i v r -> MatcherT i v m r
matchReturnX :: (MonadPlus m, Monoid v) => (i -> j) -> MatchReturn m j v s r -> MatcherT i v s m r
matchReturnX prp (PlainReturn f) = do
  vs <- pullValues
  lift $ f vs
matchReturnX prp (StateReturn f) = do
  vs <- pullValues
  st <- get
  (rslt, st') <- lift $ f vs st
  put $! st'
  return rslt
matchReturnX prp (ConditionalReturn f) = do
  mi <- fmap prp <$> preview
  vs <- pullValues
  lift $ f vs mi
matchReturnX prp (ConditionalStateReturn f) = do
  mi <- fmap prp <$> preview
  vs <- pullValues
  st <- get
  (rslt, st') <- lift $ f vs mi st
  put $! st'
  return rslt
--   eRslt <- lift $ f vs mi
--   case eRslt of
--     (Left str) -> lift $ err str
--     (Right vl) -> return vl

-- | Run `match` repeatedly until the input buffer
--   is empty, and accumulate the results in a list.
--   This list uses a right-fold to process the elements,
--   i.e.
--
--   @
--   matches err f == (rslt1 : ) <$> (rslt2 : ) <$> (rslt3 : ) <$> (return [])
--   @
--
--   If this isn't working well for whatever reason,
--   try using `matchesL` or `matchesR` with a 
--   different `Monoid`.
matchesX :: (MonadPlus m, Monoid v) => (i -> j) -> (String -> m r) -> (j -> MatchResult m j v s r) -> MatcherT i v s m [r]
matchesX prp err f = do
  mybX <- {- fmap prp <$> -} preview
  case mybX of
    Nothing  -> return []
    (Just _) -> do
      y <- matchX prp err f
      (y:) <$> matchesX prp err f

-- | Run `matchX` repeatedly until the input buffer is
--   empty, accumulating the results in the returning 
--   `Monoid` from left-to-right. i.e.
--
--   @
--   matchesL prp err f == (((rslt1 <> rslt2) <> rslt3) <> rslt4)
--   matchesL prp err f == ((((mempty <> rslt1) <> rslt2) <> rslt3) <> rslt4)
--   @
--   
--   Note that it only returns `mempty` if there is
--   nothing in the input buffer.
--
--   This function is usefuly for when the output value
--   is one intended to be used as a `Monoid`/`Semigroup`,
--   e.g. a ByteString `Data.ByteString.Builder.Builder`.
--
--   If you'd rather supply an initial value instead of
--   using mempty, use `matchesDefL` instead.
matchesLX :: (MonadPlus m, Monoid v, Monoid r) => (i -> j) -> (String -> m r) -> (j -> MatchResult m j v s r) -> MatcherT i v s m r
matchesLX prp err f = do
  mybX <- preview
  case mybX of
    Nothing  -> return mempty
    (Just _) -> do
      y <- matchX prp err f
      matchesDefLX prp y err f

-- | Like `matchesLX`, but uses a different function
--   for the first match.
matchesLX' :: (MonadPlus m, Monoid v, Monoid r) => (i -> j) -> (String -> m r) -> (j -> MatchResult m j v s r) -> (j -> MatchResult m j v s r) -> MatcherT i v s m r
matchesLX' prp err f1 f2 = do
  mybX <- preview
  case mybX of
    Nothing  -> return mempty
    (Just _) -> do
      y <- matchX prp err f1
      matchesDefLX prp y err f2


-- | Run `match` repeatedly until the input buffer is
--   empty, accumulating the results in the returning 
--   `Semigroup` from left-to-right. i.e.
--
--   @
--   matchesL def err f == ((((def <> rslt1) <> rslt2) <> rslt3) <> rslt4)
--   @
--   
--   Note that it only returns @def@ if there is
--   nothing in the input buffer.
--
--   This function can be useful if chaining several
--   `runMatcherT` operations in a row, e.g.
--
--   @
--   matchTexts txt1 txt2 txt3 = do
--     rslt1 <- runMatcherT txt1 myFunc (matchesL          myErr myMatch)
--     rslt2 <- runMatcherT txt2 myFunc (matchesDefL rslt1 myErr myMatch)
--     rslt3 <- runMatcherT txt3 myFunc (matchesDefL rslt2 myErr myMatch)
--     return rslt3
--     where
--       myMatch = \case ...
--       myFunc  = \_ -> ()
--       myErr   = \str -> ...
--   @
--
--   If you'd rather just use `mempty` for @def@, use
--   `matchesL` instead.
matchesDefLX :: (MonadPlus m, Monoid v, Semigroup r) => (i -> j) -> r -> (String -> m r) -> (j -> MatchResult m j v s r) -> MatcherT i v s m r
matchesDefLX prp acc err f = do
  mybX <- preview
  case mybX of
    Nothing  -> return acc
    (Just _) -> do
      y <- matchX prp err f
      matchesDefLX prp (acc <> y) err f

-- | Variant of `match` that uses `fail` from `MonadFail`
--   as the first argument.
matchFX :: (MonadPlus m, MonadFail m, Monoid v) => (i -> j) -> (j -> MatchResult m j v s r) -> MatcherT i v s m r
matchFX prp = matchX prp fail

-- | Variant of `matches` that uses `fail` from `MonadFail`
--   as the first argument.
matchesFX :: (MonadPlus m, MonadFail m, Monoid v) => (i -> j) -> (j -> MatchResult m j v s r) -> MatcherT i v s m [r]
matchesFX prp = matchesX prp fail

-- | Variant of `matchesL` that uses `fail` from `MonadFail`
--   as the first argument.
matchesLFX :: (MonadPlus m, MonadFail m, Monoid v, Monoid r) => (i -> j) -> (j -> MatchResult m j v s r) -> MatcherT i v s m r
matchesLFX prp = matchesLX prp fail

-- | Variant of `matchesL'` that uses `fail` from `MonadFail`
--   as the first argument.
matchesLFX' :: (MonadPlus m, MonadFail m, Monoid v, Monoid r) => (i -> j) -> (j -> MatchResult m j v s r) -> (j -> MatchResult m j v s r) -> MatcherT i v s m r
matchesLFX' prp = matchesLX' prp fail

-- | Variant of `matchesDefL` that uses `fail` from `MonadFail`
--   as the second argument
matchesDefLFX :: (MonadPlus m, MonadFail m, Monoid v, Semigroup r) => (i -> j) -> r -> (j -> MatchResult m j v s r) -> MatcherT i v s m r
matchesDefLFX prp acc = matchesDefLX prp acc fail

-- | A variant of `matches` that uses a `Monoid` instead
--   of a list to accumulate the results. This version
--   is left-associative, so it should work better with
--   `Data.Text.Lazy.Builder.Builder` from "Data.Text.Lazy.Builder".
matchesRX :: (MonadPlus m, Monoid v, Monoid r) => (i -> j) -> (String -> m r) -> (j -> MatchResult m j v s r) -> MatcherT i v s m r
matchesRX prp err f = do
  mybX <- preview
  case mybX of
    Nothing  -> return mempty
    _ -> do
      y <- matchX prp err f
      (y <>) <$> matchesRX prp err f

-- | Like `matchesL`, but uses a different function
--   for the first match.
matchesRX' :: (MonadPlus m, Monoid v, Monoid r) => (i -> j) -> (String -> m r) -> (j -> MatchResult m j v s r) -> (j -> MatchResult m j v s r) -> MatcherT i v s m r
matchesRX' prp err f1 f2 = do
  mybX <- preview
  case mybX of
    Nothing -> return mempty
    _ -> do
      y <- matchX prp err f1
      (y <>) <$> matchesRX prp err f2

-- | Like `matchesL`, but uses `fail` for
--   the first argument.
matchesRFX :: (MonadPlus m, MonadFail m, Monoid v, Monoid r) => (i -> j) -> (j -> MatchResult m j v s r) -> MatcherT i v s m r
matchesRFX prp = matchesRX prp fail

matchesRFX' :: (MonadPlus m, MonadFail m, Monoid v, Monoid r) => (i -> j) -> (j -> MatchResult m j v s r) -> (j -> MatchResult m j v s r) -> MatcherT i v s m r
matchesRFX' prp = matchesRX' prp fail

----------------------------------------------------------------
-- Matching Something Else

-- (i -> v) -> [i] -> v -> s -> m (a, [i], v, s) }

-- | Run a different matching algorithm on
--   a different list, and then return the
--   result and restore the original state.
--   Useful if the input list contains lists
--   itself.
-- matchElse :: (Monad m, Monoid w) => (j -> w) -> [j] -> s' -> MatcherT j w s' m r -> MatcherT i v s m r
-- matchElse newConv lst st action = MatcherT $ \_ inp v oldSt -> do
--   rslt <- evalMatcherT newConv lst st action
--   return (rslt, inp, v, oldSt)

-- | A variant of `matchElse` that runs a stateful
--   computation as its sub-action, instead of
--   just a plain action. 
-- matchElseS :: (Monad m, Monoid w) => (j -> w) -> [j] -> s' -> (s -> MatcherT j w s' m (r,s)) -> MatcherT i v s m r
-- matchElseS newConv lst st action = MatcherT $ \_ inp v oldSt -> do
--   (rslt, finalSt) <- evalMatcherT newConv lst st (action oldSt)
--   return (rslt, inp, v, finalSt)

----------------------------------------------------------------
-- Simple Matchers

-- | A simple matcher that just uses a simple
--   function type instead of a complex secondary
--   type.
matchSimpleX :: (Monad m, Monoid v) => (i -> j) -> (j -> MatcherT i v s m r) -> MatcherT i v s m (Maybe r)
matchSimpleX prp action = do
  x <- proceed' prp
  case x of
    Nothing  -> return Nothing
    (Just y) -> Just <$> action y

-- | Repeat `matchSimple` until out of input.
matchesSimpleX :: (Monad m, Monoid v, Monoid r) => (i -> j) -> (j -> MatcherT i v s m r) -> MatcherT i v s m r
matchesSimpleX prp action = do
  x <- matchSimpleX prp action
  case x of
    Nothing     -> return mempty
    (Just rslt) -> matchesSimpleDefX prp rslt action

-- | Repeat `matchSimple` until no input left.
--   Instead of starting out from empty, the
--   accumulated value starts out with a default
--   value.
matchesSimpleDefX :: (Monad m, Monoid v, Semigroup r) => (i -> j) -> r -> (j -> MatcherT i v s m r) -> MatcherT i v s m r
matchesSimpleDefX prp defVal action = do
  x <- matchSimpleX prp action
  case x of
    Nothing     -> return defVal
    (Just rslt) -> matchesSimpleDefX prp (defVal <> rslt) action
