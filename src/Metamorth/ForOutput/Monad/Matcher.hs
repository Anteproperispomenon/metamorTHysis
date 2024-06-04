{-# LANGUAGE TupleSections #-}

{-|
Module      : Metamorth.ForOutput.Monad.Matcher
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

This particular module defines a simple
Parser-Like Monad that can be used to
match simple input buffers.

-}

module Metamorth.ForOutput.Monad.Matcher
  -- * Main Types
  -- ** Simplified Types
  ( Matcher
  , runMatcher
  , evalMatcher
  , MatcherE
  , runMatcherE
  , evalMatcherE
  -- ** Transformer Type
  , MatcherT
  , runMatcherT
  , evalMatcherT
  -- * Primary Operations
  , match
  , matches
  , matchesL
  , matchesL'
  , matchesDefL
  , matchesR
  , matchesR'
  -- ** Automatic MonadFail Versions
  , matchF
  , matchesF
  , matchesLF
  , matchesLF'
  , matchesDefLF
  , matchesRF
  , matchesRF'
  -- ** Changing what to match
  , matchElse
  -- ** Simple Matchers
  , matchSimple
  , matchesSimple
  , matchesSimpleDef
  -- * Low-Level Operations
  , proceed
  , preview
  -- * Sub Types
  , MatchResult(..)
  , MatchReturn(..)
  ) where

import Control.Applicative

import Control.Monad
import Control.Monad.Trans.Class

import Metamorth.ForOutput.Monad.Matcher.Result

import Metamorth.ForOutput.Monad.EitherFail

-- | A simple matcher that uses `Maybe` as the
--   base `Monad`. Probably the easiest version
--   to use, but it won't return error messages.
type Matcher i v = MatcherT i v Maybe

-- | A simple matcher that works very similarly
--   to `Matcher`, but instead uses a variant
--   of `Either` as its base `Monad`. This variant
--   has instances for `Alternative`, `MonadFail`,
--   and a few other related typeclasses that make
--   it suitable for matching/parsing.
type MatcherE i v = MatcherT i v EitherFail

-- | A `Monad` akin to a parser that consumes
--   values from an input list, converts them
--   to a `Monoid` value and appends it to a
--   list, and then returns the item it processed.
--
--   The main functions with @MatcherT@ are
--   `match`, `matches`, etc..., along with
--   the lower-level functions `proceed` and
--   `preview`.
newtype MatcherT i v m a  = MatcherT { getMatcherT :: (i -> v) -> [i] -> v -> m (a, [i], v) }

-- | Run a `MatcherT`, returning the result
--   value, along with the remaining input
--   and output streams.
runMatcherT :: (Monoid v) 
  => (i -> v) -- ^ A function to convert input items to an output stream value.
  -> [i]      -- ^ The input stream to parse
  -> MatcherT i v m a -- ^ The action to parse the input stream.
  -> m (a, [i], v)
runMatcherT func inp mt = getMatcherT mt func inp mempty

-- | Run a `Matcher`, returning the result
--   value, along with the remaining input
--   and output streams.
runMatcher :: (Monoid v) => (i -> v) -> [i] -> Matcher i v a -> Maybe (a, [i], v)
runMatcher = runMatcherT

-- | Run a `MatcherE`, returning the result value,
--   along with the remaining input and output
--   streams.
runMatcherE :: (Monoid v) => (i -> v) -> [i] -> MatcherE i v a -> Either String (a, [i], v)
runMatcherE x y = toEither . runMatcherT x y

-- | Run a `MatcherT`, only returning the result value.
evalMatcherT :: (Functor m, Monoid v) => (i -> v) -> [i] -> MatcherT i v m a -> m a
evalMatcherT func inp mt = (\(x,_,_) -> x) <$> getMatcherT mt func inp mempty

-- | Run a `Matcher`, only returning the result value.
evalMatcher :: (Monoid v) => (i -> v) -> [i] -> Matcher i v a -> Maybe a
evalMatcher = evalMatcherT

-- | Run a `MatcherE`, only returning the result value.
evalMatcherE :: (Monoid v) => (i -> v) -> [i] -> MatcherE i v a -> Either String a
evalMatcherE x y = toEither . evalMatcherT x y
-- also what is that constraint???

instance (Functor m) => Functor (MatcherT i v m) where
  fmap f (MatcherT mt) = MatcherT $ \ifnc inp vs -> fmap1'3 f $ mt ifnc inp vs

fmap1'3 :: Functor f => (a -> a') -> f (a,b,c) -> f (a',b,c)
fmap1'3 f = fmap (con1'3 f)
  where
    con1'3 g (x,y,z) = (g x, y, z)

instance (Monad m) => Applicative (MatcherT i v m) where
  pure x = MatcherT $ \_ y z -> pure (x,y,z)
  (MatcherT mt1) <*> (MatcherT mt2) 
    = MatcherT $ \ifnc inp vs -> do
        (f, inp1, vs1) <- mt1 ifnc inp  vs
        (x, inp2, vs2) <- mt2 ifnc inp1 vs1
        return (f x, inp2, vs2)
  (MatcherT mt1)  *> (MatcherT mt2)
    = MatcherT $ \ifnc inp vs -> do
        (_, inp1, vs1) <- mt1 ifnc inp  vs
        (x, inp2, vs2) <- mt2 ifnc inp1 vs1
        return (x, inp2, vs2)
  (MatcherT mt1) <*  (MatcherT mt2)
    = MatcherT $ \ifnc inp vs -> do
        (x, inp1, vs1) <- mt1 ifnc inp  vs
        (_, inp2, vs2) <- mt2 ifnc inp1 vs1
        return (x, inp2, vs2)

instance (Monad m) => Monad (MatcherT i v m) where
  (MatcherT mt1) >>= f
    = MatcherT $ \ifnc inp vs -> do
        (x, inp2, vs2) <- mt1 ifnc inp vs
        getMatcherT (f x) ifnc inp2 vs2

instance (MonadPlus m) => Alternative (MatcherT i v m) where
  empty = MatcherT $ \_ _ _ -> mzero
  (MatcherT mt1) <|> (MatcherT mt2) 
    = MatcherT $ \ifnc inp vs -> (mt1 ifnc inp vs) `mplus` (mt2 ifnc inp vs)

instance (MonadPlus m) => MonadPlus (MatcherT i v m)

instance (MonadFail m) => MonadFail (MatcherT i v m) where
  fail = lift . fail

instance MonadTrans (MatcherT i v) where
  lift action = MatcherT $ \_ inp vs -> (,inp,vs) <$> action

-- | Consume an item from the input, process
--   it with the conversion function, add the
--   produced code to the output stream, and
--   return the consumed input.
proceed :: (Semigroup v, Applicative m) => MatcherT i v m (Maybe i)
proceed = MatcherT $ \ifnc inp vs -> case inp of
  []     -> pure (Nothing, inp, vs)
  (x:xs) -> pure (Just  x,  xs, vs <> ifnc x)

-- | Equivalent to `Data.Attoparsec.Text.peekChar`,
--   previewing the next character without removing
--   it from the stack.
preview :: Applicative m => MatcherT i v m (Maybe i)
preview = MatcherT $ \_ifnc inp vs -> case inp of
  []    -> pure (Nothing, inp, vs)
  (x:_) -> pure (Just  x, inp, vs)

-- | Extract the values from the accumulator, leaving
--   a blank accumulator in its place.
pullValues :: (Applicative m, Monoid v) => MatcherT i v m v
pullValues = MatcherT $ \_ifnc inp vs -> pure (vs, inp, mempty)

match :: (MonadPlus m, Monoid v) => (String -> m r) -> (i -> MatchResult m i v r) -> MatcherT i v m r
match err f = do
  mybX <- proceed
  case mybX of
    Nothing  -> lift $ err "Not Enough Input."
    (Just x) -> do
      case (f x) of
        MatchReturn rets -> msum (map matchReturn rets)
        MatchContinue mc -> match err mc
        MatchFail str    -> lift $ err str
        MatchOptions rets cont
          -> match err cont <|> msum (map matchReturn rets)

-- matchReturn :: (MonadPlus m, Monoid v) => (String -> m r) -> MatchReturn m i v r -> MatcherT i v m r
matchReturn :: (MonadPlus m, Monoid v) => MatchReturn m i v r -> MatcherT i v m r
matchReturn (PlainReturn f) = do
  vs <- pullValues
  lift $ f vs
matchReturn (ConditionalReturn f) = do
  mi <- preview
  vs <- pullValues
  lift $ f vs mi
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
--   try using `matchesL` with a different `Monoid`.
matches :: (MonadPlus m, Monoid v) => (String -> m r) -> (i -> MatchResult m i v r) -> MatcherT i v m [r]
matches err f = do
  mybX <- preview
  case mybX of
    Nothing  -> return []
    (Just _) -> do
      y <- match err f
      (y:) <$> matches err f

-- | Run `match` repeatedly until the input buffer is
--   empty, accumulating the results in the returning 
--   `Monoid` from left-to-right. i.e.
--
--   @
--   matchesL err f == (((rslt1 <> rslt2) <> rslt3) <> rslt4)
--   matchesL err f == ((((mempty <> rslt1) <> rslt2) <> rslt3) <> rslt4)
--   @
--   
--   Note that it only returns `mempty` if there is
--   nothing in the input buffer.
--
--   This function is usefuly for when the output value
--   is one intended to be used as a `Monoid`/`Semigroup`,
--   e.g. a ByteString `Data.ByteString.Builder.Builder` or
--   a `Data.Text.Lazy.Builder.Builder`.
--
--   If you'd rather supply an initial value instead of
--   using mempty, use `matchesDefL` instead.
matchesL :: (MonadPlus m, Monoid v, Monoid r) => (String -> m r) -> (i -> MatchResult m i v r) -> MatcherT i v m r
matchesL err f = do
  mybX <- preview
  case mybX of
    Nothing  -> return mempty
    (Just _) -> do
      y <- match err f
      matchesDefL y err f

-- | Like `matchesL`, but uses a different function
--   for the first match.
matchesL' :: (MonadPlus m, Monoid v, Monoid r) => (String -> m r) -> (i -> MatchResult m i v r) -> (i -> MatchResult m i v r) -> MatcherT i v m r
matchesL' err f1 f2 = do
  mybX <- preview
  case mybX of
    Nothing  -> return mempty
    (Just _) -> do
      y <- match err f1
      matchesDefL y err f2


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
matchesDefL :: (MonadPlus m, Monoid v, Semigroup r) => r -> (String -> m r) -> (i -> MatchResult m i v r) -> MatcherT i v m r
matchesDefL acc err f = do
  mybX <- preview
  case mybX of
    Nothing  -> return acc
    (Just _) -> do
      y <- match err f
      matchesDefL (acc <> y) err f

-- | Variant of `match` that uses `fail` from `MonadFail`
--   as the first argument.
matchF :: (MonadPlus m, MonadFail m, Monoid v) => (i -> MatchResult m i v r) -> MatcherT i v m r
matchF = match fail

-- | Variant of `matches` that uses `fail` from `MonadFail`
--   as the first argument.
matchesF :: (MonadPlus m, MonadFail m, Monoid v) => (i -> MatchResult m i v r) -> MatcherT i v m [r]
matchesF = matches fail

-- | Variant of `matchesL` that uses `fail` from `MonadFail`
--   as the first argument.
matchesLF :: (MonadPlus m, MonadFail m, Monoid v, Monoid r) => (i -> MatchResult m i v r) -> MatcherT i v m r
matchesLF = matchesL fail

-- | Variant of `matchesL'` that uses `fail` from `MonadFail`
--   as the first argument.
matchesLF' :: (MonadPlus m, MonadFail m, Monoid v, Monoid r) => (i -> MatchResult m i v r) -> (i -> MatchResult m i v r) -> MatcherT i v m r
matchesLF' = matchesL' fail


-- | Variant of `matchesDefL` that uses `fail` from `MonadFail`
--   as the second argument
matchesDefLF :: (MonadPlus m, MonadFail m, Monoid v, Semigroup r) => r -> (i -> MatchResult m i v r) -> MatcherT i v m r
matchesDefLF acc = matchesDefL acc fail

-- | A variant of `matches` that uses a `Monoid` instead
--   of a list to accumulate the results. This version
--   is left-associative, so it should work better with
--   `Data.Text.Lazy.Builder.Builder` from "Data.Text.Lazy.Builder".
matchesR :: (MonadPlus m, Monoid v, Monoid r) => (String -> m r) -> (i -> MatchResult m i v r) -> MatcherT i v m r
matchesR err f = do
  mybX <- preview
  case mybX of
    Nothing  -> return mempty
    _ -> do
      y <- match err f
      (y <>) <$> matchesR err f

-- | Like `matchesL`, but uses a different function
--   for the first match.
matchesR' :: (MonadPlus m, Monoid v, Monoid r) => (String -> m r) -> (i -> MatchResult m i v r) -> (i -> MatchResult m i v r) -> MatcherT i v m r
matchesR' err f1 f2 = do
  mybX <- preview
  case mybX of
    Nothing -> return mempty
    _ -> do
      y <- match err f1
      (y <>) <$> matchesR err f2

-- | Like `matchesL`, but uses `fail` for
--   the first argument.
matchesRF :: (MonadPlus m, MonadFail m, Monoid v, Monoid r) => (i -> MatchResult m i v r) -> MatcherT i v m r
matchesRF = matchesR fail

matchesRF' :: (MonadPlus m, MonadFail m, Monoid v, Monoid r) => (i -> MatchResult m i v r) -> (i -> MatchResult m i v r) -> MatcherT i v m r
matchesRF' = matchesR' fail


----------------------------------------------------------------
-- Matching Something Else

-- | Run a different matching algorithm on
--   a different list, and then return the
--   result and restore the original state.
--   Useful if the input list contains lists
--   itself.
matchElse :: (Monad m, Monoid w) => (j -> w) -> [j] -> MatcherT j w m r -> MatcherT i v m r
matchElse newConv lst action = MatcherT $ \_ inp v -> do
  rslt <- evalMatcherT newConv lst action
  return (rslt, inp, v)

----------------------------------------------------------------
-- Simple Matchers

-- | A simple matcher that just uses a simple
--   function type instead of a complex secondary
--   type.
matchSimple :: (Monad m, Monoid v) => (i -> MatcherT i v m r) -> MatcherT i v m (Maybe r)
matchSimple action = do
  x <- proceed
  case x of
    Nothing  -> return Nothing
    (Just y) -> Just <$> action y

-- | Repeat `matchSimple` until out of input.
matchesSimple :: (Monad m, Monoid v, Monoid r) => (i -> MatcherT i v m r) -> MatcherT i v m r
matchesSimple action = do
  x <- matchSimple action
  case x of
    Nothing     -> return mempty
    (Just rslt) -> matchesSimpleDef rslt action

-- | Repeat `matchSimple` until no input left.
--   Instead of starting out from empty, the
--   accumulated value starts out with a default
--   value.
matchesSimpleDef :: (Monad m, Monoid v, Semigroup r) => r -> (i -> MatcherT i v m r) -> MatcherT i v m r
matchesSimpleDef defVal action = do
  x <- matchSimple action
  case x of
    Nothing     -> return defVal
    (Just rslt) -> matchesSimpleDef (defVal <> rslt) action


