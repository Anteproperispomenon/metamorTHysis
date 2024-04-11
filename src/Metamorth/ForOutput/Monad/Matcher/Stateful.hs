{-# LANGUAGE TupleSections #-}

{-|
Module      : Metamorth.ForOutput.Monad.Matcher.Stateful
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

This module defines a simple Parser-Like 
Monad that can be used to match simple 
input buffers. In particular, this module
defines a stateful version that has more

-}

module Metamorth.ForOutput.Monad.Matcher.Stateful
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
  , matchesDefL
  -- ** Automatic MonadFail Versions
  , matchF
  , matchesF
  , matchesLF
  , matchesDefLF
  -- * Low-Level Operations
  , proceed
  , preview
  , get
  , gets
  , put
  , modify
  , modify'
  -- * Sub Types
  , MatchResult(..)
  , MatchReturn(..)
  ) where

import Control.Applicative

import Control.Monad
import Control.Monad.Trans.Class

import Metamorth.ForOutput.Monad.Matcher.Stateful.Result

import Metamorth.ForOutput.Monad.EitherFail

-- | A simple matcher that uses `Maybe` as the
--   base `Monad`. Probably the easiest version
--   to use, but it won't return error messages.
type Matcher i v s = MatcherT i v s Maybe

-- | A simple matcher that works very similarly
--   to `Matcher`, but instead uses a variant
--   of `Either` as its base `Monad`. This variant
--   has instances for `Alternative`, `MonadFail`,
--   and a few other related typeclasses that make
--   it suitable for matching/parsing.
type MatcherE i v s = MatcherT i v s EitherFail

-- | A `Monad` akin to a parser that consumes
--   values from an input list, converts them
--   to a `Monoid` value and appends it to a
--   list, and then returns the item it processed.
--
--   The main functions with @MatcherT@ are
--   `match`, `matches`, etc..., along with
--   the lower-level functions `proceed` and
--   `preview`.
newtype MatcherT i v s m a  = MatcherT { getMatcherT :: (i -> v) -> [i] -> v -> s -> m (a, [i], v, s) }

-- | Run a `MatcherT`, returning the result
--   value, along with the remaining input
--   and output streams.
runMatcherT :: (Monoid v) 
  => (i -> v) -- ^ A function to convert input items to an output stream value.
  -> [i]      -- ^ The input stream to parse
  -> s        -- ^ The initial state.
  -> MatcherT i v s m a -- ^ The action to parse the input stream.
  -> m (a, [i], v, s)
runMatcherT func inp st mt = getMatcherT mt func inp mempty st

-- | Run a `Matcher`, returning the result
--   value, along with the remaining input
--   and output streams.
runMatcher :: (Monoid v) => (i -> v) -> [i] -> s -> Matcher i v s a -> Maybe (a, [i], v, s)
runMatcher = runMatcherT

-- | Run a `MatcherE`, returning the result value,
--   along with the remaining input and output
--   streams.
runMatcherE :: (Monoid v) => (i -> v) -> [i] -> s -> MatcherE i v s a -> Either String (a, [i], v, s)
runMatcherE x y z = toEither . runMatcherT x y z

-- | Run `MatcherT`, only returning the result value.
evalMatcherT :: (Functor m, Monoid v) => (i -> v) -> [i] -> s -> MatcherT i v s m a -> m a
evalMatcherT func inp st mt = (\(x,_,_,_) -> x) <$> getMatcherT mt func inp mempty st

-- | Run `Matcher`, only returning the result value.
evalMatcher :: (Monoid v) => (i -> v) -> [i] -> s -> Matcher i v s a -> Maybe a
evalMatcher = evalMatcherT

-- | Run a `MatcherE`, only returning the result value.
evalMatcherE :: (Monoid v) => (i -> v) -> [i] -> s -> MatcherE i v s a -> Either String a
evalMatcherE x y z = toEither . evalMatcherT x y z

instance (Functor m) => Functor (MatcherT i v s m) where
  fmap f (MatcherT mt) = MatcherT $ \ifnc inp vs st -> fmap1'4 f $ mt ifnc inp vs st

fmap1'4 :: Functor f => (a -> a') -> f (a,b,c,d) -> f (a',b,c,d)
fmap1'4 f = fmap (con1'4 f)
  where
    con1'4 g (x,y,z,w) = (g x, y, z, w)

instance (Monad m) => Applicative (MatcherT i v s m) where
  pure x = MatcherT $ \_ y z w -> pure (x,y,z,w)
  (MatcherT mt1) <*> (MatcherT mt2) 
    = MatcherT $ \ifnc inp vs st -> do
        (f, inp1, vs1, st1) <- mt1 ifnc inp  vs  st
        (x, inp2, vs2, st2) <- mt2 ifnc inp1 vs1 st1
        return (f x, inp2, vs2, st2)
  (MatcherT mt1)  *> (MatcherT mt2)
    = MatcherT $ \ifnc inp vs st -> do
        (_, inp1, vs1, st1) <- mt1 ifnc inp  vs  st
        (x, inp2, vs2, st2) <- mt2 ifnc inp1 vs1 st1
        return (x, inp2, vs2, st2)
  (MatcherT mt1) <*  (MatcherT mt2)
    = MatcherT $ \ifnc inp vs st -> do
        (x, inp1, vs1, st1) <- mt1 ifnc inp  vs  st
        (_, inp2, vs2, st2) <- mt2 ifnc inp1 vs1 st1
        return (x, inp2, vs2, st2)

instance (Monad m) => Monad (MatcherT i v s m) where
  (MatcherT mt1) >>= f
    = MatcherT $ \ifnc inp vs st -> do
        (x, inp2, vs2, st2) <- mt1 ifnc inp vs st
        getMatcherT (f x) ifnc inp2 vs2 st2

instance (MonadPlus m) => Alternative (MatcherT i v s m) where
  empty = MatcherT $ \_ _ _ _ -> mzero
  (MatcherT mt1) <|> (MatcherT mt2) 
    = MatcherT $ \ifnc inp vs st -> (mt1 ifnc inp vs st) `mplus` (mt2 ifnc inp vs st)

instance (MonadPlus m) => MonadPlus (MatcherT i v s m)

instance (MonadFail m) => MonadFail (MatcherT i v s m) where
  fail = lift . fail

instance MonadTrans (MatcherT i v s) where
  lift action = MatcherT $ \_ inp vs st -> (,inp,vs,st) <$> action

-- | Consume an item from the input, process
--   it with the conversion function, add the
--   produced code to the output stream, and
--   return the consumed input.
proceed :: (Semigroup v, Applicative m) => MatcherT i v s m (Maybe i)
proceed = MatcherT $ \ifnc inp vs st -> case inp of
  []     -> pure (Nothing, inp, vs, st)
  (x:xs) -> pure (Just  x,  xs, vs <> ifnc x, st)

-- | Equivalent to `Data.Attoparsec.Text.peekChar`,
--   previewing the next character without removing
--   it from the stack.
preview :: Applicative m => MatcherT i v s m (Maybe i)
preview = MatcherT $ \_ifnc inp vs st -> case inp of
  []    -> pure (Nothing, inp, vs, st)
  (x:_) -> pure (Just  x, inp, vs, st)

-- | Extract the values from the accumulator, leaving
--   a blank accumulator in its place.
pullValues :: (Applicative m, Monoid v) => MatcherT i v s m v
pullValues = MatcherT $ \_ifnc inp vs st -> pure (vs, inp, mempty, st)

-- | Retrieve the current state.
get :: (Applicative m) => MatcherT i v s m s
get = MatcherT $ \_ifnc inp vs st -> pure (st,inp,vs,st)

-- | Retrieve the current state.
gets :: (Applicative m) => (s -> s') -> MatcherT i v s m s'
gets f = MatcherT $ \_ifnc inp vs st -> pure (f st, inp, vs, st)

modify :: (Applicative m) => (s -> s) -> MatcherT i v s m ()
modify f = MatcherT $ \_ifnc inp vs st -> pure ((), inp, vs, f st)

modify' :: (Applicative m) => (s -> s) -> MatcherT i v s m ()
modify' f = MatcherT $ \_ifnc inp vs st -> pure ((), inp, vs, f $! st)

put :: (Applicative m) => s -> MatcherT i v s m ()
put st = MatcherT $ \_ifnc inp vs _ -> pure ((), inp, vs, st)

match :: (MonadPlus m, Monoid v) => (String -> m r) -> (i -> MatchResult m i v s r) -> MatcherT i v s m r
match err f = do
  mybX <- proceed
  case mybX of
    Nothing  -> lift $ err "Not Enough Input."
    (Just x) -> do
      case (f x) of
        MatchReturn ret  -> matchReturn ret
        MatchContinue mc -> match err mc
        MatchFail str    -> lift $ err str
        MatchOptions ret cont
          -> match err cont <|> matchReturn ret

-- matchReturn :: (MonadPlus m, Monoid v) => (String -> m r) -> MatchReturn m i v r -> MatcherT i v m r
matchReturn :: (MonadPlus m, Monoid v) => MatchReturn m i v s r -> MatcherT i v s m r
matchReturn (PlainReturn f) = do
  vs <- pullValues
  lift $ f vs
matchReturn (StateReturn f) = do
  vs <- pullValues
  st <- get
  (rslt, st') <- lift $ f vs st
  put $! st'
  return rslt
matchReturn (ConditionalReturn f) = do
  mi <- preview
  vs <- pullValues
  lift $ f vs mi
matchReturn (ConditionalStateReturn f) = do
  mi <- preview
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
--   try using `matchesL` with a different `Monoid`.
matches :: (MonadPlus m, Monoid v) => (String -> m r) -> (i -> MatchResult m i v s r) -> MatcherT i v s m [r]
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
matchesL :: (MonadPlus m, Monoid v, Monoid r) => (String -> m r) -> (i -> MatchResult m i v s r) -> MatcherT i v s m r
matchesL err f = do
  mybX <- preview
  case mybX of
    Nothing  -> return mempty
    (Just _) -> do
      y <- match err f
      matchesDefL y err f

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
matchesDefL :: (MonadPlus m, Monoid v, Semigroup r) => r -> (String -> m r) -> (i -> MatchResult m i v s r) -> MatcherT i v s m r
matchesDefL acc err f = do
  mybX <- preview
  case mybX of
    Nothing  -> return acc
    (Just _) -> do
      y <- match err f
      matchesDefL (acc <> y) err f

-- | Variant of `match` that uses `fail` from `MonadFail`
--   as the first argument.
matchF :: (MonadPlus m, MonadFail m, Monoid v) => (i -> MatchResult m i v s r) -> MatcherT i v s m r
matchF = match fail

-- | Variant of `matches` that uses `fail` from `MonadFail`
--   as the first argument.
matchesF :: (MonadPlus m, MonadFail m, Monoid v) => (i -> MatchResult m i v s r) -> MatcherT i v s m [r]
matchesF = matches fail

-- | Variant of `matchesL` that uses `fail` from `MonadFail`
--   as the first argument.
matchesLF :: (MonadPlus m, MonadFail m, Monoid v, Monoid r) => (i -> MatchResult m i v s r) -> MatcherT i v s m r
matchesLF = matchesL fail

-- | Variant of `matchesDefL` that uses `fail` from `MonadFail`
--   as the second argument
matchesDefLF :: (MonadPlus m, MonadFail m, Monoid v, Semigroup r) => r -> (i -> MatchResult m i v s r) -> MatcherT i v s m r
matchesDefLF acc = matchesDefL acc fail
