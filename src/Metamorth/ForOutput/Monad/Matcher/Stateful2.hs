{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : Metamorth.ForOutput.Monad.Matcher.Stateful2
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
computation possibilities.

This particular version adds an extra 
variable to the Matcher Type to make allow
the input stream to be pre-processed before
processing.

-}

module Metamorth.ForOutput.Monad.Matcher.Stateful2
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
  , matchElseS
  -- ** Simple Matchers
  , matchSimple
  , matchesSimple
  , matchesSimpleDef
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

import GHC.Exts qualified as Exts

-- | A simple matcher that uses `Maybe` as the
--   base `Monad`. Probably the easiest version
--   to use, but it won't return error messages.
type Matcher i j v s = MatcherT i j v s Maybe

-- | A simple matcher that works very similarly
--   to `Matcher`, but instead uses a variant
--   of `Either` as its base `Monad`. This variant
--   has instances for `Alternative`, `MonadFail`,
--   and a few other related typeclasses that make
--   it suitable for matching/parsing.
type MatcherE i j v s = MatcherT i j v s EitherFail

-- | A `Monad` akin to a parser that consumes
--   values from an input list, converts them
--   to a `Monoid` value and appends it to a
--   list, and then returns the item it processed.
--
--   The main functions with @MatcherT@ are
--   `match`, `matches`, etc..., along with
--   the lower-level functions `proceed` and
--   `preview`.
newtype MatcherT i j v s m a  = MatcherT' { getMatcherT' :: (i -> v) -> (i -> j) -> [i] -> v -> s -> m (a, [i], v, s) }

-- | An explicitly bidirectional pattern to
--   allow trying out different optimizations;
--   e.g. `GHC.Exts.oneShot` from "GHC.Exts".
pattern MatcherT :: ((i -> v) -> (i -> j) -> [i] -> v -> s -> m (a, [i], v, s)) -> MatcherT i j v s m a
pattern MatcherT {getMatcherT} <- MatcherT' { getMatcherT' = getMatcherT }
  -- one-shot
  where MatcherT f = MatcherT' $ Exts.oneShot $ \fnc -> Exts.oneShot $ \fnc' -> Exts.oneShot $ \inp -> Exts.oneShot $ \v -> Exts.oneShot $ \st -> f fnc fnc' inp v st
  -- regular
  -- where MatcherT f = MatcherT' f

{-# COMPLETE MatcherT #-}

-- | Run a `MatcherT`, returning the result
--   value, along with the remaining input
--   and output streams.
runMatcherT :: (Monoid v) 
  => (i -> v) -- ^ A function to convert input items to an output stream value.
  -> (i -> j) -- ^ The function to convert input items for processing.
  -> [i]      -- ^ The input stream to parse
  -> s        -- ^ The initial state.
  -> MatcherT i j v s m a -- ^ The action to parse the input stream.
  -> m (a, [i], v, s)
runMatcherT func prp inp st mt = getMatcherT mt func prp inp mempty st

-- | Run a `Matcher`, returning the result
--   value, along with the remaining input
--   and output streams.
runMatcher :: (Monoid v) => (i -> v) -> (i -> j) -> [i] -> s -> Matcher i j v s a -> Maybe (a, [i], v, s)
runMatcher = runMatcherT

-- | Run a `MatcherE`, returning the result value,
--   along with the remaining input and output
--   streams.
runMatcherE :: (Monoid v) => (i -> v) -> (i -> j) -> [i] -> s -> MatcherE i j v s a -> Either String (a, [i], v, s)
runMatcherE w x y z = toEither . runMatcherT w x y z

-- | Run `MatcherT`, only returning the result value.
evalMatcherT :: (Functor m, Monoid v) => (i -> v) -> (i -> j) -> [i] -> s -> MatcherT i j v s m a -> m a
evalMatcherT func func2 inp st mt = (\(x,_,_,_) -> x) <$> getMatcherT mt func func2 inp mempty st

-- | Run `Matcher`, only returning the result value.
evalMatcher :: (Monoid v) => (i -> v) -> (i -> j) -> [i] -> s -> Matcher i j v s a -> Maybe a
evalMatcher = evalMatcherT

-- | Run a `MatcherE`, only returning the result value.
evalMatcherE :: (Monoid v) => (i -> v) -> (i -> j) -> [i] -> s -> MatcherE i j v s a -> Either String a
evalMatcherE w x y z = toEither . evalMatcherT w x y z

instance (Functor m) => Functor (MatcherT i j v s m) where
  fmap f (MatcherT mt) = MatcherT $ \ifnc fnc2 inp vs st -> fmap1'4 f $ mt ifnc fnc2 inp vs st

fmap1'4 :: Functor f => (a -> a') -> f (a,b,c,d) -> f (a',b,c,d)
fmap1'4 f = fmap (con1'4 f)
  where
    con1'4 g (x,y,z,w) = (g x, y, z, w)

instance (Monad m) => Applicative (MatcherT i j v s m) where
  pure x = MatcherT $ \_ _ y z w -> pure (x,y,z,w)
  (MatcherT mt1) <*> (MatcherT mt2) 
    = MatcherT $ \ifnc fnc2 inp vs st -> do
        (f, inp1, vs1, st1) <- mt1 ifnc fnc2 inp  vs  st
        (x, inp2, vs2, st2) <- mt2 ifnc fnc2 inp1 vs1 st1
        return (f x, inp2, vs2, st2)
  (MatcherT mt1)  *> (MatcherT mt2)
    = MatcherT $ \ifnc fnc2 inp vs st -> do
        (_, inp1, vs1, st1) <- mt1 ifnc fnc2 inp  vs  st
        (x, inp2, vs2, st2) <- mt2 ifnc fnc2 inp1 vs1 st1
        return (x, inp2, vs2, st2)
  (MatcherT mt1) <*  (MatcherT mt2)
    = MatcherT $ \ifnc fnc2 inp vs st -> do
        (x, inp1, vs1, st1) <- mt1 ifnc fnc2 inp  vs  st
        (_, inp2, vs2, st2) <- mt2 ifnc fnc2 inp1 vs1 st1
        return (x, inp2, vs2, st2)

instance (Monad m) => Monad (MatcherT i j v s m) where
  (MatcherT mt1) >>= f
    = MatcherT $ \ifnc fnc2 inp vs st -> do
        (x, inp2, vs2, st2) <- mt1 ifnc fnc2 inp vs st
        getMatcherT (f x) ifnc fnc2 inp2 vs2 st2

instance (MonadPlus m) => Alternative (MatcherT i j v s m) where
  empty = MatcherT $ \_ _ _ _ _ -> mzero
  (MatcherT mt1) <|> (MatcherT mt2) 
    = MatcherT $ \ifnc prp inp vs st -> (mt1 ifnc prp inp vs st) `mplus` (mt2 ifnc prp inp vs st)

instance (MonadPlus m) => MonadPlus (MatcherT i j v s m)

instance (MonadFail m) => MonadFail (MatcherT i j v s m) where
  fail = lift . fail

instance MonadTrans (MatcherT i j v s) where
  lift action = MatcherT $ \_ _ inp vs st -> (,inp,vs,st) <$> action

-- | Consume an item from the input, process
--   it with the conversion function, add the
--   produced code to the output stream, and
--   return the consumed input.
proceed :: (Semigroup v, Applicative m) => MatcherT i j v s m (Maybe j)
proceed = MatcherT $ \ifnc prp inp vs st -> case inp of
  []     -> pure (Nothing     , inp, vs, st)
  (x:xs) -> pure (Just (prp x),  xs, vs <> ifnc x, st)

-- | Equivalent to `Data.Attoparsec.Text.peekChar`,
--   previewing the next character without removing
--   it from the stack. Note that this version applies
--   the pre-processor to the input.
preview :: Applicative m => MatcherT i j v s m (Maybe j)
preview = MatcherT $ \_ifnc fnc2 inp vs st -> case inp of
  []    -> pure (Nothing      , inp, vs, st)
  (x:_) -> pure (Just (fnc2 x), inp, vs, st)

-- | Equivalent to `Data.Attoparsec.Text.peekChar`,
--   previewing the next character without removing
--   it from the stack. This version **does not**
--   apply the pre-processor to the input
preview' :: Applicative m => MatcherT i j v s m (Maybe i)
preview' = MatcherT $ \_ifnc _prp inp vs st -> case inp of
  []    -> pure (Nothing, inp, vs, st)
  (x:_) -> pure (Just  x, inp, vs, st)

-- | Extract the values from the accumulator, leaving
--   a blank accumulator in its place.
pullValues :: (Applicative m, Monoid v) => MatcherT i j v s m v
pullValues = MatcherT $ \_ifnc _fnc2 inp vs st -> pure (vs, inp, mempty, st)

-- | Retrieve the current state.
get :: (Applicative m) => MatcherT i j v s m s
get = MatcherT $ \_ifnc _fnc2 inp vs st -> pure (st,inp,vs,st)

-- | Retrieve the current state, using a view function on it.
gets :: (Applicative m) => (s -> s') -> MatcherT i j v s m s'
gets f = MatcherT $ \_ifnc _fnc2 inp vs st -> pure (f st, inp, vs, st)

-- | Modify the current state of the `MatcherT`.
modify :: (Applicative m) => (s -> s) -> MatcherT i j v s m ()
modify f = MatcherT $ \_ifnc _fnc2 inp vs st -> pure ((), inp, vs, f st)

-- | Strictly modify the current state of the `MatcherT`. Based
--   on `Control.Monad.Trans.State.Strict.modify'` from 
--   "Control.Monad.Trans.State.Strict".
modify' :: (Applicative m) => (s -> s) -> MatcherT i j v s m ()
modify' f = MatcherT $ \_ifnc _fnc2 inp vs st -> pure ((), inp, vs, f $! st)

-- | Set the current state of the `MatcherT` to a specific value.
put :: (Applicative m) => s -> MatcherT i j v s m ()
put st = MatcherT $ \_ifnc _fnc2 inp vs _ -> pure ((), inp, vs, st)

-- | Match on the input stream by applying a `MatchResult` action
--   to the next value in the stream, and then either:
--
--     - Returning, if the the action returns a `MatchReturn`.
--     - Throwing an error, if the action returns a `MatchFail`.
--     - Running `match` on the next value in the stream, if the action returns a `MatchContinue` or `MatchOptions`.
match :: (MonadPlus m, Monoid v) => (String -> m r) -> (j -> MatchResult m j v s r) -> MatcherT i j v s m r
match err f = do
  mybX <- proceed
  case mybX of
    Nothing  -> lift $ err "Not Enough Input."
    (Just x) -> do
      case (f x) of
        -- MatchReturn rets -> msum $ map matchReturn rets
        MatchReturn ret  -> matchReturn ret
        MatchContinue mc -> match err mc
        MatchFail fstr   -> lift $ err (fstr x)
        MatchOptions ret cont
          -> match err cont <|> matchReturn ret
          -- -> match err cont <|> msum (map matchReturn ret)

-- matchReturn :: (MonadPlus m, Monoid v) => (String -> m r) -> MatchReturn m i v r -> MatcherT i j v m r
matchReturn :: (MonadPlus m, Monoid v) => MatchReturn m j v s r -> MatcherT i j v s m r
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
--   try using `matchesL` or `matchesR` with a 
--   different `Monoid`.
matches :: (MonadPlus m, Monoid v) => (String -> m r) -> (j -> MatchResult m j v s r) -> MatcherT i j v s m [r]
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
--   e.g. a ByteString `Data.ByteString.Builder.Builder`.
--
--   If you'd rather supply an initial value instead of
--   using mempty, use `matchesDefL` instead.
matchesL :: (MonadPlus m, Monoid v, Monoid r) => (String -> m r) -> (j -> MatchResult m j v s r) -> MatcherT i j v s m r
matchesL err f = do
  mybX <- preview
  case mybX of
    Nothing  -> return mempty
    (Just _) -> do
      y <- match err f
      matchesDefL y err f

-- | Like `matchesL`, but uses a different function
--   for the first match.
matchesL' :: (MonadPlus m, Monoid v, Monoid r) => (String -> m r) -> (j -> MatchResult m j v s r) -> (j -> MatchResult m j v s r) -> MatcherT i j v s m r
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
matchesDefL :: (MonadPlus m, Monoid v, Semigroup r) => r -> (String -> m r) -> (j -> MatchResult m j v s r) -> MatcherT i j v s m r
matchesDefL acc err f = do
  mybX <- preview
  case mybX of
    Nothing  -> return acc
    (Just _) -> do
      y <- match err f
      matchesDefL (acc <> y) err f

-- | Variant of `match` that uses `fail` from `MonadFail`
--   as the first argument.
matchF :: (MonadPlus m, MonadFail m, Monoid v) => (j -> MatchResult m j v s r) -> MatcherT i j v s m r
matchF = match fail

-- | Variant of `matches` that uses `fail` from `MonadFail`
--   as the first argument.
matchesF :: (MonadPlus m, MonadFail m, Monoid v) => (j -> MatchResult m j v s r) -> MatcherT i j v s m [r]
matchesF = matches fail

-- | Variant of `matchesL` that uses `fail` from `MonadFail`
--   as the first argument.
matchesLF :: (MonadPlus m, MonadFail m, Monoid v, Monoid r) => (j -> MatchResult m j v s r) -> MatcherT i j v s m r
matchesLF = matchesL fail

-- | Variant of `matchesL'` that uses `fail` from `MonadFail`
--   as the first argument.
matchesLF' :: (MonadPlus m, MonadFail m, Monoid v, Monoid r) => (j -> MatchResult m j v s r) -> (j -> MatchResult m j v s r) -> MatcherT i j v s m r
matchesLF' = matchesL' fail

-- | Variant of `matchesDefL` that uses `fail` from `MonadFail`
--   as the second argument
matchesDefLF :: (MonadPlus m, MonadFail m, Monoid v, Semigroup r) => r -> (j -> MatchResult m j v s r) -> MatcherT i j v s m r
matchesDefLF acc = matchesDefL acc fail

-- | A variant of `matches` that uses a `Monoid` instead
--   of a list to accumulate the results. This version
--   is left-associative, so it should work better with
--   `Data.Text.Lazy.Builder.Builder` from "Data.Text.Lazy.Builder".
matchesR :: (MonadPlus m, Monoid v, Monoid r) => (String -> m r) -> (j -> MatchResult m j v s r) -> MatcherT i j v s m r
matchesR err f = do
  mybX <- preview
  case mybX of
    Nothing  -> return mempty
    _ -> do
      y <- match err f
      (y <>) <$> matchesR err f

-- | Like `matchesL`, but uses a different function
--   for the first match.
matchesR' :: (MonadPlus m, Monoid v, Monoid r) => (String -> m r) -> (j -> MatchResult m j v s r) -> (j -> MatchResult m j v s r) -> MatcherT i j v s m r
matchesR' err f1 f2 = do
  mybX <- preview
  case mybX of
    Nothing -> return mempty
    _ -> do
      y <- match err f1
      (y <>) <$> matchesR err f2

-- | Like `matchesL`, but uses `fail` for
--   the first argument.
matchesRF :: (MonadPlus m, MonadFail m, Monoid v, Monoid r) => (j -> MatchResult m j v s r) -> MatcherT i j v s m r
matchesRF = matchesR fail

matchesRF' :: (MonadPlus m, MonadFail m, Monoid v, Monoid r) => (j -> MatchResult m j v s r) -> (j -> MatchResult m j v s r) -> MatcherT i j v s m r
matchesRF' = matchesR' fail

----------------------------------------------------------------
-- Matching Something Else

-- (i -> v) -> [i] -> v -> s -> m (a, [i], v, s) }

-- | Run a different matching algorithm on
--   a different list, and then return the
--   result and restore the original state.
--   Useful if the input list contains lists
--   itself.
matchElse :: (Monad m, Monoid w) => (j -> w) -> (j -> j') -> [j] -> s' -> MatcherT j j' w s' m r -> MatcherT i i' v s m r
matchElse newConv newPrp lst st action = MatcherT $ \_ _ inp v oldSt -> do
  rslt <- evalMatcherT newConv newPrp lst st action
  return (rslt, inp, v, oldSt)

-- | A variant of `matchElse` that runs a stateful
--   computation as its sub-action, instead of
--   just a plain action. 
matchElseS :: (Monad m, Monoid w) => (j -> w) -> (j -> j') -> [j] -> s' -> (s -> MatcherT j j' w s' m (r,s)) -> MatcherT i i' v s m r
matchElseS newConv newPrp lst st action = MatcherT $ \_ _ inp v oldSt -> do
  (rslt, finalSt) <- evalMatcherT newConv newPrp lst st (action oldSt)
  return (rslt, inp, v, finalSt)

----------------------------------------------------------------
-- Simple Matchers

-- | A simple matcher that just uses a simple
--   function type instead of a complex secondary
--   type.
matchSimple :: (Monad m, Monoid v) => (j -> MatcherT i j v s m r) -> MatcherT i j v s m (Maybe r)
matchSimple action = do
  x <- proceed
  case x of
    Nothing  -> return Nothing
    (Just y) -> Just <$> action y

-- | Repeat `matchSimple` until out of input.
matchesSimple :: (Monad m, Monoid v, Monoid r) => (j -> MatcherT i j v s m r) -> MatcherT i j v s m r
matchesSimple action = do
  x <- matchSimple action
  case x of
    Nothing     -> return mempty
    (Just rslt) -> matchesSimpleDef rslt action

-- | Repeat `matchSimple` until no input left.
--   Instead of starting out from empty, the
--   accumulated value starts out with a default
--   value.
matchesSimpleDef :: (Monad m, Monoid v, Semigroup r) => r -> (j -> MatcherT i j v s m r) -> MatcherT i j v s m r
matchesSimpleDef defVal action = do
  x <- matchSimple action
  case x of
    Nothing     -> return defVal
    (Just rslt) -> matchesSimpleDef (defVal <> rslt) action



