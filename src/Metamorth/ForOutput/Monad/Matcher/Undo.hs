{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Metamorth.ForOutput.Monad.Matcher.Undo
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

This version of the code has support for
"undo" operations. 

-}

module Metamorth.ForOutput.Monad.Matcher.Undo
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
  -- ** Undo Operations
  , unproceed
  , unproceedVia
  , unproceedWith
  -- * Sub Types
  , MatchResult(..)
  , MatchReturn(..)
  ) where

import Control.Applicative

import Control.Monad
import Control.Monad.Trans.Class

-- import Data.Functor.Identity

import Metamorth.ForOutput.Monad.Matcher.Result

import Metamorth.ForOutput.Monad.EitherFail

-- import Data.Kind (Type)
import Data.List (uncons)

-- | A simple way to save space when
--   using no undo stack. Every type
--   just has (UndoStack v ~ [v]),
--   except for `()`, which just uses
--   `()` as its undo stack instead.
type family UndoStack v = r | r -> v where

  UndoStack () = ()
  UndoStack a  = [a]

-- | A simple class to provide operations that
--   work on any kind of `UndoStack`.
class Undoable v where
  -- | Remove and discard the latest element.
  undo      :: UndoStack v -> UndoStack v
  -- | Add an item to an `UndoStack`. Effectively
  --   just @`(:)`@.
  addItem   :: v -> UndoStack v -> UndoStack v
  -- | Retrieve the value that the `UndoStack`
  --   represents.
  retrieve  :: UndoStack v -> v
  -- | `uncons` for `UndoStack`s.
  unconsU   :: UndoStack v -> Maybe (v, UndoStack v)
  -- | `[]` for `UndoStack`s.
  emptyU    :: UndoStack v

instance {-# OVERLAPPING #-} Undoable () where
  undo = id
  addItem _ _ = ()
  retrieve _ = ()
  unconsU _ = Just ((),())
  emptyU = ()

instance (UndoStack v ~ [v], Monoid v) => Undoable v where
  undo [] = []
  undo (_:xs) = xs
  addItem = (:)
  retrieve xs = mconcat (reverse xs)
  unconsU = uncons
  emptyU = []


{-
class Undoable (v :: Type) where
  type UndoStack v :: Type
  (<+>) :: UndoStack v -> UndoStack v -> UndoStack v
  undo  :: UndoStack v -> UndoStack v

instance {-# OVERLAPPING #-} Undoable () where
  type UndoStack () = ()
  (<+>) = const
  undo = id

instance {-# OVERLAPPABLE #-} Undoable v where
  type UndoStack v = [v]
  (<+>) = (++)
  undo [] = []
  undo xs = init xs
-}

-- undo :: [a] -> [a]
-- undo [] = []
-- undo xs = init xs

{-
unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc ls = Just $ unsnoc' id ls
  where
    unsnoc' f [x]    = (f [], x)
    unsnoc' f (x:xs) = unsnoc' (f . (x:)) xs
    unsnoc' _ _ = ([], undefined) -- shouldn't be reached.
-}

-- | A `Monad` akin to a parser that consumes
--   values from an input list, converts them
--   to a `Monoid` value and appends it to a
--   list, and then returns the item it processed.
--
--   The main functions with @MatcherT@ are
--   `match`, `matches`, etc..., along with
--   the lower-level functions `proceed`,
--   `preview`, `unproceed`, etc...
newtype MatcherT i v m a  = MatcherT { getMatcherT :: (i -> v) -> [i] -> UndoStack v -> m (a, [i], UndoStack v) }

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


-- | Run a `MatcherT`, returning the result
--   value, along with the remaining input
--   and output streams.
runMatcherT :: (Undoable v, Functor m) => (i -> v) -> [i] -> MatcherT i v m a -> m (a, [i], v)
runMatcherT func inp mt = f <$> getMatcherT mt func inp emptyU
  where f (x,y,z) = (x,y,retrieve z)

-- | Run a `Matcher`, returning the result
--   value, along with the remaining input
--   and output streams.
runMatcher :: (Undoable v) => (i -> v) -> [i] -> Matcher i v a -> Maybe (a, [i], v)
runMatcher = runMatcherT

-- | Run a `MatcherE`, returning the result value,
--   along with the remaining input and output
--   streams.
runMatcherE :: (Undoable v, Functor EitherFail) => (i -> v) -> [i] -> MatcherE i v a -> Either String (a, [i], v)
runMatcherE x y = toEither . runMatcherT x y

-- | Run `MatcherT`, only returning the result value.
evalMatcherT :: (Undoable v, Functor m) => (i -> v) -> [i] -> MatcherT i v m a -> m a
evalMatcherT func inp mt = (\(x,_,_) -> x) <$> getMatcherT mt func inp emptyU

-- | Run `Matcher`, only returning the result value.
evalMatcher :: (Undoable v) => (i -> v) -> [i] -> Matcher i v a -> Maybe a
evalMatcher = evalMatcherT

-- | Run a `MatcherE`, only returning the result value.
evalMatcherE :: (Undoable v, Functor EitherFail) => (i -> v) -> [i] -> MatcherE i v a -> Either String a
evalMatcherE x y = toEither . evalMatcherT x y

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
proceed :: (Applicative m, Undoable v) => MatcherT i v m (Maybe i)
proceed = MatcherT $ \ifnc inp vs -> case inp of
  []     -> pure (Nothing, inp, vs)
  (x:xs) -> pure (Just  x,  xs, addItem (ifnc x) vs)

-- | Put a value back onto the stack, and remove
--   the latest output value from the output.
unproceed :: (Applicative m, Undoable v) => i -> MatcherT i v m ()
unproceed x = MatcherT $ \_ifnc inp vs ->
  pure ((), x : inp, undo vs)

-- | Remove the latest output value from the output,
--   and use it to create a value that will be put
--   back onto the input stack.
unproceedVia :: (Applicative m, Undoable v) => (Maybe v -> i) -> MatcherT i v m ()
unproceedVia f = MatcherT $ \_ inp vs -> case unconsU vs of
  Nothing       -> pure ((), (f  Nothing) : inp, emptyU)
  (Just (x,xs)) -> pure ((), (f (Just x)) : inp, xs)

unproceedWith :: (Applicative m, Undoable v) => (v -> i) -> MatcherT i v m ()
unproceedWith f = MatcherT $ \_ inp vs -> case unconsU vs of
  Nothing       -> pure ((), inp, vs)
  (Just (x,xs)) -> pure ((), (f x) : inp, xs)

-- | Equivalent to `Data.Attoparsec.Text.peekChar`,
--   previewing the next character without removing
--   it from the stack.
preview :: Applicative m => MatcherT i v m (Maybe i)
preview = MatcherT $ \_ifnc inp vs -> case inp of
  []    -> pure (Nothing, inp, vs)
  (x:_) -> pure (Just  x, inp, vs)

-- | Extract the values from the accumulator, leaving
--   a blank accumulator in its place.
pullValues :: (Applicative m, Undoable v) => MatcherT i v m v
pullValues = MatcherT $ \_ifnc inp vs -> pure (retrieve vs, inp, emptyU)

{-
match :: (MonadPlus m, Monoid v, Undoable v) => (String -> m r) -> (i -> MatchResult m i v r) -> MatcherT i v m r
match err f = do
  mybX <- proceed
  case mybX of
    Nothing  -> lift $ err "Not Enough Input."
    (Just x) -> do
      case (f x) of
        MatchReturn g -> do
          vs <- pullValues
          lift $ g vs
        MatchContinue mc -> match err mc
        MatchFail str -> lift $ str >>= err
        MatchOptions ret cont
          -> (match err cont) <|> (pullValues >>= \vs -> lift $ ret vs)
-}

match :: (MonadPlus m, Monoid v, Undoable v) => (String -> m r) -> (i -> MatchResult m i v r) -> MatcherT i v m r
match err f = do
  mybX <- proceed
  case mybX of
    Nothing  -> lift $ err "Not Enough Input."
    (Just x) -> do
      case (f x) of
        MatchReturn rets -> msum $ map matchReturn rets
        MatchContinue mc -> match err mc
        MatchFail str    -> lift $ err str
        MatchOptions rets cont
          -> match err cont <|> msum (map matchReturn rets)

-- matchReturn :: (MonadPlus m, Monoid v) => (String -> m r) -> MatchReturn m i v r -> MatcherT i v m r
matchReturn :: (MonadPlus m, Undoable v) => MatchReturn m i v r -> MatcherT i v m r
matchReturn (PlainReturn f) = do
  vs <- pullValues
  lift $ f vs
matchReturn (ConditionalReturn f) = do
  mi <- preview
  vs <- pullValues
  lift $ f vs mi

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
matches :: (MonadPlus m, Undoable v, Monoid v) => (String -> m r) -> (i -> MatchResult m i v r) -> MatcherT i v m [r]
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
matchesL :: (MonadPlus m, Undoable v, Monoid v, Monoid r) => (String -> m r) -> (i -> MatchResult m i v r) -> MatcherT i v m r
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
matchesDefL :: (MonadPlus m, Undoable v, Monoid v, Semigroup r) => r -> (String -> m r) -> (i -> MatchResult m i v r) -> MatcherT i v m r
matchesDefL acc err f = do
  mybX <- preview
  case mybX of
    Nothing  -> return acc
    (Just _) -> do
      y <- match err f
      matchesDefL (acc <> y) err f

-- | Variant of `match` that uses `fail` from `MonadFail`
--   as the first argument.
matchF :: (MonadPlus m, MonadFail m, Monoid v, Undoable v) => (i -> MatchResult m i v r) -> MatcherT i v m r
matchF = match fail

-- | Variant of `matches` that uses `fail` from `MonadFail`
--   as the first argument.
matchesF :: (MonadPlus m, MonadFail m, Monoid v, Undoable v) => (i -> MatchResult m i v r) -> MatcherT i v m [r]
matchesF = matches fail

-- | Variant of `matchesL` that uses `fail` from `MonadFail`
--   as the first argument.
matchesLF :: (MonadPlus m, MonadFail m, Monoid v, Undoable v, Monoid r) => (i -> MatchResult m i v r) -> MatcherT i v m r
matchesLF = matchesL fail

-- | Variant of `matchesDefL` that uses `fail` from `MonadFail`
--   as the second argument
matchesDefLF :: (MonadPlus m, MonadFail m, Monoid v, Undoable v, Semigroup r) => r -> (i -> MatchResult m i v r) -> MatcherT i v m r
matchesDefLF acc = matchesDefL acc fail



