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

-}

module Metamorth.ForOutput.Monad.Matcher
  -- * Main Type
  ( Matcher
  , MatcherT
  -- * Primary Operations
  , match
  , proceed
  , preview
  ) where

import Control.Applicative

import Control.Monad
import Control.Monad.Trans.Class

-- import Data.Foldable1

import Metamorth.ForOutput.Monad.Matcher.Result

type Matcher i v = MatcherT i v Maybe

-- | A `Monad` akin to a parser that consumes
--   values from an input list, converts them
--   to a `Monoid` value and appends it to a
--   list, and then returns the item it processed.
--
--   The main functions with @MatcherT@ are
--   `proceed`, `preview`, etc...
newtype MatcherT i v m a  = MatcherT { getMatcherT :: (i -> v) -> [i] -> v -> m (a, [i], v) }

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
        MatchReturn ret  -> matchReturn ret
        MatchContinue mc -> match err mc
        MatchFail str    -> lift $ str >>= err
        MatchOptions ret cont
          -> match err cont <|> matchReturn ret

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


{-
data MatchResult m i v r
  = MatchReturn (v -> m r)
  | MatchContinue (i -> MatchResult m i v r)
  | MatchOptions (v -> m r) (i -> MatchResult m i v r)
  | MatchFail String
-}




