{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : Metamorth.ForOutput.Monad.EitherFail
Description : Variant of Either with MonadFail Instance
Copyright   : (c) David Wilson, 2024
License     : BSD-3

This module includes functions and types 
that will be used by the generated code.

This way, the code generator doesn't
have to generate static code that will
be the same regardless of the input files.

In the future, these modules may be moved
to a separate package.

This module contains a simple @newtype@ wrapper
around @`Either` (Maybe String) a@ so that it can
be an instance of `MonadFail` etc...

This can then be used as an alternate base Monad
for `Metamorth.ForOutput.Monad.Matcher.MatcherT`.

-}


module Metamorth.ForOutput.Monad.EitherFail
  ( EitherFail(.., LeftF, RightF, EmptyF)
  , toEither
  ) where

import Control.Applicative

import Control.Monad

-- | A simple wrapper around @`Either` (Maybe String) a@
--   so that the resulting `Monad` can support `MonadFail`,
--   `Alternative`, `MonadPlus`, etc...
--
--   With the accompanying pattern synonyms, there are
--   essentially three possibilities.
--
--     * @EmptyF@ : Failure with no message
--     * @LeftF str@ : Failure with a message
--     * @RightF x@ : Success
--
--    Where
--
--      * @EmptyF == Left Nothing@
--      * @LeftF str == Left (Just str)@
--      * @RightF x == Right x@
--
--    (Except lifted into a @newtype@)
newtype EitherFail a = EitherFail { getEither :: Either (Maybe String) a }
  deriving newtype (Functor, Applicative, Monad, Foldable)

-- | Like `getEither`, but converts `Nothing`s to
--   empty strings.
toEither :: EitherFail a -> Either String a
toEither EmptyF       = Left ""
toEither (LeftF  str) = Left str
toEither (RightF val) = Right val

instance Traversable EitherFail where
  traverse _ EmptyF = pure EmptyF
  traverse _ (LeftF x) = pure (LeftF x)
  traverse f (RightF x) = RightF <$> f x
  sequenceA EmptyF     = pure EmptyF
  sequenceA (LeftF  x) = pure (LeftF x)
  sequenceA (RightF x) = RightF <$> x
  

-- Using the Pattern styles to make it easier.
instance (Show a) => Show (EitherFail a) where
  show EmptyF       = "EmptyF"
  show (LeftF  str) = "(LeftF " ++ str ++ ")"
  show (RightF str) = "(RightF " ++ show str ++ ")"

instance MonadFail EitherFail where
  fail = LeftF

pattern LeftF :: String -> EitherFail a
pattern LeftF str = EitherFail (Left (Just str))

pattern RightF :: a -> EitherFail a
pattern RightF x = EitherFail (Right x)

pattern EmptyF :: EitherFail a
pattern EmptyF = EitherFail (Left Nothing)

{-# COMPLETE LeftF, RightF, EmptyF #-}

instance Alternative EitherFail where
  empty = EmptyF
  EmptyF <|> x = x
  x <|> EmptyF = x
  (LeftF _) <|> x@(RightF _) = x
  x@(RightF _) <|> _ = x
  -- Adding a newline between errors.
  (LeftF str1) <|> (LeftF str2) = LeftF $ str1 <> "\n" <> str2

instance MonadPlus EitherFail

instance Semigroup (EitherFail a) where
  EmptyF <> x = x
  x <> EmptyF = x
  x@(RightF _) <> _ = x
  (LeftF _) <> x@(RightF _) = x
  -- Adding a newline between errors.
  (LeftF str1) <> (LeftF str2) = LeftF $ str1 <> "\n" <> str2
  
instance Monoid (EitherFail a) where
  mempty = EmptyF

------------------------------------------------
-- Alternative Laws:

-- empty is a neutral element
-- (1) empty <|> u  =  u
-- (2) u <|> empty  =  u

-- Proof : (From definition of (<|>))
-- empty === EmptyF
-- EmptyF <|> x = x (1)
-- x <|> EmptyF = x (2)

-- (<|>) is associative
-- u <|> (v <|> w)  =  (u <|> v) <|> w (3)

-- Case 1: u == EmptyF:
-- EmptyF <|> (v <|> w) 
-- == v <|> w
-- By (1), we know that (EmptyF <|> v) = v
-- Therefore, we can substitute v ==> (EmptyF <|> v)
-- == (EmptyV <|> v) <|> w (3.1)

-- Case 2: w == EmptyF:
-- Similar to above; we know (v <|> EmptyF) == v, so...
-- u <|> (v <|> w)
-- == u <|> (v <|> EmptyF)
-- == u <|> v
-- == (u <|> v)
-- == (u <|> v) <|> EmptyF by (2)
-- (3.2)

-- Case 3: v == EmptyF
-- (Not going to write this one out; similar to 3.1 and 3.2)

-- Case 4: All values are LeftF
-- u <|> (v <|> w)  =?=  (u <|> v) <|> w
-- (LeftF u) <|> ((LeftF v) <|> (LeftF w)) =?= ((LeftF u) <|> (LeftF v)) <|> (LeftF w)
-- LeftF (u ++ "\n" ++ (v ++ "\n" ++ w)) =?= LeftF ((u ++ "\n" ++ v) ++ "\n" ++ w)
-- LeftF (u ++ "\n" ++ v ++ "\n" ++ w) == LeftF (u ++ "\n" ++ v ++ "\n" ++ w)
-- (By associativity of (++)/(<>))
-- Therefore, (3.4) holds

-- Case 5: u == RightF x
-- From the definition of (<|>) for EitherFail, 
-- (RightF x <|> _) == RightF x
-- Thus, (RightF x) <|> (v <|> w) == RightF x (a)
--       (RightF x) <|>  v        == RightF x (b)
--       (RightF x) <|>  w        == RightF x (c)

-- u <|> (v <|> w)  =  (u <|> v) <|> w (3)
-- (RightF x) <|> (v <|> w) =?=  ((RightF x) <|> v) <|> w
-- (RightF x) =?= ((RightF x) <|> v) <|> w   by (a)
-- (RightF x) =?= (RightF x) <|> w           by (b)
-- (RightF x) === (RightF x)                 by (c)
--
-- Therefore, (3.5) holds.

-- Case 6: v == RightF x, and u == EmptyF or (LeftF e)

-- Therefore...
-- EmptyF    <|> (RightF x) == RightF x
-- (LeftF e) <|> (RightF x) == RightF x
-- Therefore, u <|> (RightF x) == RightF x (d)

-- u <|> (v <|> w)  =?=  (u <|> v) <|> w
-- u <|> ((RightF x) <|> w) =?= (u <|> (RightF x)) <|> w
-- u <|> ((RightF x)) =?= (u <|> (RightF x)) <|> w   by (3.5c)
-- (RightF x) =?= (u <|> (RightF x)) <|> w           by (d)
-- (RightF x) =?= (RightF x) <|> w                   by (d)
-- RightF x == RightF x                              by (3.5c)

-- Therefore, (3.6 holds)

-- Case 7: w == RightF x, and u/v == EmptyF or LeftF e/f

-- EmptyF    <|> (RightF x) == RightF x
-- (LeftF e) <|> (RightF x) == RightF x
-- Therefore, v <|> (RightF x) == RightF x (e)

-- EmptyF <|> EmptyF == EmptyF
-- EmptyF <|> (LeftF f) == LeftF f
-- (LeftF e) <|> EmptyF == LeftF e
-- (LeftF e) <|> (LeftF f) == LeftF (e ++ "\n" ++ f)
-- Therefore, (u <|> v) == EmptyF or LeftF g, for some g (f).

-- u <|> (v <|> w)  =?=  (u <|> v) <|> w
-- u <|> (v <|> (RightF x))  =?=  (u <|> v) <|> (RightF x)
-- u <|> (RightF x)  =?=  (u <|> v) <|> (RightF x)   by (e)
-- u <|> (RightF x)  =?=  (LeftF g) <|> (RightF x)   by (f)
-- u <|> (RightF x)  =?=  (RightF x)                 by definition of (<|>)
-- (RightF x) == (RightF x)                          by (3.6d)

-- Therefore, since all cases hold, condition (3) holds.

-- Thus, `EitherFail` satisfies the `Alternative` laws.

