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
--      * @EmptyF == Left Nothing@
--      * @LeftF str == Left (Just str)@
--      * @RightF x == Right x@
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

instance Semigroup (EitherFail a) where
  EmptyF <> x = x
  x <> EmptyF = x
  x@(RightF _) <> _ = x
  (LeftF _) <> x@(RightF _) = x
  -- Adding a newline between errors.
  (LeftF str1) <> (LeftF str2) = LeftF $ str1 <> "\n" <> str2
  
instance Monoid (EitherFail a) where
  mempty = EmptyF

