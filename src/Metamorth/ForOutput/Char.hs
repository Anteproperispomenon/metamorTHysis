{-|
Module      : Metamorth.ForOutput.Char
Description : Character Helpers for Generated Code
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

module Metamorth.ForOutput.Char
  ( CharCase(..)

  ) where

-- | 
data CharCase 
   = NonCased
   | LowerCase
   | UpperCase
   deriving (Show, Eq, Ord, Enum)
