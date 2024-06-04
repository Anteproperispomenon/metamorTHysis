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
  , getCase
  , getMaxCase
  , getMinCase
  , getFirstCase
  , getLastCase
  ) where

import Data.Char (isUpperCase, isLowerCase, generalCategory, GeneralCategory(TitlecaseLetter))

-- | Basic caseness value of a `Char`.
data CharCase 
   = NonCased
   | LowerCase
   | UpperCase
   deriving (Show, Eq, Ord, Enum)

getCase :: Char -> CharCase
getCase c
  | (isUpperCase c) || (generalCategory c == TitlecaseLetter) = UpperCase
  | (isLowerCase c) = LowerCase
  | otherwise       = NonCased

getMaxCase :: [CharCase] -> CharCase
getMaxCase = foldl max LowerCase

getMinCase :: [CharCase] -> CharCase
getMinCase xs = max LowerCase $ foldl min LowerCase xs

getFirstCase :: [CharCase] -> CharCase
getFirstCase [] = LowerCase
getFirstCase (LowerCase:_) = LowerCase
getFirstCase (UpperCase:_) = UpperCase
getFirstCase (NonCased:xs) = getFirstCase xs

getLastCase :: [CharCase] -> CharCase
getLastCase xs = getFirstCase $ reverse xs

