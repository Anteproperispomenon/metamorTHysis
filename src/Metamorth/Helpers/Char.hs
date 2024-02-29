module Metamorth.Helpers.Char
  ( isTupper
  , isCasable
  ) where

import Data.Char

-- | Test whether a `Char` is Upper/Title case.
--   This is here because both `isUpper` and
--   `isUpperCase` miss certain characters
--   that can be considered upper-case or
--   title-case.
isTupper :: Char -> Bool
isTupper x = isUpperCase x || (generalCategory x == TitlecaseLetter)

-- | To check whether a letter is "Casable";
--   i.e. whether it is considered a letter
--   that can be upper or lower case. The
--   predicate at the moment is not perfect,
--   but it filters out punctuation such as
--   apostrophes.
isCasable :: Char -> Bool
isCasable x = (generalCategory x) <= TitlecaseLetter
  