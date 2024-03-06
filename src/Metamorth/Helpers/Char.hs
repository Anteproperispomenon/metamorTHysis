module Metamorth.Helpers.Char
  ( isTupper
  , isCasable
  , getCases
  ) where

import Data.Char
import Data.List (nub)

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

getCases :: Char -> [Char]
getCases c = nub [c,c1,c2,c3]
  -- | (c == c1) && (c  == c2) && (c == c3) = [c]
  -- | (c == c2) && (c  == c3) = [c,c1]
  -- | (c == c1) && (c2 == c3) = [c,c2]
  -- | (c /= c1) && (c  /= c2) && (c /= c3) = nub [c,c1,c2,c3]
  where
    c1 = toLower c
    c2 = toUpper c
    c3 = toTitle c
    -- safeTail []     = []
    -- safeTail (x:xs) = xs