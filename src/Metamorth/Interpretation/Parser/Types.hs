module Metamorth.Interpretation.Parser.Types
  ( HeaderData(..)
  , PhonemePattern(..)
  , CharPattern(..)
  , processRawPhonePattern
  
  , RawPhonemePattern(..)
  , CharPatternRaw(..)
  ) where

import Data.Bifunctor

import Data.Map.Strict qualified as M

import Data.Char
import Metamorth.Helpers.Char

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Data.Maybe

-- | The data that's found in the header of the
--   parser file.
data HeaderData = HeaderData
  -- | The name of the orthography this parser is for.
  { hdOrthName    :: String
  -- | The name of the phoneme set this parser
  --   is working with.
  , hdPhoneSet    :: String
  --   A map from names of classes to
  --   the `Char`s in them.
  -- , hdCharClasses :: M.Map String [Char]
  } deriving (Show, Eq)

data CharPatternRaw
  = PlainCharR Char   -- ^ A single `Char`.
  | CharClassR String -- ^ Any member of a class from the header.
  | WordStartR        -- ^ The start of a word.
  | WordEndR          -- ^ The end of a word.
  deriving (Show, Eq, Ord)

-- | The patterns that can make up a character.
-- 
--   e.g. @ts' : t s *apost@ would become
--   > [PlainChar 't', PlainChar 's', CharClass "apost"]
data CharPattern
  = PlainChar Char   -- ^ A single `Char`.
  | CharOptCase Char -- ^ Any case of a `Char`.
  | CharClass String -- ^ Any member of a class from the header.
  | WordStart        -- ^ The start of a word.
  | WordEnd          -- ^ The end of a word.
  deriving (Show, Eq, Ord)

-- | Check whether a `CharPattern` is well-formed.
validCharPattern :: [CharPatternRaw] -> Bool
validCharPattern [] = False
validCharPattern ((PlainCharR _):xs) = validCharPattern' xs
validCharPattern ((CharClassR _):xs) = validCharPattern' xs
validCharPattern [WordStartR] = False -- can't just have a WordStart.
validCharPattern (WordStartR:WordEndR:xs) = False -- covers [WordStart,WordEnd] and other cases
validCharPattern (WordEndR:xs)   = False
validCharPattern (WordStartR:xs) = validCharPattern' xs

validCharPattern' :: [CharPatternRaw] -> Bool
validCharPattern' [] = True
validCharPattern' ((PlainCharR _):xs) = validCharPattern' xs
validCharPattern' ((CharClassR _):xs) = validCharPattern' xs
validCharPattern' [WordEndR] = True
validCharPattern' (WordStartR:xs) = False
validCharPattern' (WordEndR:xs)   = False

validCharPatternE :: [CharPatternRaw] -> Either String ()
validCharPatternE [] = Left "Can't have an empty pattern."
validCharPatternE ((PlainCharR _):xs) = validCharPatternE' xs
validCharPatternE ((CharClassR _):xs) = validCharPatternE' xs
validCharPatternE [WordStartR] = Left "Can't have a pattern with just a starting word mark."
validCharPatternE [WordStartR,WordEndR] = Left "Can't have a pattern that consists of just start and end mark(s)"
validCharPatternE (WordEndR:xs)   = Left "Can't start a pattern with a word-end mark."
validCharPatternE (WordStartR:xs) = validCharPatternE' xs

validCharPatternE' :: [CharPatternRaw] -> Either String ()
validCharPatternE' [] = Right ()
validCharPatternE' ((PlainCharR _):xs) = validCharPatternE' xs
validCharPatternE' ((CharClassR _):xs) = validCharPatternE' xs
validCharPatternE' [WordEndR] = Right ()
validCharPatternE' (WordStartR:xs) = Left "Can't have a word-start mark in the middle of a pattern."
validCharPatternE' (WordEndR:xs)   = Left "Can't have a word-end mark in the middle of a pattern."

-- | Convert a list of `CharPatternRaw`s into a
--   single `PhonemePattern`
validateCharPatternE :: [CharPatternRaw] -> Either String PhonemePattern
validateCharPatternE cpr = do
  (rslt, st) <- runStateT (validateCharPatternEX cpr) Nothing
  case st of
    (Just True) -> return $ PhonemePattern True  rslt
    _           -> return $ PhonemePattern False rslt

validateCharPatternEX :: [CharPatternRaw] -> StateT (Maybe Bool) (Either String) [CharPattern]
validateCharPatternEX [] = lift $ Left "Can't have an empty pattern."
validateCharPatternEX ((PlainCharR x):xs) = do
  when (isCasable x) (put $ Just (isTupper x))
  ((PlainChar x):) <$> validateCharPatternE' xs
validateCharPatternEX ((CharClassR x):xs) = ((CharClass x):) <$> validateCharPatternE' xs
validateCharPatternEX [WordStartR] = lift $ Left "Can't have a pattern with just a starting word mark."
validateCharPatternEX [WordStartR,WordEndR] = lift $ Left "Can't have a pattern that consists of just start and end mark(s)"
validateCharPatternEX (WordEndR:xs)   = lift $ Left "Can't start a pattern with a word-end mark."
validateCharPatternEX (WordStartR:xs) = (WordStart:) <$> validateCharPatternE' xs

validateCharPatternE' :: [CharPatternRaw] -> StateT (Maybe Bool) (Either String) [CharPattern]
validateCharPatternE' [] = return []
validateCharPatternE' ((PlainCharR x):xs) = do 
  cst <- get
  case cst of
    Nothing -> do 
      when (isCasable x) (put $ Just (isTupper x))
      ((PlainChar x):) <$> validateCharPatternE' xs
    (Just cs) -> do
      ((CharOptCase (toLower x)):) <$> validateCharPatternE' xs
  ((PlainChar x):) <$> validateCharPatternE' xs
validateCharPatternE' ((CharClassR x):xs) = ((CharClass x):) <$> validateCharPatternE' xs
validateCharPatternE' [WordEndR] = return [WordEnd]
validateCharPatternE' (WordStartR:xs) = lift $ Left "Can't have a word-start mark in the middle of a pattern."
validateCharPatternE' (WordEndR:xs)   = lift $ Left "Can't have a word-end mark in the middle of a pattern."



-- | A pattern that makes up a single phoneme.
--   Consists of multiple `CharPattern`s and 
--   a flag for whether the pattern is upper
--   or lower case.
data PhonemePattern = PhonemePattern 
  { isUpperPP  :: Bool          -- ^ Is this pattern upper-case?
  , charPatsPP :: [CharPattern] -- ^ The pattern of `Char`s for this phoneme.
  } deriving (Show, Eq)

makeLower :: PhonemePattern -> PhonemePattern
makeLower x = x { isUpperPP = False }

makeUpper :: PhonemePattern -> PhonemePattern
makeUpper x = x { isUpperPP = True }

-- | The Raw Parsed type of a phoneme pattern.
--   Needs to be converted to a list of possible
--   `PhonemePattern`s.
data RawPhonemePattern = RawPhonemePattern
  { modCharRP  :: [Char] -- ^ Modifier Characters for this pattern.
  , charPatsRP :: [CharPatternRaw]
  } deriving (Show, Eq)

-- | Process a `RawPhonemePattern` into one or more
--   valid `PhonemePattern`s. This, uh... needs some
--   more work.
processRawPhonePattern :: RawPhonemePattern -> Either [String] PhonemePattern
processRawPhonePattern pat 
  | otherwise = bimap (:collectedErrors) makeCase $ validateCharPatternE (charPatsRP pat)
  -- | otherwise = Left ["incomplete function"]
  
  where 
    mods = modCharRP pat
    hasUpper = '+' `elem` mods
    hasLower = '-' `elem` mods
    makeCase = if | hasUpper  -> makeUpper
                  | hasLower  -> makeLower
                  | otherwise -> id

    collectedErrors = concat [bothCaseError]
    -- Errors
    bothCaseError = if (hasUpper && hasLower) 
      then ["Cannot declare a pattern to be both upper and lower case."] 
      else []


