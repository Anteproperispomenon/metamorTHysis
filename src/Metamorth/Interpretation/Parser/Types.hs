module Metamorth.Interpretation.Parser.Types
  ( HeaderData(..)
  , PhonemePattern(..)
  , CharPattern(..)
  , ppCharPats
  , Caseness(..)
  , processRawPhonePattern
  , StateMod(..)

  , CheckState(..)
  , ModifyState(..)
  
  , RawPhonemePattern(..)
  , CharPatternRaw(..)
  ) where

import Data.Bifunctor

import Data.List (partition)

import Data.Map.Strict qualified as M

import Data.Char
import Metamorth.Helpers.Char

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Data.Maybe

import Metamorth.Helpers.Either

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
  | NotStartR         -- ^ NOT the start of a word.
  | NotEndR           -- ^ NOT the end of a word.
  | ValStateR String (Either String Bool) -- ^ Check that the state is a certain value.
  | SetStateR String (Either String Bool) -- ^ Set the state to a certain value.
  deriving (Show, Eq, Ord)

isStateR :: CharPatternRaw -> Bool
isStateR (ValStateR _ _) = True
isStateR (SetStateR _ _) = True
isStateR _ = False

-- | The patterns that can make up a character.
-- 
--   e.g. @ts' : t s *apost@ would become
--   > [PlainChar 't', PlainChar 's', CharClass "apost"]
data CharPattern
  = PlainChar   [CheckState] Char   -- ^ A single `Char`.
  | CharOptCase [CheckState] Char -- ^ Any case of a `Char`.
  | CharClass   [CheckState] String -- ^ Any member of a class from the header.
  | WordStart        -- ^ The start of a word.
  | WordEnd          -- ^ The end of a word.
  | NotStart         -- ^ NOT the start of a word.
  | NotEnd           -- ^ NOT the end of a word.
  deriving (Show, Eq, Ord)

data CheckState
  = CheckStateB String Bool
  | CheckStateV String String
  deriving (Show, Eq, Ord)

-- | Adds `CheckState` info to the first
--   compatible element in a pattern.
addStateToPat :: [CheckState] -> [CharPattern] -> [CharPattern]
addStateToPat [] cpats = cpats
addStateToPat sts ((PlainChar   xs c):rst) = (PlainChar   (sts ++ xs) c) : rst
addStateToPat sts ((CharOptCase xs c):rst) = (CharOptCase (sts ++ xs) c) : rst
addStateToPat sts ((CharClass   xs c):rst) = (CharClass   (sts ++ xs) c) : rst
addStateToPat sts (x:xs) = x : (addStateToPat sts xs)
addStateToPat _ [] = []

data ModifyState
  = ModifyStateB String Bool
  | ModifyStateV String String
  deriving (Show, Eq, Ord)

separateStateMod :: StateMod -> Either CheckState ModifyState
separateStateMod (ValStateB x b) = Left  (CheckStateB  x b)
separateStateMod (ValStateV x v) = Left  (CheckStateV  x v)
separateStateMod (SetStateB x b) = Right (ModifyStateB x b)
separateStateMod (SetStateV x v) = Right (ModifyStateV x v)

startPatR :: CharPatternRaw -> Bool
startPatR WordStartR = True
startPatR NotStartR  = True
startPatR _ = False

endPatR :: CharPatternRaw -> Bool
endPatR WordEndR = True
endPatR NotEndR  = True
endPatR _ = False

-- Note that NotStart must occur at the beginning of
-- a pattern; otherwise, it wouldn't make sense.

-- | "Pretty" Print a @[`CharPattern`]@.
ppCharPats :: [CharPattern] -> String
ppCharPats = concatMap ppCharPat

ppCharPat :: CharPattern -> String
ppCharPat (PlainChar   _ c) = [c]
ppCharPat (CharOptCase _ c) = [c]
ppCharPat (CharClass _ str) = "<" ++ str ++ ">"
ppCharPat WordStart = "^"
ppCharPat WordEnd   = "$"
ppCharPat NotStart  = "%"
ppCharPat NotEnd    = "&"

-- | Check whether a `CharPattern` is well-formed.
validCharPattern :: [CharPatternRaw] -> Bool
validCharPattern [] = False
validCharPattern ((PlainCharR _):xs) = validCharPattern' xs
validCharPattern ((CharClassR _):xs) = validCharPattern' xs
validCharPattern [x] | startPatR x = False -- can't just have a WordStart.
validCharPattern (x:y:xs) | (startPatR x) && (endPatR y) = False -- covers [WordStart,WordEnd] and other cases
-- validCharPattern (NotStartR:WordEndR:xs)  = False
validCharPattern (x:xs) | (endPatR x) = False
validCharPattern (WordStartR:xs) = validCharPattern' xs
validCharPattern (NotStartR :xs) = validCharPattern' xs
validCharPattern _ = False

validCharPattern' :: [CharPatternRaw] -> Bool
validCharPattern' [] = True
validCharPattern' ((PlainCharR _):xs) = validCharPattern' xs
validCharPattern' ((CharClassR _):xs) = validCharPattern' xs
validCharPattern' [x] | endPatR x = True
validCharPattern' (x:xs) | (startPatR x || endPatR x || isStateR x) = False

validCharPatternE :: [CharPatternRaw] -> Either String ()
validCharPatternE [] = Left "Can't have an empty pattern."
validCharPatternE ((PlainCharR _):xs) = validCharPatternE' xs
validCharPatternE ((CharClassR _):xs) = validCharPatternE' xs
validCharPatternE [x]    | (startPatR x) = Left "Can't have a pattern with just a [not-]start-word mark."
validCharPatternE [x,y]  | (startPatR x && endPatR y) = Left "Can't have a pattern that consists of just [not-]start and [not-]end mark(s)"
validCharPatternE (x:xs) | (endPatR x) = Left "Can't start a pattern with a [not-]word-end mark."
validCharPatternE (WordStartR:xs) = validCharPatternE' xs
validCharPatternE (NotStartR :xs) = validCharPatternE' xs
validCharPatternE (x:_)  
  | isStateR x = Left "State-patterns should have been filtered out by this point."
  | otherwise  = Left "Some type of error happened in validateCharPatternZX."


validCharPatternE' :: [CharPatternRaw] -> Either String ()
validCharPatternE' [] = Right ()
validCharPatternE' ((PlainCharR _):xs) = validCharPatternE' xs
validCharPatternE' ((CharClassR _):xs) = validCharPatternE' xs
validCharPatternE' [x] | (endPatR x) = Right ()
validCharPatternE' (x:xs) 
  | (startPatR x) = Left "Can't have a [not-]word-start mark in the middle of a pattern."
  | (endPatR   x) = Left "Can't have a [not-]word-end mark in the middle of a pattern."
  | (isStateR  x) = Left "State-patterns should have been filtered out by this point."
  | otherwise     = Left "Some type of error happened in validateCharPatternE'."

-- | Convert a list of `CharPatternRaw`s into a
--   single `PhonemePattern`
validateCharPatternE :: [ModifyState] -> [CheckState] -> [CharPatternRaw] -> Either String PhonemePattern
validateCharPatternE stms chks cpr = do
  (rslt, st) <- runStateT (validateCharPatternEX cpr) Nothing
  case st of
    (Just True) -> return $ PhonemePattern CMaj (addStateToPat chks rslt) stms
    _           -> return $ PhonemePattern CMin (addStateToPat chks rslt) stms

validateCharPatternEX :: [CharPatternRaw] -> StateT (Maybe Bool) (Either String) [CharPattern]
validateCharPatternEX [] = lift $ Left "Can't have an empty pattern."
validateCharPatternEX ((PlainCharR x):xs) = do
  when (isCasable x) (put $ Just (isTupper x))
  ((PlainChar [] x):) <$> validateCharPatternE' xs
validateCharPatternEX ((CharClassR x):xs) = ((CharClass [] x):) <$> validateCharPatternE' xs
validateCharPatternEX [x]    | (startPatR x) = lift $ Left "Can't have a pattern with just a [not-]start-word mark."
validateCharPatternEX [x,y]  | (startPatR x && endPatR y) = lift $ Left "Can't have a pattern that consists of just [not-]start and [not-]end mark(s)"
validateCharPatternEX (x:xs) | (endPatR x) = lift $ Left "Can't start a pattern with a [not-]word-end mark."
validateCharPatternEX (WordStartR:xs) = (WordStart:) <$> validateCharPatternE' xs
validateCharPatternEX (NotStartR :xs) = (NotStart :) <$> validateCharPatternE' xs
validateCharPatternEX (x:_)  | isStateR x = lift $ Left "State-patterns should have been filtered out by this point."
validateCharPatternEX _ = lift $ Left "Some type of error happened in validateCharPatternEX."

validateCharPatternE' :: [CharPatternRaw] -> StateT (Maybe Bool) (Either String) [CharPattern]
validateCharPatternE' [] = return []
validateCharPatternE' ((PlainCharR x):xs) = do 
  cst <- get
  case cst of
    Nothing -> do
      when (isCasable x) (put $ Just (isTupper x))
      ((PlainChar [] x):) <$> validateCharPatternE' xs
    (Just cs) -> do
      ((CharOptCase [] (toLower x)):) <$> validateCharPatternE' xs
  -- ((PlainChar [] x):) <$> validateCharPatternE' xs -- ???
validateCharPatternE' ((CharClassR x):xs) = ((CharClass [] x):) <$> validateCharPatternE' xs
validateCharPatternE' [WordEndR] = return [WordEnd]
validateCharPatternE' [NotEndR]  = return [NotEnd]
validateCharPatternE' (x:xs) 
  | (startPatR x) = lift $ Left "Can't have a [not-]word-start mark in the middle of a pattern."
  | (endPatR   x) = lift $ Left "Can't have a [not-]word-end mark in the middle of a pattern."
  | (isStateR  x) = lift $ Left "State-patterns should have been filtered out by this point."
  | otherwise     = lift $ Left "Some type of error happened in validateCharPatternE'."

-- | Alternate version of `validateCharPatternE`.
validateCharPatternZ :: [ModifyState] -> [CheckState] -> [CharPatternRaw] -> Either String PhonemePattern
validateCharPatternZ stms chks cpr = do
  (rslt, _st) <- runStateT (validateCharPatternZX cpr) Nothing
  return $ PhonemePattern CDep (addStateToPat chks rslt) stms

validateCharPatternZX :: [CharPatternRaw] -> StateT (Maybe Bool) (Either String) [CharPattern]
validateCharPatternZX [] = lift $ Left "Can't have an empty pattern."
validateCharPatternZX ((PlainCharR x):xs) = do
  when (isCasable x) (put $ Just (isTupper x))
  ((CharOptCase [] x):) <$> validateCharPatternZ' xs
validateCharPatternZX ((CharClassR x):xs) = ((CharClass [] x):) <$> validateCharPatternZ' xs
validateCharPatternZX [x]    | (startPatR x) = lift $ Left "Can't have a pattern with just a [not-]start-word mark."
validateCharPatternZX [x,y]  | (startPatR x && endPatR y) = lift $ Left "Can't have a pattern that consists of just [not-]start and [not-]end mark(s)"
validateCharPatternZX (x:xs) | (endPatR  x) = lift $ Left "Can't start a pattern with a [not-]word-end mark."
                             | (isStateR x) = lift $ Left "State-patterns should have been filtered out by this point."
validateCharPatternZX (WordStartR:xs) = (WordStart:) <$> validateCharPatternZ' xs
validateCharPatternZX (NotStartR :xs) = (NotStart :) <$> validateCharPatternZ' xs
validateCharPatternZX _ = lift $ Left "Some type of error happened in validateCharPatternZX."

validateCharPatternZ' :: [CharPatternRaw] -> StateT (Maybe Bool) (Either String) [CharPattern]
validateCharPatternZ' [] = return []
validateCharPatternZ' ((PlainCharR x):xs) = do 
  cst <- get
  case cst of
    Nothing -> do
      when (isCasable x) (put $ Just (isTupper x))
      ((caseChar x):) <$> validateCharPatternZ' xs
    (Just cs) -> do
      ((caseChar x):) <$> validateCharPatternZ' xs
  -- ((PlainChar x):) <$> validateCharPatternZ' xs -- ???
validateCharPatternZ' ((CharClassR x):xs) = ((CharClass [] x):) <$> validateCharPatternZ' xs
validateCharPatternZ' [WordEndR] = return [WordEnd]
validateCharPatternZ' [NotEndR]  = return [NotEnd]
validateCharPatternZ' (x:xs) 
  | (startPatR x) = lift $ Left "Can't have a [not-]word-start mark in the middle of a pattern."
  | (endPatR   x) = lift $ Left "Can't have a [not-]word-end mark in the middle of a pattern."
  | (isStateR  x) = lift $ Left "State-patterns should have been filtered out by this point."
  | otherwise     = lift $ Left "Some type of error happened when validating a CharPatternRaw."

caseChar :: Char -> CharPattern
caseChar c
  | isCasable c = CharOptCase [] (toLower c) -- Maybe ?
  | otherwise   = PlainChar [] c


data Caseness
  = CMaj -- ^ Upper-Case, or "Majuscule"
  | CMin -- ^ Lower-Case, or "Minuscule"
  | CDep -- ^ Dependent on other factors.
  deriving (Show, Eq, Ord)

data StateMod 
  -- | Set a state to a boolean value
  = SetStateB String Bool
  -- | Set a state to a enumerated value
  | SetStateV String String
  -- | Check whether a state is a certain boolean value.
  | ValStateB String Bool
  -- | Check whether a state is a certain enumerated value.
  | ValStateV String String
  deriving (Show, Eq)


--   | ValStateR String (Either String Bool) -- ^ Check that the state is a certain value.
--   | SetStateR String (Either String Bool) -- ^ Set the state to a certain value.

makeStateMod :: CharPatternRaw -> Maybe StateMod
makeStateMod (ValStateR x (Left str)) = Just $ ValStateV x str
makeStateMod (ValStateR x (Right bl)) = Just $ ValStateB x bl
makeStateMod (SetStateR x (Left str)) = Just $ SetStateV x str
makeStateMod (SetStateR x (Right bl)) = Just $ SetStateB x bl
makeStateMod _ = Nothing

-- | A pattern that makes up a single phoneme.
--   Consists of multiple `CharPattern`s and 
--   a flag for whether the pattern is upper
--   or lower case.
data PhonemePattern = PhonemePattern 
  { isUpperPP   :: Caseness      -- ^ Is this pattern upper-case?
  , charPatsPP  :: [CharPattern] -- ^ The pattern of `Char`s for this phoneme.
  , stateModsPP :: [ModifyState]
  } deriving (Show, Eq)

makeLower :: PhonemePattern -> PhonemePattern
makeLower x = x { isUpperPP = CMin }

makeUpper :: PhonemePattern -> PhonemePattern
makeUpper x = x { isUpperPP = CMaj }

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
  | (hasUpper || hasLower) = bimap (:collectedErrors) makeCase $ validateCharPatternE mstates vstates cpats
  | otherwise              = bimap (:collectedErrors) makeCase $ validateCharPatternZ mstates vstates cpats
  -- | otherwise = Left ["incomplete function"]
  
  where 
    -- Filtering out state changes
    (cstates, cpats) = partition isStateR $ charPatsRP pat
    cstates' = mapMaybe makeStateMod cstates

    (vstates, mstates) = partitionWith separateStateMod cstates'

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


