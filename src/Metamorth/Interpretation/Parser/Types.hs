{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Metamorth.Interpretation.Parser.Types
  ( HeaderData(..)
  , PhonemePattern(..)
  , CharPattern(..)
  , CharPatternF(..)
  , ppCharPats
  , Caseness(..)
  , processRawPhonePattern
  , StateMod(..)

  , CheckState(..)
  , ModifyState(..)

  , CheckStateX(..)
  , ModifyStateX(..)
  , stateSubsetOf
  , mergeStatesInto
  
  , RawPhonemePattern(..)
  , CharPatternRaw(..)
  ) where

import Data.Bifunctor

import Data.Ord (Down(..))
import Data.List (partition, find)

import Data.Map.Strict qualified as M

import Data.Char
import Metamorth.Helpers.Char

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Data.Maybe

import Data.Set qualified as S

import Metamorth.Helpers.Either
import Metamorth.Helpers.Ord

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
--
--   Note that this is implemented as a type synonym
--   over `CharPatternF`. This is to control how the
--   `Ord` instance is constructed. In particular,
--   since we want to try more restrictive patterns
--   first, we want the "lowest" value of @[`CheckStateX`]@
--   to be the one with the most elements. Thus, we
--   derive @`Ord` (`CharPatternF` [b])@ via 
--   @`CharPatternF` (Down (`SizeOrdList` b))@.
type CharPattern = CharPatternF [CheckStateX]

-- | The "internal" version of `CharPattern`.
data CharPatternF b
  = PlainChar   b Char   -- ^ A single `Char`.
  | CharOptCase b Char   -- ^ Any case of a `Char`.
  | CharClass   b String -- ^ Any member of a class from the header.
  | WordStart            -- ^ The start of a word.
  | WordEnd              -- ^ The end of a word.
  | NotStart             -- ^ NOT the start of a word.
  | NotEnd               -- ^ NOT the end of a word.
  deriving (Show, Eq)

-- Make (CharPatternF (Down b)) an instance of `Ord` so that
-- (CharPatternF b) has something to derive via. Also make this
-- function overlapping so that it can be chosen as the instance
-- instead of (CharPatternF b). Unfortunately, this means that
-- Using CharPatternF (Down b) *won't* reverse the order as
-- might be expected. ...I think. Fortunately, `CharPatternF`
-- isn't supposed to be used with anything other than [CheckStateX],
-- so it isn't much of a problem in practice.
deriving instance {-# OVERLAPPING #-} (Ord b) => Ord (CharPatternF (Down b))

deriving via (CharPatternF (Down b)) instance {-# OVERLAPPABLE #-} (Ord b) => Ord (CharPatternF b) 

-- We want this instance to be chosen over plain @b@, but
-- *not* over @Down b@.
deriving via (CharPatternF (Down (SizeOrdList b))) instance {-# OVERLAPS #-} (Ord b) => Ord (CharPatternF [b])

-- | The unvalidated "Check-state" type.
data CheckState
  = CheckStateB String Bool
  | CheckStateV String String
  deriving (Show, Eq, Ord)

-- | The validated "Check-State" type.
data CheckStateX
  -- | Boolean check on a bool-state.
  = CheckStateBB String Bool
  -- | Value check on a value-state.
  | CheckStateVV String String
  -- | Boolean check on a value-state.
  | CheckStateVB String Bool
  deriving (Show, Eq, Ord)

checkStateString :: CheckStateX -> String
checkStateString (CheckStateBB strX _) = strX
checkStateString (CheckStateVV strX _) = strX
checkStateString (CheckStateVB strX _) = strX

-- | Check whether one state is a subset of another.
stateSubsetOf :: [CheckStateX] -> [CheckStateX] -> Bool
stateSubsetOf [] [] = True
stateSubsetOf _s [] = True
stateSubsetOf [] _s = False
stateSubsetOf xs ys = (stateSubsetOfX xs ys) && (stateSupsetOfX ys xs)

-- | Check whether all of the elements in the first
--   list are either missing from the second list,
--   or less general than the equivalent entry in
--   the second list.
stateSubsetOfX :: [CheckStateX] -> [CheckStateX] -> Bool
stateSubsetOfX [] _s = True
stateSubsetOfX (st:sts) sts2
  | lst1 <- filter (findStates (checkStateString st)) sts2
  = (null lst1 || all (stateSubsetOf' st) lst1) && (stateSubsetOfX sts sts2)

-- | Check whether all of the elements in the first list
--   are at least as general as an equivalent element in
--   the second list.
stateSupsetOfX :: [CheckStateX] -> [CheckStateX] -> Bool
stateSupsetOfX [] _s = True
stateSupsetOfX (st:sts) sts2
  | lst1 <- filter (findStates (checkStateString st)) sts2
  = (notNull lst1 && all (stateSupsetOf' st) lst1) && (stateSupsetOfX sts sts2)
  where 
    notNull = not . null
    stateSupsetOf' = flip stateSubsetOf'

findStates :: String -> CheckStateX -> Bool
findStates str (CheckStateBB strX _) = str == strX
findStates str (CheckStateVV strX _) = str == strX
findStates str (CheckStateVB strX _) = str == strX

stateSubsetOf' :: CheckStateX -> CheckStateX -> Bool
stateSubsetOf' (CheckStateVV str1 _str) (CheckStateVB strX blY)
  = (str1 == strX) && blY
stateSubsetOf' (CheckStateVV str1 str2) (CheckStateVV strX strY)
  = (str1 == strX) && (str2 == strY)
stateSubsetOf' (CheckStateBB str1 bl1) (CheckStateBB str2 bl2)
  = (str1 == str2) && (bl1 == bl2)
stateSubsetOf' _ _ = False



-- | Adds `CheckState` info to the first
--   compatible element in a pattern.
addStateToPat :: [CheckStateX] -> [CharPattern] -> [CharPattern]
addStateToPat [] cpats = cpats
addStateToPat sts ((PlainChar   xs c):rst) = (PlainChar   (sts ++ xs) c) : rst
addStateToPat sts ((CharOptCase xs c):rst) = (CharOptCase (sts ++ xs) c) : rst
addStateToPat sts ((CharClass   xs c):rst) = (CharClass   (sts ++ xs) c) : rst
addStateToPat sts (x:xs) = x : (addStateToPat sts xs)
addStateToPat _ [] = []

-- | Unverified modify state; doesn't
--   know whether the state being
--   modified is avalue-state or a
--   boolean state.
data ModifyState
  = ModifyStateB String Bool
  | ModifyStateV String String
  deriving (Show, Eq, Ord)

-- | Verified modify state; provides info
--   about the state whose value is being
--   changed.
data ModifyStateX
  -- | Change the value of a boolean state.
  = ModifyStateBB String Bool
  -- | Change the value of a value-state.
  | ModifyStateVV String String
  -- | Set a value-state to `Nothing`.
  | ModifyStateVX String
  deriving (Show, Eq, Ord)

hasModX :: String -> [ModifyStateX] -> Bool
hasModX str sts = isJust $ find (isState {- str -}) sts
  where
    isState :: {- String -> -} ModifyStateX -> Bool
    isState {-str-} (ModifyStateBB str2 _) = str == str2
    isState {-str-} (ModifyStateVV str2 _) = str == str2
    isState {-str-} (ModifyStateVX str2  ) = str == str2

-- | Merge two sets of `ModifyStateX`s. Values on the 
--   right take priority
mergeStatesInto :: [ModifyStateX] -> [ModifyStateX] -> [ModifyStateX]
mergeStatesInto mods [] = mods
mergeStatesInto [] mods = mods
mergeStatesInto (modr@(ModifyStateBB str _):rst) mods
  | hasModX str mods = mergeStatesInto rst mods
  | otherwise        = modr : mergeStatesInto rst mods
mergeStatesInto (modr@(ModifyStateVV str _):rst) mods
  | hasModX str mods = mergeStatesInto rst mods
  | otherwise        = modr : mergeStatesInto rst mods
mergeStatesInto (modr@(ModifyStateVX str  ):rst) mods
  | hasModX str mods = mergeStatesInto rst mods
  | otherwise        = modr : mergeStatesInto rst mods

-- | Separate a `StateMod` into a `CheckState` or a `ModifyState`.
--   Such values will still need to be validated by `validateCheckState`
--   or `validateModifyState` respectively.
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
validCharPattern (x:y:_xs) | (startPatR x) && (endPatR y) = False -- covers [WordStart,WordEnd] and other cases
-- validCharPattern (NotStartR:WordEndR:xs)  = False
validCharPattern (x:_xs) | (endPatR x) = False
validCharPattern (WordStartR:xs) = validCharPattern' xs
validCharPattern (NotStartR :xs) = validCharPattern' xs
validCharPattern _ = False

validCharPattern' :: [CharPatternRaw] -> Bool
validCharPattern' [] = True
validCharPattern' ((PlainCharR _):xs) = validCharPattern' xs
validCharPattern' ((CharClassR _):xs) = validCharPattern' xs
validCharPattern' [x] | endPatR x = True
validCharPattern' (x:_s) | (startPatR x || endPatR x || isStateR x) = False
validCharPattern' _ = False

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
validCharPatternE' (x:_) 
  | (startPatR x) = Left "Can't have a [not-]word-start mark in the middle of a pattern."
  | (endPatR   x) = Left "Can't have a [not-]word-end mark in the middle of a pattern."
  | (isStateR  x) = Left "State-patterns should have been filtered out by this point."
  | otherwise     = Left "Some type of error happened in validateCharPatternE'."

-- | Convert a list of `CharPatternRaw`s into a
--   single `PhonemePattern`
validateCharPatternE :: [ModifyStateX] -> [CheckStateX] -> [CharPatternRaw] -> Either String PhonemePattern
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
validateCharPatternEX (x:_)  | (endPatR x) = lift $ Left "Can't start a pattern with a [not-]word-end mark."
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
    (Just _cs) -> do
      ((CharOptCase [] (toLower x)):) <$> validateCharPatternE' xs
  -- ((PlainChar [] x):) <$> validateCharPatternE' xs -- ???
validateCharPatternE' ((CharClassR x):xs) = ((CharClass [] x):) <$> validateCharPatternE' xs
validateCharPatternE' [WordEndR] = return [WordEnd]
validateCharPatternE' [NotEndR]  = return [NotEnd]
validateCharPatternE' (x:_) 
  | (startPatR x) = lift $ Left "Can't have a [not-]word-start mark in the middle of a pattern."
  | (endPatR   x) = lift $ Left "Can't have a [not-]word-end mark in the middle of a pattern."
  | (isStateR  x) = lift $ Left "State-patterns should have been filtered out by this point."
  | otherwise     = lift $ Left "Some type of error happened in validateCharPatternE'."

-- | Alternate version of `validateCharPatternE`.
validateCharPatternZ :: [ModifyStateX] -> [CheckStateX] -> [CharPatternRaw] -> Either String PhonemePattern
validateCharPatternZ stms chks cpr = do
  (rslt, _st) <- runStateT (validateCharPatternZX cpr) Nothing
  return $ PhonemePattern CDep (addStateToPat chks rslt) stms

validateCharPatternZX :: [CharPatternRaw] -> StateT (Maybe Bool) (Either String) [CharPattern]
validateCharPatternZX [] = lift $ Left "Can't have an empty pattern."
validateCharPatternZX ((PlainCharR x):xs) = do
  when (isCasable x) (put $ Just (isTupper x))
  ((CharOptCase [] x):) <$> validateCharPatternZ' xs
validateCharPatternZX ((CharClassR x):xs) = ((CharClass [] x):) <$> validateCharPatternZ' xs
validateCharPatternZX [x]   | (startPatR x) = lift $ Left "Can't have a pattern with just a [not-]start-word mark."
validateCharPatternZX [x,y] | (startPatR x && endPatR y) = lift $ Left "Can't have a pattern that consists of just [not-]start and [not-]end mark(s)"
validateCharPatternZX (x:_) | (endPatR  x) = lift $ Left "Can't start a pattern with a [not-]word-end mark."
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
    (Just _s) -> do
      ((caseChar x):) <$> validateCharPatternZ' xs
  -- ((PlainChar x):) <$> validateCharPatternZ' xs -- ???
validateCharPatternZ' ((CharClassR x):xs) = ((CharClass [] x):) <$> validateCharPatternZ' xs
validateCharPatternZ' [WordEndR] = return [WordEnd]
validateCharPatternZ' [NotEndR]  = return [NotEnd]
validateCharPatternZ' (x:_) 
  | (startPatR x) = lift $ Left "Can't have a [not-]word-start mark in the middle of a pattern."
  | (endPatR   x) = lift $ Left "Can't have a [not-]word-end mark in the middle of a pattern."
  | (isStateR  x) = lift $ Left "State-patterns should have been filtered out by this point."
  | otherwise     = lift $ Left "Some type of error happened when validating a CharPatternRaw."

-- | Validate a pattern that has null-case. That is,
--   it SHOULD ONLY use the listed character, not the
--   lower-case or upper-case equivalent of it.
validateCharPatternN :: [ModifyStateX] -> [CheckStateX] -> [CharPatternRaw] -> Either String PhonemePattern
validateCharPatternN stms chks cpr = do
  rslt <- validateCharPatternNX cpr
  return $ PhonemePattern CNul (addStateToPat chks rslt) stms

validateCharPatternNX :: [CharPatternRaw] -> Either String [CharPattern]
validateCharPatternNX [] = Left "Can't have an empty pattern."
validateCharPatternNX ((PlainCharR x):xs) = do
  ((PlainChar [] x):) <$> validateCharPatternN' xs
validateCharPatternNX ((CharClassR x):xs) = ((CharClass [] x):) <$> validateCharPatternN' xs
validateCharPatternNX [x]    | (startPatR x) = Left "Can't have a pattern with just a [not-]start-word mark."
validateCharPatternNX [x,y]  | (startPatR x && endPatR y) = Left "Can't have a pattern that consists of just [not-]start and [not-]end mark(s)"
validateCharPatternNX (x:_)  | (endPatR x) = Left "Can't start a pattern with a [not-]word-end mark."
validateCharPatternNX (WordStartR:xs) = (WordStart:) <$> validateCharPatternN' xs
validateCharPatternNX (NotStartR :xs) = (NotStart :) <$> validateCharPatternN' xs
validateCharPatternNX (x:_)  | isStateR x = Left "State-patterns should have been filtered out by this point."
validateCharPatternNX _ = Left "Some type of error happened in validateCharPatternEX."

validateCharPatternN' :: [CharPatternRaw] -> Either String [CharPattern]
validateCharPatternN' [] = return []
validateCharPatternN' ((PlainCharR x):xs) = 
  ((PlainChar [] x):) <$> validateCharPatternN' xs
  -- ((PlainChar [] x):) <$> validateCharPatternE' xs -- ???
validateCharPatternN' ((CharClassR x):xs) = ((CharClass [] x):) <$> validateCharPatternN' xs
validateCharPatternN' [WordEndR] = return [WordEnd]
validateCharPatternN' [NotEndR]  = return [NotEnd]
validateCharPatternN' (x:_) 
  | (startPatR x) = Left "Can't have a [not-]word-start mark in the middle of a pattern."
  | (endPatR   x) = Left "Can't have a [not-]word-end mark in the middle of a pattern."
  | (isStateR  x) = Left "State-patterns should have been filtered out by this point."
  | otherwise     = Left "Some type of error happened in validateCharPatternE'."



caseChar :: Char -> CharPattern
caseChar c
  | isCasable c = CharOptCase [] (toLower c) -- Maybe ?
  | otherwise   = PlainChar [] c


data Caseness
  = CMaj -- ^ Upper-Case, or "Majuscule"
  | CMin -- ^ Lower-Case, or "Minuscule"
  | CDep -- ^ Dependent on other factors.
  | CNul -- ^ Null-caseness
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
  , stateModsPP :: [ModifyStateX]
  } deriving (Show, Eq)

makeLower :: PhonemePattern -> PhonemePattern
makeLower x = x { isUpperPP = CMin }

makeUpper :: PhonemePattern -> PhonemePattern
makeUpper x = x { isUpperPP = CMaj }

makeNullCase :: PhonemePattern -> PhonemePattern
makeNullCase x = x { isUpperPP = CNul }

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
processRawPhonePattern :: M.Map String (Maybe (S.Set String)) -> RawPhonemePattern -> Either [String] PhonemePattern
processRawPhonePattern sdict pat
  | hasUpper || hasLower = bimap (:collectedErrors) makeCase $ validateCharPatternE mstates' vstates' cpats
  | hasNull              = bimap (:collectedErrors) makeCase $ validateCharPatternN mstates' vstates' cpats -- for now
  | otherwise            = bimap (:collectedErrors) makeCase $ validateCharPatternZ mstates' vstates' cpats
  -- | otherwise = Left ["incomplete function"]
  
  where 
    -- Filtering out state changes
    (cstates, cpats) = partition isStateR $ charPatsRP pat
    cstates' = mapMaybe makeStateMod cstates

    (vstates, mstates) = partitionWith separateStateMod cstates'

    (verrs, vstates') = partitionWith (validateCheckState  sdict) vstates
    (merrs, mstates') = partitionWith (validateModifyState sdict) mstates

    mods = modCharRP pat
    hasUpper = '+' `elem` mods
    hasLower = '-' `elem` mods
    hasNull  = '~' `elem` mods
    makeCase = if | hasUpper  -> makeUpper
                  | hasLower  -> makeLower
                  | hasNull   -> makeNullCase
                  | otherwise -> id

    collectedErrors = concat [bothCaseError, multiNullError, verrs, merrs]
    -- Errors
    bothCaseError = if (hasUpper && hasLower) 
      then ["Cannot declare a pattern to be both upper and lower case."] 
      else []
    multiNullError = if (hasNull && (hasUpper || hasLower))
      then ["Cannot declare a pattern to be both direct and upper/lower case."] 
      else []

-- Some of these need better error messages.
validateCheckState :: M.Map String (Maybe (S.Set String)) -> CheckState -> Either String CheckStateX
validateCheckState sdict (CheckStateB str bl ) = case (M.lookup str sdict) of
  Nothing         -> Left $ "Couldn't find state type \"" <> str <> "\"."
  (Just Nothing)  -> Right (CheckStateBB str bl)
  (Just (Just _)) -> Right (CheckStateVB str bl)
validateCheckState sdict (CheckStateV str val) = case (M.lookup str sdict) of
  Nothing              -> Left $ "Couldn't find state type \"" <> str <> "\"."
  (Just Nothing)       -> Left $ "Tried to check boolean state type \"" <> str <> "\" with value \"" <> val <> "\"."
  (Just (Just theSet)) -> if (val `S.member` theSet)
    then Right (CheckStateVV str val)
    else Left ("Tried to check value-state type \"" <> str <> "\" with unknown value \"" <> val <> "\".")

validateModifyState :: M.Map String (Maybe (S.Set String)) -> ModifyState -> Either String ModifyStateX
validateModifyState sdict (ModifyStateB str bl ) = case (M.lookup str sdict) of
  Nothing         -> Left $ "Couldn't find state type \"" <> str <> "\"."
  (Just Nothing)  -> Right (ModifyStateBB str bl)
  (Just (Just _)) -> if bl
    then Left ("Can't set value-state value \"" <> str <> "\" to true/on; can only set it to a specific value or false/off")
    else Right (ModifyStateVX str)
validateModifyState sdict (ModifyStateV str val) = case (M.lookup str sdict) of
  Nothing              -> Left $ "Couldn't find state type \"" <> str <> "\"."
  (Just Nothing)       -> Left $ "Tried to set boolean state type \"" <> str <> "\" to value \"" <> val <> "\"."
  (Just (Just theSet)) -> if (val `S.member` theSet)
    then Right (ModifyStateVV str val)
    else Left ("Tried to set value-state type \"" <> str <> "\" to unknown value \"" <> val <> "\".")


