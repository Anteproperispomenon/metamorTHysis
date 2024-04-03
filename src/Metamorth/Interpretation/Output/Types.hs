{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Metamorth.Interpretation.Output.Types
  -- * Various types
  ( OutputPattern(..)
  , OutputCase(..)
  , CaseSource(..)
  , CaseApply(..)
  , PhoneName(..)
  , PhonePattern
  , PhonePatternF(PhonemeName, PhoneAtStart, PhoneNotStart, PhoneAtEnd, PhoneNotEnd, PhoneFollow)
  , PhoneFollow(..)
  , CharPatternItem(..)
  , CharPattern(..)
  -- * State Operations
  , CheckState(..)
  , CheckStateX(..)
  , validateCheckState
  , ModifyState(..)
  , ModifyStateX(..)
  , validateModifyState
  ) where

import Data.Ord (Down(..))

import Data.Map.Strict qualified as M

import Data.Set qualified as S

import Metamorth.Helpers.Ord

data OutputPattern = OutputPattern
  { opPhonePattern :: [PhonePattern]
  , opCharPattern  :: CharPattern
  , opCasedness    :: OutputCase
  } deriving (Show, Eq)

-- | The case of an output pattern.
--   **NOTE**: this does NOT mean that
--   the pattern produced will be a
--   certain case, but rather that the
--   matched pattern only accepts upper-case
--   etc... patterns.
data OutputCase
  -- | Upper-Case Pattern only.
  = OCMaj
  -- | Lower-Case Pattern only.
  | OCMin
  -- | Uncasable Pattern. In this case, both cases
  --   will use the same value.
  | OCNull
  -- | Automatically create cases for this pattern.
  --   The parameters tell you how to transform
  --   the input case to the output case. 
  | OCDetect CaseSource CaseApply
  | OCDetectIndividual
  deriving (Show, Eq)

-- | Which phoneme of the input list to
--   use to determine the case of the
--   output.
data CaseSource
   -- | Use the case of the first casable character.
   = CSFirst
   -- | Use the case of the last casable character
   | CSLast
   -- | Use lower-case, unless all characters are
   --   upper-case
   | CSLow
   -- | Use upper-case, unless all characters are
   --   lower-case.
   | CSHigh
   deriving (Show, Eq)

-- | Which output character(s) to apply the
--   case to.
data CaseApply
   -- | Use Title case if the input is capitalised.
   = CATitle
   -- | Apply case to all characters.
   | CAAll
   deriving (Show, Eq)

-- See `Metamorth.Interpretation.Parser.Types.CharPatternF`
-- for more info on this.
-- | The pattern of Phonemes on the left-hand-side
--   of a pattern.
type PhonePattern = PhonePatternF [CheckStateX] [PhoneFollow]
{-# COMPLETE PhonemeName, PhoneAtStart, PhoneNotStart, PhoneAtEnd, PhoneNotEnd, PhoneFollow :: PhonePattern #-}

pattern PhonemeName :: [CheckStateX] -> PhoneName -> PhonePattern
pattern PhonemeName st nom = PhonemeNameX st nom
pattern PhoneAtStart :: PhonePattern
pattern PhoneAtStart = PhoneAtStartX
pattern PhoneNotStart :: PhonePattern
pattern PhoneNotStart = PhoneNotStartX
pattern PhoneAtEnd :: PhonePattern
pattern PhoneAtEnd = PhoneAtEndX
pattern PhoneNotEnd :: PhonePattern
pattern PhoneNotEnd = PhoneNotEndX
pattern PhoneFollow :: [PhoneFollow] -> PhonePattern
pattern PhoneFollow fols = PhoneFollowX fols

-- | The pattern of Phonemes on the left-hand-side
--   of a pattern.
data PhonePatternF b c
  = PhonemeNameX b PhoneName
  | PhoneAtStartX
  | PhoneNotStartX
  | PhoneAtEndX
  | PhoneNotEndX
  | PhoneFollowX c
  -- PhoneValStateR String (Either String Bool)
  deriving (Show, Eq)

deriving instance {-# OVERLAPPING #-} (Ord b, Ord c) => Ord (PhonePatternF (Down b) (Down c))

deriving via (PhonePatternF (Down b) (Down c)) instance {-# OVERLAPPABLE #-} (Ord b, Ord c) => Ord (PhonePatternF b c) 

-- We want this instance to be chosen over plain @b@, but
-- *not* over @Down b@.
deriving via (PhonePatternF (Down (SizeOrdList b)) (Down (SizeOrdList c))) 
  instance {-# OVERLAPS #-} (Ord b, Ord c) => Ord (PhonePatternF [b] [c])

data PhoneFollow
  = PhoneFollowedByGroup String
  | PhoneFollowedByTrait String
  | PhoneFollowedByTraitAt String String
  | PhoneFollowedByAspect String
  | PhoneFollowedByAspectAt String String
  | PhoneFollowedByPhone String
  deriving (Show, Eq, Ord)

-- | A constructor for how phoneme names
--   are written in phoneme patterns.
-- 
--   Taken from the Parser Types.
data PhoneName = PhoneName 
  { pnName :: String
  , pnCons :: [String] 
  } deriving (Show, Eq, Ord)

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

-- | The information for a `Char` pattern.
data CharPatternItem
  = CasableChar   Char -- ^ A single `Char`.
  | UncasableChar Char -- ^ A single uncasable `Char`.
  deriving (Show, Eq, Ord)

data CharPattern = CharPattern
  { cpPatterns     :: [CharPatternItem]
  , cpStateChanges :: [ModifyStateX]
  } deriving (Show, Eq)

-- SetStateR String (Either String Bool) -- ^ Set the state to a certain value.


