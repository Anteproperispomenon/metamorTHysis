{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Metamorth.Interpretation.Output.Types.Alt
  -- * Most Important Type
  ( OutputParserOutput(..)
  -- * Other Types
  , PhonePatternAlt(..)
  , PhoneResultActionX(..)
  , PhoneResult
  -- , PhoneResultText
  , PhoneResultX(..)
  -- * Other Helpers
  , renewOutputPattern
  , convertPhonePattern
  , isConfirmState
  , isModifyState
  , isAtEnd
  , isNotEnd
  , isCheckNext
  , partConfirmStates
  , partModifyStates
  , partAtEnds
  , partNotEnds
  , partCheckNexts
  , addAutoStates
  , implementAutoStates
  ) where

-- Alternate forms of types.

-- import Data.Functor.Compose

-- import Control.Arrow ((***), first, second)

-- import Data.String (IsString(..))

-- import Data.Text qualified as T

import Data.List qualified as L

import Data.Trie.Map qualified as TM

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

import Data.Ord (Down(..))

import Metamorth.Helpers.List (partitionMap)
import Metamorth.Helpers.Ord

import Metamorth.Interpretation.Output.Types

-- | Alternate form of `PhonePatternX` that
--   instead puts state and follow data into
--   the value instead of the key.
data PhonePatternAlt
  = PhonemeNameZ PhoneName
  | PhoneAtStartZ
  | PhoneNotStartZ
  -- | PhoneAtEndZ
  -- | PhoneNotEndZ
  deriving (Show, Eq, Ord)

data PhoneResultActionX
  = PRConfirmState CheckStateX
  | PRModifyState  ModifyStateX
  | PRAtEnd
  | PRNotEnd
  | PRCheckNext PhoneFollow
  deriving (Show, Eq, Ord)

isConfirmState :: PhoneResultActionX -> Bool
isConfirmState (PRConfirmState _) = True
isConfirmState _ = False

isModifyState :: PhoneResultActionX -> Bool
isModifyState (PRModifyState _) = True
isModifyState _ = False

isAtEnd :: PhoneResultActionX -> Bool
isAtEnd PRAtEnd = True
isAtEnd _ = False

isNotEnd :: PhoneResultActionX -> Bool
isNotEnd PRNotEnd = True
isNotEnd _ = False

isCheckNext :: PhoneResultActionX -> Bool
isCheckNext (PRCheckNext _) = True
isCheckNext _ = False

-- | Partition Confirm States.
partConfirmStates :: [PhoneResultActionX] -> ([CheckStateX], [PhoneResultActionX])
partConfirmStates = partitionMap $ \case
  (PRConfirmState cs) -> Just cs
  _ -> Nothing

-- | Partition Modify States
partModifyStates :: [PhoneResultActionX] -> ([ModifyStateX], [PhoneResultActionX])
partModifyStates = partitionMap $ \case
  (PRModifyState cs) -> Just cs
  _ -> Nothing

-- | Partition 'At-Ends'
partAtEnds :: [PhoneResultActionX] -> ([()], [PhoneResultActionX])
partAtEnds = partitionMap $ \case
  PRAtEnd -> Just ()
  _ -> Nothing

-- | Partition 'Not-Ends'
partNotEnds :: [PhoneResultActionX] -> ([()], [PhoneResultActionX])
partNotEnds = partitionMap $ \case
  PRNotEnd -> Just ()
  _ -> Nothing

partCheckNexts :: [PhoneResultActionX] -> ([PhoneFollow], [PhoneResultActionX])
partCheckNexts = partitionMap $ \case
  (PRCheckNext cs) -> Just cs
  _ -> Nothing

type PhoneResult = PhoneResultX [PhoneResultActionX]
-- type PhoneResult str = PhoneResultX str    [PhoneResultActionX]
-- type PhoneResultText = PhoneResultX T.Text [PhoneResultActionX]

data PhoneResultX a = PhoneResult
  { prPhoneConditions :: a
  , prPhoneOutput :: [CharPatternItem]
  , prOutputCase  :: OutputCase
  } deriving (Show, Eq)

deriving stock instance {-# OVERLAPPING #-} (Ord b) => Ord (PhoneResultX (Down b))

deriving via (PhoneResultX (Down b)) instance {-# OVERLAPPABLE #-} (Ord b) => Ord (PhoneResultX b)

-- Copied over from another module.
-- deriving via (PhoneResultX (Compose Down SizeOrdList b))
deriving via (PhoneResultX (Down (SizeOrdList b))) 
  instance {-# OVERLAPS #-} (Ord b) => Ord (PhoneResultX [b])

-- | Convert an old-style trie pair into the
--   new-style version.
renewOutputPattern :: ([PhonePattern], OutputPattern) -> ([PhonePatternAlt], PhoneResult)
renewOutputPattern (phPats, outPats) = (ppAlt, PhoneResult (L.sort (prActs ++ cMods')) cItems (opCasedness outPats))
  where 
    (ppAlt, prActs) = convertPhonePattern phPats
    cPats  = opCharPattern outPats
    cItems = cpPatterns cPats
    cMods  = cpStateChanges cPats
    cMods' = map PRModifyState cMods


{-
data OutputPattern = OutputPattern
  { opCharPattern  :: CharPattern
  , opCasedness    :: OutputCase
  } deriving (Show, Eq)

-- | The information for a `Char` pattern.
data CharPatternItem
  = CasableChar   Char -- ^ A single `Char`.
  | UncasableChar Char -- ^ A single uncasable `Char`.
  deriving (Show, Eq, Ord)

data CharPattern = CharPattern
  { cpPatterns     :: [CharPatternItem]
  , cpStateChanges :: [ModifyStateX]
  } deriving (Show, Eq)
-}



convertPhonePattern :: [PhonePattern] -> ([PhonePatternAlt], [PhoneResultActionX])
convertPhonePattern [] = ([],[])
-- Temp test?
-- convertPhonePattern ((PhonemeName _ nom):rst) = consFst (PhonemeNameZ nom) $ convertPhonePattern rst
-- convertPhonePattern (PhoneAtStart :rst) = consFst PhoneAtStartZ  $ convertPhonePattern rst
-- convertPhonePattern (PhoneNotStart:rst) = consFst PhoneNotStartZ $ convertPhonePattern rst
-- convertPhonePattern (_:rst) = convertPhonePattern rst

convertPhonePattern ((PhonemeName [] nom):rst) = consFst (PhonemeNameZ nom) $ convertPhonePattern rst
convertPhonePattern ((PhonemeName st nom):rst) = prpBth [PhonemeNameZ nom] (map PRConfirmState st) $ convertPhonePattern rst
convertPhonePattern (PhoneAtStart :rst) = consFst PhoneAtStartZ  $ convertPhonePattern rst
convertPhonePattern (PhoneNotStart:rst) = consFst PhoneNotStartZ $ convertPhonePattern rst
convertPhonePattern (PhoneAtEnd :rst) = consSnd PRAtEnd  $ convertPhonePattern rst
convertPhonePattern (PhoneNotEnd:rst) = consSnd PRNotEnd $ convertPhonePattern rst
convertPhonePattern [PhoneFollow fs]  = ([],map PRCheckNext fs)
convertPhonePattern (PhoneFollow fs:rst) = prpSnd (map PRCheckNext fs) $ convertPhonePattern rst

consFst :: a -> ([a],[b]) -> ([a],[b])
consFst x (xs,ys) = (x:xs,ys)
consSnd :: b -> ([a],[b]) -> ([a],[b])
consSnd y (xs,ys) = (xs,y:ys)
consBth :: a -> b -> ([a],[b]) -> ([a],[b])
consBth x y (xs, ys) = (x:xs,y:ys)
prpSnd :: [b] -> ([a],[b]) -> ([a],[b])
prpSnd ys (xs, zs) = (xs,ys++zs)

prpBth :: [a] -> [b] -> ([a],[b]) -> ([a],[b])
prpBth x y (xs, ys) = (x++xs, y++ys)

{-
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

-}

-- | The Output from an `OutputParser`.
data OutputParserOutput = OutputParserOutput
  -- | The state dictionary. This is updated as
  --   the parser parses the state declarations.
  { opoStateDictionary :: M.Map String (Maybe (S.Set String))
  -- | The set of States that are "auto-off". 
  --   The `Bool` indicates whether the state is
  --   a value-state (True) or a bool-state (False).
  , opoAutoStates      :: M.Map String Bool
  -- | The Group "Dictionary". This is supplied by
  --   the phoneme parser when the output files
  --   are run.
  , opoGroupDictionary :: S.Set String
  -- | The Trait Dictionary. This is supplied by
  --   the phoneme parser when the output files
  --   are run. The `S.Set` contains the possible
  --   values for the trait, if relevant.
  , opoTraitDictionary :: M.Map String (Maybe (S.Set String))
  -- | The Aspect Dictionary. This is supplied by
  --   the phoneme parser when the output files
  --   are run. The `S.Set` contains the possible
  --   values for the trait.
  , opoAspectDictionary :: M.Map String (S.Set String)
  -- | The main trie to be used for determining
  --   output.
  , opoOutputTrie       :: TM.TMap PhonePatternAlt (S.Set PhoneResult)
  -- , opoOutputTrie       :: TM.TMap PhonePattern (M.Map OutputCase OutputPattern)
  } deriving (Show, Eq)

------------------------------------------------
-- Code for handling auto-off states

isModState :: String -> ModifyStateX -> Bool
isModState str (ModifyStateBB str' _) = str == str'
isModState str (ModifyStateVV str' _) = str == str'
isModState str (ModifyStateVX str'  ) = str == str'

isModState' :: String -> PhoneResultActionX -> Bool
isModState' str (PRModifyState ms) = isModState str ms
isModState' _ _ = False

hasModState :: String -> [PhoneResultActionX] -> Bool
hasModState str = any (isModState' str)

-- | Add a single auto-state to the list of actions.
addAutoState :: [PhoneResultActionX] -> String -> Bool -> [PhoneResultActionX]
addAutoState prs str typ
  -- Already modifies this state: do nothing
  | (hasModState str prs) = prs
  -- Otherwise, add "turn off state" to list of actions.
  | otherwise = newAction : prs
  where 
    newAction
      | typ       = PRModifyState $ ModifyStateVX str
      | otherwise = PRModifyState $ ModifyStateBB str False

addAutoState' :: [PhoneResultActionX] -> (String, Bool) -> [PhoneResultActionX]
addAutoState' prs = uncurry $ addAutoState prs

addAutoStates' :: M.Map String Bool -> [PhoneResultActionX] -> [PhoneResultActionX]
addAutoStates' stMap prs = foldl addAutoState' prs (M.assocs stMap)

-- | Add the auto-off states to the `PhoneResult`.
addAutoStates :: M.Map String Bool -> PhoneResult -> PhoneResult
addAutoStates stMap prOrig = prOrig { prPhoneConditions = prActsNew }
  where prActsNew = addAutoStates' stMap (prPhoneConditions prOrig)

-- | Use the data from `OutputParserOutput` to add auto-state
--   information to its branches.
implementAutoStates :: OutputParserOutput -> OutputParserOutput
implementAutoStates opo
  -- If there are no auto-states, do nothing
  | M.null autoSt = opo
  | otherwise = opo {opoOutputTrie = newTrie}
  where 
    autoSt = opoAutoStates opo
    newTrie = fmap (S.map $ addAutoStates autoSt) $ opoOutputTrie opo



