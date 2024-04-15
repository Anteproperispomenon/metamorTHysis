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

