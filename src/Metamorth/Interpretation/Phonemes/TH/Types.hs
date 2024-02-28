module Metamorth.Interpretation.Phonemes.TH.Types
  ( PhonemeReferences(..)
  , GroupReferences(..)
  ) where

-- Most of the more ad-hoc types are declared in
-- Metamorth.Interpretation.Phonemes.TH, since
-- this module was created later on.

import Data.Map.Strict qualified as M

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

-- | All the references needed from a Phoneme.
--   This is the type to be used once all code
--   has been generated and you just need the
--   references to the names of the functions
--   and types that have been generated.
--
--   For examples below, imagine the following
--   data declaration:
--  
--   > data Phoneme = PhVow Vowel | PhCons Consonant deriving (Show, Eq, Ord)
--   >
--   > data Vowel = VFront Front | VCentre Centre | VBack Back deriving (Show, Eq, Ord)
--   > 
--   > data Front = I | E | A deriving (Show, Eq, Ord)
--   >
--   > data Centre = Schwa | Ih deriving (Show, Eq, Ord)
--   >
--   > data Back = U | O | Aw deriving (Show, Eq, Ord)
--   >
--   > data Consonant = CLab Labial | CAlv Alveolar | CPal Palatal | CVel Velar deriving (Show, Eq, Ord)
--   >
--   > data Labial = M | P | B | F | V deriving (Show, Eq, Ord)
--   > 
--   > data Alveolar = T | D | S | Z | R | L deriving (Show, Eq, Ord)
--   > 
--   > data Palatal = Sh | Ch | Zh | J | Y deriving (Show, Eq, Ord)
--   >
--   > data Velar = K | G | H deriving (Show, Eq, Ord)
data PhonemeReferences = PhonemeReferences
  -- | The `Name` of the (final) constructor of the phoneme. e.g. for "Schwa"...
  --   > prPhonemeName = "Schwa"
  { prPhonemeName :: Name
  -- | The number of arguments this phoneme's constructor takes.
  , prPhonemeArgs :: Int
  -- | The `Name` of the type of this phoneme. e.g. for "Schwa"...
  --   > prPhonemeType = "Centre"
  , prPhonemeType :: Name
  -- | All the necessary constructor `Name`s and `Name`s of types 
  --   leading up to the final constructor. e.g. for "Schwa"...
  --   > prPhonemeConstructors = [("PhVow","Phoneme"), (VCentre, Vowel)]
  , prPhonemeConstructors :: [(Name, Name)]
  -- | The `Name` of the pattern leading from the top-level 
  --   phoneme type to this phoneme. e.g.
  --   > pattern Ch_Phoneme = Phoneme (Consonant (Palatal Ch))
  , prPhonemeTopPattern :: Name
  -- | A `M.Map` from group `String`s to patterns to this phoneme. e.g.
  --   > pattern D_Consonant = Consonant (Alveolar D)
  --   would be equivalent to entry ("consonant","D_Consonant").
  , prPhonemeOtherPatterns :: M.Map String Name
  -- | A `M.Map` from the `String`s of aspects to the `Name`s
  --   of aspects, along with a `M.Map` of the values they can
  --   take.
  , prPhonemeAspects :: M.Map String (Name, M.Map String Name)
  -- | A `M.Map` of the traits this Phoneme has, together with
  --   the value of it they have, if applicable.
  , prPhonemeTraits :: M.Map String (Name, Maybe (String, Name))
  } deriving (Show, Eq)

-- | Various references for the code generated
--   related to a group.
data GroupReferences = GroupReferences
  -- | The `Name` of the type of the group in question.
  { grGroupName :: Name
  -- | The parents of this group, listed from highest
  --   to nearest. 
  , grGroupParents :: [(String, Name)]
  -- | The constructors needed to create this group.
  , grGroupConstructors :: [Name]
  } deriving (Show, Eq)

