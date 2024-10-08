module Metamorth.Interpretation.Output.Types.Interact
  ( PhonemeNameInformation(..)
  ) where

-- Types to use for interactions between output
-- and the other parts of the parser generator.

import Language.Haskell.TH (Name, Exp)

import Data.Map.Strict qualified as M

-- | A portable variant of `Metamorth.Interpretation.Phonemes.TH.PhonemeDatabase`
--   from "Metamorth.Interpretation.Phonemes.TH".
data PhonemeNameInformation = PhonemeNameInformation
  { pniPhones  :: M.Map String (Name, [M.Map String Name])
  , pniAspects :: M.Map String (Name, (Name, M.Map String Name))
  , pniTraits  :: M.Map String (Name, Maybe (Name, M.Map String Name))
  , pniGroups  :: M.Map String Name -- :: Phoneme -> Bool
  , pniWordTypeNames  :: (Name, (Name, Name))
  , pniCaseExpr   :: Exp
  , pniPhoneType  :: Name
  , pniCanBeCased :: Bool
  } deriving (Show, Eq)


{-
data PropertyData = PropertyData
  -- | Table of aspects. Return value is
  --   @Map of Aspect String -> (Type Name, (Record Name, Map of Option String -> Type Name ))@
  { aspectTable  :: M.Map String (Name, (Name, M.Map String Name))
  , traitTable   :: M.Map String (Name, Maybe (Name, M.Map String Name))
  , traitData    :: Maybe TraitData
  , propertyDecs :: [Dec]
  } deriving (Show, Eq)

-- | Since traits aren't embedded in the types themselves,
--   information about traits is limited to a function of
--   type @Phoneme -> TraitInfo@, or something like that.
data TraitData = TraitData
  { traitInfoName  :: Name
  , traitTypeTable :: M.Map String (Name, Type)
  , traitDefName   :: Name
  } deriving (Show, Eq)

data PhonemeDatabase = PhonemeDatabase
  { pdbPropertyData   :: PropertyData
  -- | A `M.Map` from Strings of Phonemes to the
  --   `Name`s of their constructors.
  , pdbPhonemeInfo    :: M.Map String PhonemeInformation
  -- | The top-type of the Phonemes.
  , pdbTopPhonemeType :: Name
  -- | The `Name` of the "Word" type, along
  --   with its two constructors.
  , pdbWordTypeNames  :: (Name, (Name, Name))
  -- | Make an uncased expression an upper-case expression.
  , pdbMkMaj :: Exp -> Exp
  -- | Make an uncased expression a  lower-case expression.
  , pdbMkMin :: Exp -> Exp
  }

data PhonemeInformation = PhonemeInformation
  -- | The name of the top-level Pattern Synonym.
  { phiPatternName :: Name
  -- | A list of the aspect options of the Phoneme.
  --   This is necessary to be able to build a 
  --   constructor.
  --
  --   e.g. Consider the following specification:
  -- 
  --   @
  --   aspect length : short long
  --   aspect nasal  : plain nasalised
  --   ...
  --   ====
  --   ...
  --   * vowel
  --     a : length nasal
  --   ...
  --   @
  --   
  --   Then the argument list for phoneme 'a'
  --   would look something like...
  --
  --   @
  --   [ M.fromList 
  --      [ ("long" , mkName "Long" )
  --      , ("short", mkName "Short")
  --      ]
  --   , M.fromList
  --      [ ("nasalised", mkName "Nasalised")
  --      , ("plain"    , mkName "Plain"    )
  --      ]
  --   ]
  --   @
  --
  , phiArgumentOptions :: [M.Map String Name]
  } deriving (Show, Eq)

-}

