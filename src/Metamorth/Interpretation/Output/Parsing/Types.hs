module Metamorth.Interpretation.Output.Parsing.Types
  ( OutputParser
  , OutputParsingState(..)
  , runOnPhoneme
  , CharPatternRaw(..)
  , ImportProperty(..)
  , PhoneName(..)
  , PhonePatternRaw(..)
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS

import Data.Attoparsec.Text qualified as AT

import Data.Char (ord, chr, isSpace, isLower, isAlpha)
import Data.Text qualified as T
import Data.Text (Text)

import Data.Map.Strict qualified as M

import Data.Set qualified as S

import Metamorth.Helpers.Error
import Metamorth.Helpers.Error.RWS

import Metamorth.Interpretation.Output.Types

--------------------------------
-- Types for Parsing

-- | A `AT.Parser` wrapped in an `RWST`.
-- 
--   * The reader holds the name of the phoneme(s) being processed.
--   * The writer is used to collect error messages
--   * The state will hold stateful info.
type OutputParser a = RWST String [ParserMessage] OutputParsingState AT.Parser a

-- | The State type for `OutputParser`.
data OutputParsingState = OutputParsingState
  -- | The state dictionary. This is updated as
  --   the parser parses the state declarations.
  { opsStateDictionary :: M.Map String (Maybe (S.Set String))
  -- | The Group "Dictionary". This is supplied by
  --   the phoneme parser when the output files
  --   are run.
  , opsGroupDictionary  :: S.Set String
  -- | The full group dictionary supplied by
  --   the phoneme code.
  , opsGroupDictionary' :: S.Set String
  -- | The Trait Dictionary. This is supplied by
  --   the phoneme parser when the output files
  --   are run. The `S.Set` contains the possible
  --   values for the trait, if relevant.
  , opsTraitDictionary  :: M.Map String (Maybe (S.Set String))
  -- | The full trait dictionary supplied by the driver.
  , opsTraitDictionary' :: M.Map String (Maybe (S.Set String))
  -- | The Aspect Dictionary. This is supplied by
  --   the phoneme parser when the output files
  --   are run. The `S.Set` contains the possible
  --   values for the trait.
  , opsAspectDictionary  :: M.Map String (S.Set String)
  , opsAspectDictionary' :: M.Map String (S.Set String)
  } deriving (Show, Eq)

-- | Use this when running a function that might
--   error with a phoneme name. e.g.
--   
--   > parsePhonePat = do
--   >   phoneName <- getPhoneName
--   >   rslt <- runOnPhoneme phoneName $ checkForStuff
runOnPhoneme :: String -> OutputParser a -> OutputParser a
runOnPhoneme str = local (const str)

-- | Data type for declarations of the form
--   `import (aspect | group | trait) prop_name`.
data ImportProperty
  = ImportAspect String
  | ImportGroup  String
  | ImportTrait  String
  deriving (Show, Eq, Ord)

-- | The parsed values on the left-hand side
--   of a phoneme pattern.
data PhonePatternRaw
  = PhoneNameR PhoneName
  | PhoneAtStartR
  | PhoneNotStartR
  | PhoneAtEndR
  | PhoneNotEndR
  | PhoneFollowedBy String
  | PhoneStateVal String String
  deriving (Show, Eq)


-- | A constructor for how phoneme names
--   are written in phoneme patterns.
-- 
--   Taken from the Parser Types.
data PhoneName = PhoneName 
  { pnName :: String
  , pnCons :: [String] 
  } deriving (Show, Eq, Ord)

-- | The Raw data for a single "Character"
--   on the RHS of a pattern.
data CharPatternRaw
  = PlainCharR Char   -- ^ A single `Char`.
  -- | CharClassR String -- ^ Any member of a class from the header.
  -- | WordStartR        -- ^ The start of a word.
  -- | WordEndR          -- ^ The end of a word.
  -- | NotStartR         -- ^ NOT the start of a word.
  -- | NotEndR           -- ^ NOT the end of a word.
  | ValStateR String (Either String Bool) -- ^ Check that the state is a certain value.
  | SetStateR String (Either String Bool) -- ^ Set the state to a certain value.
  deriving (Show, Eq, Ord)


