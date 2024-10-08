module Metamorth.Interpretation.Parser.Parsing.Types
  -- * Parser Types
  ( ParserParser
  , ParserParsingState(..)
  , ParserMessage(..)
  , mkError
  , mkErrors
  , tellError
  , tellErrors
  , warn
  , warns
  , message
  , messages
  , partitionMessages
  , addPhonemePattern
  , addPhonemesPattern
  , PhoneName(..)
  , PhoneResult(..)
  , eqOnPN
  , sameArgsPN
  , runOnPhoneme
  , runParserParser
  , execParserParser
  , embedParserParser
  , embedParserParserNew
  ) where

import Data.Function
import Data.List (intercalate)

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty(..))

import Data.Attoparsec.Text qualified as AT

import Data.Text qualified as T

import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS

import Metamorth.Helpers.Parsing
import Metamorth.Interpretation.Parser.Types

import Metamorth.Helpers.Error
import Metamorth.Helpers.Error.RWS

-- Based heavily on Metamorth.Interpretation.Phonemes.Parsing.Types 

--------------------------------
-- Types for Parsing

-- | A `AT.Parser` wrapped in an `RWST`.
-- 
--   * The reader holds the name of the phoneme being processed.
--   * The writer is used to collect error messages
--   * The state will hold stateful info.
type ParserParser a = RWST String [ParserMessage] ParserParsingState AT.Parser a

-- | Use this when running a function that might
--   error with a phoneme name. e.g.
--   
--   > parsePhonePat = do
--   >   phoneName <- getPhoneName
--   >   rslt <- runOnPhoneme phoneName $ checkForStuff
runOnPhoneme :: String -> ParserParser a -> ParserParser a
runOnPhoneme str = local (const str)

-- | Add a new phoneme pattern for a specific `PhoneName`.
--   Multiple `PhonemePattern`s are allowed per `PhoneName`.
addPhonemePattern :: PhoneName -> PhonemePattern -> ParserParser ()
addPhonemePattern phone pat = do
  theMap <- gets ppsPhonemePatterns
  let stMods = stateModsPP pat
      newMap = M.insertWith (flip (++)) (PhoneResult (NE.singleton phone) stMods) [pat] theMap
  modify $ \st -> st {ppsPhonemePatterns = newMap}

-- | Add a new phoneme pattern for a specific `PhoneName`.
--   Multiple `PhonemePattern`s are allowed per `PhoneName`.
addPhonemesPattern :: NonEmpty PhoneName -> PhonemePattern -> ParserParser ()
addPhonemesPattern phones pat = do
  theMap <- gets ppsPhonemePatterns
  let stMods = stateModsPP pat
      newMap = M.insertWith (flip (++)) (PhoneResult phones stMods) [pat] theMap
  modify $ \st -> st {ppsPhonemePatterns = newMap}


data ParserParsingState = ParserParsingState
  { ppsClassDictionary   :: M.Map String (S.Set Char)
  , ppsStateDictionary   :: M.Map String (Bool, Maybe (S.Set String))
  , ppsPhonemePatterns   :: M.Map PhoneResult [PhonemePattern]
  , ppsGroupDictionary   :: S.Set String
  , ppsGroupDictionary'  :: S.Set String
  , ppsTraitDictionary   :: M.Map String (Maybe (S.Set String))
  , ppsTraitDictionary'  :: M.Map String (Maybe (S.Set String))
  , ppsAspectDictionary  :: M.Map String (S.Set String)
  , ppsAspectDictionary' :: M.Map String (S.Set String)
  , ppsPhoneDictionary   :: S.Set String
  } deriving (Show, Eq)

defParseState :: ParserParsingState
defParseState = ParserParsingState
  { ppsClassDictionary   = M.empty
  , ppsStateDictionary   = M.empty
  , ppsPhonemePatterns   = M.empty
  , ppsGroupDictionary   = S.empty
  , ppsGroupDictionary'  = S.empty
  , ppsTraitDictionary   = M.empty
  , ppsTraitDictionary'  = M.empty
  , ppsAspectDictionary  = M.empty
  , ppsAspectDictionary' = M.empty
  , ppsPhoneDictionary   = S.empty
  }

-- | The final result that can be found at a leaf.
--   Contains the phoneme(s) and the state modifications.
data PhoneResult = PhoneResult
  { prPhonemes  :: NonEmpty PhoneName
  , prStateMods :: [ModifyStateX]
  } deriving (Show, Eq, Ord)


-- | Run a `ParserParser`, returning the result if
--   no errors occurred, and failing with a list of
--   errors if any occurred.
runParserParser :: ParserParser a -> T.Text -> Either String a
runParserParser prs txt = forParseOnly txt $ do
  (rslt, _stt, msgs) <- runRWST prs "N/A" defParseState
  let (errs, wrns, nts) = partitionMessages msgs
  case errs of
    [] -> return rslt
    xs -> fail $ intercalate "\n" xs

execParserParser :: ParserParser a -> T.Text -> Either String ParserParsingState
execParserParser prs txt = forParseOnly txt $ do
  (_rslt, stt, msgs) <- runRWST prs "N/A" defParseState
  let (errs, wrns, nts) = partitionMessages msgs
  case errs of
    [] -> return stt
    xs -> fail $ intercalate "\n" xs

embedParserParser :: ParserParser a -> AT.Parser (a, ParserParsingState, [ParserMessage])
embedParserParser prs = runRWST prs "N/A" defParseState

embedParserParserNew   
  -- | The `S.Set` of group names.
  :: S.Set String 
  -- | A `M.Map` from trait names to the values
  --   the trait can take (if applicable).
  -> M.Map String (Maybe (S.Set String)) 
  -- | A `M.Map` from aspect names to the values
  --   the aspect can take.
  -> M.Map String (S.Set String)
  -- | A `S.Set` of the phonemes used for this
  --   output.
  -> S.Set String
  -> ParserParser a 
  -> AT.Parser (a, ParserParsingState, [ParserMessage])
embedParserParserNew grps trts asps phones prs = 
  runRWST prs "N/A" 
    (defParseState
      { ppsGroupDictionary'  = grps
      , ppsTraitDictionary'  = trts
      , ppsAspectDictionary' = asps
      , ppsPhoneDictionary   = phones
      }
    )


-- | A constructor for how phoneme names
--   are written in phoneme patterns.
data PhoneName = PhoneName 
  { pnName :: String
  , pnCons :: [String] 
  } deriving (Show, Eq, Ord)

-- | Meant to be used with `Data.List.groupBy`.
eqOnPN :: PhoneName -> PhoneName -> Bool
eqOnPN = (==) `on` pnName

-- | Check that a list of `PhoneName`s all have
--   the same number of arguments.
sameArgsPN :: [PhoneName] -> Bool
sameArgsPN []  = True
sameArgsPN [_] = True
sameArgsPN ((PhoneName _ args):xs) = all (\(PhoneName _ args') -> len == (length args')) xs
  where len = length args

