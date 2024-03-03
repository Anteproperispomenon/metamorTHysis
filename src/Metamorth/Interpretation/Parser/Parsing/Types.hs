module Metamorth.Interpretation.Parser.Parsing.Types
  ( ParserParser
  , ParserParsingState(..)
  , addPhonemePattern
  , PhoneName(..)
  , eqOnPN
  , sameArgsPN
  , runOnPhoneme
  , runParserParser
  , execParserParser
  , embedParserParser
  ) where

import Data.Function
import Data.List (intercalate)

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

import Data.Attoparsec.Text qualified as AT

import Data.Text qualified as T

import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS

import Metamorth.Helpers.Parsing
import Metamorth.Interpretation.Parser.Types

-- Based heavily on Metamorth.Interpretation.Phonemes.Parsing.Types 

--------------------------------
-- Types for Parsing

-- | A `AT.Parser` wrapped in an `RWST`.
-- 
--   * The reader holds the name of the phoneme being processed.
--   * The writer is used to collect error messages
--   * The state will hold stateful info.
type ParserParser a = RWST String [String] ParserParsingState AT.Parser a

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
  let newMap = M.insertWith (flip (++)) phone [pat] theMap
  modify $ \st -> st {ppsPhonemePatterns = newMap}

data ParserParsingState = ParserParsingState
  { ppsClassDictionary :: M.Map String (S.Set Char)
  , ppsPhonemePatterns :: M.Map PhoneName [PhonemePattern]
  } deriving (Show, Eq)

defParseState :: ParserParsingState
defParseState = ParserParsingState
  { ppsClassDictionary = M.empty
  , ppsPhonemePatterns = M.empty
  }

-- | Run a `ParserParser`, returning the result if
--   no errors occurred, and failing with a list of
--   errors if any occurred.
runParserParser :: ParserParser a -> T.Text -> Either String a
runParserParser prs txt = forParseOnly txt $ do
  (rslt, _stt, errs) <- runRWST prs "N/A" defParseState
  case errs of
    [] -> return rslt
    xs -> fail $ intercalate "\n" xs

execParserParser :: ParserParser a -> T.Text -> Either String ParserParsingState
execParserParser prs txt = forParseOnly txt $ do
  (_rslt, stt, errs) <- runRWST prs "N/A" defParseState
  case errs of
    [] -> return stt
    xs -> fail $ intercalate "\n" xs

embedParserParser :: ParserParser a -> AT.Parser (a, ParserParsingState, [String])
embedParserParser prs = runRWST prs "N/A" defParseState

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

