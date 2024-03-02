module Metamorth.Interpretation.Parser.Parsing.Types
  ( ParserParser
  , runParserParser
  , execParserParser
  ) where

import Data.List (intercalate)

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

import Data.Attoparsec.Text qualified as AT

import Data.Text qualified as T

import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS

import Metamorth.Helpers.Parsing

-- Based heavily on Metamorth.Interpretation.Phonemes.Parsing.Types 

--------------------------------
-- Types for Parsing

-- | A `AT.Parser` wrapped in an `RWST`.
-- 
--   * The writer is used to collect error messages
--   * The state will hold stateful info.
type ParserParser a = RWST () [String] ParserParsingState AT.Parser a

data ParserParsingState = ParserParsingState
  { ppsClassDictionary :: M.Map String (S.Set Char)
  } deriving (Show, Eq)

defParseState :: ParserParsingState
defParseState = ParserParsingState
  { ppsClassDictionary = M.empty
  }

-- | Run a `ParserParser`, returning the result if
--   no errors occurred, and failing with a list of
--   errors if any occurred.
runParserParser :: ParserParser a -> T.Text -> Either String a
runParserParser prs txt = forParseOnly txt $ do
  (rslt, _stt, errs) <- runRWST prs () defParseState
  case errs of
    [] -> return rslt
    xs -> fail $ intercalate "\n" xs

execParserParser :: ParserParser a -> T.Text -> Either String ParserParsingState
execParserParser prs txt = forParseOnly txt $ do
  (_rslt, stt, errs) <- runRWST prs () defParseState
  case errs of
    [] -> return stt
    xs -> fail $ intercalate "\n" xs


