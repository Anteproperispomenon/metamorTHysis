{-|
Module      : Metamorth.Interpretation.Phonemes.Parsing.Types 
Description : Types specifically to assist parsing.
Copyright   : (c) David Wilson, 2024
License     : BSD-3

This module contains types that are only used
while parsing the Phoneme specification. This
is as opposed to `Metamorth.Interpretation.Phonemes.Types`,
whose types are meant to be used to be kept
around for when the code is converted into
Template Haskell.

-}

module Metamorth.Interpretation.Phonemes.Parsing.Types 
  ( PhonemeParsingState(..)
  , Property(..)
  , PhonemeParser
  , runPhonemeParser
  , execPhonemeParser
  , modifyStructure
  ) where

import Metamorth.Helpers.Parsing
import Metamorth.Interpretation.Phonemes.Types

import Data.List (intercalate)

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

import Data.Attoparsec.Text qualified as AT

import Data.Text qualified as T

import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS

-- | Type to be used when parsing, to show
--   what type of property was parsed.
data Property
  = Aspect String [String]
  | Trait  String [String]
  deriving (Show, Eq)

data PhonemeParsingState
   = PhonemeParsingState
      { psStructure     :: PhonemeParsingStructure
      , psUsedGroups    :: S.Set String
      , psUsedPhones    :: S.Set String
      , psDepth         :: Int
      } deriving (Show, Eq)

defaultParsingState :: PhonemeParsingState
defaultParsingState
  = PhonemeParsingState
      defaultPhonemeStructure
      S.empty
      S.empty
      0

--------------------------------
-- Types for Parsing

-- | A `AT.Parser` wrapped in an `RWST`.
-- 
--   * The writer is used to collect error messages
--   * The state holds all the important info.
type PhonemeParser a = RWST () [String] PhonemeParsingState AT.Parser a

-- | Run a `PhonemeParser`, returning the result if
--   no errors occurred, and failing with a list of
--   errors if any occurred.
runPhonemeParser :: PhonemeParser a -> T.Text -> Either String a
runPhonemeParser prs txt = forParseOnly txt $ do
  (rslt, _stt, errs) <- runRWST prs () defaultParsingState
  case errs of
    [] -> return rslt
    xs -> fail $ intercalate "\n" xs

execPhonemeParser :: PhonemeParser a -> T.Text -> Either String PhonemeParsingStructure
execPhonemeParser prs txt = forParseOnly txt $ do
  (_rslt, stt, errs) <- runRWST prs () defaultParsingState
  case errs of
    [] -> return (psStructure stt)
    xs -> fail $ intercalate "\n" xs

-- Consonants that take 'an' when
-- used in an (English) acronym:
-- FHLMNRSX

modifyStructure :: (PhonemeParsingStructure -> Either String PhonemeParsingStructure) -> PhonemeParser ()
modifyStructure act = do
  rslt <- gets (act . psStructure)
  case rslt of
    (Left msg) -> tell [msg]
    (Right ps) -> modify (\pss -> pss {psStructure = ps})

modifyStructure' :: (PhonemeParsingStructure -> Either [String] PhonemeParsingStructure) -> PhonemeParser ()
modifyStructure' act = do
  rslt <- gets (act . psStructure)
  case rslt of
    (Left msgs) -> tell msgs
    (Right  ps) -> modify (\pss -> pss {psStructure = ps})

