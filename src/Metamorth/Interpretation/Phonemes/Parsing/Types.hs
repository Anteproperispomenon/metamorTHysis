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

  ) where

import Metamorth.Interpretation.Phonemes.Types

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

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




