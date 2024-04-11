{-|
Module      : Metamorth.Interpretation.Output.TH.Types
Description : Various Output TH Helper Types
Copyright   : (c) David Wilson, 2024
License     : BSD-3

This module contains types that are used to
lookup `Name` information from Strings etc...

-}


module Metamorth.Interpretation.Output.TH.Types
  -- * Main Database Type
  ( OutputNameDatabase(..)
  -- ** Helper functions/Pseudo-Record Accessors
  , ondPhonemeNames
  , ondPhonemeConsts
  , ondAspectChecks
  , ondAspectConsts
  , ondValTraits
  , ondBoolTraits
  ) where

import Language.Haskell.TH

import Metamorth.Interpretation.Output.Types
import Metamorth.Interpretation.Output.Types.Alt


import Data.Trie.Map qualified as TM

import Data.Map.Strict qualified as M

-- | The main type used to get information about the 
--   various `Name`s used in the `Q` monad etc... 
data OutputNameDatabase = OutputNameDatabase
  -- | Information about the Phonemes for this
  --   module. Contains both the `Names` and the
  --   constructor lists. If you only need one
  --   or the other, see `ondPhonemeNames` and
  --   `ondPhonemeConsts`.
  { ondPhonemes :: M.Map String (Name, [M.Map String Name])
  -- | A `M.Map` from `String` names of aspects
  --   to `Name`s of functions that check whether
  --   a phoneme has that aspect, as well as what
  --   value it has. It also gives a `M.Map` for
  --   each aspect from `String` names to actual
  --   constructor `Name`s. If you only need one
  --   or the other, see `ondAspectChecks` and
  --   `ondAspectConsts`.
  , ondAspects :: M.Map String (Name, M.Map String Name)
  -- | A `M.Map` from `String` names of traits to
  --   `Name`s of functions that check whether a
  --   certain phoneme has that trait, and which
  --   member of a trait it has, if applicable.
  --   It also has a `M.Map` of constructors, if
  --   applicable. If you only want val-traits
  --   or bool-traits, see `ondValTraits` and
  --   `ondBoolTraits`.
  , ondTraits  :: M.Map String (Name, Maybe (M.Map String Name))
  -- | A simple `M.Map` from `String` names to
  --   `Names of functions that check whether a
  --   phoneme is a member of a certain group.
  , ondGroups  :: M.Map String Name
  -- | The `Name` of the type used to hold
  --   state throughout the computation.
  , ondStateType :: Name
  -- | A `M.Map` from state names to the acutal
  --   record constructors `Name`s for the 
  --   state in question. For value-states,
  --   it also includes the `Name` of the state's
  --   data type, along with a `M.Map` from
  --   state `String`s to constructor `Name`s.
  , ondStates  :: M.Map String (Name, Maybe (Name, M.Map String Name))
  } deriving (Show, Eq)

--------------------------------
-- Pseudo-Accessors

ondPhonemeNames :: OutputNameDatabase -> M.Map String Name
ondPhonemeNames = fmap fst . ondPhonemes

ondPhonemeConsts :: OutputNameDatabase -> M.Map String [M.Map String Name]
ondPhonemeConsts = fmap snd . ondPhonemes

ondAspectChecks :: OutputNameDatabase -> M.Map String Name
ondAspectChecks = fmap fst . ondAspects

ondAspectConsts :: OutputNameDatabase -> M.Map String (M.Map String Name)
ondAspectConsts = fmap snd . ondAspects

ondValTraits :: OutputNameDatabase -> M.Map String (Name, M.Map String Name)
ondValTraits = M.mapMaybe tupMaybe2of2 . ondTraits

ondBoolTraits :: OutputNameDatabase -> M.Map String Name
ondBoolTraits = M.mapMaybe tupOnlyNothing2of2 . ondTraits

--------------------------------
-- Helper Functions

tupMaybe2of2 :: (a, Maybe b) -> Maybe (a,b)
tupMaybe2of2 (_, Nothing) = Nothing
tupMaybe2of2 (x, Just y)  = Just (x,y)

tupOnlyNothing2of2 :: (a, Maybe b) -> Maybe a
tupOnlyNothing2of2 (x, Nothing) = Just x
tupOnlyNothing2of2 _ = Nothing
