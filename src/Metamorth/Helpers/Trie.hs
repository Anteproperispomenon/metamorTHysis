{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Metamorth.Helpers.Trie
Description : Helpers for Tries
Copyright   : (c) David Wilson, 2024
License     : BSD-3

-}

module Metamorth.Helpers.Trie
  ( TrieAnnotation(..)
  , annotateTrie
  ) where

import Data.Trie.Map qualified as TM
import Data.Trie.Set qualified as TS

import Metamorth.Helpers.List

import Data.List

-- | A way to annotate all the "branches" of a Trie TM.Map.
newtype TrieAnnotation = TrieAnn Int
  deriving stock (Ord, Eq)

instance Show TrieAnnotation where
  show (TrieAnn x) = "TrieBranch_" ++ show x

infTrieAnns :: [TrieAnnotation]
infTrieAnns = map TrieAnn [1..]

annotateTrie :: forall c a. (Ord c) => TM.TMap c a -> TM.TMap c (TrieAnnotation, Maybe a)
annotateTrie initialMap = finalTrie
  where
    keyStrings :: TS.TSet c
    keyStrings = {-TS.delete [[]] $-} TS.prefixes $ TM.keysTSet initialMap
    -- hmm...
    stringPairs = zip (TS.toList keyStrings) infTrieAnns
    newTrie1a = TM.fromList stringPairs
    newTrie1b = fmap (\x -> (x,Nothing)) newTrie1a
    newTrie2a = fmap (\x -> (TrieAnn 0, Just x)) initialMap
    finalTrie = TM.unionWith (\(idx, _) (_, val) -> (idx, val)) newTrie1b newTrie2a


-- TM.revise :: Ord c => (Maybe a -> a) -> [c] -> TMap c a -> TMap c a
-- Tm.revise (\myb -> )

