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
  , isLeafAnn
  , unifyPaths
  -- , annotateTrie
  -- , annotifyTrie
  -- , setifyTrie
  ) where

import Data.List (mapAccumL)
import Data.Maybe

import Data.Trie.Map qualified as TM
import Data.Trie.Set qualified as TS

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

import Metamorth.Helpers.List

import Data.List

-- | A way to annotate all the "branches" of a Trie TM.TMap.
data TrieAnnotation 
  = TrieLeaf    -- ^ Used for leafs of a `TM.TMap`.
  | TrieAnn Int -- ^ Used for all internal branches.
  deriving stock (Eq, Ord)

isLeafAnn :: TrieAnnotation -> Bool
isLeafAnn TrieLeaf = True
isLeafAnn _ = False

instance Show TrieAnnotation where
  show TrieLeaf = "TrieLeaf"
  show (TrieAnn x) = "TrieBranch_" ++ show x

infTrieAnns :: [TrieAnnotation]
infTrieAnns = map TrieAnn [1..]

-- | Annotate all branches of a `TM.TMap` so 
--   that any prefix in the Trie has an identifier
--   tied to it. 
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

-- | Invert `TM.TMap` by creating a `M.Map` from values
--   to lists of paths (in reverse) that lead to that
--   value.
-- invertTrie :: (Ord c, Ord a) => TM.TMap c a -> M.Map a [[c]]
-- invertTrie

-- | Get a list of the sub-tries of a trie.
getSubTries :: (Ord c) => TM.TMap c a -> [TM.TMap c a]
getSubTries tm
  = map (\x -> snd (TM.match x tm)) keys1c
  where
    keys1c = filter len1 $ TS.toList $ TS.prefixes $ TM.keysTSet tm

getSubTries' :: (Ord c) => TM.TMap c a -> [(c,(Maybe a, TM.TMap c a))]
getSubTries' tm
  = map (\x -> (x, (TM.match [x] tm))) keys1c
  where
    keys1c = mapMaybe len1' $ TS.toList $ TS.prefixes $ TM.keysTSet tm

len1 :: [a] -> Bool
len1 [_] = True
len1 _   = False

len1' :: [a] -> Maybe a
len1' [x] = (Just x)
len1' _   = Nothing

okayWhat :: (Ord c, Ord a) => TM.TMap c a -> M.Map (TM.TMap c a) TrieAnnotation
okayWhat = M.fromAscList . (`zip` infTrieAnns) . S.toAscList . setifyTrie

setifyTrie  :: (Ord c, Ord a) => TM.TMap c a -> S.Set (TM.TMap c a)
setifyTrie trie = setifyTrie' S.empty trie

setifyTrie' :: (Ord c, Ord a) => S.Set (TM.TMap c a) -> TM.TMap c a -> S.Set (TM.TMap c a)
setifyTrie' st tmp = case sbTries of
  [] -> st -- Maybe?
  xs -> foldl setifyTrie' (S.insert tmp st) sbTries
  where sbTries = getSubTries tmp

-- | Unify and annotate a `TM.TMap`.
annotifyTrie  :: (Ord c, Ord a) => TM.TMap c a -> TM.TMap c (TrieAnnotation, Maybe a)
annotifyTrie trie = annotifyTrie' (okayWhat trie) [] (TM.lookup [] trie) TM.empty trie

annotifyTrie' :: (Ord c, Ord a) => (M.Map (TM.TMap c a) TrieAnnotation) -> [c] -> Maybe a -> (TM.TMap c (TrieAnnotation, Maybe a)) -> TM.TMap c a -> (TM.TMap c (TrieAnnotation, Maybe a))
annotifyTrie' trieDict prfx thisMVal trieAcc thisTrie = case sbTries of
  -- [] -> trieAcc -- ? Maybe TM.insert prfx thisMVal trieAcc
  [] -> TM.insert prfx (annot, thisMVal) trieAcc
  _  -> 
    foldl (\theTrieMap (x,(mVal, subTrie)) -> annotifyTrie' trieDict (prfx ++ [x]) mVal theTrieMap subTrie) (TM.insert prfx (annot, thisMVal) trieAcc) sbTries
  where
    annot = fromMaybe TrieLeaf $ M.lookup thisTrie trieDict
    sbTries = getSubTries' thisTrie

-- (c,(Maybe a,TM.TMap c a))
-- (x,(mVal, subTrie))
-- --> annotifyTrie' (prfx ++ [x]) (TM.insert (prfx ++ [c]) (annot,))
-- where (Just annot) = M.lookup thisTrie trieDict



-- This is gonna be tricky...
-- Maybe... For each node, see if any sub-tries
-- in the overall Trie are equivalent, at which point,
-- put join them in a group?

-- | Annotate a `TM.TMap` such that identical sub-tries have
--   the same annotation. I don't know how well this will
--   work in practice, but it should reduce the number of
--   distinct branches generated.
unifyPaths :: (Ord c, Ord a) => TM.TMap c a -> TM.TMap c (TrieAnnotation, Maybe a)
unifyPaths = annotifyTrie


