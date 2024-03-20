{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Metamorth.Helpers.Trie
Description : Helpers for Tries
Copyright   : (c) David Wilson, 2024
License     : BSD-3

Helpers for `TM.TMap`s. from the package "simple-trie".

Of particular use to this project is `unifyPaths`. This
annotates all internal nodes of a `TM.TMap` with a label
such that if two nodes have the same label, then the same
parser function generated can be used for either branch.

-}

module Metamorth.Helpers.Trie
  ( TrieAnnotation(..)
  , isLeafAnn
  , unifyPaths
  , annotateTrie
  , getSubTries
  , deleteBranch
  , getFirstSteps
  , matchPred
  -- , annotateTrie
  -- , annotifyTrie
  -- , setifyTrie
  ) where

import Data.List (mapAccumL)
import Data.Maybe

import Data.Trie.Map qualified as TM
import Data.Trie.Set qualified as TS

import Data.Trie.Map.Internal qualified as TMI

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
getSubTriesX :: (Ord c) => TM.TMap c a -> [TM.TMap c a]
getSubTriesX tm
  = map (\x -> snd (TM.match x tm)) keys1c
  where
    keys1c = filter len1 $ TS.toList $ TS.prefixes $ TM.keysTSet tm

-- | Get a list of the sub-tries of a trie, along
--   with their elements and prefix.
getSubTries :: (Ord c) => TM.TMap c a -> [(c,(Maybe a, TM.TMap c a))]
getSubTries tm
  = map (\x -> (x, (TM.match [x] tm))) keys1c
  where
    keys1c = mapMaybe len1' $ TS.toList $ TS.prefixes $ TM.keysTSet tm

-- | Newer version of `getSubTries`.
getSubTriesNew :: forall c a. (Ord c) => TM.TMap c a -> [(c,(Maybe a, TM.TMap c a))]
getSubTriesNew (TMI.TMap (TMI.Node val0 cmap))
  = map extractNode (M.assocs cmap)
  where
    extractNode :: (c, TM.TMap c a) -> (c, (Maybe a, TM.TMap c a))
    extractNode (cv, tm@(TMI.TMap (TMI.Node valx _tmap))) = (cv, (valx, tm))

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
  where sbTries = getSubTriesX tmp

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
    sbTries = getSubTries thisTrie

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

-- | Delete the branch starting with a value.
deleteBranch :: (Ord c) => c -> TM.TMap c a -> TM.TMap c a
deleteBranch x (TMI.TMap (TMI.Node val0 cmap))
  = TMI.TMap (TMI.Node val0 (M.delete x cmap))

-- | Get the first "steps" of a `TM.TMap`.
--   i.e. get the first element of any possible
--   taken in the trie.
--
--   e.g. if @`TM.keys` trie == ["about","asdf","back", "enough", "epoxy", "stone", "stop"]@
--   then @`getFirstSteps` trie == ['a', 'b', 'e', 's']@
getFirstSteps :: (Ord c) => TM.TMap c a -> [c]
getFirstSteps (TMI.TMap (TMI.Node _val0 cmap)) = M.keys cmap

-- | Like `TM.match`, but works over a list
--   of predicates instead of just a list of
--   values. Since multiple values can now
--   be matched, the result is returned as a
--   list instead.
matchPred :: (Ord c) => [c -> Bool] -> TM.TMap c a -> [(Maybe a, TM.TMap c a)]
matchPred []     t@(TMI.TMap (TMI.Node ma _)) = [(ma, t)]
matchPred (p:ps)   (TMI.TMap (TMI.Node _  e))
  | nods <- M.elems $ M.filterWithKey (\c _ -> p c) e
  = nods >>= (matchPred ps)

-- | Like `matchPred`, but only returns the
--   values at the resulting nodes.
matchPred' :: (Ord c) => [c -> Bool] -> TM.TMap c a -> [a]
matchPred' ps tm = mapMaybe fst $ matchPred ps tm

