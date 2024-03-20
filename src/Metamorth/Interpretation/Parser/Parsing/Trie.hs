module Metamorth.Interpretation.Parser.Parsing.Trie
  ( charPatSubsetOf
  , getAltTries
  ) where

import Data.Map.Strict qualified as M

import Data.Trie.Map qualified as TM

import Metamorth.Helpers.Char
import Metamorth.Helpers.Trie

import Metamorth.Interpretation.Parser.Parsing.Types
import Metamorth.Interpretation.Parser.Types

import Witherable qualified as W

getTrieAlts' :: [CharPattern] -> TM.TMap CharPattern a -> [([CharPattern],a)]
getTrieAlts' pat trie = TM.toList $ W.ifilter (\patX _ -> pat `charPatSubsetOf` patX) trie



-- | Check that the second pattern is at least
--   as general as the first pattern. e.g. if
--   the two patterns are the same, but the first
--   pattern has some conditions on the state,
--   then this function will return `True`.
charPatSubsetOf :: [CharPattern] -> [CharPattern] -> Bool
charPatSubsetOf [] [] = True
charPatSubsetOf (WordStart:xs) (WordStart:ys) = charPatSubsetOf xs ys
charPatSubsetOf (NotStart :xs) (NotStart :ys) = charPatSubsetOf xs ys
charPatSubsetOf ((PlainChar stX c):xs) ((PlainChar stY d):ys)
  | (c == d) && (stX `stateSubsetOf` stY)
  = charPatSubsetOf' xs ys
  | otherwise = False
charPatSubsetOf ((PlainChar stX c):xs) ((CharOptCase stY d):ys)
  | (c `elem` (getCases d)) && (stX `stateSubsetOf` stY)
  = charPatSubsetOf' xs ys
  | otherwise = False
charPatSubsetOf ((CharOptCase stX c):xs) ((CharOptCase stY d):ys)
  | (notDisjoint (getCases c) (getCases d)) && (stX `stateSubsetOf` stY)
  = charPatSubsetOf' xs ys
  | otherwise = False
charPatSubsetOf _ _ = False

charPatSubsetOf' :: [CharPattern] -> [CharPattern] -> Bool
charPatSubsetOf' [] [] = True
charPatSubsetOf' ((PlainChar _ c):xs) ((PlainChar _ d):ys)
  = (c == d) && (charPatSubsetOf' xs ys)
charPatSubsetOf' ((PlainChar _ c):xs) ((CharOptCase _ d):ys)
  = (c `elem` (getCases d)) && (charPatSubsetOf' xs ys)
charPatSubsetOf' ((CharOptCase _ c):xs) ((CharOptCase _ d):ys)
  = (notDisjoint (getCases c) (getCases d)) && (charPatSubsetOf' xs ys)
charPatSubsetOf' [WordEnd] [WordEnd] = True
charPatSubsetOf' [NotEnd] [NotEnd] = True
charPatSubsetOf' _ _ = False

notDisjoint :: (Eq a) => [a] -> [a] -> Bool
notDisjoint xs ys = any (`elem` ys) xs


-- matchPred :: (Ord c) => [c -> Bool] -> TM.TMap c a -> [(Maybe a, TM.TMap c a)]

-- | Get the tries that are more general than this pattern.
getAltTries :: [CharPattern] -> TM.TMap CharPattern a -> [(Maybe a, TM.TMap CharPattern a)]
getAltTries pats trie = matchPred (charPatSubsetOfZ pats) trie

charPatSubsetOfZ :: [CharPattern] -> [CharPattern -> Bool]
charPatSubsetOfZ [] = []
charPatSubsetOfZ (WordStart:xs) = (== WordStart) : (charPatSubsetOfZ xs)
charPatSubsetOfZ (NotStart :xs) = (==  NotStart) : (charPatSubsetOfZ xs)
charPatSubsetOfZ ((PlainChar stX c):xs) = (\case 
    (PlainChar   stY d) -> ((c == d)                && (stX `stateSubsetOf` stY))
    (CharOptCase stY d) -> ((c `elem` (getCases d)) && (stX `stateSubsetOf` stY))
    _ -> False
  ) : (charPatSubsetOfZ' xs)
charPatSubsetOfZ ((CharOptCase stX c):xs) = (\case
    (CharOptCase stY d) -> (notDisjoint (getCases c) (getCases d)) && (stX `stateSubsetOf` stY)
    _ -> False
  ) : (charPatSubsetOfZ' xs)
charPatSubsetOfZ (WordEnd : xs) = (const False) : (charPatSubsetOfZ' xs)
charPatSubsetOfZ (NotEnd  : xs) = (const False) : (charPatSubsetOfZ' xs)

charPatSubsetOfZ' :: [CharPattern] -> [CharPattern -> Bool]
charPatSubsetOfZ' [] = []
charPatSubsetOfZ' ((PlainChar _ c):xs) = (\case 
    (PlainChar   _ d) -> (c == d)               
    (CharOptCase _ d) -> (c `elem` (getCases d))
    _ -> False
  ) : (charPatSubsetOfZ' xs)
charPatSubsetOfZ' ((CharOptCase _ c):xs) = (\case
    (CharOptCase _ d) -> (notDisjoint (getCases c) (getCases d))
    _ -> False
  ) : (charPatSubsetOfZ' xs)
charPatSubsetOfZ' [WordEnd] = [(== WordEnd)]
charPatSubsetOfZ' [NotEnd]  = [(== NotEnd) ]
charPatSubsetOfZ' (_:xs) = (const False) : (charPatSubsetOfZ' xs)

