{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Metamorth.Interpretation.Parser.TH
Description : Generating the Parsers for Orthographies
Copyright   : (c) David Wilson, 2024
License     : BSD-3

This is the module that probably does the most(?) 
complicated part of the process. 

An obvious(?) way to generate a set of parser
functions is to just make a Trie out of the
Phoneme Patterns, and then just make a function
at each branch. The problem with this method
is that you'll get a lot of duplicate functions.
e.g. If "ts" and "ʦ" (U+02A6) are both acceptable
ways to represent the same phoneme path, then we'd
wind up with something like:

@
  ╔═════╗
  ║Start║
  ╚══╦══╝
     ║
  ╔══╩══╗
 ┌╨┐   ┌╨┐
 │t│   │ʦ│
 └╥┘   └╥┘
  ║     ║
╔═╩═╗   ║
║St1║   ║
╚═╦═╝   ║
  ║     ║
 ┌╨┐    ║
 │s│    ║  
 └╥┘    ║  
  ║     ║
╔═╩═╗ ╔═╩═╗  
║St2║ ║St3║
╚═╦═╝ ╚═╦═╝
  ║     ║
@

... where St2 and St3 would generate two different
functions, even though it would make more sense to
use the same state in those two spots, like so:

@
  ╔═════╗
  ║Start║
  ╚══╦══╝
     ║
  ╔══╩══╗
 ┌╨┐   ┌╨┐
 │t│   │ʦ│
 └╥┘   └╥┘
  ║     ║
╔═╩═╗   ║
║St1║   ║
╚═╦═╝   ║
  ║     ║
 ┌╨┐    ║
 │s│    ║  
 └╥┘    ║  
  ║     ║
╔═╩═════╩═╗  
║   St2   ║
╚════╦════╝
     ║
@

This can be accomplished by using `unifyPaths`
from "Metamorth.Helpers.Trie". It annotates each
branch of a `TM.TMap`/Trie with a label such that
two branches with the same label can be assumed
to be the same state. Thus only one function
needs to be generated for either state, reducing
the total number of generated functions.

-}


module Metamorth.Interpretation.Parser.TH
  ( setupTrie'
  , pathifyTrie
  , tempTester
  ) where

import Data.Attoparsec.Text qualified as AT

import Data.Text qualified as T -- ?

import Data.Map.Strict qualified as M

import Data.Trie.Map qualified as TM
import Metamorth.Helpers.Trie

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

import Metamorth.Interpretation.Parser.Types
import Metamorth.Interpretation.Parser.Parsing.Types
import Metamorth.Helpers.TH


-- How to check for the end of a word:
-- Create a bool function that checks
-- whether a character is any of the
-- `Char`s that can start a (midword) 
-- phoneme. Then, just run
-- @ do 
--     x <- AT.peekChar 
--     case x of 
--       Nothing -> pure phone
--       (Just z) -> if (isLetter z)
--                    then (fail "Not at word's end")
--                    else (pure phone) 
-- @

----------------------------------------------------------------
-- Main Constructor
----------------------------------------------------------------


makeTheParser 
  :: M.Map String Name                  -- ^ A `M.Map` from `String`s to Pattern Synonym `Name`s.
  -> M.Map String ([M.Map String Name]) -- ^ A list of `M.Map`s for the constructors of each argument of the Phoneme.
  -> (Exp -> Exp)                       -- ^ How to convert a Pattern synonym to an upper-case character.
  -> (Exp -> Exp)                       -- ^ How to convert a Pattern synonym to an lower-case character.
  -> ()
makeTheParser _ _ _ _ = ()

{-
x <- AT.peekChar'
case x of
  c1 -> AT.anyChar 

-}


----------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------

{-
data ParserParsingState = ParserParsingState
  { ppsClassDictionary :: M.Map String (S.Set Char)
  , ppsPhonemePatterns :: M.Map PhoneName [PhonemePattern]
  } deriving (Show, Eq)

parseOrthographyFile :: AT.Parser (HeaderData, ParserParsingState, [String])

data PhonemePattern = PhonemePattern 
  { isUpperPP  :: Caseness      -- ^ Is this pattern upper-case?
  , charPatsPP :: [CharPattern] -- ^ The pattern of `Char`s for this phoneme.
  } deriving (Show, Eq)

-}

-- :m + Metamorth.Interpretation.Parser.Parsing Metamorth.Interpretation.Parser.Types Data.Either Control.Monad Metamorth.Interpretation.Parser.TH
-- import Data.Text.IO qualified as TIO
-- import Data.Attoparsec.Text qualified as AT
-- setupTrie' <$> ppsPhonemePatterns <$> (tempTester (\(_,x,_) -> x)) =<< AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample1.thyp"
-- (tempTester (\(_,x,_) -> setupTrie' $ ppsPhonemePatterns $ x)) =<< AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample1.thyp"
-- 

pathifyTrie :: M.Map PhoneName [PhonemePattern] -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneName, Caseness))
pathifyTrie = unifyPaths . setupTrie'

setupTrie' :: M.Map PhoneName [PhonemePattern] -> TM.TMap CharPattern (PhoneName, Caseness)
setupTrie' phonePats = TM.fromList $ concatMap phoneStuff $ M.toList phonePats

  where
    phoneStuff :: (PhoneName, [PhonemePattern]) -> [([CharPattern], (PhoneName, Caseness))]
    phoneStuff (pname, phPats) = forMap phPats $ \thisPat ->
      (charPatsPP thisPat, (pname, isUpperPP thisPat))

tempTester :: (a -> b) -> Either String a -> IO b
tempTester f (Right x) = return $ f x
tempTester f (Left st) = fail $ st

{-


-}


