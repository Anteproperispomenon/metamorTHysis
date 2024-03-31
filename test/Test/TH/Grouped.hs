{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Test.TH.Grouped
  ( theActualParser
  , Phoneme(..)
  ) where

import Metamorth.Interaction.TH

import Metamorth.Interpretation.Parser.TH (ParserOptions(..))

{-
phonemeFile :: String
phonemeFile = "examples/phonemes/example_inuktitut.thyt"
{-# INLINE phonemeFile #-}

parserFile :: String
parserFile = "examples/parsing/parsing_example_02.thyp"
{-# INLINE parserFile #-}
-}

-- add this after to turn off unifyBranches:
-- {epdParserOptions = defParserOptions {poUnifyBranches = False}}

-- Doesn't seem like "addDependentFile" is working...
-- ...annoying... argh...

declareParsers 
  "examples/phonemes/example_inuktitut_03.thyt"
  [("examples/parsing/parsing_example_02.thyp", defExtraParserDetails' {epdParserName = "theActualParser"} )]
  []

{-
$( do
    let phonemeFile = "examples/phonemes/example_inuktitut.thyt"
        parserFile  = "examples/parsing/parsing_example_02.thyp"
    declareParsers phonemeFile [(parserFile, defExtraParserDetails)] []
 )
-}