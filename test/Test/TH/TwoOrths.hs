{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-unused-top-binds #-}

module Test.TH.TwoOrths
  ( Phoneme(..)
  , syllabicParser
  , latinParser
  ) where

import Metamorth.Interaction.TH

import Metamorth.Interpretation.Parser.TH (ParserOptions(..))

declareParsers 
  "examples/phonemes/example_inuktitut_02.thyt"
  [ ("examples/parsing/parsing_example_02.thyp", (defExtraParserDetails "_syl") {epdParserName = "syllabicParser"} )
  , ("examples/parsing/inuktitut_latin.thyp"   , (defExtraParserDetails "_lat") {epdParserName = "latinParser"   } )
  ]
  []

{-
$( do
    let phonemeFile = "examples/phonemes/example_inuktitut.thyt"
        parserFile  = "examples/parsing/parsing_example_02.thyp"
    declareParsers phonemeFile [(parserFile, defExtraParserDetails)] []
 )
-}