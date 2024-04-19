{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-unused-top-binds #-}

module Test.TH.InAndOut
  ( Phoneme(..)
  , syllabicParser
  , latinParser
  , syllabicOutput
  , convertOrthography
  , convertOrthographyLazy
  , InOrth(..)
  , OutOrth(..)
  ) where

import Metamorth.Interaction.TH

import Metamorth.Interpretation.Parser.TH (ParserOptions(..))

declareFullParsers 
  "examples/phonemes/example_inuktitut_02.thyt"
  [ ("examples/parsing/parsing_example_02.thyp", (defExtraParserDetails "_syl") {epdParserName = "syllabicParser", epdOtherNames = ["Syllabic", "Syl"]} )
  , ("examples/parsing/inuktitut_latin.thyp"   , (defExtraParserDetails "_lat") {epdParserName = "latinParser"   , epdOtherNames = ["Latin"]} )
  ]
  [ ("examples/output/example_inuktitut_01.thyo", defExtraOutputDetails {eodSuffix = "_sylout", eodOutputName = "syllabicOutput", eodOtherNames = ["Syllabic", "Syl"]})
  ]

{-
$( do
    let phonemeFile = "examples/phonemes/example_inuktitut.thyt"
        parserFile  = "examples/parsing/parsing_example_02.thyp"
    declareParsers phonemeFile [(parserFile, defExtraParserDetails)] []
 )
-}

-- declareParsers "examples/phonemes/example_inuktitut_02.thyt" [] [ ("examples/output/example_inuktitut_01.thyo", defExtraOutputDetails {eodSuffix = "_sylout", eodOutputName = "syllabicOutput"})]

-- fst . (TM.match []) . opoOutputTrie . fst . extractRight . AT.parseOnly testOutputFile <$> readFileUTF8 "examples/output/example_inuktitut_01.thyo"