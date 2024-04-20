{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-unused-top-binds #-}

module Test.TH.Kwakwala
  ( Phoneme(..)
  -- , umistaParser
  -- , grubbParser
  -- , syllabicOutput
  , convertOrthography
  , convertOrthographyLazy
  , convertOrthographyBS
  , InOrth(..)
  , OutOrth(..)
  ) where

import Metamorth.Interaction.TH

import Metamorth.Interpretation.Parser.TH (ParserOptions(..))

declareFullParsers 
  "examples/phonemes/kwakwala.thym"
  [ ("examples/parsing/kwakwala_umista.thyp", (defExtraParserDetails "_ums") {epdParserName = "umistaParser", epdOtherNames = ["Umista", "Ums", "U", "U'mista"], epdUnifyBranches = False} )
  , ("examples/parsing/kwakwala_grubb.thyp" , (defExtraParserDetails "_grb") {epdParserName = "grubbParser" , epdOtherNames = ["Grubb", "Grubb-ascii", "grb", "g"]} )
  ]
  [ ("examples/output/kwakwala_umista.thyo", defExtraOutputDetails {eodSuffix = "_umsout", eodOutputName = "umistaOutput", eodOtherNames = ["Umista", "Ums", "U", "U'mista"]})
  , ("examples/output/kwakwala_grubb.thyo" , defExtraOutputDetails {eodSuffix = "_grbout", eodOutputName = "grubbOutput" , eodOtherNames = ["Grubb", "Grubb-ascii", "grb", "g"]})
  ]

-- What's weird is that addDependentFile sometimes works and sometimes doesn't

{-
$( do
    let phonemeFile = "examples/phonemes/example_inuktitut.thyt"
        parserFile  = "examples/parsing/parsing_example_02.thyp"
    declareParsers phonemeFile [(parserFile, defExtraParserDetails)] []
 )
-}

-- declareParsers "examples/phonemes/example_inuktitut_02.thyt" [] [ ("examples/output/example_inuktitut_01.thyo", defExtraOutputDetails {eodSuffix = "_sylout", eodOutputName = "syllabicOutput"})]

-- fst . (TM.match []) . opoOutputTrie . fst . extractRight . AT.parseOnly testOutputFile <$> readFileUTF8 "examples/output/example_inuktitut_01.thyo"