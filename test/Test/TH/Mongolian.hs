{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-unused-top-binds -Wno-unused-matches #-}

module Test.TH.Mongolian
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
  "examples/phonemes/example_mongolian_01.thym"
  [ ("examples/parsing/parsing_example_01.thyp", (defExtraParserDetails "_lat") {epdParserName = "latinParser", epdOtherNames = ["Latin", "Lat", "L"], epdUnifyBranches = True} )
  -- , ("examples/parsing/kwakwala_grubb.thyp" , (defExtraParserDetails "_grb") {epdParserName = "grubbParser" , epdOtherNames = ["Grubb", "Grubb-ascii", "grb", "g"]} )
  ]
  [ ("examples/output/example_mongolian_01.thyo", defExtraOutputDetails {eodSuffix = "_latout", eodOutputName = "latinOutput", eodOtherNames = ["Latin", "Lat", "L"]})
  -- , ("examples/output/kwakwala_grubb.thyo" , defExtraOutputDetails {eodSuffix = "_grbout", eodOutputName = "grubbOutput" , eodOtherNames = ["Grubb", "Grubb-ascii", "grb", "g"]})
  ]

-- What's weird is that addDependentFile sometimes works and sometimes doesn't
-- oh

{-
$( do
    let phonemeFile = "examples/phonemes/example_inuktitut.thyt"
        parserFile  = "examples/parsing/parsing_example_02.thyp"
    declareParsers phonemeFile [(parserFile, defExtraParserDetails)] []
 )
-}

-- declareParsers "examples/phonemes/example_inuktitut_02.thyt" [] [ ("examples/output/example_inuktitut_01.thyo", defExtraOutputDetails {eodSuffix = "_sylout", eodOutputName = "syllabicOutput"})]

-- fst . (TM.match []) . opoOutputTrie . fst . extractRight . AT.parseOnly testOutputFile <$> readFileUTF8 "examples/output/example_inuktitut_01.thyo"