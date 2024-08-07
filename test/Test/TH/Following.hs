{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-unused-top-binds -Wno-unused-matches #-}

module Test.TH.Following
  ( Phoneme(..)
  , convertOrthography
  , convertOrthographyLazy
  , convertOrthographyBS
  , InOrth(..)
  , OutOrth(..)
  ) where

import Metamorth.Interaction.TH

import Metamorth.Interpretation.Parser.TH (ParserOptions(..))

declareFullParsers 
  "examples/phonemes/example_follow.thym"
  [ ("examples/parsing/example_follow_01.thyp", (defExtraParserDetails "_f01") {epdParserName = "follow1Parser", epdOtherNames = ["f1"]} )
  , ("examples/parsing/lookahead_example.thyp", (defExtraParserDetails "_lah") {epdParserName = "followLParser", epdOtherNames = ["la"]} )
  ]
  [ ("examples/output/example_follow_01.thyo", defExtraOutputDetails {eodSuffix = "_f01out", eodOutputName = "follow1Output", eodOtherNames = ["f1"]}) 
  , ("examples/output/example_follow_02.thyo", defExtraOutputDetails {eodSuffix = "_f02out", eodOutputName = "follow2Output", eodOtherNames = ["f2"]}) 
  , ("examples/output/example_follow_03.thyo", defExtraOutputDetails {eodSuffix = "_f03out", eodOutputName = "follow3Output", eodOtherNames = ["f3"]}) 
  ]

