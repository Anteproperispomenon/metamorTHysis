{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Test.TH.Basic
  ( theActualParser
  , Phoneme(..)
  ) where

import Metamorth.Interaction.TH


{-
phonemeFile :: String
phonemeFile = "examples/phonemes/example_inuktitut.thyt"
{-# INLINE phonemeFile #-}

parserFile :: String
parserFile = "examples/parsing/parsing_example_02.thyp"
{-# INLINE parserFile #-}
-}

declareParsers 
  "examples/phonemes/example_inuktitut_02.thyt"
  [("examples/parsing/parsing_example_02.thyp", defExtraParserDetails)]
  []

{-
$( do
    let phonemeFile = "examples/phonemes/example_inuktitut.thyt"
        parserFile  = "examples/parsing/parsing_example_02.thyp"
    declareParsers phonemeFile [(parserFile, defExtraParserDetails)] []
 )
-}