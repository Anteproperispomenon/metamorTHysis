{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-unused-top-binds -Wno-unused-matches #-}

module Test.TH.Backtrack
  ( Phoneme(..)
  , convertOrthography
  , convertOrthographyLazy
  , convertOrthographyBS
  , InOrth(..)
  , OutOrth(..)
  , inputOrthNameMap
  , outputOrthNameMap
  ) where

import Metamorth.Interaction.Quasi

[metamorth|

language : "Backtrack"

phonemes : "examples/phonemes/backtrack_test.thym"

Backtracker
  input  : "examples/parsing/backtrack_example.thyp"
  output : "examples/output/backtrack_example.thyo"
  suffix : "_bkt"
  parser-name : "backtrackParser"
  output-name : "backtrackOutput"
  unify-branches : off
  extension : .bkt
  description : "Test Orthography."

Backtracker2
  output : "examples/output/backtrack_example2.thyo"
  suffix : "_bk2"
  parser-name : "backtrackParser2"
  output-name : "backtrackOutput2"
  unify-branches : off
  extension : .bk2
  description : "Another Test Orthography."


|]

{-
Umista
  input  : "parsers/umista.thyp"
  output : "output/umista.thyo"
  cli-names : "umista", "ums", "u"
  suffix : "ums"
  parser-name : umistaParser -- optional
  output-name : umistaOutput -- optional
  unify-paths : off

-}

