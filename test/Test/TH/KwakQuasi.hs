{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-unused-top-binds -Wno-unused-matches #-}

module Test.TH.KwakQuasi
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

import Metamorth.Interaction.Quasi

[metamorth|


phonemes : "examples/phonemes/kwakwala.thym"

Umista
  input  : "examples/parsing/kwakwala_umista.thyp"
  output : "examples/output/kwakwala_umista.thyo"
  suffix : "_ums"
  parser-name : "umistaParser"
  output-name : "umistaOutput"

Grubb
  input  : "examples/parsing/kwakwala_grubb.thyp"
  output : "examples/output/kwakwala_grubb.thyo"
  suffix : "_grb"
  parser-name : "grubbParser"
  output-name : "grubbOutput"

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

