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
  , inputOrthNameMap
  , outputOrthNameMap
  ) where

import Metamorth.Interaction.Quasi

[metamorth|

language : "Kwak'wala"

phonemes : "examples/phonemes/kwakwala.thym"

cased : yes

Umista  
  input  : "examples/parsing/kwakwala_umista.thyp"
  output : "examples/output/kwakwala_umista.thyo"
  suffix : "_ums"
  parser-name : "umistaParser"
  output-name : "umistaOutput"
  unify-branches : off
  extension : .umi
  description : "Orthography developed by the U'mista Cultural Centre."

Umista2  
  # input  : "examples/parsing/kwakwala_umista.thyp"
  output : "examples/output/kwakwala_umista_alt.thyo"
  suffix : "_ums2"
  # parser-name : "umistaParser2"
  output-name : "umistaOutput2"
  # unify-branches : off
  extension : .umi2
  description : "Alternate version of U'mista for testing."


Grubb
  input  : "examples/parsing/kwakwala_grubb.thyp"
  output : "examples/output/kwakwala_grubb.thyo"
  suffix : "_grb"
  parser-name : "grubbParser"
  output-name : "grubbOutput"
  extension : ".grb"

Boas
  input  : "examples/parsing/kwakwala_boas.thyp"
  suffix : "_boas"
  parser-name : "boasParser"
  extension : ".boas"
  unify-branches : off
  description : "Collection of Boas's orthographies."

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

