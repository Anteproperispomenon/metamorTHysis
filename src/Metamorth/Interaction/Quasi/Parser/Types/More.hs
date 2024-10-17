{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Metamorth.Interaction.Quasi.Parser.Types.More
  ( parsePhoneFileNameQH
  , parseLanguageNameQH
  , parseCaseOptionQH
  ) where

import Metamorth.Interaction.Quasi.Parser.Types
import Metamorth.Interaction.Quasi.Parser.Helpers

import Control.Applicative
import Control.Monad

import Data.Maybe

import Data.String (IsString(..))

import Metamorth.Helpers.Error

import Data.Attoparsec.Text qualified as AT

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Data.Text qualified as T

parsePhoneFileNameQH :: StateT QuasiHeader ParserQQ ()
parsePhoneFileNameQH = do
  fld <- lift ("phonemes" <|> "phoneme set" <|> "phoneme-set" <|> "phones")
  lift $ parseKeySepX $ T.unpack fld
  fp <- lift $ lift (parseQuoteString <|> parseUnquoteString)
  modify' $ \x -> x {qhFilePath = fp}

parseLanguageNameQH :: StateT QuasiHeader ParserQQ ()
parseLanguageNameQH = do
  _ <- lift "lang"
  z <- lift (isJust <$> optional "uage")
  lift $ parseKeySepX $ if z then "language" else "lang"
  lng <- lift $ lift (parseQuoteString <|> parseUnquoteString)
  modify' $ \x -> x {qhLanguage = Just lng}

parseCaseOptionQH :: StateT QuasiHeader ParserQQ ()
parseCaseOptionQH = do
  fld <- lift ("use-case" <|> "cased" <|> "is-cased" <|> "use case")
  lift $ parseKeySepX $ T.unpack fld
  bl <- lift $ lift parseBool
  modify' $ \x -> x {qhIsCased = bl}


