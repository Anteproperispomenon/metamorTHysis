module Metamorth.Interpretation.Shared.Types
  ( ImportProperty(..)
  , getImport
  ) where

import Data.Attoparsec.Text qualified as AT

import Metamorth.Helpers.Parsing

import Data.Text qualified as T

import Control.Applicative

import Data.Char

-- | Data type for declarations of the form
--   `import (aspect | group | trait) prop_name`.
data ImportProperty
  = ImportAspect String
  | ImportGroup  String
  | ImportTrait  String
  deriving (Show, Eq, Ord)

getImport :: AT.Parser ImportProperty
getImport = do
  _ <- "import"
  skipHoriz1
  getAspect <|> getTrait <|> getGroup

getAspect :: AT.Parser ImportProperty
getAspect = do
  _ <- "aspect"
  skipHoriz1
  ImportAspect . T.unpack <$> takeIdentifier isAlpha isFollowId

getTrait :: AT.Parser ImportProperty
getTrait = do
  _ <- "trait"
  skipHoriz1
  ImportTrait . T.unpack <$> takeIdentifier isAlpha isFollowId

getGroup :: AT.Parser ImportProperty
getGroup = do
  _ <- "group"
  skipHoriz1
  ImportGroup . T.unpack <$> takeIdentifier isAlpha isFollowId
