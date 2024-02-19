module Metamorth.Helpers.Parsing
  ( skipHoriz
  , skipHorizontalSpace
  , skipHoriz1
  , skipHorizontalSpace1

  , takeIdentifier
  ) where

import Data.Attoparsec.Text qualified as AT

import Data.Text qualified as T

skipHoriz :: AT.Parser ()
skipHoriz = AT.skipWhile AT.isHorizontalSpace

skipHorizontalSpace :: AT.Parser ()
skipHorizontalSpace = skipHoriz

skipHoriz1 :: AT.Parser ()
skipHoriz1 = AT.satisfy AT.isHorizontalSpace *> skipHoriz

skipHorizontalSpace1 :: AT.Parser ()
skipHorizontalSpace1 = skipHoriz1

-- | Meant for parsing identifiers where the
--   acceptable set of characters for the 
--   first character is different from the
--   acceptable set of the rest of the chars.
takeIdentifier :: (Char -> Bool) -> (Char -> Bool) -> AT.Parser T.Text
takeIdentifier p1 p2 = T.cons <$> AT.satisfy p1 <*> AT.takeWhile p2
