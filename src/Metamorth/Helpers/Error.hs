module Metamorth.Helpers.Error
  ( ParserMessage(..)
  , partitionMessages
  ) where

import Data.String (IsString(..))

-- | An error type for Parsers. There are multiple
--   levels of messages, from simple messages, to
--   warnings, to errors.
data ParserMessage
   = ParserError   String
   | ParserWarning String
   | ParserMessage String
   deriving (Eq)

instance Show ParserMessage where
  show (ParserError   str) = "Error: "   ++ str
  show (ParserWarning str) = "Warning: " ++ str
  show (ParserMessage str) = "Message: " ++ str

-- There's probably a better way to do this.

-- | Partition `ParserMessage`s into three lists,
--   in the order @(errors, warnings, messages)@.
partitionMessages :: [ParserMessage] -> ([String], [String], [String])
partitionMessages [] = ([],[],[])
partitionMessages ((ParserError   str):msgs) = cons1 str (partitionMessages msgs)
partitionMessages ((ParserWarning str):msgs) = cons2 str (partitionMessages msgs)
partitionMessages ((ParserMessage str):msgs) = cons3 str (partitionMessages msgs)

cons1, cons2, cons3 :: String -> ([String], [String], [String]) -> ([String], [String], [String])
cons1 x (xs,ys,zs) = (x:xs,ys,zs)
cons2 y (xs,ys,zs) = (xs,y:ys,zs)
cons3 z (xs,ys,zs) = (xs,ys,z:zs)

-- In order to allow backwards-compatibility.
instance IsString ParserMessage where
  fromString str = ParserError str
