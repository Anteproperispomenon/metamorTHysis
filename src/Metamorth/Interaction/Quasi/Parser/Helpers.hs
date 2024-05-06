module Metamorth.Interaction.Quasi.Parser.Helpers
  -- * Low-level helpers
  ( countHoriz
  , consumeEndComment
  -- * Parsing Items in Lists
  , parseQuoteString
  , parseQuoteText
  , parseUnquoteString
  , parseUnquoteText
  -- * Parsing Lists Themselves
  , parseKeySep
  , parseListSep
  ) where

import Control.Applicative

import Data.Attoparsec.Text qualified as AT

import Data.Text qualified as T

import Data.Char

import Metamorth.Helpers.Parsing

-- | Consume and count horizontal spaces. Tabs 
--   are treated as two spaces.
countHoriz :: AT.Parser Int
countHoriz = countHoriz' 0

countHoriz' :: Int -> AT.Parser Int
countHoriz' n = do
  c <- AT.peekChar
  case c of
    (Just '\t') -> do
        _ <- AT.anyChar
        countHoriz' (n+2)
    (Just ' ') -> do
        _ <- AT.anyChar
        countHoriz' (n+1)
    _ -> return n

-- | Variant of the one from "Metamorth.Helpers.Parsing"
--   that also accepts @'--'@ as the start of a comment.
consumeEndComment :: AT.Parser ()
consumeEndComment = do
  skipHoriz
  AT.choice [choice1, choice2, choice3]
  where
    choice1 = AT.endOfLine
    choice2 = do
      _ <- "--"
      AT.skipWhile (\x -> x /= '\n' && x /= '\r')
      AT.endOfLine
    choice3 = do
      _ <- AT.char '#'
      AT.skipWhile (\x -> x /= '\n' && x /= '\r')
      AT.endOfLine

-- | Simple `String` version that doesn't treat
--   backslashes any differently.
--   Note that this doesn't work on quotations
--   that have linebreaks in them.
parseQuoteString :: AT.Parser String
parseQuoteString = T.unpack <$> parseQuoteText

-- | Simple `T.Text` version that doesn't treat
--   backslashes any differently.
--   Note that this doesn't work on quotations
--   that have linebreaks in them.
parseQuoteText :: AT.Parser T.Text
parseQuoteText = do
  _ <- AT.char '\"'
  txt <- AT.takeWhile (\x -> x /= '\"' && x /= '\n' && x /= '\r')
  _ <- AT.char '\"'
  return txt

-- | Parsing a String that doesn't have spaces,
--   separators, or quotation marks.
parseUnquoteString :: AT.Parser String
parseUnquoteString = T.unpack <$> parseUnquoteText

-- | Parsing `T.Text` that doesn't have spaces,
--   separators, or quotation marks.
parseUnquoteText :: AT.Parser T.Text
parseUnquoteText = AT.takeWhile (\x -> not (isSpace x) && x /= '\"' && x /= ',' && x /= ';' && x /= '|')

-- | Parse the separator between a key and its value.
--   Can be either @':'@ or @'='@.
parseKeySep :: AT.Parser ()
parseKeySep = do
  skipHoriz
  AT.satisfy (\x -> x == ':' || x == '=')
  skipHoriz
  return ()

-- | Parse a separator between two items in a list.
--   It can be one of @','@, @';'@, @'|'@, or just @' '@.
--   Any of these options can be surrounded by extra spaces
--   or tabs. Note that this function is tricky to write
--   properly, so check to make sure this function is
--   working properly when using it.
parseListSep :: AT.Parser ()
parseListSep = parseListSepX <|> parseListSepY
  
-- For when an explicit separator occurs right
-- after the previous code
parseListSepX :: AT.Parser ()
parseListSepX = do
  _ <- AT.satisfy (\x -> x == ',' || x == ';' || x == '|')
  skipHoriz
  return ()

parseListSepY :: AT.Parser ()
parseListSepY = do
  skipHoriz1
  _ <- optional parseListSepX
  return ()

