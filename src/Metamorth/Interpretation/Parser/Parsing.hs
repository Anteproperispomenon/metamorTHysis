module Metamorth.Interpretation.Parser.Parsing
  (

  ) where

import Control.Applicative

import Data.Attoparsec.Text qualified as AT

import Data.Functor

import Data.Char (ord, chr, isSpace, isLower)
import Data.Text qualified as T
import Data.Text (Text)

import Metamorth.Helpers.Char
import Metamorth.Helpers.Parsing

import Metamorth.Interpretation.Parser.Parsing.Types
import Metamorth.Interpretation.Parser.Types

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

-- Special Characters:

-- + : This is an uppercase sequence
-- - : This is a  lowercase sequence
-- 

--------------------------------
-- Helper Parsers

----------------------------------------------------------------
-- Helper Parsers
----------------------------------------------------------------

-- | Parse a codepoint rendered in the form @U+00DB@ etc...
parseCodepoint :: AT.Parser Char
parseCodepoint = do
  _ <- "U+"
  chr <$> AT.hexadecimal

parseCodepointR :: AT.Parser CharPatternRaw
parseCodepointR = do
  x <- parseCodepoint
  return $ PlainCharR x

parseStartPoint :: AT.Parser CharPatternRaw
parseStartPoint = (AT.char '^') $> WordStartR

parseEndPoint :: AT.Parser CharPatternRaw
parseEndPoint = (AT.char '$') $> WordEndR

-- | Parse an escaped character.
-- 
--   Currently, not many (if any) special
--   characters are escaped, just characters
--   that have a specific meaning.
parseEscaped :: AT.Parser Char
parseEscaped = do
  _ <- AT.char '\\'
  x <- AT.peekChar'
  case x of
    -- Don't consume the space.
    ' ' -> return '\\'
    'U' -> consProd 'U'
    '*' -> consProd '*'
    '+' -> consProd '+'
    '-' -> consProd '-'
    y   -> consProd y

  {- -- older version
  x <- AT.anyChar
  return $ case x of
    'U' -> 'U'
    '*' -> '*'
    '+' -> '+'
    '-' -> '-'
    y   -> y
  -}

-- | For parsing class names in phoneme patterns.
parseClassName :: AT.Parser String
parseClassName = do
  _ <- AT.char '*'
  T.unpack <$> takeIdentifier isLower isFollowId

parseClassNameR :: AT.Parser CharPatternRaw
parseClassNameR = CharClassR <$> parseClassName

-- | The main character selector parser, after
--   all the specialised ones have been run.
parseNonSpace :: AT.Parser Char
parseNonSpace = AT.satisfy (not . isSpace)

parseNonSpaceR :: AT.Parser CharPatternRaw
parseNonSpaceR = PlainCharR <$> parseNonSpace


----------------------------------------------------------------
-- Class Pattern Parsers
----------------------------------------------------------------

parseClassDec :: AT.Parser (String, S.Set Char)
parseClassDec = do
  _ <- "class"
  skipHoriz1
  className <- takeIdentifier isLower isFollowId
  skipHoriz
  _ <- AT.char ':'
  skipHoriz
  -- Previously, there would be an issue here if "\ " occurred
  -- in the text. Now, it is interpreted as "\\ ".
  chrs <- AT.sepBy1' (parseCodepoint <|> parseEscaped <|> parseNonSpace) (skipHoriz1)
  return $ (T.unpack className, S.fromList chrs)



----------------------------------------------------------------
-- Phoneme Pattern Parsers
----------------------------------------------------------------











