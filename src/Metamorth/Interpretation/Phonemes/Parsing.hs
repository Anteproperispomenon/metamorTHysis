module Metamorth.Interpretation.Phonemes.Parsing
  ( parsePhoneme
  ) where

import Control.Applicative ((<|>), asum)
import Control.Monad (void)

import Data.Char

import Data.Attoparsec.Text qualified as AT

import Data.Map.Strict qualified as M
import Data.Text       qualified as T

import Metamorth.Interpretation.Phonemes.Types
import Metamorth.Interpretation.Phonemes.Parsing.Types

import Metamorth.Helpers.Parsing




----------------------------------------------------------------
-- Individual Line Parsers
----------------------------------------------------------------

--------------------------------
-- Parsing (Sub-)Groups

parseGroup :: AT.Parser (Int, String)
parseGroup = do
  skipHoriz
  depth <- T.length <$> AT.takeWhile (== '*')
  skipHoriz
  groupName <- takeIdentifier isAlpha isFollowId
  skipHoriz
  _ <- AT.option ('_') (AT.char ':')
  skipHoriz
  pure (depth, T.unpack groupName)




--------------------------------
-- Parsing Aspects/Traits

parsePropertyLine :: AT.Parser Property
parsePropertyLine = parseAspect <|> parseTrait

parseAspect :: AT.Parser Property
parseAspect = do
  _ <- "aspect"
  skipHoriz1
  aspName <- takeIdentifier isAlpha isFollowId
  skipHoriz

  ops <- AT.sepBy (takeIdentifier isAlpha isFollowId) parseSeparator
  pure $ Aspect (T.unpack aspName) (map T.unpack ops)

parseTrait :: AT.Parser Property
parseTrait = do
  _ <- "trait"
  skipHoriz1
  aspName <- takeIdentifier isAlpha isFollowId
  skipHoriz

  ops <- AT.sepBy (takeIdentifier isAlpha isFollowId) parseSeparator
  pure $ Trait  (T.unpack aspName) (map T.unpack ops)

  

--------------------------------
-- Parsing Phoneme Lines

parsePhoneme :: AT.Parser (String, PhonemePropertiesRaw)
parsePhoneme = do
  skipHoriz
  phoneme <- takeIdentifier isAlpha isFollowId
  skipHoriz
  props <- parseProperties <|> (pure $ PhonemePropertiesRaw [])
  pure (T.unpack phoneme, props)

parseProperties :: AT.Parser PhonemePropertiesRaw
parseProperties = do
  _ <- AT.char ':'
  skipHoriz
  PhonemePropertiesRaw <$> AT.sepBy parseProperty parseSeparator
  
parseProperty :: AT.Parser (String, Maybe String)
parseProperty = do
  prop <- T.unpack <$> takeIdentifier isAlpha isFollowId
  skipHoriz
  (prop,) <$> ((Just <$> parseOption) <|> (pure Nothing))
  

parseOption :: AT.Parser String
parseOption = do
  _ <- AT.char '='
  skipHoriz
  opt <- takeIdentifier isAlpha isFollowId
  return $ T.unpack opt

----------------------------------------------------------------
-- Helper Parsers
----------------------------------------------------------------

isFollowId :: Char -> Bool
isFollowId x = isAlphaNum x || (x == '_') || (x == '-') || (x == '\'')

parseSeparator :: AT.Parser ()
parseSeparator = 
  (skipHoriz1 *> ((takeSeparator *> skipHoriz) <|> (pure ())))
  <|> (takeSeparator *> skipHoriz)

-- An easier, but more backtracking way:
-- (skipHoriz *> takeSeparator *> skipHoriz) <|> skipHoriz1

takeSeparator :: AT.Parser ()
takeSeparator = void $ AT.satisfy (\x -> x == ',' || x == ';')


