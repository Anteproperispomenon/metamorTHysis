module Metamorth.Interpretation.Phonemes.Parsing
  ( parsePhoneme
  , stParseGroup
  ) where


import Control.Applicative ((<|>), asum, many, some)
import Control.Monad (void, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS

import Data.Char
import Data.Functor (($>))
import Data.Maybe

import Data.Attoparsec.Text qualified as AT

import Data.Map.Strict qualified as M
import Data.Set        qualified as S
import Data.Text       qualified as T

import Metamorth.Interpretation.Phonemes.Types
import Metamorth.Interpretation.Phonemes.Parsing.Types

import Metamorth.Helpers.Parsing




----------------------------------------------------------------
-- Stateful Parsers
----------------------------------------------------------------

--------------------------------
-- Parsing (Sub-)Groups

{-

data PhonemeParsingState
   = PhonemeParsingState
      { psStructure     :: PhonemeParsingStructure
      , psUsedGroups    :: S.Set String
      , psUsedPhones    :: S.Set String
      , psDepth         :: Int
      } deriving (Show, Eq)


stParseGroup :: PhonemeParser (String, PhonemeInventory)
stParseGroup = do
  (depth, groupName) <- lift parseGroup
  pss <- get
  -- Fail if trying to go up a level.
  -- This will cause backtracking as
  -- the parser ascends levels. There's
  -- probably a better way to do this,
  -- but I'm not sure how.
  when (depth >= (psDepth pss)) (fail "Trying to parse a higher level.")
  when (groupName `elem` (psUsedGroups pss)) $ do
    tell ["Group name \'" <> groupName "\' is already in use."]
  let newGNs = S.insert groupName (psUsedGroups pss)
  
  uh <- lift $ (many ((stParsePhoneme <* stParseBlankLine) <|> stParseBlankLine))
     <|> ()

-}

stParseGroup :: Int -> PhonemeParser (String, PhonemeInventory)
stParseGroup dp = do
  groupName <- lift $ parseGroupAt dp
  pss <- get
  -- Fail if trying to go up a level.
  -- This will cause backtracking as
  -- the parser ascends levels. There's
  -- probably a better way to do this,
  -- but I'm not sure how.
  -- when (depth >= (psDepth pss)) (fail "Trying to parse a higher level.")
  when (groupName `elem` (psUsedGroups pss)) $ do
    tell ["Group name \'" <> groupName <> "\' is already in use."]
  let newGNs = S.insert groupName (psUsedGroups pss)
  
  -- Parse all blank/comment lines.
  many stParseBlankLine

  -- This... should work?
  -- In the older version, the parser could
  -- decide on the first branch prematurely 
  -- if the first line was empty/comments.
  phi <-  (PhonemeSet   . M.fromList . catMaybes <$> some (stParsePhoneme <* (many stParseBlankLine)) )
      <|> (PhonemeGroup . M.fromList . catMaybes <$> some ((Just <$> stParseGroup (dp+1)) <* (many (stParseBlankLine $> Nothing) )) )
  
  pure (groupName, phi)  



stParsePhoneme :: PhonemeParser (Maybe (String, PhonemeProperties))
stParsePhoneme = do
  lift skipHoriz
  (phoneName, props) <- lift parsePhoneme
  pss <- get

  when (phoneName `elem` (psUsedPhones pss)) $ do
    tell ["Phoneme name \'" <> phoneName <> "\' is already in use."]
  
  let eprops = validateRawProperties (psStructure pss) phoneName props

  case eprops of
    (Left  errs) -> tell errs $> Nothing
    (Right rsts) -> pure $ Just (phoneName, rsts)

  -- modify $ \pss -> pss { psStructure { ppsPhonemeInventory = } }


stParseBlankLine :: PhonemeParser ()
stParseBlankLine = lift parseEndLine

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

-- | Parses a group only if it's the
--   "correct" depth level.
parseGroupAt :: Int -> AT.Parser String
parseGroupAt dp = do
  skipHoriz
  depth <- T.length <$> AT.takeWhile (== '*')
  when (dp /= depth) $ fail "Unexpected depth level."
  skipHoriz
  groupName <- takeIdentifier isAlpha isFollowId
  skipHoriz
  _ <- AT.option ('_') (AT.char ':')
  skipHoriz
  pure (T.unpack groupName)



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

-- | Parse comments and ends of lines.
parseEndLine :: AT.Parser ()
parseEndLine = do
  skipHoriz
  AT.option () $ do
    _ <- AT.char '#'
    AT.skipWhile (/= '\n')
  AT.endOfLine

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


