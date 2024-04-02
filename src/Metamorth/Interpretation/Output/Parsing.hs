module Metamorth.Interpretation.Output.Parsing
  (

  ) where

import Control.Applicative

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS

import Data.Functor

import Data.Attoparsec.Text qualified as AT

import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty(..))

import Data.Char (ord, chr, isSpace, isLower, isAlpha)
import Data.Text qualified as T
import Data.Text (Text)

import Metamorth.Helpers.Char
import Metamorth.Helpers.List
import Metamorth.Helpers.Parsing
import Metamorth.Helpers.Error
import Metamorth.Helpers.Error.RWS

import Metamorth.Interpretation.Output.Parsing.Types
import Metamorth.Interpretation.Output.Types

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

-- A lot of the code here is taken from
-- `Metamorth.Interpretation.Parser.Parsing`.

----------------------------------------------------------------
-- Helper Parsers
----------------------------------------------------------------

-- | Parse a codepoint rendered in the form @U+00DB@ etc...
parseCodepoint :: AT.Parser Char
parseCodepoint = do
  _ <- "U+"
  chr <$> AT.hexadecimal

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
    -- consProd x = AT.anyChar $> x
    'U' -> consProd 'U'
    '*' -> consProd '*'
    '+' -> consProd '+'
    '-' -> consProd '-'
    '%' -> consProd '%'
    's' -> consProd ' '
    y   -> consProd y


-- | Import a trait/group/aspect from the phoneme file.
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

----------------------------------------------------------------
-- State Info Parsers
----------------------------------------------------------------

parseStateDec :: AT.Parser (String, Maybe (S.Set String))
parseStateDec = do
  _ <- "state"
  skipHoriz1
  stateName <- takeIdentifier isLower isFollowId
  skipHoriz
  vals <- optional $ do
    _ <- AT.char ':' <?> "State declaration has no ':'."
    skipHoriz
    let thisPrs = takeIdentifier isAlpha isFollowId
    (AT.sepBy1' thisPrs skipHoriz1) <?> "State declaration has ':' but no options."
  -- parseEndComment
  case vals of
    Nothing -> return ()
    (Just xs) -> when (not $ allUnique xs) $ fail $ "State \"" <> (T.unpack stateName) <> "\" has multiple options with the same name."
  return (T.unpack stateName, (S.fromList . map T.unpack) <$> vals)

-- | Parse a state declaration and add it to
--   the state dictionary.
parseStateDecS :: OutputParser ()
parseStateDecS = do
  (stName, stSet) <- lift parseStateDec
  theMap <- gets opsStateDictionary
  case (M.lookup stName theMap) of
    (Just _) -> mkError $ "Error: state \"" <> stName <> "\" has multiple definitions."
    Nothing  -> do
        let newMap = M.insert stName stSet theMap
        modify $ \x -> x {opsStateDictionary = newMap}

--------------------------------
-- "State-must-be" Parser(s)

parseStateVal :: AT.Parser (String, Text)
parseStateVal = do
  _ <- AT.char '@'
  stName <- takeIdentifier isLower isFollowId
  _ <- AT.char '='
  stVal  <- takeIdentifier isAlpha isFollowId
  return (T.unpack stName, stVal)

{-
parseStateValR :: AT.Parser CharPatternRaw
parseStateValR = do
  (st,val) <- parseStateVal
  return $ ValStateR st (T.unpack val)
-}

parseStateValRS :: OutputParser (Maybe CharPatternRaw)
parseStateValRS = do
  (st, val) <- lift parseStateVal
  let val' = T.unpack val
  stDict <- gets opsStateDictionary
  let mVal = M.lookup st stDict
  case mVal of
    Nothing -> do 
      mkError $ "Couldn't find state name \"" <> st <> "\" in dictionary."
      return Nothing
    (Just Nothing) -> case (checkBoolString val) of
      Nothing -> do 
        mkError $ "Couldn't interpret \"" <> val' <> "\" as boolean value for state \"" <> st <> "\"."
        return Nothing
      (Just bl) -> return $ Just (ValStateR st (Right bl))
    (Just (Just valSet)) -> if (val' `S.member` valSet)
      then (return $ Just $ ValStateR st (Left val'))
      else case (checkBoolString val) of
        Nothing -> do
          mkError $ "Couldn't find \"" <> val' <> "\" as an option for state \"" <> st <> "\"."
          return Nothing
        (Just bl) -> return $ Just (ValStateR st (Right bl))


parseStateValRS' :: OutputParser CharPatternRaw
parseStateValRS' = do
  (cpat, wrtr) <- listens safeHead parseStateValRS
  case cpat of
    Nothing -> case wrtr of
      Nothing    -> fail "Couldn't parse a state-val expression"
      (Just err) -> fail $ show err
    (Just cptrn) -> return cptrn

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

checkBoolString :: T.Text -> Maybe Bool
checkBoolString txt
  | (txt' == "yes" || txt' == "y" || txt' == "t" || txt' == "true"  || txt' == "on" ) = Just True
  | (txt' == "no"  || txt' == "n" || txt' == "f" || txt' == "false" || txt' == "off") = Just False
  | otherwise = Nothing
  where txt' = T.toLower txt

checkOffString :: T.Text -> Maybe Bool
checkOffString txt
  | (txt' == "no"  || txt' == "n" || txt' == "f" || txt' == "false" || txt' == "off") = Just False
  | otherwise = Nothing
  where txt' = T.toLower txt

--------------------------------
-- "Set-state-to" Parser(s)

parseStateSet :: AT.Parser (String, Text)
parseStateSet = do
  _ <- AT.char '!'
  stName <- takeIdentifier isLower isFollowId
  _ <- AT.char '='
  stVal  <- takeIdentifier isAlpha isFollowId
  return (T.unpack stName, stVal)

{-
parseStateSetR :: AT.Parser CharPatternRaw
parseStateSetR = do
  (st,val) <- parseStateSet
  return $ SetStateR st val
-}

parseStateSetRS :: OutputParser (Maybe CharPatternRaw)
parseStateSetRS = do
  (st, val) <- lift parseStateSet
  let val' = T.unpack val
  stDict <- gets opsStateDictionary
  let mVal = M.lookup st stDict
  case mVal of
    Nothing -> do 
      mkError $ "Couldn't find state name \"" <> st <> "\" in dictionary."
      return Nothing
    (Just Nothing) -> case (checkBoolString val) of
      Nothing -> do 
        mkError $ "Couldn't interpret \"" <> val' <> "\" as boolean value for state \"" <> st <> "\"."
        return Nothing
      (Just bl) -> return $ Just (SetStateR st (Right bl))
    (Just (Just valSet)) -> if (val' `S.member` valSet)
      then (return $ Just $ SetStateR st (Left val'))
      -- need to use `checkOffString` since you can't just
      -- set an option string to `On`; you need a value to
      -- set it to.
      else case (checkOffString val) of
        Nothing -> do
          mkError $ "Couldn't find \"" <> val' <> "\" as an option for state \"" <> st <> "\"."
          return Nothing
        (Just bl) -> return $ Just (SetStateR st (Right bl))

parseStateSetRS' :: OutputParser CharPatternRaw
parseStateSetRS' = do
  (cpat, wrtr) <- listens safeHead parseStateSetRS
  case cpat of
    Nothing -> case wrtr of
      Nothing    -> fail "Couldn't parse a state-set expression"
      (Just err) -> fail $ show err
    (Just cptrn) -> return cptrn


----------------------------------------------------------------
-- Case Type parsers
----------------------------------------------------------------

-- | Get the case type of this pattern.
getCaseType :: AT.Parser OutputCase
getCaseType = AT.peekChar' >>= \case
  '+' -> AT.anyChar $> OCMaj
  '-' -> AT.anyChar $> OCMin
  '/' -> AT.anyChar >> AT.peekChar >>= \case
    (Just 'a') -> AT.anyChar $> OCDetectFirst
    (Just 'A') -> AT.anyChar $> OCDetectFirst
    (Just 'z') -> AT.anyChar $> OCDetectLast
    (Just 'Z') -> AT.anyChar $> OCDetectLast
    (Just '0') -> AT.anyChar $> OCDetectLow
    (Just '9') -> AT.anyChar $> OCDetectHigh
    (Just 't') -> AT.anyChar $> OCDetectTitle
    (Just 'T') -> AT.anyChar $> OCDetectTitle
    -- These two will probably be redundant.
    (Just 'i') -> AT.anyChar $> OCDetectIndividual
    (Just 'I') -> AT.anyChar $> OCDetectIndividual
    _          -> return OCNull
  _ -> fail "Not a case type."




----------------------------------------------------------------
-- Multiple Phoneme list parsers
----------------------------------------------------------------


parsePhonemeBrack :: AT.Parser (PhonePatternRaw)
parsePhonemeBrack = do
  _ <- AT.char '('
  skipHoriz
  phoneName <- T.unpack <$> ((takeIdentifier isAlpha isFollowId) <?> "Can't read phoneme name")
  rst <- AT.many' $ do
    skipHoriz1 -- to ensure a space between each value
    T.unpack <$> ((takeIdentifier isAlpha isFollowId) <?> "Can't read aspect value")
  skipHoriz
  _ <- AT.char ')'
  return $ PhoneNameR $ PhoneName phoneName rst

parsePhoneme1 :: AT.Parser PhonePatternRaw
parsePhoneme1 = parsePhonemeBrack <|> parseAlt
  where 
    parseAlt = (\x -> (PhoneNameR $ PhoneName (T.unpack x) [])) <$> ((takeIdentifier isAlpha isFollowId) <?> "Can't read phoneme name")

parsePhonemeList :: AT.Parser (NonEmpty PhonePatternRaw)
parsePhonemeList = do
  skipHoriz
  fstPhone  <- parsePhoneme1
  rstPhones <- AT.many' $ do
    skipHoriz1
    parsePhoneme1
  return (fstPhone :| rstPhones)




