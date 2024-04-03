module Metamorth.Interpretation.Output.Parsing
  (

  ) where

import Control.Applicative
import Control.Arrow ((&&&))

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

-- | Get an import declaration and check
--   that it is a valid import.
getImportS :: OutputParser ImportProperty
getImportS = do
  imp <- lift getImport
  case imp of
    (ImportGroup grpName) -> do
      (thisDict, fullDict) <- gets (opsGroupDictionary &&& opsGroupDictionary')
      if (grpName `elem` thisDict)
        then warn ("Importing group \"" ++ grpName ++ "\" more than once.")
        else if (grpName `elem` fullDict)
          then do
            let newDict = S.insert grpName thisDict
            modify $ \x -> x {opsGroupDictionary = newDict}
          else tellError ("Can't find group \"" ++ grpName ++ "\" in phoneme definitions.")
    (ImportAspect aspName) -> do
      (thisDict, fullDict) <- gets (opsAspectDictionary &&& opsAspectDictionary')
      case (M.lookup aspName thisDict) of
        (Just _) -> warn ("Importing aspect \"" ++ aspName ++ "\" more than once.")
        Nothing  -> case (M.lookup aspName fullDict) of
          Nothing  -> tellError ("Can't find aspect \"" ++ aspName ++ "\" in phoneme definitions.")
          (Just v) -> do
            let newDict = M.insert aspName v thisDict
            modify $ \x -> x {opsAspectDictionary = newDict}
    (ImportTrait trtName) -> do
      (thisDict, fullDict) <- gets (opsTraitDictionary &&& opsTraitDictionary')
      case (M.lookup trtName thisDict) of
        (Just _) -> warn ("Importing trait \"" ++ trtName ++ "\" more than once.")
        Nothing  -> case (M.lookup trtName fullDict) of
          Nothing  -> tellError ("Can't find trait \"" ++ trtName ++ "\" in phoneme definitions.")
          (Just v) -> do
            let newDict = M.insert trtName v thisDict
            modify $ \x -> x {opsTraitDictionary = newDict}
  return imp

-- | Get an import declaration and check
--   that it is a valid import.
getImportS_ :: OutputParser ()
getImportS_ = void getImportS
      

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
    (Just 'a') -> AT.anyChar >> getCaseType' <%> CSFirst
    (Just 'A') -> AT.anyChar >> getCaseType' <%> CSFirst
    (Just 'z') -> AT.anyChar >> getCaseType' <%> CSLast
    (Just 'Z') -> AT.anyChar >> getCaseType' <%> CSLast
    (Just '0') -> AT.anyChar >> getCaseType' <%> CSLow
    (Just '9') -> AT.anyChar >> getCaseType' <%> CSHigh
    (Just 't') -> AT.anyChar >> getCaseType' <%> CSHigh
    (Just 'T') -> AT.anyChar >> getCaseType' <%> CSHigh
    -- These two will probably be redundant.
    (Just 'i') -> AT.anyChar $> OCDetectIndividual
    (Just 'I') -> AT.anyChar $> OCDetectIndividual
    _          -> return OCNull
  _ -> fail "Not a case type."
    
-- | Simplified version of @f <*> (pure x)@.
(<%>) :: Applicative f => f (a -> b) -> a -> f b    
f <%> x = f <*> (pure x)

infixl 4 <%>

-- Simple helper for `getCaseType`.
getCaseType' :: AT.Parser (CaseSource -> OutputCase)
getCaseType' = AT.peekChar >>= \case
  (Just 't') -> AT.anyChar >> return (\x -> OCDetect x CATitle)
  (Just 'T') -> AT.anyChar >> return (\x -> OCDetect x CATitle)
  (Just 'a') -> AT.anyChar >> return (\x -> OCDetect x CAAll)
  (Just 'A') -> AT.anyChar >> return (\x -> OCDetect x CAAll)
  _ -> return $ \x -> OCDetect x CATitle





----------------------------------------------------------------
-- Multiple Phoneme list parsers
----------------------------------------------------------------


parsePhonemeBrack :: AT.Parser PhonePatternRaw
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
parsePhoneme1 = parsePhonemeBrack <|> parseAlt <|> parseSpecPhoneme
  where 
    parseAlt  = (\x -> (PhoneNameR $ PhoneName (T.unpack x) [])) <$> ((takeIdentifier isAlpha isFollowId) <?> "Can't read phoneme name")

-- Parse a character/string representing a special
-- `PhonePatternRaw` value.
parseSpecPhoneme :: AT.Parser PhonePatternRaw
parseSpecPhoneme = AT.peekChar' >>= \case
  '^' -> AT.anyChar $> PhoneAtStartR
  '%' -> AT.anyChar $> PhoneNotStartR
  '$' -> AT.anyChar $> PhoneAtEndR
  '&' -> AT.anyChar $> PhoneNotEndR
  _   -> fail "Can't read phoneme symbol/name."


parsePhonemeList :: AT.Parser (NonEmpty PhonePatternRaw)
parsePhonemeList = do
  skipHoriz
  fstPhone  <- parsePhoneme1
  rstPhones <- AT.many' $ do
    skipHoriz1
    parsePhoneme1
  return (fstPhone :| rstPhones)

parsePhonemeListS :: OutputParser (NonEmpty PhonePatternRaw)
parsePhonemeListS = do
  lst@(phoneFst :| phoneRst) <- lift parsePhonemeList
  let phoneName = showPPRs (NE.toList lst)
  -- Use the phone name when trying the next check.
  phoneLast <- fmap join $ optional $ runOnPhoneme phoneName $ do
    lift skipHoriz1
    parseNextCheckS
  
  case phoneLast of
    Nothing   -> return lst
    (Just pl) -> return (phoneFst :| (phoneRst ++ [pl]))

-- | Parse the next-phoneme string, checking
--   that it matches one of the given groups,
--   traits, aspects, or phonemes.
parseNextCheckS :: OutputParser (Maybe PhonePatternRaw)
parseNextCheckS = do
  (prop, mval) <- lift parseNextCheck
  dicts <- get
  case mval of
    -- If nothing, then following chances,
    -- from most likely to least likely:
    -- Group, Trait, Phoneme, Aspect.
    Nothing -> if (prop `elem` (opsGroupDictionary dicts))
      then return $ Just $ PhoneFollowedByGroupR prop
      else case (M.lookup prop (opsTraitDictionary dicts)) of
        -- Works with either type of trait.
        (Just _) -> return $ Just $ PhoneFollowedByTraitR prop
        Nothing -> if (prop `elem` (opsPhoneDictionary dicts))
          then return $ Just $ PhoneFollowedByPhoneR prop
          else case (M.lookup prop (opsAspectDictionary dicts)) of
            (Just _) -> return $ Just $ PhoneFollowedByAspectR prop
            Nothing  -> do
              phoneName <- ask
              tellError $ "Phoneme \"" ++ phoneName ++ "\" : Couldn't match next-phoneme string: \">" ++ prop ++ "\"."
              return Nothing
    -- There IS a second value here.
    (Just val) -> case (M.lookup prop (opsTraitDictionary dicts)) of
      (Just (Just vs)) -> if (val `elem` vs)
        then return $ Just $ PhoneFollowedByTraitAtR prop val
        else do
          phoneName <- ask
          tellError $ "Phoneme \"" ++ phoneName ++ "\" : Can't find value \"" ++ val ++ "\" for trait \"" ++ prop ++ "\"."
          return Nothing
      (Just _) -> do
          phoneName <- ask
          tellError $ "Phoneme \"" ++ phoneName ++ "\" : Can't find value \"" ++ val ++ "\" for trait \"" ++ prop ++ "\"."
          return Nothing
      Nothing -> case (M.lookup prop (opsAspectDictionary dicts)) of
        (Just vs) -> if (val `elem` vs)
          then return $ Just $ PhoneFollowedByAspectAtR prop val
          else do
            phoneName <- ask
            tellError $ "Phoneme \"" ++ phoneName ++ "\" : Can't find value \"" ++ val ++ "\" for aspect \"" ++ prop ++ "\"."
            return Nothing
        Nothing -> if (prop `elem` (opsGroupDictionary dicts))
          then do
            phoneName <- ask
            tellError $ "Phoneme \"" ++ phoneName ++ "\" : Can't match two strings for a group: \">" ++ prop ++ "=" ++ val ++ "\"."
            return Nothing
          else if (prop `elem` (opsPhoneDictionary dicts))
            then do
              phoneName <- ask
              tellError $ "Phoneme \"" ++ phoneName ++ "\" : Can't match two strings for a phoneme: \">" ++ prop ++ "=" ++ val ++ "\"."
              return Nothing
            else do
              phoneName <- ask
              tellError $ "Phoneme \"" ++ phoneName ++ "\" : Can't match \"next-phoneme\" string : \">" ++ prop ++ "=" ++ val ++ "\"."
              return Nothing

-- At the moment, doesn't work with specific
-- phonemes... but maybe it should? Okay
-- now it (maybe) does.
parseNextCheck :: AT.Parser (String, Maybe String)
parseNextCheck = do
  _ <- AT.char '>'
  skipHoriz
  strProp <- takeIdentifier isAlpha isFollowId
  strVal <- optional $ do
    _ <- AT.char '='
    takeIdentifier isAlpha isFollowId
  return (T.unpack strProp, T.unpack <$> strVal)








