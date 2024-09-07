module Metamorth.Interpretation.Output.Parsing
  -- * Main Parsers
  ( parseOutputFile
  -- * Other Parsers
  , parsePhonemeList
  , parsePhonemeListS
  -- * Testing
  , testOutputFile
  , testOutputParser
  -- * Debug
  , parsePhonemePatMulti
  , parsePhonemePatMulti'
  , parsePatternSection
  ) where

import Control.Applicative
import Control.Arrow ((&&&))

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS

import Data.Functor
import Data.Maybe

import Data.Attoparsec.Text qualified as AT

import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty(..))

import Data.Char (ord, chr, isSpace, isLower, isAlpha)
import Data.Text qualified as T
import Data.Text (Text)

import Metamorth.Helpers.Applicative 
import Metamorth.Helpers.Char
import Metamorth.Helpers.List
import Metamorth.Helpers.Parsing
import Metamorth.Helpers.Error ( ParserMessage )
import Metamorth.Helpers.Error.RWS

import Metamorth.Interpretation.Output.Parsing.Trie
import Metamorth.Interpretation.Output.Parsing.Types
import Metamorth.Interpretation.Output.Types
import Metamorth.Interpretation.Output.Types.Alt

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

-- For handling more complex lookahead operations
import Metamorth.Interpretation.Parser.Parsing.Expr
import Metamorth.Interpretation.Parser.Parsing.Boolean

-- A lot of the code here is taken from
-- "Metamorth.Interpretation.Parser.Parsing".

import Metamorth.Interpretation.Shared.Types

----------------------------------------------------------------
-- Main Parser
----------------------------------------------------------------

-- | A quick tester for the output file, that just
--   gives empty sets for the various dictionaries.
testOutputFile :: AT.Parser (OutputParserOutput, OutputHeader, [ParserMessage])
testOutputFile = parseOutputFile S.empty M.empty M.empty S.empty

testOutputParser :: OutputParser x -> T.Text -> Either String (OutputParsingState)
testOutputParser prsr txt = tup2'3 <$> AT.parseOnly (embedOutputParser S.empty M.empty M.empty S.empty prsr) txt
  where tup2'3 (_,y,_) = y

-- | The main runner of the output file.
parseOutputFile 
  -- | The `S.Set` of group names.
  :: S.Set String 
  -- | A `M.Map` from trait names to the values
  --   the trait can take (if applicable).
  -> M.Map String (Maybe (S.Set String)) 
  -- | A `M.Map` from aspect names to the values
  --   the aspect can take.
  -> M.Map String (S.Set String)
  -- | A `S.Set` of the phonemes used for this
  --   output.
  -> S.Set String
  -> AT.Parser (OutputParserOutput, OutputHeader, [ParserMessage])
parseOutputFile grps trts asps phones = do
  (hdr, cas) <- parseHeaderSection
  (_, ops, msgs) <- embedOutputParser grps trts asps phones $ do
    -- Fill out actions here.
    -- (hdr, cas) <- parseHeaderSection
    modify $ \x -> x { opsDefaultCasing = cas }
    parseStateDecSection
    parsePatternSection
    -- return hdr
  let opo = OutputParserOutput
        { opoStateDictionary  = fmap snd $ opsStateDictionary ops
        , opoAutoStates       = getAutoStates $ opsStateDictionary ops
        , opoTraitDictionary  = opsTraitDictionary ops
        , opoGroupDictionary  = opsGroupDictionary ops
        , opoAspectDictionary = opsAspectDictionary ops
        , opoOutputTrie = opsOutputTrie ops
        }
  -- Process the auto-states for the opo.
  return (implementAutoStates opo, hdr, msgs)

getAutoStates :: M.Map String (Bool, Maybe a) -> M.Map String Bool
getAutoStates = (M.map (isJust . snd)) . (M.filter fst)

----------------------------------------------------------------
-- Section Parsers
----------------------------------------------------------------

parseHeaderSection :: AT.Parser (OutputHeader, OutputCase)
parseHeaderSection = do
  many_ parseEndComment
  name1 <- optional (parseHeaderName <* (some_ parseEndComment))
  casity <- AT.option OCNull parseDefaultCase
  some_ parseEndComment
  name2 <- case name1 of
    Nothing  -> optional (parseHeaderName <* (some_ parseEndComment))
    (Just x) -> return (Just x)
  
  let name3 = fromMaybe "" name2
  
  _ <- ("====" <?> "Header: Separator1")
  _ <- ((AT.takeWhile (== '=')) <?> "Header: Separator2")
  parseEndComment
  let finalHeader = OutputHeader {ohOrthName = name3}
  return (finalHeader, casity)

parseDefaultCase :: AT.Parser OutputCase
parseDefaultCase = do
  _ <- "default" 
  skipHoriz1
  _ <- "case"
  skipHoriz
  _ <- (AT.char ':') <|> (AT.char '=')
  skipHoriz
  cas <- getCaseType
  return cas

parseHeaderName :: AT.Parser String
parseHeaderName = do
  skipHoriz
  _ <- "name"
  skipHoriz
  (AT.char ':') <|> (AT.char '=') <|> (AT.char ' ') <|> (AT.char '\t')
  skipHoriz
  -- hmm...s
  T.unpack <$> takeIdentifier isAlpha isFollowId


-- | Parse the class declaration section.
--   This also includes states.
parseStateDecSection :: OutputParser ()
parseStateDecSection = do
  -- lift AT.skipSpace
  _ <- lift $ many parseEndComment
  many'_ $ do
    getImportS_ <|> parseStateDecS -- <|> parseUnspecifiedClassOrState
    lift $ AT.many1 parseEndComment
  _ <- lift $ many  parseEndComment
  _ <- lift ("====" <?> "States: Separator1")
  _ <- lift ((AT.takeWhile (== '=')) <?> "States: Separator2")
  lift parseEndComment

parsePatternSection :: OutputParser ()
parsePatternSection = do
  many_ $ lift parseEndComment
  AT.many1' $ do
    parsePhonemePatMulti
    AT.many1' $ lift parseEndComment
  return ()
  -- hmm...

----------------------------------------------------------------
-- Helper Parsers
----------------------------------------------------------------

-- | Parse a codepoint rendered in the form @U+00DB@ etc...
parseCodepoint :: AT.Parser Char
parseCodepoint = do
  _ <- "U+"
  chr <$> AT.hexadecimal

parseCodepointRS :: OutputParser CharPatternRaw
parseCodepointRS = PlainCharR <$> lift parseCodepoint

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

parseEscapedRS :: OutputParser CharPatternRaw
parseEscapedRS = PlainCharR <$> lift parseEscaped

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
--   Moved to Metamorth.Interpretation.Shared.Types
-- getImport :: AT.Parser ImportProperty
-- getImport = do
--   _ <- "import"
--   skipHoriz1
--   getAspect <|> getTrait <|> getGroup
-- 
-- getAspect :: AT.Parser ImportProperty
-- getAspect = do
--   _ <- "aspect"
--   skipHoriz1
--   ImportAspect . T.unpack <$> takeIdentifier isAlpha isFollowId
-- 
-- getTrait :: AT.Parser ImportProperty
-- getTrait = do
--   _ <- "trait"
--   skipHoriz1
--   ImportTrait . T.unpack <$> takeIdentifier isAlpha isFollowId
-- 
-- getGroup :: AT.Parser ImportProperty
-- getGroup = do
--   _ <- "group"
--   skipHoriz1
--   ImportGroup . T.unpack <$> takeIdentifier isAlpha isFollowId

----------------------------------------------------------------
-- State Info Parsers
----------------------------------------------------------------

parseStateDec :: AT.Parser (String, Bool, Maybe (S.Set String))
parseStateDec = do
  isAuto <- optionalBool $ do
    parseAutoOff
    void "-" <|> skipHoriz1 -- to allow "auto-state" etc...
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
  return (T.unpack stateName, isAuto, (S.fromList . map T.unpack) <$> vals)

-- | Parse a state declaration and add it to
--   the state dictionary.
parseStateDecS :: OutputParser ()
parseStateDecS = do
  (stName, stIsAuto, stSet) <- lift parseStateDec
  theMap <- gets opsStateDictionary
  case (M.lookup stName theMap) of
    (Just _) -> mkError $ "Error: state \"" <> stName <> "\" has multiple definitions."
    Nothing  -> do
        let newMap = M.insert stName (stIsAuto, stSet) theMap
        modify $ \x -> x {opsStateDictionary = newMap}

-- | Parse the prefix for states to indicate that they
--   are auto-off.
parseAutoOff :: AT.Parser ()
parseAutoOff = void
  ("auto-off" <|> "auto" <|> "short" <|> "off")


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

{-
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

-}

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
    Just (_isAuto, Nothing) -> case (checkBoolString val) of
      Nothing -> do 
        mkError $ "Couldn't interpret \"" <> val' <> "\" as boolean value for state \"" <> st <> "\"."
        return Nothing
      (Just bl) -> return $ Just (SetStateR st (Right bl))
    Just (_isAuto, (Just valSet)) -> if (val' `S.member` valSet)
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

getCaseTypeS :: OutputParser OutputCase
getCaseTypeS = do
  mct <- optional $ lift getCaseType
  case mct of
    (Just ct) -> return ct
    Nothing   -> gets opsDefaultCasing

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
    (Just '?') -> AT.anyChar >> OCDetectSep  <$> getCaseTypeZ
    -- These two will probably be redundant.
    -- (Just 'i') -> AT.anyChar $> OCDetectIndividual
    -- (Just 'I') -> AT.anyChar $> OCDetectIndividual
    _          -> return OCNull
  _ -> fail "Not a case type."
    
-- | Simplified version of @f <*> (pure x)@.
-- (<%>) :: Applicative f => f (a -> b) -> a -> f b    
-- f <%> x = f <*> (pure x)
-- 
-- infixl 4 <%>

-- Simple helper for `getCaseType`.
getCaseType' :: AT.Parser (CaseSource -> OutputCase)
getCaseType' = AT.peekChar >>= \case
  (Just 't') -> AT.anyChar >> return (\x -> OCDetect x CATitle)
  (Just 'T') -> AT.anyChar >> return (\x -> OCDetect x CATitle)
  (Just 'a') -> AT.anyChar >> return (\x -> OCDetect x CAAll)
  (Just 'A') -> AT.anyChar >> return (\x -> OCDetect x CAAll)
  (Just 'u') -> AT.anyChar >> return (\x -> OCDetect x CAExactUpper)
  (Just 'U') -> AT.anyChar >> return (\x -> OCDetect x CAExactUpper)
  (Just '+') -> AT.anyChar >> return (\x -> OCDetect x CAExactUpper)
  (Just 'l') -> AT.anyChar >> return (\x -> OCDetect x CAExactLower)
  (Just 'L') -> AT.anyChar >> return (\x -> OCDetect x CAExactLower)
  (Just '-') -> AT.anyChar >> return (\x -> OCDetect x CAExactLower)
  _ -> return $ \x -> OCDetect x CATitle

getCaseTypeZ :: AT.Parser CaseSource
getCaseTypeZ = AT.peekChar >>= \case
  (Just 'a') -> AT.anyChar $> CSFirst
  (Just 'A') -> AT.anyChar $> CSFirst
  (Just 'z') -> AT.anyChar $> CSLast
  (Just 'Z') -> AT.anyChar $> CSLast
  (Just '0') -> AT.anyChar $> CSLow
  (Just '9') -> AT.anyChar $> CSHigh
  (Just 't') -> AT.anyChar $> CSHigh
  (Just 'T') -> AT.anyChar $> CSHigh
  _ -> return CSFirst

----------------------------------------------------------------
-- Phoneme list parsers
----------------------------------------------------------------


parsePhonemeBrack :: AT.Parser PhonePatternRaw
parsePhonemeBrack = do
  _ <- AT.char '('
  skipHoriz
  phoneName <- T.unpack <$> ((takeIdentifier isAlpha isFollowId) <?> "Can't read phoneme name")
  rst <- AT.many' $ do
    skipHoriz1 -- to ensure a space between each value
    T.unpack <$> (takeAspConstructor <?> "Can't read aspect value")
  skipHoriz
  _ <- AT.char ')'
  return $ PhoneNameR $ PhoneName phoneName rst

takeAspConstructor :: AT.Parser T.Text
takeAspConstructor =
  -- Converting all wildcards to '*'s so that
  -- they get compared as the same.
  takeIdentifier isAlpha isFollowId <|> "*" <|> ("_" $> "*")

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

parsePhonemeListS' :: OutputParser (NonEmpty PhonePatternRaw)
parsePhonemeListS' = do
  lift skipHoriz
  fstPhone  <- (lift parsePhoneme1) <|> parseStateValRSX'
  rstPhones <- AT.many' $ do
    lift skipHoriz1
    (lift parsePhoneme1) <|> parseStateValRSX'
  return (fstPhone :| rstPhones)

parsePhonemeListS :: OutputParser (NonEmpty PhonePatternRaw)
parsePhonemeListS = do
  lst@(phoneFst :| phoneRst) <- parsePhonemeListS'
  let phoneName = showPPRs (NE.toList lst)
  -- Use the phone name when trying the next check.
  phoneLast <- fmap join $ optional $ runOnPhoneme phoneName $ do
    lift skipHoriz1
    -- sequenceA :: (Traversable t) => t (Maybe a) -> Maybe (t a)
    sequenceA <$> parseNextCheckSB
  
  case phoneLast of
    Nothing   -> return lst
    (Just pl) -> return (phoneFst :| (phoneRst ++ [PhoneFollowR pl]))

parseNextCheckB :: AT.Parser (Boolean2 (String, Maybe String))
parseNextCheckB  = do
  _ <- AT.char '>'
  skipHoriz
  parseBooleanExpr $ do
    strProp <- takeIdentifier isAlpha isFollowId
    strVal <- optional $ do
      _ <- AT.char '='
      takeIdentifier isAlpha isFollowId
    return (T.unpack strProp, T.unpack <$> strVal)

-- | Parse the next-phoneme string, checking
--   that it matches one of the given groups,
--   traits, aspects, or phonemes. Old version
--   only around for compatibility.
parseNextCheckS :: OutputParser (Maybe PhoneFollow)
parseNextCheckS = do
  mval <- parseNextCheckSB
  case mval of
    (PlainB2 x) -> return x
    _ -> return Nothing

-- | Parse the next-phoneme string, checking
--   that it matches one of the given groups,
--   traits, aspects, or phonemes.
parseNextCheckSB :: OutputParser (Boolean2 (Maybe PhoneFollow))
parseNextCheckSB = do
  -- (prop, mval) <- lift parseNextCheck
  dicts <- get
  props <- lift parseNextCheckB
  -- NOTE: May want to change the order of the
  -- error messages, in case the user is explicitly
  -- NOT importing a group/aspect/trait that overlaps
  -- with another value. Unlikely, but possible.
  forM props $ \(prop, mval) -> do
    case mval of
      -- If nothing, then following chances,
      -- from most likely to least likely:
      -- Group, Trait, Phoneme, Aspect.
      Nothing -> if (prop `elem` (opsGroupDictionary dicts))
        then return $ Just $ PhoneFollowedByGroup prop
        else if (prop `elem` (opsGroupDictionary' dicts))
          then do 
            tellError $ "Group \"" ++ prop ++ "\" has not been imported;\nAdd \"import group " ++ prop ++ "\" to the class/state/import section to import it."
            return Nothing
          else case (M.lookup prop (opsTraitDictionary dicts)) of
            -- Works with either type of trait.
            (Just _) -> return $ Just $ PhoneFollowedByTrait prop
            Nothing -> case (M.lookup prop (opsTraitDictionary' dicts)) of
              (Just _) -> do
                tellError $ "Trait \"" ++ prop ++ "\" has not been imported;\nAdd \"import trait " ++ prop ++ "\" to the class/state/import section to import it."
                return Nothing
              Nothing -> if (prop `elem` (opsPhoneDictionary dicts))
                then return $ Just $ PhoneFollowedByPhone prop
                else case (M.lookup prop (opsAspectDictionary dicts)) of
                  (Just _) -> return $ Just $ PhoneFollowedByAspect prop
                  Nothing  -> case (M.lookup prop (opsAspectDictionary' dicts)) of
                    (Just _) -> do
                      tellError $ "Aspect \"" ++ prop ++ "\" has not been imported;\nAdd \"import aspect " ++ prop ++ "\" to the class/state/import section to import it."
                      return Nothing
                    Nothing -> do
                      phoneName <- ask
                      tellError $ "Phoneme \"" ++ phoneName ++ "\" : Couldn't match next-phoneme string: \">" ++ prop ++ "\"."
                      return Nothing
      -- There IS a second value here in this case.
      -- Therefore it should only be a property
      -- that can take on a second value, e.g. a
      -- trait or an aspect.
      (Just val) -> case (M.lookup prop (opsTraitDictionary' dicts)) of
        (Just (Just vs)) -> if (val `elem` vs)
          then if (isJust ((M.lookup prop (opsTraitDictionary dicts)))) 
            then return $ Just $ PhoneFollowedByTraitAt prop val
            else do
                tellError $ "Trait \"" ++ prop ++ "\" has not been imported (v1);\nAdd \"import trait " ++ prop ++ "\" to the class/state/import section to import it."
                return Nothing
          else do
            phoneName <- ask
            when (isNothing $ M.lookup prop (opsTraitDictionary dicts)) $ 
              tellError $ "Trait \"" ++ prop ++ "\" has not been imported (v2);\nAdd \"import trait " ++ prop ++ "\" to the class/state/import section to import it."
            tellError $ "Phoneme \"" ++ phoneName ++ "\" : Can't find value \"" ++ val ++ "\" for trait \"" ++ prop ++ "\"."
            return Nothing
        (Just _) -> do
            phoneName <- ask
            when (isNothing $ M.lookup prop (opsTraitDictionary dicts)) $ 
              tellError $ "Trait \"" ++ prop ++ "\" has not been imported (v3);\nAdd \"import trait " ++ prop ++ "\" to the class/state/import section to import it."
            tellError $ "Phoneme \"" ++ phoneName ++ "\" : Can't find value \"" ++ val ++ "\" for trait \"" ++ prop ++ "\"."
            return Nothing
        Nothing -> case (M.lookup prop (opsAspectDictionary' dicts)) of
          (Just vs) -> if (val `elem` vs)
            then if (isJust ((M.lookup prop (opsAspectDictionary dicts))))
              then return $ Just $ PhoneFollowedByAspectAt prop val
              else do
                tellError $ "Aspect \"" ++ prop ++ "\" has not been imported;\nAdd \"import aspect " ++ prop ++ "\" to the class/state/import section to import it."
                return Nothing
            else do
              phoneName <- ask
              when (prop `notElem` (M.keysSet $ opsAspectDictionary dicts)) $
                tellError $ "Aspect \"" ++ prop ++ "\" has not been imported;\nAdd \"import aspect " ++ prop ++ "\" to the class/state/import section to import it."
              tellError $ "Phoneme \"" ++ phoneName ++ "\" : Can't find value \"" ++ val ++ "\" for aspect \"" ++ prop ++ "\"."
              return Nothing
          Nothing -> if (prop `elem` (opsGroupDictionary' dicts))
            then do
              phoneName <- ask
              when (prop `notElem` (opsGroupDictionary dicts)) $
                tellError $ "Group \"" ++ prop ++ "\" has not been imported;\nAdd \"import group " ++ prop ++ "\" to the class/state/import section to import it."
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

{-
parseStateValR :: AT.Parser CharPatternRaw
parseStateValR = do
  (st,val) <- parseStateVal
  return $ ValStateR st (T.unpack val)
-}

parseStateValRSX :: OutputParser (Maybe PhonePatternRaw)
parseStateValRSX = do
  (st, val) <- lift parseStateVal
  let val' = T.unpack val
  stDict <- gets opsStateDictionary
  let mVal = M.lookup st stDict
  case mVal of
    Nothing -> do 
      mkError $ "Couldn't find state name \"" <> st <> "\" in dictionary."
      return Nothing
    Just (_isAuto, Nothing) -> case (checkBoolString val) of
      Nothing -> do 
        mkError $ "Couldn't interpret \"" <> val' <> "\" as boolean value for state \"" <> st <> "\"."
        return Nothing
      (Just bl) -> return $ Just (PhoneValStateR st (Right bl))
    Just (_isAuto, (Just valSet)) -> if (val' `S.member` valSet)
      then (return $ Just $ PhoneValStateR st (Left val'))
      else case (checkBoolString val) of
        Nothing -> do
          mkError $ "Couldn't find \"" <> val' <> "\" as an option for state \"" <> st <> "\"."
          return Nothing
        (Just bl) -> return $ Just (PhoneValStateR st (Right bl))

parseStateValRSX' :: OutputParser PhonePatternRaw
parseStateValRSX' = do
  (cpat, wrtr) <- listens safeHead parseStateValRSX
  case cpat of
    Nothing -> case wrtr of
      Nothing    -> fail "Couldn't parse a state-val expression"
      (Just err) -> fail $ show err
    (Just cptrn) -> return cptrn

----------------------------------------------------------------
-- Character list parsers
----------------------------------------------------------------

-- parseStateSetRS' :: OutputParser CharPatternRaw

-- | The main character selector parser, after
--   all the specialised ones have been run.
parseNonSpace :: AT.Parser Char
parseNonSpace = AT.satisfy isNonSpace

isNonSpace :: Char -> Bool
isNonSpace = \x -> (not $ isSpace x) && (x /= '#') && (x /= '@') && (x /= '!')

isNonSpace' :: Char -> Bool
isNonSpace' = \x -> (not $ isSpace x) && (x /= '#') && (x /= '@') && (x /= '!') && (x /= '*')

parseNonSpaceR :: AT.Parser CharPatternRaw
parseNonSpaceR = PlainCharR <$> parseNonSpace

parseNonSpaceRS :: OutputParser CharPatternRaw
parseNonSpaceRS = lift parseNonSpaceR


parseMultiNonSpace :: AT.Parser (Either T.Text Char)
parseMultiNonSpace = do
  c1 <- AT.satisfy isNonSpace'
  nxt <- AT.peekChar
  case nxt of
    Nothing  -> return $ Right c1
    (Just x) -> case x of
      y | isNonSpace' y -> Left . (T.cons c1) <$> AT.takeWhile1 isNonSpace
      _ -> return $ Right c1

parseMultiNonSpaceS :: OutputParser (Either T.Text Char)
parseMultiNonSpaceS = do
  rslt <- lift parseMultiNonSpace
  case rslt of
    (Left txt) -> do
      phone <- ask
      warn $ "Phoneme \"" <> phone <> "\" is missing spaces in one of its patterns: ... " <> (T.unpack txt) <> " ..."
      return rslt
    _ -> return rslt

parseMultiNonSpaceRS :: OutputParser [CharPatternRaw]
parseMultiNonSpaceRS = do
  rslt <- parseMultiNonSpaceS
  case rslt of
    (Left txt) -> return $ map PlainCharR $ T.unpack txt
    (Right ch) -> return   [PlainCharR ch]

mkList :: (Functor f) => f a -> f [a]
mkList fx = (:[]) <$> fx

----------------------------------------------------------------
-- Full Pattern parsers
----------------------------------------------------------------

parsePhonemePatMulti :: OutputParser ()
parsePhonemePatMulti = do
  pr <- parsePhonemePatMulti'
  -- return ()
  addOutputPattern pr

parsePhonemePatMulti' :: OutputParser ([PhonePattern], OutputPattern)
parsePhonemePatMulti' = do
  phones <- parsePhonemeListS
  lift skipHoriz
  let phoneName = showPPRs $ NE.toList phones
  _ <- lift ((AT.char ':') <?> ("Phoneme pattern for \"" <> phoneName <> "\" is missing ':'."))
  lift skipHoriz
  -- hmm...
  theCase <- getCaseTypeS
  
  -- Set the internal phoneme name to phoneName.
  runOnPhoneme phoneName $ do
    thePats <- concat <$> AT.sepBy1' 
      ( (mkList parseCodepointRS )
        -- <|> (mkList parseClassNameRS)
        <|> (mkList parseEscapedRS  )
        <|> (parseMultiNonSpaceRS   )
        <|> (mkList parseNonSpaceRS ) -- this will consume almost any individual `Char`, so it must go (almost) last.
        -- These two should probably be combined into one function for better errors.
        -- <|> (mkList parseStateValRS')
        <|> (mkList parseStateSetRS')
      ) 
      (lift skipHoriz1)
    -- hmm...
    -- return ()
    
    sdict <- gets (M.map snd . opsStateDictionary)
    let  ePhonePats = validatePhonePattern sdict (NE.toList phones)
    case ePhonePats of
      (Left  errs) -> do 
        mkErrors $ map (\err -> "Error with phoneme pattern for \"" ++ phoneName ++ "\": " ++ err) [errs]
        return ([], OutputPattern (CharPattern [] []) OCNull)
      (Right phonePats) -> -- addPhonemesPattern phones rslt
        case (validateCharPattern sdict thePats) of
          (Left  errs) -> do 
            mkErrors $ map (\err -> "Error with phoneme pattern for \"" ++ phoneName ++ "\": " ++ err) [errs]
            return ([], OutputPattern (CharPattern [] []) OCNull)
          (Right cPats) -> return (phonePats, OutputPattern cPats theCase)


-- testing:

-- :m + Metamorth.Interpretation.Output.Parsing Metamorth.Helpers.IO
-- import Data.Attoparsec.Text qualified as AT
-- AT.parseOnly testOutputFile <$> readFileUTF8 "examples/output/example_inuktitut_01.thyo"
-- read errors:
-- (either (const []) snd) . AT.parseOnly testOutputFile <$> readFileUTF8 "examples/output/example_inuktitut_01.thyo"





