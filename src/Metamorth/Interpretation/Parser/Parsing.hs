module Metamorth.Interpretation.Parser.Parsing
  ( parseOrthographyFile
  , parseOrthographyFileNew
  ) where

import Control.Applicative
import Control.Arrow ((&&&))

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS

import Data.Attoparsec.Text qualified as AT

import Data.List (groupBy, intercalate)
import Data.Functor
import Data.Bifunctor

import Data.Char (ord, chr, isSpace, isLower, isAlpha)
import Data.Text qualified as T
import Data.Text (Text)
import Data.Maybe (isNothing, isJust)

import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty(..))

import Metamorth.Helpers.Char
import Metamorth.Helpers.List
import Metamorth.Helpers.Parsing

import Metamorth.Interpretation.Parser.Parsing.Types
import Metamorth.Interpretation.Parser.Types

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

import Metamorth.Interpretation.Shared.Types

-- Special Characters:

-- + : This is an uppercase sequence
-- - : This is a  lowercase sequence
-- 

-- TODO: Allow comments in data.
-- Will use '#' to indicate comments.

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

parseCodepointRS :: ParserParser CharPatternRaw
parseCodepointRS = lift parseCodepointR

parseStartPoint :: AT.Parser CharPatternRaw
parseStartPoint = (AT.char '^') $> WordStartR

parseEndPoint :: AT.Parser CharPatternRaw
parseEndPoint = (AT.char '$') $> WordEndR

parseNotStart :: AT.Parser CharPatternRaw
parseNotStart = (AT.char '%') $> NotStartR

parseNotEnd :: AT.Parser CharPatternRaw
parseNotEnd = (AT.char '&') $> NotEndR

parseStartPointS :: ParserParser CharPatternRaw
parseStartPointS = lift parseStartPoint

parseEndPointS :: ParserParser CharPatternRaw
parseEndPointS = lift parseEndPoint

parseNotStartS :: ParserParser CharPatternRaw
parseNotStartS = lift parseNotStart

parseNotEndS :: ParserParser CharPatternRaw
parseNotEndS = lift parseNotEnd

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
    '~' -> consProd '~'
    '!' -> consProd '!'
    '@' -> consProd '@'
    '>' -> consProd '>'
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

parseEscapedR :: AT.Parser CharPatternRaw
parseEscapedR = PlainCharR <$> parseEscaped

parseEscapedRS :: ParserParser CharPatternRaw
parseEscapedRS = lift parseEscapedR

-- | For parsing class names in phoneme patterns.
parseClassName :: AT.Parser String
parseClassName = do
  _ <- AT.char '*'
  T.unpack <$> takeIdentifier isLower isFollowId

parseClassNameR :: AT.Parser CharPatternRaw
parseClassNameR = CharClassR <$> parseClassName

-- | Parse a class name and raise an error
--   if the class hasn't been defined.
parseClassNameRS :: ParserParser CharPatternRaw
parseClassNameRS = do
  -- hmm...
  nm <- lift $ parseClassName
  theMap <- gets ppsClassDictionary
  case (M.lookup nm theMap) of
    Nothing -> do
        phoneName <- ask
        mkError $ "Error with phoneme \"" <> phoneName <> "\": Calls for undefined class \"" <> nm <> "\"."
        return $ CharClassR nm
    (Just _) -> return $ CharClassR nm

-- | The main character selector parser, after
--   all the specialised ones have been run.
parseNonSpace :: AT.Parser Char
parseNonSpace = AT.satisfy isNonSpace

isNonSpace :: Char -> Bool
isNonSpace = \x -> (not $ isSpace x) && (x /= '#') && (x /= '@') && (x /= '!')

isNonSpace' :: Char -> Bool
isNonSpace' = \x -> (not $ isSpace x) && (x /= '#') && (x /= '@') && (x /= '!') && (x /= '*') && (x /= '>')

parseNonSpaceR :: AT.Parser CharPatternRaw
parseNonSpaceR = PlainCharR <$> parseNonSpace

parseNonSpaceRS :: ParserParser CharPatternRaw
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

parseMultiNonSpaceS :: ParserParser (Either T.Text Char)
parseMultiNonSpaceS = do
  rslt <- lift parseMultiNonSpace
  case rslt of
    (Left txt) -> do
      phone <- ask
      warn $ "Phoneme \"" <> phone <> "\" is missing spaces in one of its patterns: ... " <> (T.unpack txt) <> " ..."
      return rslt
    _ -> return rslt

parseMultiNonSpaceRS :: ParserParser [CharPatternRaw]
parseMultiNonSpaceRS = do
  rslt <- parseMultiNonSpaceS
  case rslt of
    (Left txt) -> return $ map PlainCharR $ T.unpack txt
    (Right ch) -> return $ [PlainCharR ch]

mkList :: (Functor f) => f a -> f [a]
mkList fx = (:[]) <$> fx



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

parseStateValRS :: ParserParser (Maybe CharPatternRaw)
parseStateValRS = do
  (st, val) <- lift parseStateVal
  let val' = T.unpack val
  stDict <- gets ppsStateDictionary
  let mVal = M.lookup st stDict
  case mVal of
    Nothing -> do 
      mkError $ "Couldn't find state name \"" <> st <> "\" in dictionary."
      return Nothing
    (Just (_, Nothing)) -> case (checkBoolString val) of
      Nothing -> do 
        mkError $ "Couldn't interpret \"" <> val' <> "\" as boolean value for state \"" <> st <> "\"."
        return Nothing
      (Just bl) -> return $ Just (ValStateR st (Right bl))
    (Just (_, Just valSet)) -> if (val' `S.member` valSet)
      then (return $ Just $ ValStateR st (Left val'))
      else case (checkBoolString val) of
        Nothing -> do
          mkError $ "Couldn't find \"" <> val' <> "\" as an option for state \"" <> st <> "\"."
          return Nothing
        (Just bl) -> return $ Just (ValStateR st (Right bl))


parseStateValRS' :: ParserParser CharPatternRaw
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

parseStateSetRS :: ParserParser (Maybe CharPatternRaw)
parseStateSetRS = do
  (st, val) <- lift parseStateSet
  let val' = T.unpack val
  stDict <- gets ppsStateDictionary
  let mVal = M.lookup st stDict
  case mVal of
    Nothing -> do 
      mkError $ "Couldn't find state name \"" <> st <> "\" in dictionary."
      return Nothing
    (Just (_, Nothing)) -> case (checkBoolString val) of
      Nothing -> do 
        mkError $ "Couldn't interpret \"" <> val' <> "\" as boolean value for state \"" <> st <> "\"."
        return Nothing
      (Just bl) -> return $ Just (SetStateR st (Right bl))
    (Just (_, Just valSet)) -> if (val' `S.member` valSet)
      then (return $ Just $ SetStateR st (Left val'))
      -- need to use `checkOffString` since you can't just
      -- set an option string to `On`; you need a value to
      -- set it to.
      else case (checkOffString val) of
        Nothing -> do
          mkError $ "Couldn't find \"" <> val' <> "\" as an option for state \"" <> st <> "\"."
          return Nothing
        (Just bl) -> return $ Just (SetStateR st (Right bl))

parseStateSetRS' :: ParserParser CharPatternRaw
parseStateSetRS' = do
  (cpat, wrtr) <- listens safeHead parseStateSetRS
  case cpat of
    Nothing -> case wrtr of
      Nothing    -> fail "Couldn't parse a state-set expression"
      (Just err) -> fail $ show err
    (Just cptrn) -> return cptrn


--------------------------------
-- Special Char Parsers

parseSpecials :: AT.Parser [Char] 
parseSpecials = AT.sepBy (AT.satisfy (\x -> x == '+' || x == '-' || x == '~')) skipHoriz

parseSpecialsS :: ParserParser [Char]
parseSpecialsS = lift parseSpecials

----------------------------------------------------------------
-- Class Pattern/Declaration Parsers
----------------------------------------------------------------

-- | Parse a class declaration.
parseClassDec :: AT.Parser (String, S.Set Char)
parseClassDec = do
  _ <- "class"
  skipHoriz1
  className <- takeIdentifier isLower isFollowId
  skipHoriz
  _ <- AT.char ':' <?> "Class declaration has no ':'."
  skipHoriz
  -- Previously, there would be an issue here if "\ " occurred
  -- in the text. Now, it is interpreted as "\\ ".
  -- TODO : Check for characters with no spaces between them
  -- and report a warning.
  chrs <- AT.sepBy1' (parseCodepoint <|> parseEscaped <|> parseNonSpace) (skipHoriz1)
  return $ (T.unpack className, S.fromList chrs)

-- | Parse a class declaration and add it to
--   the class dictionary.
parseClassDecS :: ParserParser ()
parseClassDecS = do
  (clName, clSet) <- lift $ parseClassDec
  theMap <- gets ppsClassDictionary
  case (M.lookup clName theMap) of
    (Just _) -> mkError $ "Error: class \"" <> clName <> "\" has multiple definitions."
    Nothing  -> do
        let newMap = M.insert clName clSet theMap
        modify $ \x -> x {ppsClassDictionary = newMap}

-- | Parse the class declaration section.
--   This also includes states.
parseClassDecSection :: ParserParser ()
parseClassDecSection = do
  -- lift AT.skipSpace
  _ <- lift $ many parseEndComment
  _ <- AT.many' $ do
    parseClassDecS <|> parseStateDecS <|> parseUnspecifiedClassOrState
    lift $ some_ parseEndComment
  _ <- lift $ many  parseEndComment
  _ <- lift ("====" <?> "Classes: Separator1")
  _ <- lift ((AT.takeWhile (== '=')) <?> "Classes: Separator2")
  lift parseEndComment

-- | Parse the class declaration section.
--   This also includes states and imports.
parseClassDecSectionNew :: ParserParser ()
parseClassDecSectionNew = do
  -- lift AT.skipSpace
  _ <- lift $ many parseEndComment
  _ <- AT.many' $ do
    parseClassDecS <|> parseStateDecS <|> getImportS_ <|> parseUnspecifiedClassOrState
    lift $ some_ parseEndComment
  _ <- lift $ many  parseEndComment
  _ <- lift ("====" <?> "Classes: Separator1")
  _ <- lift ((AT.takeWhile (== '=')) <?> "Classes: Separator2")
  lift parseEndComment


-- | Parse a class/state that's missing the
--   "class"/"state" keyword at the beginning.
--   This is to improve error messages.
parseUnspecifiedClassOrState :: ParserParser ()
parseUnspecifiedClassOrState = do
  lift skipHoriz
  thingName <- T.unpack <$> lift (takeIdentifier isLower isFollowId)
  lift skipHoriz
  c <- lift AT.peekChar'
  case c of
    ':' -> do
      lift $ void AT.anyChar
      lift skipHoriz
      (txt, bl) <- lift $ AT.match classOrStateDecider'
      let outp = case bl of
                  (Right  True) -> "  class " <> thingName <> " : " <> (T.unpack txt)
                  (Right False) -> "  state " <> thingName <> " : " <> (T.unpack txt)
                  (Left   True) -> "  class/state " <> thingName <> " : " <> (T.unpack txt)
                  (Left  False) -> "  state " <> thingName
      mkError $ "Unspecified declaration \"" <> thingName <> "\" in Class/State section; did you mean to write\n\n" <> outp <> "\n"
      -- lift parseEndComment
    '#' -> do
      mkError $ "Unspecified declaration \"" <> thingName <> "\" in Class/State section; did you mean to write\n\n  state " <> thingName <> "\n"
      -- lift parseEndComment
    '\n' -> do
      mkError $ "Unspecified declaration \"" <> thingName <> "\" in Class/State section; did you mean to write\n\n  state " <> thingName <> "\n"
      -- lift parseEndComment
    _ -> do
      txt <- lift $ AT.takeWhile (\x -> (x /= '#') && (x /= '\n'))
      mkError $ "Unspecified declaration in Class/State section:\n\n" <> thingName <> " " <> (T.unpack txt) <> "\n"
      -- lift parseEndComment

-- see also
-- parseCodepoint
-- parseEscaped
singleChar :: AT.Parser Char
singleChar = do
  c <- AT.satisfy (\x -> (x /= '#') && (x /= '\n'))
  m <- AT.peekChar
  case m of
    Nothing  -> return c
    (Just x) -> if (isSpace x) then (return c) else (fail "not a space")

singleChar' :: AT.Parser (Maybe Bool)
singleChar' = do
  _ <- singleChar
  return Nothing

classChar :: AT.Parser (Maybe Bool)
classChar = do 
  _ <- parseCodepoint <|> parseEscaped
  return (Just True)

stateVal :: AT.Parser (Maybe Bool)
stateVal = do
  txt <- takeIdentifier isLower isFollowId
  return $ if (T.length txt > 1) then (Just False) else Nothing

classOrStateSub :: AT.Parser (Maybe Bool)
classOrStateSub = singleChar' <|> classChar <|> stateVal

classOrStateDecider :: AT.Parser (Maybe Bool)
classOrStateDecider = do 
  firstJust <$> AT.sepBy1' classOrStateSub skipHoriz1

classOrStateDecider' :: AT.Parser (Either Bool Bool)
classOrStateDecider' = do
  rslt <- optional classOrStateDecider
  return $ case rslt of
    Nothing         -> Left False
    (Just Nothing)  -> Left True
    (Just (Just x)) -> Right x

----------------------------------------------------------------
-- State Info Parsers
----------------------------------------------------------------

parseStateDec :: AT.Parser (String, Bool, Maybe (S.Set String))
parseStateDec = do
  isAuto <- optionalBool $ do
    parseAutoOff
    void "-" <|> skipHoriz1 -- to allow 
  _ <- "state"
  skipHoriz1
  stateName <- takeIdentifier isLower isFollowId
  skipHoriz
  vals <- optional $ do
    _ <- AT.char ':' <?> ("State declaration for \"" ++ T.unpack stateName ++ "\" has no ':'.")
    skipHoriz
    let thisPrs = takeIdentifier isAlpha isFollowId
    (AT.sepBy1' thisPrs skipHoriz1) <?> ("State declaration for \"" ++ T.unpack stateName ++ "\" has ':' but no options.")
  -- parseEndComment
  case vals of
    Nothing -> return ()
    (Just xs) -> when (not $ allUnique xs) $ fail $ "State \"" <> (T.unpack stateName) <> "\" has multiple options with the same name."
  return (T.unpack stateName, isAuto, (S.fromList . map T.unpack) <$> vals)

-- | Parse a state declaration and add it to
--   the state dictionary.
parseStateDecS :: ParserParser ()
parseStateDecS = do
  (stName, isAuto, stSet) <- lift parseStateDec
  theMap <- gets ppsStateDictionary
  case (M.lookup stName theMap) of
    (Just _) -> mkError $ "Error: state \"" <> stName <> "\" has multiple definitions."
    Nothing  -> do
        let newMap = M.insert stName (isAuto, stSet) theMap
        modify $ \x -> x {ppsStateDictionary = newMap}

-- | Parse the prefix for states to indicate that they
--   are auto-off.
parseAutoOff :: AT.Parser ()
parseAutoOff = void
  ("auto-off" <|> "auto" <|> "short" <|> "off")

-- | Get an import declaration and check
--   that it is a valid import.
getImportS :: ParserParser ImportProperty
getImportS = do
  imp <- lift getImport
  case imp of
    (ImportGroup grpName) -> do
      (thisDict, fullDict) <- gets (ppsGroupDictionary &&& ppsGroupDictionary')
      if (grpName `elem` thisDict)
        then warn ("Importing group \"" ++ grpName ++ "\" more than once.")
        else if (grpName `elem` fullDict)
          then do
            let newDict = S.insert grpName thisDict
            modify $ \x -> x {ppsGroupDictionary = newDict}
          else tellError ("Can't find group \"" ++ grpName ++ "\" in phoneme definitions.")
    (ImportAspect aspName) -> do
      (thisDict, fullDict) <- gets (ppsAspectDictionary &&& ppsAspectDictionary')
      case (M.lookup aspName thisDict) of
        (Just _) -> warn ("Importing aspect \"" ++ aspName ++ "\" more than once.")
        Nothing  -> case (M.lookup aspName fullDict) of
          Nothing  -> tellError ("Can't find aspect \"" ++ aspName ++ "\" in phoneme definitions.")
          (Just v) -> do
            let newDict = M.insert aspName v thisDict
            modify $ \x -> x {ppsAspectDictionary = newDict}
    (ImportTrait trtName) -> do
      (thisDict, fullDict) <- gets (ppsTraitDictionary &&& ppsTraitDictionary')
      case (M.lookup trtName thisDict) of
        (Just _) -> warn ("Importing trait \"" ++ trtName ++ "\" more than once.")
        Nothing  -> case (M.lookup trtName fullDict) of
          Nothing  -> tellError ("Can't find trait \"" ++ trtName ++ "\" in phoneme definitions.")
          (Just v) -> do
            let newDict = M.insert trtName v thisDict
            modify $ \x -> x {ppsTraitDictionary = newDict}
  return imp

getImportS_ :: ParserParser ()
getImportS_ = void getImportS

-- | A variant of `getImportS` that defers
--   the actual lookup of imported properties.
--   (Unused)
getImportDeferredS :: ParserParser ImportProperty
getImportDeferredS = do
  imp <- lift getImport
  case imp of
    (ImportGroup grpName) -> do
      thisDict <- gets ppsGroupDictionary
      if (grpName `elem` thisDict)
        then warn ("Importing group \"" ++ grpName ++ "\" more than once.")
        else do
            let newDict = S.insert grpName thisDict
            modify $ \x -> x {ppsGroupDictionary = newDict}
    (ImportAspect aspName) -> do
      thisDict <- gets ppsAspectDictionary
      case (M.lookup aspName thisDict) of
        (Just _) -> warn ("Importing aspect \"" ++ aspName ++ "\" more than once.")
        Nothing  -> do
            let newDict = M.insert aspName S.empty thisDict
            modify $ \x -> x {ppsAspectDictionary = newDict}
    (ImportTrait trtName) -> do
      thisDict <- gets ppsTraitDictionary
      case (M.lookup trtName thisDict) of
        (Just _) -> warn ("Importing trait \"" ++ trtName ++ "\" more than once.")
        Nothing  -> do
            let newDict = M.insert trtName Nothing thisDict
            modify $ \x -> x {ppsTraitDictionary = newDict}
  return imp

----------------------------------------------------------------
-- Orthography Properties Parsers
----------------------------------------------------------------

-- | Parsing the name of this orthography.
--   Used to co-ordinate with other files.
parseOrthographyName :: AT.Parser String
parseOrthographyName = do
  -- _ <- "orthography" <|> "Orth" <|> "orth" <|> "Orth"
  _ <- "orth" <|> "Orth"
  _ <- AT.option "" "ography" -- reduces backtracking
  -- This line is awkward...
  _ <- AT.option "" (skipHoriz1 >> ("name" <|> "Name"))
  skipHoriz
  _ <- AT.char ':' <?> "Orthography name declaration is missing ':'"
  skipHoriz
  T.unpack <$> (parseFileName <|> (takeIdentifier isAlpha isFollowId))

-- | Parse the name of the set of phonemes
--   that this orthography will use.
parseOrthographyChoice :: AT.Parser String
parseOrthographyChoice = do
  _ <- "phone"
  _ <- AT.option "" "me"
  _ <- AT.option "" "s"
  -- Yes, this will mean that "phonemesset" is acceptable.
  skipHoriz
  _ <- AT.option "" "set"
  skipHoriz
  _ <- AT.char ':' <?> "Phoneme set declaration is missing ':'."
  skipHoriz
  (T.unpack <$> (parseFileName <|> (takeIdentifier isAlpha isFollowId))) <?> "Phoneme Set Name"

-- Place other phoneme property parsers here.

-- | Parse the orthography's `HeaderData` and
--   the separator.
parseOrthographyProps :: AT.Parser HeaderData
parseOrthographyProps = do
  (many_ parseEndComment) <?> "Header: skipSpace 1"
  -- AT.skipSpace 
  nom <- (parseOrthographyName) <?> "Header: parseOrthographyName"
  (some_ parseEndComment) <?> "Header: skipSpace 2"
  pho <- parseOrthographyChoice <?> "Header: parseOrthographyChoice"
  (some_ parseEndComment) <?> "Header: skipSpace 3"
  _ <- "===="  <?> "Header: Separator"
  _ <- (AT.takeWhile (== '=')) <?> "Separator"
  parseEndComment <?> "Header: skipSpace 4"
  return $ HeaderData nom pho

----------------------------------------------------------------
-- Multiple Phoneme list parsers
----------------------------------------------------------------

parsePhonemeBrack :: AT.Parser PhoneName
parsePhonemeBrack = do
  _ <- AT.char '('
  skipHoriz
  phoneName <- T.unpack <$> ((takeIdentifier isAlpha isFollowId) <?> "Can't read phoneme name")
  rst <- AT.many' $ do
    skipHoriz1 -- to ensure a space between each value
    T.unpack <$> ((takeIdentifier isAlpha isFollowId) <?> "Can't read aspect value")
  skipHoriz
  _ <- AT.char ')'
  return $ PhoneName phoneName rst

parsePhoneme1 :: AT.Parser PhoneName
parsePhoneme1 = parsePhonemeBrack <|> parseAlt
  where 
    parseAlt = (\x -> (PhoneName (T.unpack x) [])) <$> ((takeIdentifier isAlpha isFollowId) <?> "Can't read phoneme name")

parsePhonemeList :: AT.Parser (NonEmpty PhoneName)
parsePhonemeList = do
  skipHoriz
  fstPhone  <- parsePhoneme1
  rstPhones <- AT.many' $ do
    skipHoriz1
    parsePhoneme1
  return (fstPhone :| rstPhones)

----------------------------------------------------------------
-- Lookahead Pattern Parsers
----------------------------------------------------------------

-- (Taken from the output parser).
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

-- | Parse the next-phoneme string, checking
--   that it matches one of the given groups,
--   traits, aspects, or phonemes.
parseNextCheckS :: ParserParser (Maybe FollowPattern)
parseNextCheckS = do
  (prop, mval) <- lift parseNextCheck
  dicts <- get
  -- NOTE: May want to change the order of the
  -- error messages, in case the user is explicitly
  -- NOT importing a group/aspect/trait that overlaps
  -- with another value. Unlikely, but possible.
  case mval of
    -- If nothing, then following chances,
    -- from most likely to least likely:
    -- Group, Trait, Phoneme, Aspect.
    Nothing -> if (prop `elem` (ppsGroupDictionary dicts))
      then return $ Just $ FollowGroup prop
      else if (prop `elem` (ppsGroupDictionary' dicts))
        then do 
          tellError $ "Group \"" ++ prop ++ "\" has not been imported;\nAdd \"import group " ++ prop ++ "\" to the class/state/import section to import it."
          return Nothing
        else case (M.lookup prop (ppsTraitDictionary dicts)) of
          -- Works with either type of trait.
          (Just _) -> return $ Just $ FollowTrait prop
          Nothing -> case (M.lookup prop (ppsTraitDictionary' dicts)) of
            (Just _) -> do
              tellError $ "Trait \"" ++ prop ++ "\" has not been imported;\nAdd \"import trait " ++ prop ++ "\" to the class/state/import section to import it."
              return Nothing
            Nothing -> if (prop `elem` (ppsPhoneDictionary dicts))
              then return $ Just $ FollowPhone prop
              else case (M.lookup prop (ppsAspectDictionary dicts)) of
                (Just _) -> return $ Just $ FollowAspect prop
                Nothing  -> case (M.lookup prop (ppsAspectDictionary' dicts)) of
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
    (Just val) -> case (M.lookup prop (ppsTraitDictionary' dicts)) of
      (Just (Just vs)) -> if (val `elem` vs)
        then if (isJust ((M.lookup prop (ppsTraitDictionary dicts)))) 
          then return $ Just $ FollowTraitAt prop val
          else do
              tellError $ "Trait \"" ++ prop ++ "\" has not been imported (v1);\nAdd \"import trait " ++ prop ++ "\" to the class/state/import section to import it."
              return Nothing
        else do
          phoneName <- ask
          when (isNothing $ M.lookup prop (ppsTraitDictionary dicts)) $ 
            tellError $ "Trait \"" ++ prop ++ "\" has not been imported (v2);\nAdd \"import trait " ++ prop ++ "\" to the class/state/import section to import it."
          tellError $ "Phoneme \"" ++ phoneName ++ "\" : Can't find value \"" ++ val ++ "\" for trait \"" ++ prop ++ "\"."
          return Nothing
      (Just _) -> do
          phoneName <- ask
          when (isNothing $ M.lookup prop (ppsTraitDictionary dicts)) $ 
            tellError $ "Trait \"" ++ prop ++ "\" has not been imported (v3);\nAdd \"import trait " ++ prop ++ "\" to the class/state/import section to import it."
          tellError $ "Phoneme \"" ++ phoneName ++ "\" : Can't find value \"" ++ val ++ "\" for trait \"" ++ prop ++ "\"."
          return Nothing
      Nothing -> case (M.lookup prop (ppsAspectDictionary' dicts)) of
        (Just vs) -> if (val `elem` vs)
          then if (isJust ((M.lookup prop (ppsAspectDictionary dicts))))
            then return $ Just $ FollowAspectAt prop val
            else do
              tellError $ "Aspect \"" ++ prop ++ "\" has not been imported;\nAdd \"import aspect " ++ prop ++ "\" to the class/state/import section to import it."
              return Nothing
          else do
            phoneName <- ask
            when (prop `notElem` (M.keysSet $ ppsAspectDictionary dicts)) $
              tellError $ "Aspect \"" ++ prop ++ "\" has not been imported;\nAdd \"import aspect " ++ prop ++ "\" to the class/state/import section to import it."
            tellError $ "Phoneme \"" ++ phoneName ++ "\" : Can't find value \"" ++ val ++ "\" for aspect \"" ++ prop ++ "\"."
            return Nothing
        Nothing -> if (prop `elem` (ppsGroupDictionary' dicts))
          then do
            phoneName <- ask
            when (prop `notElem` (ppsGroupDictionary dicts)) $
              tellError $ "Group \"" ++ prop ++ "\" has not been imported;\nAdd \"import group " ++ prop ++ "\" to the class/state/import section to import it."
            tellError $ "Phoneme \"" ++ phoneName ++ "\" : Can't match two strings for a group: \">" ++ prop ++ "=" ++ val ++ "\"."
            return Nothing
          else if (prop `elem` (ppsPhoneDictionary dicts))
            then do
              phoneName <- ask
              tellError $ "Phoneme \"" ++ phoneName ++ "\" : Can't match two strings for a phoneme: \">" ++ prop ++ "=" ++ val ++ "\"."
              return Nothing
            else do
              phoneName <- ask
              tellError $ "Phoneme \"" ++ phoneName ++ "\" : Can't match \"next-phoneme\" string : \">" ++ prop ++ "=" ++ val ++ "\"."
              return Nothing

parseNextCheckRS :: ParserParser CharPatternRaw
parseNextCheckRS = do
  chk <- parseNextCheckS
  case chk of
    (Just fpat) -> return $ FollowPatR fpat
    Nothing     -> return $ PlainCharR '>'
    -- Can't just run "fail", as that would
    -- trigger backtracking and remove the
    -- errors from the output. Instead, we
    -- just pass a dummy value.

----------------------------------------------------------------
-- Phoneme Pattern Parsers
----------------------------------------------------------------

-- | Parsing a phoneme pattern from the
--   single-phoneme section.
parsePhonemePatS :: ParserParser ()
parsePhonemePatS = do
  lift skipHoriz
  phoneName <- T.unpack <$> lift ((takeIdentifier isAlpha isFollowId) <?> "Can't read phoneme name")
  -- This... should work? Since `takeIdentifier` will consume
  -- all the possible characters that any property name could
  -- use.
  lift skipHoriz
  argNames  <- lift (AT.sepBy (T.unpack <$> takeIdentifier isAlpha isFollowId) skipHoriz1)
  let pn = PhoneName phoneName argNames
  lift skipHoriz 
  _ <- lift ((AT.char ':') <?> ("Phoneme pattern for \"" <> phoneName <> "\" is missing ':'."))
  lift skipHoriz
  -- hmm...
  specs <- AT.option [] (parseSpecialsS <* (lift skipHoriz1))
  
  -- Set the internal phoneme name to phoneName.
  runOnPhoneme phoneName $ do
    thePats <- concat <$> AT.sepBy1' 
      ( (mkList parseCodepointRS)
        <|> (mkList parseClassNameRS) 
        <|> (mkList parseEscapedRS  )
        <|> (mkList parseStartPointS)
        <|> (mkList parseEndPointS  )
        <|> (mkList parseNotStartS  )
        <|> (mkList parseNotEndS    )
        <|> (mkList parseNextCheckRS)
        <|> (parseMultiNonSpaceRS   )
        <|> (mkList parseNonSpaceRS ) -- this will consume almost any individual `Char`, so it must go (almost) last.
        -- These two should probably be combined into one function for better errors.
        <|> (mkList parseStateValRS')
        <|> (mkList parseStateSetRS')
      ) 
      (lift skipHoriz1)
    -- hmm...
    sdict <- gets ppsStateDictionary
    let ePhonePats = processRawPhonePattern (snd <$> sdict) (RawPhonemePattern specs thePats)
    case ePhonePats of
      (Left  errs) -> mkErrors $ map (\err -> "Error with phoneme pattern for \"" ++ phoneName ++ "\": " ++ err) errs
      (Right rslt) -> addPhonemePattern pn rslt

-- | Parsing a phoneme pattern from the
--   multi-phoneme section.
parsePhonemePatMulti :: ParserParser ()
parsePhonemePatMulti = do
  phones <- lift parsePhonemeList
  lift skipHoriz
  let patString = map pnName $ NE.toList phones
      phoneName = intercalate " " patString
  _ <- lift ((AT.char ':') <?> ("Phoneme pattern for \"" <> phoneName <> "\" is missing ':'."))
  lift skipHoriz
  -- hmm...
  specs <- AT.option [] (parseSpecialsS <* (lift skipHoriz1))
  
  -- Set the internal phoneme name to phoneName.
  runOnPhoneme phoneName $ do
    thePats <- concat <$> AT.sepBy1' 
      ( (mkList parseCodepointRS )
        <|> (mkList parseClassNameRS)
        <|> (mkList parseEscapedRS  )
        <|> (mkList parseStartPointS)
        <|> (mkList parseEndPointS  )
        <|> (mkList parseNotStartS  )
        <|> (mkList parseNotEndS    )
        <|> (mkList parseNextCheckRS)
        <|> (parseMultiNonSpaceRS   )
        <|> (mkList parseNonSpaceRS ) -- this will consume almost any individual `Char`, so it must go (almost) last.
        -- These two should probably be combined into one function for better errors.
        <|> (mkList parseStateValRS')
        <|> (mkList parseStateSetRS')
      ) 
      (lift skipHoriz1)
    -- hmm...
    sdict <- gets ppsStateDictionary
    let ePhonePats = processRawPhonePattern (snd <$> sdict) (RawPhonemePattern specs thePats)
    case ePhonePats of
      (Left  errs) -> mkErrors $ map (\err -> "Error with phoneme pattern for \"" ++ phoneName ++ "\": " ++ err) errs
      (Right rslt) -> addPhonemesPattern phones rslt

----------------------------------------------------------------
-- Full File Parsers
----------------------------------------------------------------

parseOrthographyFile :: AT.Parser (HeaderData, ParserParsingState, [String], [String])
parseOrthographyFile = do
  hdr <- parseOrthographyProps
  (_rslt, stt, msgs) <- embedParserParser $ do
    -- Parse the class instances
    parseClassDecSectionNew
    lift $ many_ parseEndComment

    -- Parse the phoneme patterns
    _ <- AT.many' $ do
      -- Skip spaces...
      lift $ many_ parseEndComment
      parsePhonemePatS
      -- lift skipHoriz
      lift parseEndComment
    lift AT.skipSpace
    lift $ many_ parseEndComment
    _ <- AT.option () $ do
      _ <- lift ("====" <?> "Phoneme Patterns: Separator")
      _ <- AT.many' $ do
        lift $ many_ parseEndComment
        parsePhonemePatMulti
        lift parseEndComment
      return ()
    return ()
    -- hmm...

  
  let (errs, wrns, notes) = partitionMessages msgs
      phonePatsM  = ppsPhonemePatterns stt
      phoneNames  = M.keys phonePatsM
      phoneNames' = concatMap (NE.toList . prPhonemes) phoneNames
      phoneGroups = groupBy eqOnPN phoneNames'
      wrongPhones = filter (not . sameArgsPN) phoneGroups
      phoneErrs   = map (\ph -> "Error: Patterns for \"" <> (getStrPN ph) <> "\" have differing numbers of arguments.") wrongPhones
  return (hdr,stt,errs ++ phoneErrs, wrns)

-- | Newer version that relies on input from the
--   phoneme file.
parseOrthographyFileNew 
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
  -> AT.Parser (HeaderData, ParserParsingState, [String], [String])
parseOrthographyFileNew grps trts asps phones = do
  hdr <- parseOrthographyProps
  (_rslt, stt, msgs) <- embedParserParserNew grps trts asps phones $ do
    -- Parse the class instances
    parseClassDecSectionNew
    lift $ many_ parseEndComment

    -- Parse the phoneme patterns
    _ <- AT.many' $ do
      -- Skip spaces...
      lift $ many_ parseEndComment
      parsePhonemePatS
      -- lift skipHoriz
      lift parseEndComment
    lift AT.skipSpace
    lift $ many_ parseEndComment
    _ <- AT.option () $ do
      _ <- lift ("====" <?> "Phoneme Patterns: Separator")
      _ <- AT.many' $ do
        lift $ many_ parseEndComment
        parsePhonemePatMulti
        lift parseEndComment
      return ()
    return ()
    -- hmm...

  
  let (errs, wrns, notes) = partitionMessages msgs
      phonePatsM  = ppsPhonemePatterns stt
      phoneNames  = M.keys phonePatsM
      phoneNames' = concatMap (NE.toList . prPhonemes) phoneNames
      phoneGroups = groupBy eqOnPN phoneNames'
      wrongPhones = filter (not . sameArgsPN) phoneGroups
      phoneErrs   = map (\ph -> "Error: Patterns for \"" <> (getStrPN ph) <> "\" have differing numbers of arguments.") wrongPhones
  return (hdr,stt,errs ++ phoneErrs, wrns)



getStrPN :: [PhoneName] -> String
getStrPN [] = "<??>"
getStrPN ((PhoneName nom _):_) = nom


-- :m + Metamorth.Interpretation.Parser.Parsing Metamorth.Interpretation.Parser.Types Data.Either Control.Monad
-- import Data.Text.IO qualified as TIO
-- import Data.Attoparsec.Text qualified as AT
-- AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample1.thyp"


{-
eqOnPN = (==) `on` pnName

-- | Check that a list of `PhoneName`s all have
--   the same number of arguments.
sameArgsPN :: [PhoneName] -> Bool
sameArgsPN []  = True
-}


