module Metamorth.Interpretation.Parser.Parsing
  ( parseOrthographyFile

  ) where

import Control.Applicative

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

import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty(..))

import Metamorth.Helpers.Char
import Metamorth.Helpers.List
import Metamorth.Helpers.Parsing

import Metamorth.Interpretation.Parser.Parsing.Types
import Metamorth.Interpretation.Parser.Types

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

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
        tell ["Error with phoneme \"" <> phoneName <> "\": Calls for undefined class \"" <> nm <> "\"."]
        return $ CharClassR nm
    (Just _) -> return $ CharClassR nm

-- | The main character selector parser, after
--   all the specialised ones have been run.
parseNonSpace :: AT.Parser Char
parseNonSpace = AT.satisfy (\x -> (not $ isSpace x) && (x /= '#') && (x /= '@') && (x /= '!'))

parseNonSpaceR :: AT.Parser CharPatternRaw
parseNonSpaceR = PlainCharR <$> parseNonSpace

parseNonSpaceRS :: ParserParser CharPatternRaw
parseNonSpaceRS = lift parseNonSpaceR

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
      tell ["Couldn't find state name \"" <> st <> "\" in dictionary."]
      return Nothing
    (Just Nothing) -> case (checkBoolString val) of
      Nothing -> do 
        tell ["Couldn't interpret \"" <> val' <> "\" as boolean value for state \"" <> st <> "\"."]
        return Nothing
      (Just bl) -> return $ Just (ValStateR st (Right bl))
    (Just (Just valSet)) -> if (val' `S.member` valSet)
      then (return $ Just $ ValStateR st (Left val'))
      else case (checkBoolString val) of
        Nothing -> do
          tell ["Couldn't find \"" <> val' <> "\" as an option for state \"" <> st <> "\"."]
          return Nothing
        (Just bl) -> return $ Just (ValStateR st (Right bl))


parseStateValRS' :: ParserParser CharPatternRaw
parseStateValRS' = do
  (cpat, wrtr) <- listens safeHead parseStateValRS
  case cpat of
    Nothing -> case wrtr of
      Nothing    -> fail "Couldn't parse a state-val expression"
      (Just err) -> fail err
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
      tell ["Couldn't find state name \"" <> st <> "\" in dictionary."]
      return Nothing
    (Just Nothing) -> case (checkBoolString val) of
      Nothing -> do 
        tell ["Couldn't interpret \"" <> val' <> "\" as boolean value for state \"" <> st <> "\"."]
        return Nothing
      (Just bl) -> return $ Just (SetStateR st (Right bl))
    (Just (Just valSet)) -> if (val' `S.member` valSet)
      then (return $ Just $ SetStateR st (Left val'))
      -- need to use `checkOffString` since you can't just
      -- set an option string to `On`; you need a value to
      -- set it to.
      else case (checkOffString val) of
        Nothing -> do
          tell ["Couldn't find \"" <> val' <> "\" as an option for state \"" <> st <> "\"."]
          return Nothing
        (Just bl) -> return $ Just (SetStateR st (Right bl))

parseStateSetRS' :: ParserParser CharPatternRaw
parseStateSetRS' = do
  (cpat, wrtr) <- listens safeHead parseStateSetRS
  case cpat of
    Nothing -> case wrtr of
      Nothing    -> fail "Couldn't parse a state-set expression"
      (Just err) -> fail err
    (Just cptrn) -> return cptrn


--------------------------------
-- Special Char Parsers

parseSpecials :: AT.Parser [Char] 
parseSpecials = AT.sepBy (AT.satisfy (\x -> x == '+' || x == '-')) skipHoriz

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
  chrs <- AT.sepBy1' (parseCodepoint <|> parseEscaped <|> parseNonSpace) (skipHoriz1)
  return $ (T.unpack className, S.fromList chrs)

-- | Parse a class declaration and add it to
--   the class dictionary.
parseClassDecS :: ParserParser ()
parseClassDecS = do
  (clName, clSet) <- lift $ parseClassDec
  theMap <- gets ppsClassDictionary
  case (M.lookup clName theMap) of
    (Just _) -> tell ["Error: class \"" <> clName <> "\" has multiple definitions."]
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
    lift $ AT.many1 parseEndComment
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
  lift $ skipHoriz
  c <- lift $ AT.peekChar'
  case c of
    ':' -> do
      lift $ AT.anyChar
      lift $ skipHoriz
      (txt, bl) <- lift $ AT.match classOrStateDecider'
      let outp = case bl of
                  (Right  True) -> "  class " <> thingName <> " : " <> (T.unpack txt)
                  (Right False) -> "  state " <> thingName <> " : " <> (T.unpack txt)
                  (Left   True) -> "  class/state " <> thingName <> " : " <> (T.unpack txt)
                  (Left  False) -> "  state " <> thingName
      tell ["Unspecified declaration \"" <> thingName <> "\" in Class/State section; did you mean to write\n\n" <> outp <> "\n"]
      -- lift parseEndComment
    '#' -> do
      tell ["Unspecified declaration \"" <> thingName <> "\" in Class/State section; did you mean to write\n\n  state " <> thingName <> "\n"]
      -- lift parseEndComment
    '\n' -> do
      tell ["Unspecified declaration \"" <> thingName <> "\" in Class/State section; did you mean to write\n\n  state " <> thingName <> "\n"]
      -- lift parseEndComment
    _ -> do
      txt <- lift $ AT.takeWhile (\x -> (x /= '#') && (x /= '\n'))
      tell ["Unspecified declaration in Class/State section:\n\n" <> thingName <> " " <> (T.unpack txt) <> "\n"]
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
parseStateDecS :: ParserParser ()
parseStateDecS = do
  (stName, stSet) <- lift $ parseStateDec
  theMap <- gets ppsStateDictionary
  case (M.lookup stName theMap) of
    (Just _) -> tell ["Error: state \"" <> stName <> "\" has multiple definitions."]
    Nothing  -> do
        let newMap = M.insert stName stSet theMap
        modify $ \x -> x {ppsStateDictionary = newMap}

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
  T.unpack <$> takeIdentifier isAlpha isFollowId

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
  (T.unpack <$> takeIdentifier isAlpha isFollowId) <?> "Phoneme Set Name"

-- Place other phoneme property parsers here.

-- | Parse the orthography's `HeaderData` and
--   the separator.
parseOrthographyProps :: AT.Parser HeaderData
parseOrthographyProps = do
  AT.skipSpace <?> "Header: skipSpace 1"
  nom <- parseOrthographyName <?> "Header: parseOrthographyName"
  AT.skipSpace <?> "Header: skipSpace 2"
  pho <- parseOrthographyChoice <?> "Header: parseOrthographyChoice"
  skipHoriz    <?> "Header: skipHoriz 1"
  AT.endOfLine <?> "Header: endOfLine 1"
  AT.skipSpace <?> "Header: skipSpace 3"
  _ <- "===="  <?> "Header: Separator"
  _ <- (AT.takeWhile (== '=')) <?> "Separator"
  skipHoriz <?> "Header: skipHoriz 2"
  AT.endOfLine <?> "Header: endOfLine 2"
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
-- Phoneme Pattern Parsers
----------------------------------------------------------------

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
    thePats <- AT.sepBy1' 
      ( parseCodepointRS 
        <|> parseClassNameRS 
        <|> parseEscapedRS 
        <|> parseStartPointS 
        <|> parseEndPointS 
        <|> parseNotStartS
        <|> parseNotEndS
        <|> parseNonSpaceRS -- this will consume almost any individual `Char`, so it must go (almost) last.
        -- These two should probably be combined into one function for better errors.
        <|> parseStateValRS'
        <|> parseStateSetRS'
      ) 
      (lift skipHoriz1)
    -- hmm...
    sdict <- gets ppsStateDictionary
    let ePhonePats = processRawPhonePattern sdict (RawPhonemePattern specs thePats)
    case ePhonePats of
      (Left  errs) -> tell $ map (\err -> "Error with phoneme pattern for \"" ++ phoneName ++ "\": " ++ err) errs
      (Right rslt) -> addPhonemePattern pn rslt

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
    thePats <- AT.sepBy1' 
      ( parseCodepointRS 
        <|> parseClassNameRS 
        <|> parseEscapedRS 
        <|> parseStartPointS
        <|> parseEndPointS
        <|> parseNotStartS
        <|> parseNotEndS
        <|> parseNonSpaceRS -- this will consume almost any individual `Char`, so it must go (almost) last.
        -- These two should probably be combined into one function for better errors.
        <|> parseStateValRS'
        <|> parseStateSetRS'
      ) 
      (lift skipHoriz1)
    -- hmm...
    sdict <- gets ppsStateDictionary
    let ePhonePats = processRawPhonePattern sdict (RawPhonemePattern specs thePats)
    case ePhonePats of
      (Left  errs) -> tell $ map (\err -> "Error with phoneme pattern for \"" ++ phoneName ++ "\": " ++ err) errs
      (Right rslt) -> addPhonemesPattern phones rslt

----------------------------------------------------------------
-- Full File Parsers
----------------------------------------------------------------

parseOrthographyFile :: AT.Parser (HeaderData, ParserParsingState, [String])
parseOrthographyFile = do
  hdr <- parseOrthographyProps
  (_rslt, stt, errs) <- embedParserParser $ do
    -- Parse the class instances
    parseClassDecSection
    void $ lift $ many parseEndComment

    -- Parse the phoneme patterns
    _ <- AT.many' $ do
      -- Skip spaces...
      void $ lift $ many parseEndComment
      parsePhonemePatS
      -- lift skipHoriz
      lift parseEndComment
    lift AT.skipSpace
    lift $ many parseEndComment
    _ <- AT.option () $ do
      _ <- lift ("====" <?> "Phoneme Patterns: Separator")
      _ <- AT.many' $ do
        lift $ many parseEndComment
        parsePhonemePatMulti
        lift parseEndComment
      return ()
    return ()
    -- hmm...

  
  let phonePatsM  = ppsPhonemePatterns stt
      phoneNames  = M.keys phonePatsM
      phoneNames' = concatMap (NE.toList . prPhonemes) phoneNames
      phoneGroups = groupBy eqOnPN phoneNames'
      wrongPhones = filter (not . sameArgsPN) phoneGroups
      phoneErrs   = map (\ph -> "Error: Patterns for \"" <> (getStrPN ph) <> "\" have differing numbers of arguments.") wrongPhones
  return (hdr,stt,errs ++ phoneErrs)

getStrPN :: [PhoneName] -> String
getStrPN [] = "<??>"
getStrPN ((PhoneName nom _):_) = nom

-- | Parse the end of line, possibly preceded by a comment.
parseEndComment :: AT.Parser ()
parseEndComment = do
  skipHoriz
  AT.option () $ do
    _ <- AT.char '#'
    AT.skipWhile (\x -> x /= '\n' && x /= '\r')
  AT.endOfLine


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


