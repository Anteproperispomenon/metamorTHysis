module Metamorth.Interpretation.Parser.Parsing
  (

  ) where

import Control.Applicative

import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS

import Data.Attoparsec.Text qualified as AT

import Data.Functor
import Data.Bifunctor

import Data.Char (ord, chr, isSpace, isLower, isAlpha)
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

parseStartPointS :: ParserParser CharPatternRaw
parseStartPointS = lift parseStartPoint

parseEndPointS :: ParserParser CharPatternRaw
parseEndPointS = lift parseEndPoint

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
parseNonSpace = AT.satisfy (not . isSpace)

parseNonSpaceR :: AT.Parser CharPatternRaw
parseNonSpaceR = PlainCharR <$> parseNonSpace

parseNonSpaceRS :: ParserParser CharPatternRaw
parseNonSpaceRS = lift parseNonSpaceR

--------------------------------
-- Special Char Parsers

parseSpecials :: AT.Parser [Char] 
parseSpecials = AT.sepBy (AT.satisfy (\x -> x == '+' || x == '-')) skipHoriz1

parseSpecialsS :: ParserParser [Char]
parseSpecialsS = lift parseSpecials

----------------------------------------------------------------
-- Class Pattern/Declaration Parsers
----------------------------------------------------------------

-- | Parser a class declaration.
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
  T.unpack <$> takeIdentifier isAlpha isFollowId

-- Place other phoneme property parsers here.

----------------------------------------------------------------
-- Phoneme Pattern Parsers
----------------------------------------------------------------

parsePhonemePatS :: ParserParser ()
parsePhonemePatS = do
  lift skipHoriz
  phoneName <- T.unpack <$> lift (takeIdentifier isAlpha isFollowId)
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
        <|> parseNonSpaceRS -- this will consume almost any individual `Char`, so it must go last.
      ) 
      (lift skipHoriz1)
    -- hmm...
    let ePhonePats = processRawPhonePattern (RawPhonemePattern specs thePats)
    case ePhonePats of
      (Left  errs) -> tell $ map (\err -> "Error with phoneme pattern for \"" ++ phoneName ++ "\": " ++ err) errs
      (Right rslt) -> addPhonemePattern pn rslt



