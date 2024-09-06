module Metamorth.Interaction.Quasi.Parser
  ( parseOrthographyBlock
  , parseOrthographyBlocks
  , parseOrthographyDetails
  , parseOrthographyDetailsDebug
  -- * Debug
  , parseIndentedOptions
  ) where

import Data.List (intercalate)
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

import Data.Attoparsec.Text qualified as AT

import Data.Char
import Data.Text qualified as T

import Metamorth.Helpers.Parsing

import Metamorth.Interaction.Quasi.Parser.Helpers
import Metamorth.Interaction.Quasi.Parser.Types
import Metamorth.Interaction.Quasi.Parser.Types.More

import Control.Monad.Trans.State.Strict


{-
pattern ExtraParserDetails :: String -> [String] -> Bool -> Bool -> Bool -> String -> String -> ExtraParserDetails
pattern ExtraParserDetails 
  { epdParserName
  , epdOtherNames
  , epdUnifyBranches
  , epdGroupGuards
  , epdCheckStates
  , epdMainFuncName
  , epdNameSuffix 

data ExtraOutputDetails = ExtraOutputDetails 
  { eodOutputName :: String
  , eodSuffix     :: String
  , eodOtherNames :: [String]
  } deriving (Show, Eq)

-- moved to "Metamorth.Interaction.Quasi.Parser.Types"
data OrthographyDetails = OrthographyDetails
  { odName :: String -- or T.Text
  , odInputFile  :: Maybe FilePath
  , odOutputFile :: Maybe FilePath
  -- | Whether to unify branches for parser.
  , odUnifyBranches :: Maybe Bool
  -- | Whether to group guards for parser.
  , odGroupGuards   :: Maybe Bool
  -- | Whether to check states for parser.
  , odCheckStates   :: Maybe Bool
  -- | The name of the input parser function.
  , odInputName     :: Maybe String
  -- | The name of the output function.
  , odOutputName    :: Maybe String
  -- | The suffix used for internal parser functions.
  , odInSuffix      :: Maybe String
  -- | The suffix used for internal output functions.
  , odOutSuffix     :: Maybe String
  -- | The strings used to identify this parser
  --   for the CLI interface.
  , odCLINames      :: [String]
  } deriving (Show, Eq)
-}

parseOrthographyDetails :: ParserQQ (FilePath, Maybe String, [OrthographyDetails])
parseOrthographyDetails = do
  -- lift $ many'_ consumeEndComment
  -- fp <- parsePhoneFileName
  -- lift $ many'_ consumeEndComment
  (fp, mLang) <- parseHeader
  dts <- parseOrthographyBlocks
  return (fp, mLang, dts)

parseOrthographyDetailsDebug :: ParserQQ (FilePath, Maybe String, [OrthographyDetails], [OrthographyDetails], String)
parseOrthographyDetailsDebug = do
  -- lift $ many'_ consumeEndComment
  -- fp <- parsePhoneFileName
  -- lift $ many'_ consumeEndComment
  (fp, mLang) <- parseHeader
  dts  <- parseOrthographyBlocks'
  ond  <- verifyOrthographies dts
  dts' <- fillInOrthographies ond dts
  return (fp, mLang, dts', dts, show ond)


parseOrthographyBlocks :: ParserQQ [OrthographyDetails]
parseOrthographyBlocks = do
  blocks <- AT.many1 $ do
    lift $ many'_ consumeEndComment
    parseOrthographyBlock
  
  lift $ many'_ consumeEndComment
  -- Fix up the orthographies.
  verifyAndFillInOrthographies blocks

parseOrthographyBlocks' :: ParserQQ [OrthographyDetails]
parseOrthographyBlocks' = do
  blocks <- AT.many1 $ do
    lift $ many'_ consumeEndComment
    parseOrthographyBlock
  
  lift $ many'_ consumeEndComment
  return blocks

parseOrthographyBlock :: ParserQQ OrthographyDetails
parseOrthographyBlock = do
  orthNom <- lift $ takeIdentifier isAlpha isFollowId
  lift $ do
    skipHoriz
    _ <- optional $ do
      _ <- AT.char ':'
      skipHoriz
    consumeEndComment
  embedQQ1_ (T.unpack orthNom) parseIndentedOptions

--------------------------------
-- Parsing the Phoneme file

parsePhoneFileName :: ParserQQ String
parsePhoneFileName = do
  fld <- "phonemes" <|> "phoneme set" <|> "phoneme-set" <|> "phones"
  parseKeySepX $ T.unpack fld
  lift (parseQuoteString <|> parseUnquoteString)

parseLanguageName :: ParserQQ String
parseLanguageName = do
  _ <- "lang"
  z <- isJust <$> optional "uage"
  parseKeySepX $ if z then "language" else "lang"
  lift (parseQuoteString <|> parseUnquoteString)

parseHeader :: ParserQQ (String, Maybe String)
parseHeader = do
  lift $ many'_ consumeEndComment
  opt1 <- (Right <$> parsePhoneFileName) <|> (Left <$> parseLanguageName)
  lift $ many'_ consumeEndComment
  case opt1 of
    (Right fp) -> do
      lang <- optional $ do
        lng <- parseLanguageName
        lift $ many'_ consumeEndComment
        return lng
      return (fp, lang)
    (Left lang) -> do
      fp <- parsePhoneFileName <|> fail "Missing Phoneme File Location"
      lift $ many'_ consumeEndComment
      return (fp, Just lang)

parseHeaderNew :: ParserQQ QuasiHeader
parseHeaderNew = do
  qh <- execStateT' defQuasiHeader $ do
    many'_ $ do
      lift $ lift $ many'_ consumeEndComment
      parsePhoneFileNameQH <|> parseLanguageNameQH <|> parseCaseOptionQH
      lift $ lift $ many'_ consumeEndComment
  -- Now check that the filepath is well-formed...
  let fp = qhFilePath qh
  when (fp == "" || fp == ".") $ tellError "Missing Phoneme File Location."
  return qh
  where execStateT' = flip execStateT


--------------------------------
-- Parsing Options

parseIndentedOptions :: ParserQQ1 ()
parseIndentedOptions = do
  (n, ()) <- findIndentQQ1 parseOption
  if (n > 0)
    then many'_ (indentedToQQ1 n parseOption)
    else do
      nom <- getOrthName
      lift $ tellError $ "Error with orthography \"" ++ nom ++ "\": options must be indented at least one space."
      fail             $ "Error with orthography \"" ++ nom ++ "\": options must be indented at least one space."
      

parseOption :: ParserQQ1 ()
parseOption = AT.choice
  [ parseInputName  -- must come before "parseInputFile"
  , parseInputFile
  , parseOutputName -- must come before "parseOutputFile"
  , parseOutputFile
  , parseCLINames
  , parseInSuffix
  , parseOutSuffix
  , parseSuffix
  , parseUnifyBranches
  , parseGroupGuards
  , parseCheckStates
  , parseExtension
  , parseDescription
  -- Must be the final option.
  , parseFailedOption
  ]

parseInputFile :: ParserQQ1 ()
parseInputFile = do
  _ <- "input"
  z <- isJust <$> optional "-file"
  parseKeySep' (if z then "input-file" else "input")
  str <- liftQQ1 (parseQuoteString <|> parseUnquoteString)
  setInputFile str

parseOutputFile :: ParserQQ1 ()
parseOutputFile = do
  _ <- "output"
  z <- isJust <$> optional "-file"
  parseKeySep' (if z then "output-file" else "output")
  str <- liftQQ1 (parseQuoteString <|> parseUnquoteString)
  setOutputFile str -- oh wow that was it?

parseCLINames :: ParserQQ1 ()
parseCLINames = do
  _ <- "cli-names"
  parseKeySep' "cli-names"
  strs <- liftQQ1 (AT.sepBy1' (parseQuoteString <|> parseUnquoteString) parseListSep)
  addCLINames strs

parseSuffix :: ParserQQ1 ()
parseSuffix = do
  _ <- "suffix"
  parseKeySep' "suffix"
  str <- liftQQ1 (parseQuoteString <|> parseUnquoteString)
  setInSuffix str
  setOutSuffix (str ++ "_out")

parseInSuffix :: ParserQQ1 ()
parseInSuffix = do
  fld <- "suffix-in" <|> "in-suffix"
  parseKeySep' $ T.unpack fld
  str <- liftQQ1 (parseQuoteString <|> parseUnquoteString)
  setInSuffix str

parseOutSuffix :: ParserQQ1 ()
parseOutSuffix = do
  fld <- "suffix-out" <|> "out-suffix"
  parseKeySep' $ T.unpack fld
  str <- liftQQ1 (parseQuoteString <|> parseUnquoteString)
  setInSuffix str

parseInputName :: ParserQQ1 ()
parseInputName = do
  fld <- "parser-name" <|> "input-name"
  parseKeySep' $ T.unpack fld
  str <- liftQQ1 (parseQuoteString <|> parseUnquoteString)
  setInputName str

parseOutputName :: ParserQQ1 ()
parseOutputName = do
  _ <- "output-name"
  parseKeySep' "output-name"
  str <- liftQQ1 (parseQuoteString <|> parseUnquoteString)
  setOutputName str

parseExtension :: ParserQQ1 ()
parseExtension = do
  _ <- "ext"
  z <- isJust <$> optional "ension"
  parseKeySep' (if z then "extension" else "ext")
  ext <- liftQQ1 (parseQuoteString <|> parseUnquoteString)
  setExtension ext

parseDescription :: ParserQQ1 ()
parseDescription = do
  fld <- "dsc" <|> "description"
  parseKeySep' $ T.unpack fld
  dsc <- liftQQ1 (parseQuoteLineString <|> parseUnquoteLineString)
  setDescription dsc

parseFailedOption :: ParserQQ1 ()
parseFailedOption = do
  optName <- liftQQ1 $ AT.takeWhile (\c -> isAlpha c || c == '-' || c == '_')
  parseKeySep' $ T.unpack optName
  strs <- liftQQ1 (AT.sepBy' (parseQuoteString <|> parseUnquoteString) parseListSep)
  case strs of
    []    -> lift $ tellError $ "Couldn't parse option \"" ++ T.unpack optName ++ "\" with empty value list."
    [str] -> lift $ tellError $ "Couldn't parse option \"" ++ T.unpack optName ++ "\" with value \"" ++ str ++ "\"."
    _     -> lift $ tellError $ "Couldn't parse option \"" ++ T.unpack optName ++ "\" with values " ++ (intercalate ", " (map quotify strs)) ++ "."
  where
    quotify str = '\"' : (str ++ "\"")


{-
  -- | Whether to unify branches for parser.
  , odUnifyBranches :: Maybe Bool
  -- | Whether to group guards for parser.
  , odGroupGuards   :: Maybe Bool
  -- | Whether to check states for parser.
  , odCheckStates   :: Maybe Bool
-}

parseUnifyBranches :: ParserQQ1 ()
parseUnifyBranches = do
  _ <- "unify-branches"
  parseKeySep' "unify-branches"
  bl <- liftQQ1 parseBool
  setUnifyBranches bl

parseGroupGuards :: ParserQQ1 ()
parseGroupGuards = do
  _ <- "group-guards"
  parseKeySep' "group-guards"
  bl <- liftQQ1 parseBool
  setGroupGuards bl

parseCheckStates :: ParserQQ1 ()
parseCheckStates = do
  _ <- "check-states"
  parseKeySep' "check-states"
  bl <- liftQQ1 parseBool
  setCheckStates bl

{-

example configuration:

phonemes : "phonemes.thym"

Umista
  input  : "parsers/umista.thyp"
  output : "output/umista.thyo"
  cli-names : "umista", "ums", "u"
  suffix : "ums"
  parser-name : umistaParser -- optional
  output-name : umistaOutput -- optional
  unify-paths : off

-}






