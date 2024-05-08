module Metamorth.Interaction.Quasi.Parser
  ( parseOrthographyBlock
  , parseOrthographyBlocks
  ) where

import Data.List (intercalate)

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

import Data.Attoparsec.Text qualified as AT

import Data.Char
import Data.Text qualified as T

import Metamorth.Helpers.Parsing

import Metamorth.Interaction.Quasi.Parser.Helpers
import Metamorth.Interaction.Quasi.Parser.Types



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

parseOrthographyBlocks :: ParserQQ [OrthographyDetails]
parseOrthographyBlocks = do
  blocks <- AT.many1 $ do
    lift $ many'_ consumeEndComment
    parseOrthographyBlock
  
  lift $ many'_ consumeEndComment
  -- Fix up the orthographies.
  verifyAndFillInOrthographies blocks

parseOrthographyBlock :: ParserQQ OrthographyDetails
parseOrthographyBlock = do
  orthNom <- lift $ takeIdentifier isAlpha isFollowId
  lift $ do
    skipHoriz
    _ <- optional $ do
      AT.char ':'
      skipHoriz
    consumeEndComment
  embedQQ1_ (T.unpack orthNom) parseIndentedOptions

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
  [ parseInputFile
  , parseOutputFile
  , parseCLINames
  , parseInputName
  , parseOutputName
  , parseInSuffix
  , parseOutSuffix
  , parseSuffix
  , parseUnifyBranches
  , parseGroupGuards
  , parseCheckStates
  , parseFailedOption
  ]

parseInputFile :: ParserQQ1 ()
parseInputFile = do
  _ <- "input"
  liftQQ1 parseKeySep
  str <- liftQQ1 (parseQuoteString <|> parseUnquoteString)
  setInputFile str

parseOutputFile :: ParserQQ1 ()
parseOutputFile = do
  _ <- "output"
  liftQQ1 parseKeySep
  str <- liftQQ1 (parseQuoteString <|> parseUnquoteString)
  setInputFile str

parseCLINames :: ParserQQ1 ()
parseCLINames = do
  _ <- "cli-names"
  liftQQ1 parseKeySep
  strs <- liftQQ1 (AT.sepBy1' (parseQuoteString <|> parseUnquoteString) parseListSep)
  addCLINames strs

parseSuffix :: ParserQQ1 ()
parseSuffix = do
  _ <- "suffix"
  liftQQ1 parseKeySep
  str <- liftQQ1 (parseQuoteString <|> parseUnquoteString)
  setInSuffix str
  setOutSuffix (str ++ "_out")

parseInSuffix :: ParserQQ1 ()
parseInSuffix = do
  _ <- "suffix-in" <|> "in-suffix"
  liftQQ1 parseKeySep
  str <- liftQQ1 (parseQuoteString <|> parseUnquoteString)
  setInSuffix str

parseOutSuffix :: ParserQQ1 ()
parseOutSuffix = do
  _ <- "suffix-out" <|> "out-suffix"
  liftQQ1 parseKeySep
  str <- liftQQ1 (parseQuoteString <|> parseUnquoteString)
  setInSuffix str

parseInputName :: ParserQQ1 ()
parseInputName = do
  _ <- "parser-name" <|> "input-name"
  liftQQ1 parseKeySep
  str <- liftQQ1 (parseQuoteString <|> parseUnquoteString)
  setInputName str

parseOutputName :: ParserQQ1 ()
parseOutputName = do
  _ <- "output-name"
  liftQQ1 parseKeySep
  str <- liftQQ1 (parseQuoteString <|> parseUnquoteString)
  setOutputName str

parseFailedOption :: ParserQQ1 ()
parseFailedOption = do
  optName <- liftQQ1 $ AT.takeWhile (\c -> isAlpha c || c == '-' || c == '_')
  liftQQ1 parseKeySep
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
  liftQQ1 parseKeySep
  bl <- liftQQ1 parseBool
  setUnifyBranches bl

parseGroupGuards :: ParserQQ1 ()
parseGroupGuards = do
  _ <- "group-guards"
  liftQQ1 parseKeySep
  bl <- liftQQ1 parseBool
  setGroupGuards bl

parseCheckStates :: ParserQQ1 ()
parseCheckStates = do
  _ <- "check-states"
  liftQQ1 parseKeySep
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






