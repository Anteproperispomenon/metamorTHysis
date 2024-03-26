module Metamorth.Interaction.TH
  ( createParsers
  -- * Re-Exports
  , ParserOptions(..)
  , defParserOptions
  ) where

import Control.Monad

import Data.Maybe

import Data.Attoparsec.Text qualified as AT

import Metamorth.Interpretation.Parser.TH   qualified as Parser
import Metamorth.Interpretation.Phonemes.TH qualified as Phonemes

import Metamorth.Interpretation.Parser.Parsing

import Metamorth.Interpretation.Parser.TH
  ( makeTheParser
  , ParserOptions(..)
  , defParserOptions
  , StaticParserInfo(..)
  )

import Metamorth.Interpretation.Phonemes.TH
  ( PhonemeDatabase(..)
  , PhonemeInformation(..)
  , producePhonemeDatabase
  , PropertyData(..)
  )

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

import Metamorth.Helpers.IO

import System.Directory

import Data.Text qualified as T
import Data.Text (Text)

-- | Simple type for the output of the generated
--   declarations.
data GeneratedDecs = GeneratedDecs
  { gdPhonemeDecs :: [Dec]
  , gdParserDecs  :: [[Dec]]
  , gdOutputDecs  :: [[Dec]]
  } deriving (Show, Eq)

-- | Options to go along with each parser file.
data ExtraParserDetails = ExtraParserDetails
  { epdParserOptions :: ParserOptions
  } deriving (Show, Eq)

defExtraParserDetails :: ExtraParserDetails
defExtraParserDetails  = ExtraParserDetails
  { epdParserOptions = defParserOptions
  }

data ExtraOutputDetails = ExtraOutputDetails {} deriving (Show, Eq)

defExtraoutputDetails :: ExtraOutputDetails
defExtraoutputDetails  = ExtraOutputDetails
  {}

-- | Create a `GeneratedDecs` from the desired input files.
createParsers :: FilePath -> [(FilePath, ExtraParserDetails)] -> [(FilePath, ExtraOutputDetails)] -> Q GeneratedDecs
createParsers phonemePath parserPaths _outputPaths = do
  
  -- get the parser files
  -- Note that this is probably fairly inefficient;
  -- all the text files are read into memory before
  -- being processed. This should change in the future.
  rslts@(mPhoneData, eParseFiles, _) <- runIO $ do
    eParsers <- mapM readParserFile parserPaths

    return (Nothing, eParsers, Nothing)

  (pdb, phoneDecs) <- case mPhoneData of
    Nothing  -> fail "Couldn't parse/find phoneme file."
    (Just z) -> return z

  parserResults <- fmap catMaybes $ forM eParseFiles $ \(eParseFile) -> do
    case eParseFile of
      (Left err)        -> (qReport True (err)) >> return Nothing
      (Right (txt,epd)) -> Just <$> getParserData pdb txt epd

  return $ GeneratedDecs [] (map fst parserResults) []


readParserFile :: (FilePath, ExtraParserDetails) -> IO (Either String (Text, ExtraParserDetails))
readParserFile (fp, epd) = do
  bl <- doesFileExist fp
  case bl of
    False -> return $ Left $ "Could not find file \"" ++ fp ++ "\"."
    True  -> do
      txt <- readFileUTF8 fp -- for now...
      return $ Right (txt, epd)


getParserData :: PhonemeDatabase -> Text -> ExtraParserDetails -> Q ([Dec], StaticParserInfo)
getParserData pdb txt epd = do
  -- here
  let eParseRslt = AT.parseOnly parseOrthographyFile txt
  case eParseRslt of
    (Left err) -> fail $ "Couldn't parse input: " ++ err
    -- (HeaderData, ParserParsingState, [String])
    (Right (hdr, pps, errStrings)) -> do
      case errStrings of
        [] -> return ()
        xs -> do
          qReport True "Encountered errors while parsing patterns:"
          mapM_ (qReport True) errStrings 
      makeTheParser
            (fmap phiPatternName     $ pdbPhonemeInfo pdb)
            (fmap phiArgumentOptions $ pdbPhonemeInfo pdb)
            (pdbTopPhonemeType pdb)
            (pdbMkMaj pdb)
            (pdbMkMin pdb)
            (pdbWordTypeNames pdb)
            (pps)
            (epdParserOptions epd)



