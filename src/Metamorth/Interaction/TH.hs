{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Interaction.TH
  ( createParsers
  , declareParsers
  , ExtraParserDetails
  , defExtraParserDetails
  , ExtraOutputDetails
  , defExtraOutputDetails
  -- * Re-Exports
  , ParserOptions(..)
  , defParserOptions
  ) where

import Control.Monad

import Data.Maybe

import Data.Attoparsec.Text qualified as AT

import Metamorth.Interpretation.Parser.TH   qualified as Parser
import Metamorth.Interpretation.Phonemes.TH qualified as Phonemes

import Metamorth.Interpretation.Phonemes.Parsing (parsePhonemeFile)

import Metamorth.Interpretation.Phonemes.Parsing.Types 

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

import System.IO
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
  , epdParserName    :: String
  } deriving (Show, Eq)

defExtraParserDetails :: ExtraParserDetails
defExtraParserDetails  = ExtraParserDetails
  { epdParserOptions = defParserOptions
  , epdParserName    = "anotherParser"
  }

data ExtraOutputDetails = ExtraOutputDetails {} deriving (Show, Eq)

defExtraOutputDetails :: ExtraOutputDetails
defExtraOutputDetails  = ExtraOutputDetails
  {}

-- join $ runQ <$> producePropertyData <$> fromRight defaultPhonemeStructure <$> execPhonemeParser parsePhonemeFile <$> TIO.readFile "local/example1.thy"

declareParsers :: FilePath -> [(FilePath, ExtraParserDetails)] -> [(FilePath, ExtraOutputDetails)] -> Q [Dec]
declareParsers fp1 fps2 fps3 = do
  (GeneratedDecs d1 ds2 ds3) <- createParsers fp1 fps2 fps3
  return (d1 ++ (concat ds2) ++ (concat ds3))


-- | Create a `GeneratedDecs` from the desired input files.
createParsers :: FilePath -> [(FilePath, ExtraParserDetails)] -> [(FilePath, ExtraOutputDetails)] -> Q GeneratedDecs
createParsers phonemePath parserPaths _outputPaths = do
  
  -- Be careful running IO here...
  ePhoneData <- runIO $ readPhonemeFile phonemePath
  phoneText <- case ePhoneData of
    (Left err)  -> fail err
    (Right txt) -> return txt

  -- hmm...
  let ePhonemeRslt = execPhonemeParser parsePhonemeFile phoneText
  pps <- case ePhonemeRslt of
    (Left err) -> fail $ "Error(s) parsing \"" ++ phonemePath ++ "\":\n" ++ err
    (Right pb) -> return pb
  
  -- Get the phoneme database and the decs.
  (pdb, phoneDecs) <- producePhonemeDatabase pps

  -- get the parser files
  -- Note that this is probably fairly inefficient;
  -- all the text files are read into memory before
  -- being processed. This should change in the future.
  rslts@(eParseFiles, _) <- runIO $ do
    eParsers <- mapM readParserFile parserPaths
    return (eParsers, Nothing)

  parserResults <- fmap catMaybes $ forM eParseFiles $ \(eParseFile) -> do
    case eParseFile of
      (Left err)        -> (qReport True (err)) >> return Nothing
      (Right (txt,epd)) -> Just <$> getParserData pdb txt epd

  return $ GeneratedDecs phoneDecs (map fst parserResults) []

readPhonemeFile :: FilePath -> IO (Either String (Text))
readPhonemeFile fp = do
  bl <- doesFileExist fp
  case bl of
    False -> return $ Left $ "Could not find file \"" ++ fp ++ "\"."
    True  -> Right <$> readFileUTF8 fp

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
      newNameStr = epdParserName epd
  (dcs1, spi, funcNom) <- case eParseRslt of
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
  -- back here now
  let newNameNom = mkName newNameStr
  newDec <- [d| $(pure $ VarP newNameNom) = $(pure $ VarE funcNom) |]

  return ((newDec<>dcs1), spi)



