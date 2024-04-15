{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Interaction.TH
  ( createParsers
  , declareParsers
  , ExtraParserDetails(..)
  , defExtraParserDetails
  , defExtraParserDetails'
  , ExtraOutputDetails(..)
  , defExtraOutputDetails
  -- * Re-Exports
  , ParserOptions(..)
  , defParserOptions
  ) where

import Control.Monad

import Data.Maybe

import Data.Attoparsec.Text qualified as AT

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

import Metamorth.Helpers.Error

import Metamorth.Helpers.Q
import Metamorth.Helpers.TH

-- import Metamorth.Interpretation.Parser.TH   qualified as Parser
-- import Metamorth.Interpretation.Phonemes.TH qualified as Phonemes

import Metamorth.Interpretation.Phonemes.Parsing (parsePhonemeFile)

import Metamorth.Interpretation.Phonemes.Parsing.Types 

import Metamorth.Interpretation.Parser.Parsing

import Metamorth.Interpretation.Parser.TH
  ( makeTheParser
  , ParserOptions(..)
  , defParserOptions
  , defParserOptions'
  , StaticParserInfo(..)
  )

import Metamorth.Interpretation.Phonemes.TH
  ( PhonemeDatabase(..)
  , PhonemeInformation(..)
  , producePhonemeDatabase
  , PropertyData(..)
  )

import Metamorth.ForOutput.Char

import Metamorth.Interpretation.Output.TH ( generateOutputDecs )
import Metamorth.Interpretation.Output.Parsing ( parseOutputFile )
import Metamorth.Interpretation.Output.Types.Interact (PhonemeNameInformation(..))

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

import Metamorth.Helpers.IO

-- import System.IO
import System.Directory

import Data.Text qualified as T
import Data.Text (Text)

import System.IO

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

defExtraParserDetails' :: ExtraParserDetails
defExtraParserDetails'  = ExtraParserDetails
  { epdParserOptions = defParserOptions'
  , epdParserName    = "anotherParser"
  }

defExtraParserDetails :: String -> ExtraParserDetails
defExtraParserDetails str = ExtraParserDetails
  { epdParserOptions = defParserOptions str
  , epdParserName    = "anotherParser"
  }

data ExtraOutputDetails = ExtraOutputDetails 
  { eodOutputName :: String
  , eodSuffix     :: String
  } deriving (Show, Eq)

defExtraOutputDetails :: ExtraOutputDetails
defExtraOutputDetails  = ExtraOutputDetails
  { eodOutputName = "mainOutput"
  , eodSuffix     = "_op1"
  }

addLocalDependentFile' :: (Quasi q) => FilePath -> q ()
addLocalDependentFile' = \_ -> return ()
-- addLocalDependentFile' = addLocalDependentFile

-- join $ runQ <$> producePropertyData <$> fromRight defaultPhonemeStructure <$> execPhonemeParser parsePhonemeFile <$> TIO.readFile "local/example1.thy"

declareParsers :: FilePath -> [(FilePath, ExtraParserDetails)] -> [(FilePath, ExtraOutputDetails)] -> Q [Dec]
declareParsers fp1 fps2 fps3 = do
  -- Maybe this will help?
  let allFps = fp1 : (map fst fps2) ++ (map fst fps3)
  mapM_ addLocalDependentFile' allFps
  (GeneratedDecs d1 ds2 ds3) <- createParsers fp1 fps2 fps3
  return (d1 ++ (concat ds2) ++ (concat ds3))

-- I wonder why addLocalDependentFile doesn't work?

-- | Create a `GeneratedDecs` from the desired input files.
createParsers :: FilePath -> [(FilePath, ExtraParserDetails)] -> [(FilePath, ExtraOutputDetails)] -> Q GeneratedDecs
createParsers phonemePath parserPaths outputPaths = do
  
  -- Be careful running IO here...
  ePhoneData <- runIO $ readPhonemeFile phonemePath
  addLocalDependentFile' phonemePath
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
  _rslts@(eParseFiles, _) <- runIO $ do
    eParsers <- mapM readParserFile parserPaths
    return (eParsers, Nothing)

  parserResults <- fmap catMaybes $ forM eParseFiles $ \(eParseFile) -> do
    case eParseFile of
      (Left err)           -> (qReport True (err)) >> return Nothing
      (Right (txt,epd,fp)) -> do
        addLocalDependentFile' fp
        Just <$> getParserData pdb txt epd

  _rslts2@(eOutputFiles, _) <- runIO $ do
    eOutputs <- mapM readOutputFile outputPaths
    return (eOutputs, Nothing)

  -- let pni = makePhonemeInformation pdb
  outputResults <- fmap catMaybes $ forM eOutputFiles $ \eOutputFile -> do
    case eOutputFile of
      (Left err) -> (qReportError err) >> return Nothing
      (Right (txt, eod)) -> do
        Just <$> getTheOutput pdb txt eod

  return $ GeneratedDecs phoneDecs (map fst parserResults) outputResults

readPhonemeFile :: FilePath -> IO (Either String (Text))
readPhonemeFile fp = do
  bl <- doesFileExist fp
  case bl of
    False -> return $ Left $ "Could not find file \"" ++ fp ++ "\"."
    True  -> Right <$> readFileUTF8 fp

-- | Returns the file data along with any info
--   the code needs.
readParserFile :: (FilePath, ExtraParserDetails) -> IO (Either String (Text, ExtraParserDetails, FilePath))
readParserFile (fp, epd) = do
  bl <- doesFileExist fp
  case bl of
    False -> return $ Left $ "Could not find file \"" ++ fp ++ "\"."
    True  -> do
      txt <- readFileUTF8 fp -- for now...
      return $ Right (txt, epd, fp)

readOutputFile :: (FilePath, ExtraOutputDetails) -> IO (Either String (Text, ExtraOutputDetails))
readOutputFile (fp, eod) = do
  bl <- doesFileExist fp
  case bl of
    False -> return $ Left $ "Could not find file \"" ++ fp ++ "\"."
    True  -> do
      txt <- readFileUTF8 fp -- for now...
      return $ Right (txt, eod)


getParserData :: PhonemeDatabase -> Text -> ExtraParserDetails -> Q ([Dec], StaticParserInfo)
getParserData pdb txt epd = do
  -- here
  let eParseRslt = AT.parseOnly parseOrthographyFile txt
      newNameStr = epdParserName epd
  (dcs1, spi, funcNom) <- case eParseRslt of
    (Left err) -> fail $ "Couldn't parse input: " ++ err
    -- (HeaderData, ParserParsingState, [String])
    (Right (_hdr, pps, errStrings, warnStrings)) -> do
      case errStrings of
        [] -> return ()
        _  -> do
          -- reportError "Encountered errors while parsing patterns:"
          mapM_ reportError errStrings 
      case warnStrings of
        [] -> return ()
        _  -> do
          -- reportWarning "Encountered warnings while parsing patterns:"
          mapM_ reportWarning warnStrings 
          return ()
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
  newSig <- SigD newNameNom <$> [t| AT.Parser [$(pure $ ConT $ fst $ pdbWordTypeNames pdb)]  |]
  newDec <- [d| $(pure $ VarP newNameNom) = $(pure $ VarE funcNom) |]

  return ((newSig:newDec<>dcs1), spi)

makePhonemeInformation :: PhonemeDatabase -> PhonemeNameInformation
makePhonemeInformation pdb = PhonemeNameInformation
  { pniPhones   = M.map unwrap $ pdbPhonemeInfo pdb
  , pniAspects  = aspectTable pdt
  , pniTraits   = M.empty
  , pniGroups   = M.empty -- oh well.
  , pniWordTypeNames = pdbWordTypeNames pdb
  , pniCaseExpr  = AppE (VarE 'const) (ConE 'LowerCase)
  , pniPhoneType = pdbTopPhonemeType pdb
  }
  where 
    pdt = pdbPropertyData pdb
    unwrap (PhonemeInformation x y) = (x,y)

getTheOutput :: PhonemeDatabase -> Text -> ExtraOutputDetails -> Q [Dec]
getTheOutput pdb txt eod = do
  let pni = makePhonemeInformation pdb
      aspectSet = M.map (M.keysSet . snd . snd) (pniAspects pni)
      phoneSet  = M.keysSet $ pniPhones pni
  let eParseRslt = AT.parseOnly (parseOutputFile S.empty M.empty aspectSet phoneSet) txt
      newNameStr = eodOutputName eod
      newNameSfx = eodSuffix eod
  decs <- case eParseRslt of
    (Left err) -> fail $ "Couldn't parse output specification: " ++ err
    (Right (opo, errMsgs)) -> do
      runIO $ do
        hPutStrLn stdout "Finished Parsing Output Spec..."
        hPrint    stdout opo
        hFlush stderr
      let (errs, wrns, msgs) = partitionMessages errMsgs
      mapM_ qReportError   errs
      mapM_ qReportWarning wrns
      generateOutputDecs newNameStr newNameSfx opo pni
  
  return decs

{-

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
  -> AT.Parser (OutputParserOutput, [ParserMessage])


data PhonemeNameInformation = PhonemeNameInformation
  { pniPhones  :: M.Map String (Name, [M.Map String Name])
  , pniAspects :: M.Map String (Name, (Name, M.Map String Name))
  , pniTraits  :: M.Map String (Name, Maybe (Name, M.Map String Name))
  , pniGroups  :: M.Map String Name -- :: Phoneme -> Bool
  , pniWordTypeNames  :: (Name, (Name, Name))
  , pniCaseExpr  :: Exp
  , pniPhoneType :: Name
  } deriving (Show, Eq)

data PhonemeDatabase = PhonemeDatabase
  { pdbPropertyData   :: PropertyData
  -- | A `M.Map` from Strings of Phonemes to the
  --   `Name`s of their constructors.
  , pdbPhonemeInfo    :: M.Map String PhonemeInformation
  -- | The top-type of the Phonemes.
  , pdbTopPhonemeType :: Name
  -- | The `Name` of the "Word" type, along
  --   with its two constructors.
  , pdbWordTypeNames  :: (Name, (Name, Name))
  -- | Make an uncased expression an upper-case expression.
  , pdbMkMaj :: Exp -> Exp
  -- | Make an uncased expression a  lower-case expression.
  , pdbMkMin :: Exp -> Exp
  }

-- | A type to make understanding the output
--   of `producePropertyData` easier.
data PropertyData = PropertyData
  -- | Table of aspects. Return value is
  --   @Map of Aspect String -> (Type Name, (Record Name, Map of Option String -> Type Name ))@
  { aspectTable  :: M.Map String (Name, (Name, M.Map String Name))
  , traitTable   :: M.Map String (Name, Maybe (Name, M.Map String Name))
  , traitData    :: Maybe TraitData
  , propertyDecs :: [Dec]
  } deriving (Show, Eq)

-}

