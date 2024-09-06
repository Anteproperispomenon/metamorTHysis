{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

module Metamorth.Interaction.TH
  ( createParsers
  , declareParsers
  , declareFullParsers
  , declareFullParsersNew
  , ExtraParserDetails(ExtraParserDetails, epdParserName, epdOtherNames, epdUnifyBranches, epdGroupGuards, epdCheckStates, epdMainFuncName, epdNameSuffix)
  -- , ParserOptions(..)
  , defExtraParserDetails
  , defExtraParserDetails'
  -- , editParserOptions
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

import Data.ByteString.Lazy qualified as BL

import Data.Text              qualified as T
import Data.Text.Lazy         qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text (Text)


import Metamorth.Helpers.Error

import Metamorth.Helpers.Q
import Metamorth.Helpers.TH

-- import Metamorth.Interpretation.Parser.TH   qualified as Parser
-- import Metamorth.Interpretation.Phonemes.TH qualified as Phonemes

import Metamorth.Interpretation.Output.Types (OutputHeader(..))

import Metamorth.Interpretation.Phonemes.Parsing (parsePhonemeFile)

import Metamorth.Interpretation.Phonemes.Parsing.Types 

import Metamorth.Interpretation.Parser.Parsing
import Metamorth.Interpretation.Parser.Types

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

import System.IO

import THLego.Helpers

import Metamorth.ForOutput.Functor.Cased

-- | Simple type for the output of the generated
--   declarations.
data GeneratedDecs = GeneratedDecs
  { gdPhonemeDecs   :: [Dec]
  , gdParserDecs    :: [[Dec]]
  , gdOutputDecs    :: [[Dec]]
  , gdTypeDecs      :: [Dec]
  , gdOrthTypeNames :: (Name, Name)
  , gdInputTypes    :: M.Map Name Name
  , gdOutputTypes   :: M.Map Name Name
  , gdOutputTypesBS :: M.Map Name Name
  , gdInputMap      :: [Dec]
  , gdOutputMap     :: [Dec]
  } deriving (Show, Eq)

-- | Options to go along with each parser file.
data ExtraParserDetails = ExtraParserDetails'
  { epdParserOptions :: ParserOptions
  , epdParserName'    :: String
  , epdOtherNames'    :: [String]
  } deriving (Show, Eq)

{-# COMPLETE ExtraParserDetails #-}

-- | A record pattern synonym that exposes all options at the top level.
pattern ExtraParserDetails :: String -> [String] -> Bool -> Bool -> Bool -> String -> String -> ExtraParserDetails
pattern ExtraParserDetails 
  { epdParserName
  , epdOtherNames
  , epdUnifyBranches
  , epdGroupGuards
  , epdCheckStates
  , epdMainFuncName
  , epdNameSuffix 
  }
  = ExtraParserDetails' 
    { epdParserName'   = epdParserName
    , epdOtherNames'   = epdOtherNames
    , epdParserOptions = ParserOptions 
       { poUnifyBranches = epdUnifyBranches
       , poGroupGuards   = epdGroupGuards
       , poCheckStates   = epdCheckStates
       , poMainFuncName  = epdMainFuncName
       , poNameSuffix    = epdNameSuffix
       }
    }

defExtraParserDetails' :: ExtraParserDetails
defExtraParserDetails'  = ExtraParserDetails'
  { epdParserOptions = defParserOptions'
  , epdParserName'    = "anotherParser"
  , epdOtherNames'    = []
  }

{-
data ParserOptions = ParserOptions
  { poUnifyBranches :: Bool
  , poGroupGuards   :: Bool
  , poCheckStates   :: Bool
  , poMainFuncName  :: String
  , poNameSuffix    :: String
  } deriving (Show, Eq)

-}

defExtraParserDetails :: String -> ExtraParserDetails
defExtraParserDetails str = ExtraParserDetails'
  { epdParserOptions = defParserOptions str
  , epdParserName'    = "anotherParser"
  , epdOtherNames'    = []
  }

-- editParserOptions :: (ParserOptions -> ParserOptions) -> ExtraParserDetails -> ExtraParserDetails
-- editParserOptions f epd = epd {epdParserOptions = f (epdParserOptions epd)}

data ExtraOutputDetails = ExtraOutputDetails 
  { eodOutputName :: String
  , eodSuffix     :: String
  , eodExtension  :: String
  , eodOtherNames :: [String]
  } deriving (Show, Eq)

defExtraOutputDetails :: ExtraOutputDetails
defExtraOutputDetails  = ExtraOutputDetails
  { eodOutputName = "mainOutput"
  , eodSuffix     = "_op1"
  , eodExtension  = ".op"
  , eodOtherNames = []
  }

-- Swap this when trying to run it
-- with IO.
addLocalDependentFile' :: FilePath -> Q ()
-- addLocalDependentFile' = \_ -> return ()
addLocalDependentFile' = addLocalDependentFile

-- join $ runQ <$> producePropertyData <$> fromRight defaultPhonemeStructure <$> execPhonemeParser parsePhonemeFile <$> TIO.readFile "local/example1.thy"

-- | Declare input parsers only, not declaring
--   the output parsers. Only use this if your
--   list of output files is empty.
declareParsers :: FilePath -> [(FilePath, ExtraParserDetails)] -> [(FilePath, ExtraOutputDetails)] -> Q [Dec]
declareParsers fp1 fps2 fps3 = do
  -- Maybe this will help?
  let allFps = fp1 : (map fst fps2) ++ (map fst fps3)
  mapM_ addLocalDependentFile' allFps
  (GeneratedDecs d1 ds2 ds3 ds4 (iNom, oNom) inMap outMap outMapBS inMapDec outMapDec) <- createParsers fp1 fps2 fps3
  
  -- Create the full function...
  -- funcDecs <- makeFullFunction iNom oNom inMap outMap

  return (d1 ++ (concat ds2) ++ (concat ds3))

-- | Declare the full parser, parsing from
--   any input type to any output type. This
--   will fail if either the input list or
--   the output list is empty.
declareFullParsers :: FilePath -> [(FilePath, ExtraParserDetails)] -> [(FilePath, ExtraOutputDetails)] -> Q [Dec]
declareFullParsers = declareFullParsersNew True

-- | Declare the full parser, parsing from
--   any input type to any output type. This
--   will fail if either the input list or
--   the output list is empty.
declareFullParsersNew :: Bool -> FilePath -> [(FilePath, ExtraParserDetails)] -> [(FilePath, ExtraOutputDetails)] -> Q [Dec]
declareFullParsersNew isCas fp1 fps2 fps3 = do
  -- Maybe this will help?
  let allFps = fp1 : (map fst fps2) ++ (map fst fps3)
  mapM_ addLocalDependentFile' allFps
  (GeneratedDecs d1 ds2 ds3 ds4 (iNom, oNom) inMap outMap outMapBS inMapDec outMapDec) <- createParsersNew isCas fp1 fps2 fps3
  
  -- Create the full function...
  funcDecs <- makeFullFunction iNom oNom inMap outMap outMapBS

  return (funcDecs ++ ds4 ++ d1 ++ (concat ds2) ++ (concat ds3) ++ inMapDec ++ outMapDec)

-- I wonder why addLocalDependentFile doesn't work?
-- Answer: It's not that it doesn't work, it's that
-- stack won't try to rebuild if none of the .hs files
-- have changed. i.e. if you change Spec.hs slightly,
-- stack will trigger a rebuild, which will cause any
-- modules that depend on a modified external file
-- (i.e. one added via `addLocalDependentFile`) to
-- recompile.

-- | Create a `GeneratedDecs` from the desired input files.
createParsers :: FilePath -> [(FilePath, ExtraParserDetails)] -> [(FilePath, ExtraOutputDetails)] -> Q GeneratedDecs
createParsers = createParsersNew True

-- | Create a `GeneratedDecs` from the desired input files.
createParsersNew :: Bool -> FilePath -> [(FilePath, ExtraParserDetails)] -> [(FilePath, ExtraOutputDetails)] -> Q GeneratedDecs
createParsersNew canBeCased phonemePath parserPaths outputPaths = do
  
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
  (pdb', phoneDecs) <- producePhonemeDatabase pps

  let pdb = pdb' {pdbIsCased = canBeCased}

  -- Check the group map:
  -- qDebugNotice $ "Group Map: " ++ show (pdbGroupMemberFuncs pdb)

  -- get the parser files
  -- Note that this is probably fairly inefficient;
  -- all the text files are read into memory before
  -- being processed. This should change in the future.
  _rslts@(eParseFiles, _) <- runIO $ do
    eParsers <- mapM readParserFile parserPaths
    return (eParsers, Nothing)

  parserResultsBoth' <- fmap catMaybes $ forM eParseFiles $ \eParseFile -> do
    case eParseFile of
      (Left err)           -> (qReport True err) >> return Nothing
      (Right (txt,epd,fp)) -> do
        addLocalDependentFile' fp
        Just <$> getParserData fp pdb txt epd
  
  let parserResultsBoth = map fst parserResultsBoth'
      parserResults     = map fst parserResultsBoth
      parserTypes       = map snd parserResultsBoth
      parserNameExprs   = concatMap snd parserResultsBoth'
      parserNameExprs'  = map (\(x1,x2) -> TupE [Just x1, Just x2]) parserNameExprs

  -- Originally, the name creation was handled here.
  -- Hence the weird usage of forM.
  inOrthMap <- fmap M.fromList $ forM parserTypes $ \(str, funcNom) -> do
    -- Maybe need to create an alternative if name is empty...
    return (str, funcNom)

  _rslts2@(eOutputFiles, _) <- runIO $ do
    eOutputs <- mapM readOutputFile outputPaths
    return (eOutputs, Nothing)

  -- let pni = makePhonemeInformation pdb
  outputResults' <- fmap catMaybes $ forM eOutputFiles $ \eOutputFile -> do
    case eOutputFile of
      (Left err) -> (qReportError err) >> return Nothing
      (Right (txt, eod)) -> do
        Just <$> getTheOutput pdb txt eod
  
  -- The lift of orthography names.
  let outputResults  = map fst outputResults'
      outOrthNames'  = map fst outputResults
      outOrthNames   = map fst outOrthNames'
      outOrthNamesBS = map snd outOrthNames'
      outOrthMap     = M.fromList outOrthNames
      outOrthMapBS   = M.fromList outOrthNamesBS
      outputNamers   = concatMap snd outputResults'
      outputNamers'  = map (\(x1,x2) -> TupE [Just x1, Just x2]) outputNamers

  outOrthTypeDec <- makeOutputOrthType outOrthMap
  inOrthTypeDec  <-  makeInputOrthType  inOrthMap


  -- Make the map of input names.
  inputOrthNameMap  <- [| M.fromList $(pure $ ListE parserNameExprs') |]
  inputOrthNameType <- [t| M.Map String $(pure $ ConT $ fst inOrthTypeDec) |]
  inputOrthNameName <- newName "inputOrthNameMap"
  let inputOrthNameSign = SigD inputOrthNameName inputOrthNameType
      inputOrthNameDefn = ValD (VarP inputOrthNameName) (NormalB inputOrthNameMap) []
      inputOrthNameDecl = [inputOrthNameSign, inputOrthNameDefn]


  -- Make the map of output names.
  outputOrthNameMap  <- [| M.fromList $(pure $ ListE outputNamers') |]
  outputOrthNameType <- [t| M.Map String ($(pure $ ConT $ fst outOrthTypeDec), String) |]
  outputOrthNameName <- newName "outputOrthNameMap"
  let outputOrthNameSign = SigD outputOrthNameName outputOrthNameType
      outputOrthNameDefn = ValD (VarP outputOrthNameName) (NormalB $ outputOrthNameMap) []
      outputOrthNameDecl = [outputOrthNameSign, outputOrthNameDefn]

  return $ GeneratedDecs 
             phoneDecs 
             (map fst parserResults)
             (map snd outputResults)
             [snd inOrthTypeDec, snd outOrthTypeDec]
             (fst inOrthTypeDec, fst outOrthTypeDec)
             inOrthMap
             outOrthMap
             outOrthMapBS
             inputOrthNameDecl
             outputOrthNameDecl

readPhonemeFile :: FilePath -> IO (Either String Text)
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


getParserData :: FilePath -> PhonemeDatabase -> Text -> ExtraParserDetails -> Q ((([Dec], StaticParserInfo), (Name, Name)), [(Exp, Exp)])
getParserData fp pdb txt epd = do
  -- New information needed
  let pni = makePhonemeInformation pdb
      aspectSet = M.map (M.keysSet . snd . snd) (pniAspects pni)
      phoneSet  = M.keysSet $ pniPhones pni
      
      traitDict = pdbTraitInformation pdb 
      -- From Map String (Name, Maybe (Name, Map String Name))
      -- To   Map String (Maybe (Set String))\
      traitMaps = fmap (\(_, mb) -> fmap (\(_,mp) -> M.keysSet mp) mb) traitDict

      -- (HeaderData, ParserParsingState, [String], [String])
      eParseRslt = AT.parseOnly (parseOrthographyFileNew (M.keysSet (pdbGroupMemberFuncs pdb)) traitMaps aspectSet phoneSet) txt
      newNameStr = epdParserName epd
  ((dcs1, spi, funcNom), typeNom) <- case eParseRslt of
    (Left err) -> fail $ "Couldn't parse input file \"" ++ fp ++ "\": " ++ err
    -- (HeaderData, ParserParsingState, [String])
    (Right (hdr, pps, errStrings, warnStrings)) -> do
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
      prs <- makeTheParser
            (fmap phiPatternName     $ pdbPhonemeInfo pdb)
            (fmap phiArgumentOptions $ pdbPhonemeInfo pdb)
            (pdbTopPhonemeType pdb)
            (pdbMkMaj pdb)
            (pdbMkMin pdb)
            (pdbIsCased pdb)
            (pdbWordTypeNames pdb)
            (pniAspects pni)
            (pdbTraitInformation pdb)
            (pdbGroupMemberFuncs pdb)
            (pps)
            (epdParserOptions epd)
      return (prs, hdOrthName hdr)
  -- back here now
  let newNameNom = mkName newNameStr
  newSig <- SigD newNameNom <$> [t| AT.Parser [$(pure $ ConT $ fst $ pdbWordTypeNames pdb)]  |]
  newDec <- [d| $(pure $ VarP newNameNom) = $(pure $ VarE funcNom) |]

  orthName <- newName $ "In" ++ (dataName typeNom)

  let otherNames = map strE $ epdOtherNames epd
      otherPairs = map (,ConE orthName) otherNames

  return (((newSig:newDec<>dcs1, spi), (orthName, funcNom)), otherPairs)

makePhonemeInformation :: PhonemeDatabase -> PhonemeNameInformation
makePhonemeInformation pdb = PhonemeNameInformation
  { pniPhones   = M.map unwrap $ pdbPhonemeInfo pdb
  , pniAspects  = aspectTable pdt
  , pniTraits   = pdbTraitInformation pdb
  , pniGroups   = pdbGroupMemberFuncs pdb
  , pniWordTypeNames = pdbWordTypeNames pdb
  , pniCaseExpr  = AppE (VarE 'const) (ConE 'LowerCase)
  , pniPhoneType = pdbTopPhonemeType pdb
  , pniCanBeCased = pdbIsCased pdb
  }
  where 
    pdt = pdbPropertyData pdb
    unwrap (PhonemeInformation x y) = (x,y)

getTheOutput :: PhonemeDatabase -> Text -> ExtraOutputDetails -> Q ((((Name, Name), (Name, Name)), [Dec]), [(Exp, Exp)])
getTheOutput pdb txt eod = do
  let pni = makePhonemeInformation pdb
      aspectSet = M.map (M.keysSet . snd . snd) (pniAspects pni)
      phoneSet  = M.keysSet $ pniPhones pni
      
      traitDict = pdbTraitInformation pdb 
      -- From Map String (Name, Maybe (Name, Map String Name))
      -- To   Map String (Maybe (Set String))\
      traitMaps = fmap (\(_, mb) -> fmap (\(_,mp) -> M.keysSet mp) mb) traitDict
  let eParseRslt = AT.parseOnly (parseOutputFile (M.keysSet (pdbGroupMemberFuncs pdb)) traitMaps aspectSet phoneSet) txt
      newNameStr = eodOutputName eod
      newNameSfx = eodSuffix eod
  -- (decs, hdrX) <- case eParseRslt of
  case eParseRslt of
    (Left err) -> fail $ "Couldn't parse output specification: " ++ err
    (Right (opo, hdr, errMsgs)) -> do
      let (errs, wrns, _msgs) = partitionMessages errMsgs
      mapM_ qReportError   errs
      mapM_ qReportWarning wrns
      ((userFuncName, userFuncNameBS), decs) <- generateOutputDecs newNameStr newNameSfx opo pni

      hdrOut <- case (ohOrthName hdr) of
        "" -> newName $ "OutOrth" ++ newNameSfx
        x  -> newName $ "Out" ++ dataName x

      let otherNames = map strE  $ eodOtherNames eod
          thisExt    = stringExp $ eodExtension  eod
          otherPairs = map (,tupleE [ConE hdrOut, thisExt]) otherNames

      return ((((hdrOut, userFuncName), (hdrOut, userFuncNameBS)), decs), otherPairs)
  
  -- return (hdrX, decs)

-- sumAdtDecDeriv

-- | Create the data type that will allow you
--   to select the output orthography.
makeOutputOrthType :: (Quote q) => M.Map Name Name -> q (Name, Dec)
makeOutputOrthType mps = do
  mainTypeName <- newName "OutOrth"
  let funcs = map (,[]) $ M.keys mps
  return (mainTypeName, sumAdtDecDeriv mainTypeName funcs [eqC, ordC, showC, enumC, boundC, liftC])
  where
    ordC   = ConT ''Ord
    eqC    = ConT ''Eq
    showC  = ConT ''Show
    enumC  = ConT ''Enum
    boundC = ConT ''Bounded
    liftC  = ConT ''Lift

-- | Create the data type that will allow you
--   to select the input orthography.
makeInputOrthType :: (Quote q) => M.Map Name Name -> q (Name, Dec)
makeInputOrthType mps = do
  mainTypeName <- newName "InOrth"
  let funcs = map (,[]) $ M.keys mps
  return (mainTypeName, sumAdtDecDeriv mainTypeName funcs [eqC, ordC, showC, enumC, boundC, liftC])
  where
    ordC   = ConT ''Ord
    eqC    = ConT ''Eq
    showC  = ConT ''Show
    enumC  = ConT ''Enum
    boundC = ConT ''Bounded
    liftC  = ConT ''Lift


-- | Make the full function that can easily
--   be called by a CLI application.
makeFullFunction :: forall q. (Quote q, Quasi q) => Name -> Name -> M.Map Name Name -> M.Map Name Name -> M.Map Name Name -> q [Dec]
makeFullFunction iNom oNom inNames outNames outNamesBS = do 
  funcName <- newName "convertOrthography"
  funcType <- [t| $(pure $ ConT iNom) -> $(pure $ ConT oNom) -> Text -> Either String Text |]
  funcSign <- return $ SigD funcName funcType

  funcNameL <- newName "convertOrthographyLazy"
  funcTypeL <- [t| $(pure $ ConT iNom) -> $(pure $ ConT oNom) -> Text -> Either String TL.Text |]
  funcSignL <- return $ SigD funcNameL funcTypeL

  funcNameBS <- newName "convertOrthographyBS"
  funcTypeBS <- [t| $(pure $ ConT iNom) -> $(pure $ ConT oNom) -> Text -> Either String BL.ByteString |]
  funcSignBS <- return $ SigD funcNameBS funcTypeBS
  
  -- For the functions that select the input/output
  -- function to use based on the selector type.
  tempNameI <- newName "selectI"
  tempNameO <- newName "selectO"
  tempNameL <- newName "selectOL"
  tempNameB <- newName "selectBS"

  extraName1 <- newName "abc"

  -- Too lazy to fill out the type signature...
  tempTypeI <- [t| $(pure $ ConT iNom) -> Text -> Either String _   |]
  tempTypeO <- [t| $(pure $ ConT oNom) -> _    -> Either String Text|]
  
  -- Link up the signatures and definitions.
  let tempDefnI = M.elems $ M.mapWithKey makeClauseI  inNames
      tempDefnO = M.elems $ M.mapWithKey makeClauseO outNames
      tempDefnL = M.elems $ M.mapWithKey (makeClauseOB extraName1) outNames
      tempDefnB = M.elems $ M.mapWithKey makeClauseBS outNamesBS
  
      tempFuncI = FunD tempNameI tempDefnI
      tempFuncO = FunD tempNameO tempDefnO
      tempFuncL = FunD tempNameL tempDefnL
      tempFuncB = FunD tempNameB tempDefnB

      tempSignI = SigD tempNameI tempTypeI
      tempSignO = SigD tempNameO tempTypeO

      -- whereDecs = [tempSignI, tempFuncI, tempSignO, tempFuncO]
      whereDecs  = [tempFuncI, tempFuncO]
      whereDecsL = [tempFuncI, tempFuncL]
      whereDecsB = [tempFuncI, tempFuncB]
  
  when (M.null  inNames) $ qReportWarning  "There are no Input Orthography Names"
  when (M.null outNames) $ qReportWarning "There are no Output Orthography Names"

  iVarNom <- newName "iType"
  oVarNom <- newName "oType"
  tVarNom <- newName "txt"

  -- i.e. (selectI iType txt) >>= (selectO oType)
  funcExp  <- [| ($(pve tempNameI) $(pve iVarNom) $(pve tVarNom)) >>= ($(pve tempNameO) $(pve oVarNom)) |]
  funcExpL <- [| ($(pve tempNameI) $(pve iVarNom) $(pve tVarNom)) >>= ($(pve tempNameL) $(pve oVarNom)) |]
  funcExpB <- [| ($(pve tempNameI) $(pve iVarNom) $(pve tVarNom)) >>= ($(pve tempNameB) $(pve oVarNom)) |]

  -- Okay now...
  let mainFuncDefn = FunD funcName 
        [ Clause
            [VarP iVarNom, VarP oVarNom, VarP tVarNom]
            (NormalB funcExp)
            whereDecs
        ]

      mainFuncDefnLazy = FunD funcNameL
        [ Clause
            [VarP iVarNom, VarP oVarNom, VarP tVarNom]
            (NormalB funcExpL)
            whereDecsL
        ]
      
      mainFuncDefnBS = FunD funcNameBS
        [ Clause
            [VarP iVarNom, VarP oVarNom, VarP tVarNom]
            (NormalB funcExpB)
            whereDecsB
        ]

  
  return [funcSign, mainFuncDefn, funcSignL, mainFuncDefnLazy, funcSignBS, mainFuncDefnBS]
  where
    makeClauseO :: Name -> Name -> Clause
    makeClauseO dNom fNom = Clause [ConP dNom [] []] (NormalB (AppE (VarE fNom) (VarE 'id))) []

    -- make Clause for Builder-based text, which will be lazy, I guess?
    makeClauseOB :: Name -> Name -> Name -> Clause
    makeClauseOB exNom dNom fNom = Clause 
      [ConP dNom [] [], VarP exNom] 
      (NormalB $ AppE (AppE (VarE 'fmap) (VarE 'TB.toLazyText)) $ multiAppE (VarE fNom) [VarE 'TB.fromText, VarE exNom] ) 
      []
    
    makeClauseBS :: Name -> Name -> Clause
    makeClauseBS dNom fNom = Clause
      [ConP dNom [] []]
      (NormalB $ VarE fNom)
      []

    makeClauseI :: Name -> Name -> Clause
    makeClauseI dnom fNom = Clause [ConP dnom [] []] (NormalB (AppE (VarE 'AT.parseOnly) (VarE fNom))) []

    pve :: Name -> q Exp
    pve = pure . VarE 


{-

opsTraitDictionary' :: M.Map String (Maybe (S.Set String))

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
  -- | Functions for checking membership in a group.
  , pdbGroupMemberFuncs :: M.Map String Name
  -- | Functions for checking whether a function has
  --   a trait, and whether that trait is a value trait
  --   (@True@) or a boolean trait (@False@).
  , pdbTraitInformation :: M.Map String (Name, (Maybe (Name, M.Map String Name)))
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

