{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Metamorth.Interpretation.Output.TH
Description : Generating Phoneme Output Code
Copyright   : (c) David Wilson, 2024
License     : BSD-3

This is the module for generating output code.

-}

module Metamorth.Interpretation.Output.TH
  -- * Main Function
  ( generateOutputDecs
  -- * Extra Functions
  , makeOutputDatabase

  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

-- import Metamorth.Interpretation.Output.TH.Constructors
-- import Metamorth.Interpretation.Output.Types

-- searching for incorrectly entered modules in VS Code:
-- `[^`.]*\.[^`]*`

import Data.Text qualified as T

import Data.String (IsString(..))

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

import Data.Trie.Map qualified as TM

import Metamorth.ForOutput.Char

import Metamorth.Helpers.Q
import Metamorth.Helpers.QS
import Metamorth.Helpers.TH

import Metamorth.Helpers.Error
import Metamorth.Helpers.Map
import Metamorth.Helpers.Monad

import Metamorth.Interpretation.Output.TH.Trie
import Metamorth.Interpretation.Output.TH.Trie.Branches
import Metamorth.Interpretation.Output.TH.Types

import Metamorth.Interpretation.Output.Types
import Metamorth.Interpretation.Output.Types.Alt
import Metamorth.Interpretation.Output.Types.Interact
import Metamorth.Interpretation.Output.TH.Misc

import Metamorth.Interpretation.Output.Parsing.Types

import Metamorth.ForOutput.Monad.Matcher.Stateful
import Metamorth.ForOutput.Monad.Matcher.Stateful.Result

-- | The main function for creating output 
--   declarations.
generateOutputDecs 
  -- | The name of the function you want to create.
  :: String 
  -- | The suffix for generated internal functions/variables.
  -> String 
  -- | The output from the output parser.
  -> OutputParserOutput 
  -- | Information from the Phoneme parser.
  -> PhonemeNameInformation 
  -> Q [Dec]
generateOutputDecs userName sfx opo pni = runQS sfx $ do
  (ondDecs, ond, tmap) <- makeOutputDatabase opo pni
  ((stNom, stDecs), (nstNom, nstDecs)) <- generateBranches2 ond tmap
  
  userFuncName  <- qsPlainNewName $ varName userName
  userFuncName2 <- qsPlainNewName $ varName $ userName ++ "M"

  let (wordType, (wordCon1, wordCon2)) = ondWordTypes ond
      defSt = ondDefState ond
      charCase = ondCaseExpr ond
  
  xyz <- newName "xyz"
  makeOutStr <- newName "makeMonoid"

  caseFunc <- [| \x -> [$(pure charCase) x] |]

  func1Exp <- [| matchesLF' $(pure $ VarE stNom) $(pure $ VarE nstNom) |]
  func2Exp <- [| \case { $(pure $ ConP wordCon1 [] [VarP xyz] ) -> matchElse $(pure caseFunc) $(pure $ VarE xyz) $(pure defSt) $(pure func1Exp) 
                       ; $(pure $ ConP wordCon2 [] [VarP xyz] ) -> return $(pure $ AppE (VarE makeOutStr) (VarE xyz))
                       }  
              |]
  func3Exp <- [| matchesSimple $(pure func2Exp) |]

  outputSign1 <- [t| forall str. (IsString str, Monoid str) => (T.Text -> str) -> MatcherE $(pure $ ConT wordType) () () str |]
  let outputSig1  = SigD userFuncName2 outputSign1
      -- outputDec1  = ValD (VarP userFuncName2) (NormalB func3Exp) []
      outputDec1 = FunD userFuncName2 [Clause [VarP makeOutStr] (NormalB func3Exp) []]
      outputDefn1 = [outputSig1, outputDec1]
  
  outputSign2 <- [t| forall str. (IsString str, Monoid str) => (T.Text -> str) -> [$(pure $ ConT wordType)] -> Either String str |]
  outputFuncX <- [| evalMatcherE (const ()) $(pure $ VarE xyz) () $(pure $ AppE (VarE userFuncName2) (VarE makeOutStr)) |]
  let outputSig2  = SigD userFuncName outputSign2
      outputDec2  = FunD userFuncName [Clause [VarP makeOutStr, VarP xyz] (NormalB outputFuncX) []]
      outputDefn2 = [outputSig2, outputDec2]
  
  -- matchElse :: (Monad m, Monoid w) => (j -> w) -> [j] -> s' -> MatcherT j w s' m r -> MatcherT i v s m r
  -- matchesSimple :: (Monad m, Monoid v, Monoid r) => (i -> MatcherT i v s m r) -> MatcherT i v s m r


  return (outputDefn1 ++ outputDefn2 ++ ondDecs ++ stDecs ++ nstDecs)

-- generateBranches3 :: (Quasi q, Quote q) => OutputNameDatabase -> TM.TMap PhonePatternAlt (S.Set PhoneResult) -> q GroupedTrieDecs

makeOutputDatabase :: (Quasi q, Quote q) => OutputParserOutput -> PhonemeNameInformation -> q ([Dec], OutputNameDatabase, TM.TMap PhonePatternAlt (S.Set PhoneResult))
makeOutputDatabase opo pni = do
  (stDecs, stDict, stRecName, stRecNameC) <- makeStates (opoStateDictionary opo)
  -- hmm...
  -- , opoGroupDictionary :: S.Set String
  
  -- Create the group dict by looking up the names of the
  -- necessary functions in the PhonemeNameInformation.
  groupDict <- M.mapMaybe id <$> forMapFromSet (opoGroupDictionary opo) (\str -> qLookupErrorS str (pniGroups pni))

  let ond = OutputNameDatabase
        { ondPhonemes  = pniPhones  pni
        , ondAspects   = pniAspects pni -- just pass it in directly, not much point doing otherwise.
        , ondTraits    = M.empty
        , ondGroups    = groupDict
        , ondStateType = stRecName
        , ondStates    = stDict
        , ondPhoneType = pniPhoneType pni
        , ondWordTypes = pniWordTypeNames pni
        , ondCaseExpr  = AppE (VarE 'const) (ConE 'LowerCase) -- temp
        , ondDefState  = makeDefaultState stRecNameC stDict
        }
  return (stDecs, ond, opoOutputTrie opo)

{-
, pniAspects :: M.Map String (Name, (Name, M.Map String Name))
, opoAspectDictionary :: M.Map String (S.Set String)
, ondAspects :: M.Map String (Name, M.Map String Name)

-- | Convert a `S.Set` to a `M.Map` using
--   an applicative action on each element.
forMapFromSet :: (Applicative m) => S.Set a -> (a -> m b) -> m (M.Map a b)
forMapFromSet st f = sequenceA $ M.fromSet f st

-- | Convert a `S.Set` to a `M.Map` using
--   a monadic action on each element.
forMapFromSetM :: (Monad m) => S.Set a -> (a -> m b) -> m (M.Map a b)
forMapFromSetM st f = sequence $ M.fromSet f st
-}

makeStates :: (Quasi q, Quote q) => M.Map String (Maybe (S.Set String)) -> q ([Dec], M.Map String (Name, Maybe (Name, M.Map String Name)), Name, Name)
makeStates mp = do
  stateRecNameT <- newName "StateRecordsT"
  stateRecNameD <- newName "StateRecordsD"
  rslts <- forWithKey mp $ \str mset -> do
    stateNameT <- newName $ dataName str
    stateNameR <- newName $ varName  str
    case mset of 
      Nothing   -> return ((stateNameR, ConT ''Bool), (Nothing,[]))
      (Just st) -> do
        -- stateNameT is the name of the type
        rsltStates <- forMapFromSet st $ \newStr -> do
          newName $ dataName newStr
        
        let stateDecs = sumAdtDecDeriv stateNameT (map (,[]) $ M.elems rsltStates) [ConT ''Show, ConT ''Eq, ConT ''Ord]

        return ((stateNameR, AppT (ConT ''Maybe) (ConT stateNameT)), (Just (stateNameT, rsltStates), [stateDecs])  )


  let recFields  = M.map fst rslts
      subDecs    = concat $ M.elems $ M.map (snd . snd) rslts
      rsltState1 = M.map (second fst) rslts
      rsltStates = M.map (first fst) rsltState1
      recordDecs = recordAdtDecDeriv stateRecNameT stateRecNameD (M.elems recFields) [ConT ''Show, ConT ''Eq, ConT ''Ord]
  
  return (recordDecs:subDecs, rsltStates, stateRecNameT, stateRecNameD)

makeDefaultState :: Name -> M.Map String (Name, Maybe (Name, M.Map String Name)) -> Exp
makeDefaultState conName oldMap = RecConE conName (M.elems newMap)
  where 
    newMap = M.map f oldMap
    f (nom, Nothing) = (nom, ConE 'False)
    f (nom, Just  _) = (nom, ConE 'Nothing)

{-
recordAdtDecDeriv :: Name -> Name -> [(Name, Type)] -> [Type] -> Dec
recordAdtDecDeriv typeName consName fields ders =

sumAdtDecDeriv :: Name -> [(Name, [Type])] -> [Type] -> Dec
sumAdtDecDeriv a b ders =
  DataD [] a [] Nothing (fmap (uncurry sumCon) b) [DerivClause Nothing ders]


-}

{-
data OutputParserOutput = OutputParserOutput
  -- | The state dictionary. This is updated as
  --   the parser parses the state declarations.
  { opoStateDictionary :: M.Map String (Maybe (S.Set String))
  -- | The Group "Dictionary". This is supplied by
  --   the phoneme parser when the output files
  --   are run.
  , opoGroupDictionary :: S.Set String
  -- | The Trait Dictionary. This is supplied by
  --   the phoneme parser when the output files
  --   are run. The `S.Set` contains the possible
  --   values for the trait, if relevant.
  , opoTraitDictionary :: M.Map String (Maybe (S.Set String))
  -- | The Aspect Dictionary. This is supplied by
  --   the phoneme parser when the output files
  --   are run. The `S.Set` contains the possible
  --   values for the trait.
  , opoAspectDictionary :: M.Map String (S.Set String)
  -- | The main trie to be used for determining
  --   output.
  , opoOutputTrie       :: TM.TMap PhonePatternAlt (S.Set PhoneResult)
  -- , opoOutputTrie       :: TM.TMap PhonePattern (M.Map OutputCase OutputPattern)
  } deriving (Show, Eq)

data OutputNameDatabase = OutputNameDatabase
  { ondPhonemes :: M.Map String (Name, [M.Map String Name])
  , ondAspects :: M.Map String (Name, M.Map String Name)
  , ondTraits  :: M.Map String (Name, Maybe (M.Map String Name))
  , ondGroups  :: M.Map String Name
  , ondStateType :: Name
  , ondStates  :: M.Map String (Name, Maybe (Name, M.Map String Name))
  , ondPhoneType :: Name
  , ondCaseExpr :: Exp
  } deriving (Show, Eq)


-}






