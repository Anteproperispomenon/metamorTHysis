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
  (

  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

-- import Metamorth.Interpretation.Output.TH.Constructors
-- import Metamorth.Interpretation.Output.Types

-- searching for incorrectly entered modules in VS Code:
-- `[^`.]*\.[^`]*`

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

import Data.Trie.Map qualified as TM

import Metamorth.Helpers.Q
import Metamorth.Helpers.QS
import Metamorth.Helpers.TH

import Metamorth.Helpers.Map

import Metamorth.Interpretation.Output.TH.Trie
import Metamorth.Interpretation.Output.TH.Trie.Branches
import Metamorth.Interpretation.Output.TH.Types

import Metamorth.Interpretation.Output.Types
import Metamorth.Interpretation.Output.Types.Alt
import Metamorth.Interpretation.Output.Types.Interact

import Metamorth.Interpretation.Output.Parsing.Types

import Metamorth.ForOutput.Monad.Matcher.Stateful
import Metamorth.ForOutput.Monad.Matcher.Stateful.Result

makeOutputDatabase :: (Quasi q, Quote q) => OutputParserOutput -> PhonemeNameInformation -> q (OutputNameDatabase, TM.TMap PhonePatternAlt (S.Set PhoneResult))
makeOutputDatabase ond pni = do
  -- hmm...
  fail "not yet implemented"


makeStates :: (Quasi q, Quote q) => M.Map String (Maybe (S.Set String)) -> q ([Dec], M.Map String (Name, (Maybe (Name, M.Map String Name))))
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
  
  return (recordDecs:subDecs, rsltStates)


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






