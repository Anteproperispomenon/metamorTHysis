{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Interpretation.Output.TH.Trie.Branches
  (

  ) where

import Data.String (IsString(..))

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Data.Trie.Map qualified as TM

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

import Metamorth.Helpers.Q
import Metamorth.Helpers.Trie
import Metamorth.Helpers.Monad

import Metamorth.ForOutput.Char
import Metamorth.ForOutput.Monad.Matcher.Stateful
import Metamorth.ForOutput.Monad.Matcher.Stateful.Result

import Metamorth.Interpretation.Output.TH.Types

import Metamorth.Interpretation.Output.Types
import Metamorth.Interpretation.Output.Types.Alt
-- import Metamorth.Interpretation.Output.Parser.Types

import Metamorth.Interpretation.Output.TH.Constructors
import Metamorth.Interpretation.Output.TH.Trie

import Text.Printf

-- TM.TMap PhonePatternAlt (S.Set PhoneResult)

{-
data PhonePatternAlt
  = PhonemeNameZ PhoneName
  | PhoneAtStartZ
  | PhoneNotStartZ
  -- | PhoneAtEndZ
  -- | PhoneNotEndZ
  deriving (Show, Eq, Ord)
-}

-- | Simple type to save typing.
data GroupedTrie = GroupedTrie
  { plainTrie    :: TM.TMap PhonePatternAlt (S.Set PhoneResult)
  , atStartTrie  :: TM.TMap PhonePatternAlt (S.Set PhoneResult)
  , notStartTrie :: TM.TMap PhonePatternAlt (S.Set PhoneResult)
  } deriving (Show, Eq)

-- | Group the branches into plain, at-start,
--   and not-start.
groupBranches :: TM.TMap PhonePatternAlt (S.Set PhoneResult) -> GroupedTrie
groupBranches tm = GroupedTrie tm' atStart notStart
  where 
    (_, atStart) = TM.match tm [PhoneAtStartZ ]
    (_,notStart) = TM.match tm [PhoneNotStartZ]
    tm' = deleteBranches [PhoneAtStartZ, PhoneNotStartZ] tm

incState :: (Monad m) => StateT Int m Int
incState = do
  z <- get
  put (z+1)
  return z

-- | Create the branches at each step of the way...
generateBranches' :: (Quasi q, Quote q) => OutputNameDatabase -> TM.TMap PhonePatternAlt (S.Set PhoneResult) -> StateT Int q (Name, Name, [Dec])
generateBranches' ond tmp = do
  let (mRslt,_) = TM.match [] tmp
      phoneMap  = ondPhonemeNames  ond
      phoneCns  = ondPhonemeConsts ond
  
  theMainVar <- lift $ newName "curPhone"
  n <- incState
  let funcString  = printf "outputBranch_%04d" n
      funcString2 = printf "outputCrunch_%04d" n
  funcName  <- newName funcString
  fundName2 <- newName funcString2

  -- Match over the sub-results... I guess...
  rslts <- forMaybeM (getSubTries tmp) $ \(c, (_elem, subTrie)) -> do
    -- (subNom, subDecs) <- generateBranches ond subTrie
    case c of
      PhoneAtStartZ -> do
        qReportError $ "Can't have an `AtStart` in the middle of a pattern."
        return Nothing
      PhoneNotStartZ -> do
        qReportError $ "Can't have a `NotStart` in the middle of a pattern."
        return Nothing
      (PhonemeNameZ pn) -> do
        let ePat = makePhoneConstructorPat phoneMap phoneCns pn
        case ePat of
          (Left errs) -> do
            mapM_ qReportError errs
            return Nothing
          (Right pat)  -> do
            -- myExp <- [|  |]
            (subNom, subNom2, subDecs) <- generateBranches' ond subTrie
            let myMatch = Match pat (NormalB $ VarE subNom2) []
            return $ Just (subDecs, myMatch)
  
  failExp <- [| MatchFail "Example" |]
  let failPat = Match WildP (NormalB failExp) []
      phoneType = ConT $ ondPhoneType ond
      stateType = ConT $ ondStateType ond

  funcSign <- [t| forall m s. (MonadFail m, IsString s) => $(pure phoneType) -> MatchResultT m $(pure phoneType) [CharCase] $(pure stateType) s |]
  othrSign <- [t| forall m s. (MonadFail m, IsString s) => MatchResultT m $(pure phoneType) [CharCase] $(pure stateType) s |]

  let (subDecs', mats) = unzip rslts
      myCase = CaseE (VarE theMainVar) (mats ++ [failPat])

  return (funcName, funcName2, [] ++ (concat subDecs'))

-- data MatchResult m i v s r
-- MatcherT i v s m a
-- i == (ConT $ ondPhoneType ond)
-- v == ([CharCase])
-- s == (ConT $ ondStateType ond)
-- m == 


-- CaseE Exp [Match]
-- Match Pat Body [Dec]

-- makePhoneConstructorPat :: M.Map String Name -> M.Map String [M.Map String Name] -> PhoneName -> Either [String] Pat


