{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Interpretation.Output.TH.Trie.Branches
  ( generateBranches
  , GroupedTrieDecs(..)
  , generateBranches2
  , generateBranches3
  ) where

import Data.String (IsString(..))

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict qualified as State

import Data.Trie.Map qualified as TM

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

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

import Data.Semigroup (Arg(..))

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

-- | Simple type to save typing.
data GroupedTrieDecs = GroupedTrieDecs
  { plainTrieDecs    :: (Name, [Dec])
  , atStartTrieDecs  :: (Name, [Dec])
  , notStartTrieDecs :: (Name, [Dec])
  } deriving (Show, Eq)


-- | Group the branches into plain, at-start,
--   and not-start.
groupBranches :: TM.TMap PhonePatternAlt (S.Set PhoneResult) -> GroupedTrie
groupBranches tm = GroupedTrie tm' atStart notStart
  where 
    (_, atStart) = TM.match [PhoneAtStartZ ] tm 
    (_,notStart) = TM.match [PhoneNotStartZ] tm 
    tm' = deleteBranches [PhoneAtStartZ, PhoneNotStartZ] tm

incState :: (Monad m) => State.StateT Int m Int
incState = do
  z <- State.get
  State.put (z+1)
  return z

doTheThing :: S.Set PhoneResult -> S.Set (Arg [PhoneResultActionX] PhoneResult)
doTheThing = S.map (\x -> Arg (prPhoneConditions x) x)

undoTheThing :: S.Set (Arg [PhoneResultActionX] PhoneResult) -> S.Set PhoneResult
undoTheThing = S.map f
  where f (Arg _ y) = y

-- | Try merging two `TM.TMap`s of `Set`s. Difficult,
--   since the `Set`s might not quite work as expected.
mergeTrieSets :: TM.TMap PhonePatternAlt (S.Set PhoneResult) -> TM.TMap PhonePatternAlt (S.Set PhoneResult) -> TM.TMap PhonePatternAlt (S.Set PhoneResult)
mergeTrieSets tm1 tm2
  = undoTheThing <$> TM.unionWith S.union tm1' tm2'
  where
    tm1' = fmap doTheThing tm1
    tm2' = fmap doTheThing tm2

generateBranches3 :: (Quasi q, Quote q) => OutputNameDatabase -> TM.TMap PhonePatternAlt (S.Set PhoneResult) -> q GroupedTrieDecs
generateBranches3 ond tmap = do
  let grpd = groupBranches tmap
  -- (nom, _, decs) <- State.evalStateT (generateBranches' ond tmap) 0
  evalStateT' 0 $ do
    let tm1  = plainTrie grpd
        tm2  = atStartTrie grpd
        tm3  = notStartTrie grpd
        tm2' = mergeTrieSets tm2 tm1
        tm3' = mergeTrieSets tm3 tm1
    (nom1, _, decs1) <- generateBranches' ond tm1
    (nom2, _, decs2) <- generateBranches' ond tm2'
    (nom3, _, decs3) <- generateBranches' ond tm3'
    return (GroupedTrieDecs (nom1, decs1) (nom2, decs2) (nom3, decs3))
  where
    evalStateT' = flip State.evalStateT

generateBranches2 :: (Quasi q, Quote q) => OutputNameDatabase -> TM.TMap PhonePatternAlt (S.Set PhoneResult) -> q ((Name, [Dec]), (Name, [Dec]))
generateBranches2 ond tmap = do
  let grpd = groupBranches tmap
  -- (nom, _, decs) <- State.evalStateT (generateBranches' ond tmap) 0
  evalStateT' 0 $ do
    let tm1  = plainTrie grpd
        tm2  = atStartTrie grpd
        tm3  = notStartTrie grpd
        tm2' = mergeTrieSets tm2 tm1
        tm3' = mergeTrieSets tm3 tm1
    -- (nom1, _, decs1) <- generateBranches' ond tm1
    (nom2, _, decs2) <- generateBranches' ond tm2'
    (nom3, _, decs3) <- generateBranches' ond tm3'
    return ( (nom2, decs2), (nom3, decs3))
  where
    evalStateT' = flip State.evalStateT



generateBranches :: (Quasi q, Quote q) => OutputNameDatabase -> TM.TMap PhonePatternAlt (S.Set PhoneResult) -> q (Name, [Dec])
generateBranches ond tmap = do
  (nom, _, decs) <- State.evalStateT (generateBranches' ond tmap) 0
  return (nom,decs)

-- | Create the branches at each step of the way...
generateBranches' :: (Quasi q, Quote q) => OutputNameDatabase -> TM.TMap PhonePatternAlt (S.Set PhoneResult) -> State.StateT Int q (Name, Name, [Dec])
generateBranches' ond tmp = do
  let (mRslt,_) = TM.match [] tmp
      phoneMap  = ondPhonemeNames  ond
      phoneCns  = ondPhonemeConsts ond
  
  theMainVar <- lift $ newName "curPhone"
  n <- incState
  let funcString  = printf "outputBranch_%04d" n
      funcString2 = printf "outputCrunch_%04d" n
      funcString3 = printf "outputReturn_%04d" n
  funcName  <- lift $ newName funcString
  funcName2 <- lift $ newName funcString2
  -- lift $ qDebugNoticeUnflushed $ "got " ++ show mRslt ++ "."
  -- lift $ qDebugNotice $ "Working on \"" ++ funcString ++ "\" now."

  -- Match over the sub-results... I guess...
  rslts <- forMaybeM (getSubTries tmp) $ \(c, (_elem, subTrie)) -> do
    -- (subNom, subDecs) <- generateBranches ond subTrie
    -- lift $ qDebugNotice $ "In \"" ++ show c ++ "\" now."
    case c of
      PhoneAtStartZ -> do
        lift $ qReportError "Can't have an `AtStart` in the middle of a pattern."
        return Nothing
      PhoneNotStartZ -> do
        lift $ qReportError "Can't have a `NotStart` in the middle of a pattern."
        return Nothing
      (PhonemeNameZ pn) -> do
        let ePat = makePhoneConstructorPat phoneMap phoneCns pn
        -- lift $ qDebugNotice "Help..."
        case ePat of
          (Left errs) -> do
            lift $ mapM_ qReportError errs
            return Nothing
          (Right pat)  -> do
            -- myExp <- [|  |]
            -- lift $ qDebugNotice "About to recurse..."
            (subNom, subNom2, subDecs) <- generateBranches' ond subTrie
            let myMatch = Match pat (NormalB $ VarE subNom2) []
            return $ Just (subDecs, myMatch)
  
  -- lift $ qDebugNotice $ "Wait a minute..."

  failExp <- lift [| MatchFail $ \x -> "Error encountered at phoneme " ++ (show x) |]
  let failPat = Match WildP (NormalB failExp) []
      phoneType = ConT $ ondPhoneType ond
      stateType = ConT $ ondStateType ond

  funcSign <- lift [t| forall m s. (MonadFail m, IsString s) => $(pure phoneType) -> MatchResult m $(pure phoneType) [CharCase] $(pure stateType) s |]
  othrSign <- lift [t| forall m s. (MonadFail m, IsString s) => MatchResult m $(pure phoneType) [CharCase] $(pure stateType) s |]

  (appliedExpr, extraDecs) <- case mRslt of
    Nothing  -> return (ConE 'MatchContinue, [])
    (Just x) -> do
      -- hmm...
      (retExpr, retDecs) <- lift $ makeReturnFunctionAlt ond funcString3 x
      return (AppE (ConE 'MatchOptions) retExpr, retDecs)

  let (subDecs', mats) = unzip rslts
      myCase = CaseE (VarE theMainVar) (mats ++ [failPat])
      
      sign1 = SigD funcName funcSign
      defn1 = FunD funcName [Clause [VarP theMainVar] (NormalB myCase) []]

      sign2 = SigD funcName2 othrSign
      defn2 = ValD (VarP funcName2) (NormalB $ AppE appliedExpr (VarE funcName)) []

  return (funcName, funcName2, [sign1, defn1, sign2, defn2] ++ extraDecs ++ (concat subDecs'))

-- makeReturnFunction :: (Quasi q, Quote q) => OutputNameDatabase -> String -> S.Set PhoneResult -> q (Name, [Dec])

-- MatchContinue (i -> MatchResult m i v s r)

-- data MatchResult m i v s r
-- MatcherT i v s m a
-- i == (ConT $ ondPhoneType ond)
-- v == ([CharCase])
-- s == (ConT $ ondStateType ond)
-- m == 


-- CaseE Exp [Match]
-- Match Pat Body [Dec]

-- makePhoneConstructorPat :: M.Map String Name -> M.Map String [M.Map String Name] -> PhoneName -> Either [String] Pat


