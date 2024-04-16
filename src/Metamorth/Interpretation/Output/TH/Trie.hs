{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Metamorth.Interpretation.Output.TH.Trie
  ( makeReturnFunction
  , makeReturnFunctionAlt
  ) where

import Control.Arrow ((&&&))
import Control.Monad

import Data.Functor.Identity

import Data.String (IsString(..))

import Data.Char

import Data.List (partition)

import Data.Text qualified as T

import Data.Maybe

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

import Metamorth.Interpretation.Output.Types
import Metamorth.Interpretation.Output.Types.Alt
import Metamorth.Interpretation.Output.Parsing.Types

import Metamorth.Interpretation.Output.TH.Types

import Data.Trie.Map qualified as TM

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

import Metamorth.ForOutput.Monad.EitherFail

import Metamorth.ForOutput.Monad.Matcher.Stateful
import Metamorth.ForOutput.Monad.Matcher.Stateful.Result

import Metamorth.Helpers.Q
import Metamorth.Helpers.TH (strE, intersperseInfixEDef, andE, boolE)
import Metamorth.Helpers.Monad

import Metamorth.ForOutput.Char

import THLego.Helpers

-- | Since each condition requires a different
--   number of arguments, we need to keep the
--   different kinds of arguments separated.
data ReturnClauses = ReturnClauses
  { plainRet :: [Name -> (Guard, Exp)]
  , stateRet :: [Name -> Name -> (Guard, Exp)]
  , condPlainRet :: [Name -> Name -> (Guard, Exp)]
  , condStateRet :: [Name -> Name -> Name -> (Guard, Exp)]
  } -- deriving (Show ,Eq)

pattern EmptyRCs :: ReturnClauses
pattern EmptyRCs = ReturnClauses [] [] [] []

-- addPhoneActionClause :: (Quasi q, Quote q) => OutputNameDatabase -> OutputCase -> RetType -> [CharPatternItem] -> PhoneActionData -> ReturnClauses -> q ReturnClauses

makeReturnFunctionAlt :: (Quasi q, Quote q) => OutputNameDatabase -> String -> S.Set PhoneResult -> q (Exp, [Dec])
makeReturnFunctionAlt ond str phoneSet = do
  -- collectActionData :: OutputNameDatabase -> [PhoneResultActionX] -> Either String PhoneActionData
  
  retStuff <- forMaybeM (S.toAscList phoneSet) $ \(PhoneResult conds cpi oc) -> do
    let eActData = collectActionData ond conds
    case eActData of
      (Left err)  -> do 
        qReportError $ "Error making return function \"" ++ str ++  "\": " ++ err
        return Nothing
      (Right pad) -> return $ Just (pad, cpi, oc)

  let retType = getTopRetType $ map (\(x,_,_) -> x) retStuff

  -- Fold over the result clauses, since you can't
  -- just do it with a map.
  retClauses <- forFoldM EmptyRCs retStuff $ \rcs (pad, cpi, oc) -> do
    addPhoneActionClause ond oc retType cpi pad rcs

  -- Run the main function.
  (nom, decs) <- makeReturnFunction' ond str retClauses

  case retType of
    PlainRet -> return (AppE (ConE 'PlainReturn) (VarE nom), decs)
    StateRet -> return (AppE (ConE 'StateReturn) (VarE nom), decs)
    CondPlainRet -> return (AppE (ConE 'ConditionalReturn) (VarE nom), decs)
    CondStateRet -> return (AppE (ConE 'ConditionalStateReturn) (VarE nom), decs)

{-
  = PlainReturn (v -> m r)
  | StateReturn (v -> s -> m (r,s))
  -- | Returning the results, but using
  --   the next value in the stream to
  --   determine what exactly should be
  --   returned. 
  | ConditionalReturn (v -> Maybe i -> m r)
  | ConditionalStateReturn (v -> Maybe i -> s -> m (r,s))

data RetType
  = PlainRet
  | StateRet
  | CondPlainRet
  | CondStateRet
  deriving (Show, Eq, Ord)
-}


makeReturnFunction :: (Quasi q, Quote q) => OutputNameDatabase -> String -> S.Set PhoneResult -> q (Name, [Dec])
makeReturnFunction ond str phoneSet = do
  -- collectActionData :: OutputNameDatabase -> [PhoneResultActionX] -> Either String PhoneActionData
  
  retStuff <- forMaybeM (S.toAscList phoneSet) $ \(PhoneResult conds cpi oc) -> do
    let eActData = collectActionData ond conds
    case eActData of
      (Left err)  -> do 
        qReportError $ "Error making return function \"" ++ str ++  "\": " ++ err
        return Nothing
      (Right pad) -> return $ Just (pad, cpi, oc)

  let retType = getTopRetType $ map (\(x,_,_) -> x) retStuff

  -- Fold over the result clauses, since you can't
  -- just do it with a map.
  retClauses <- forFoldM EmptyRCs retStuff $ \rcs (pad, cpi, oc) -> do
    addPhoneActionClause ond oc retType cpi pad rcs

  -- Run the main function.
  makeReturnFunction' ond str retClauses

{-
collectActionData :: OutputNameDatabase -> [PhoneResultActionX] -> Either String PhoneActionData
type PhoneResult = PhoneResultX [PhoneResultActionX]

data PhoneResultX a = PhoneResult
  { prPhoneConditions :: a
  , prPhoneOutput :: [CharPatternItem]
  , prOutputCase  :: OutputCase
  } deriving (Show, Eq)
-}


makeReturnFunction' :: (Quasi q, Quote q) => OutputNameDatabase -> String -> ReturnClauses -> q (Name, [Dec])
makeReturnFunction' ond strName (ReturnClauses prs [] [] []) = do
  funcName <- newName strName
  varName1 <- newName "v"
  typVar1  <- newName "str"
  monVar   <- newName "mon"
  let rets = map ($ varName1) prs
  sign <- [t| (IsString $(pure $ VarT typVar1), Monad $(pure $ VarT monVar)) => [CharCase] -> $(pure $ VarT monVar) $(pure $ VarT typVar1) |]
  let signDec = SigD funcName sign
      bodyDec = FunD funcName [Clause [VarP varName1] (GuardedB rets) []]
  return (funcName, [signDec, bodyDec])
makeReturnFunction' ond strName (ReturnClauses [] srs [] []) = do
  funcName <- newName strName
  varName1 <- newName "v"
  varName2 <- newName "s"
  typVar1  <- newName "str"
  let rets = map (($ varName2) . ($ varName1)) srs
      stateType = pure $ ConT (ondStateType ond)
  sign <- [t| (IsString $(pure $ VarT typVar1)) => [CharCase] -> $stateType -> ($(pure $ VarT typVar1) , $stateType) |]
  let signDec = SigD funcName sign
      bodyDec = FunD funcName [Clause [VarP varName1, VarP varName2] (GuardedB rets) []]
  return (funcName, [signDec, bodyDec])
makeReturnFunction' ond strName (ReturnClauses [] [] nrs []) = do
  funcName <- newName strName
  varName1 <- newName "v"
  varName2 <- newName "n"
  typVar1  <- newName "str"
  let rets = map (($ varName2) . ($ varName1)) nrs
      nextType = pure $ ConT (ondPhoneType ond)
  sign <- [t| (IsString $(pure $ VarT typVar1)) => [CharCase] -> Maybe $nextType -> ($(pure $ VarT typVar1)) |]
  let signDec = SigD funcName sign
      bodyDec = FunD funcName [Clause [VarP varName1, VarP varName2] (GuardedB rets) []]
  return (funcName, [signDec, bodyDec])
makeReturnFunction' ond strName (ReturnClauses [] [] [] xrs) = do
  funcName <- newName strName
  varName1 <- newName "v"
  varName2 <- newName "n"
  varName3 <- newName "s"
  typVar1  <- newName "str"
  let rets = map (($ varName3) . ($ varName2) . ($ varName1)) xrs
      stateType = pure $ ConT (ondStateType ond)
      nextType  = pure $ ConT (ondPhoneType ond)
  sign <- [t| (IsString $(pure $ VarT typVar1)) => [CharCase] -> Maybe $nextType -> $stateType -> ($(pure $ VarT typVar1) , $stateType) |]
  let signDec = SigD funcName sign
      bodyDec = FunD funcName [Clause [VarP varName1, VarP varName2, VarP varName3] (GuardedB rets) []]
  return (funcName, [signDec, bodyDec])
makeReturnFunction' _ond strName (ReturnClauses {}) = do
  funcName <- newName strName
  qReportError $ "Couldn't create declarations for function \"" ++ strName ++ "\".\nIt had Clauses of different numbers of arguments."
  return (funcName, [])


data RetType
  = PlainRet
  | StateRet
  | CondPlainRet
  | CondStateRet
  deriving (Show, Eq, Ord)

-- Lattice-like operation.
(/\) :: RetType -> RetType -> RetType
PlainRet /\ x = x
x /\ PlainRet = x
StateRet /\ StateRet     = StateRet
StateRet /\ CondPlainRet = CondStateRet
CondPlainRet /\ StateRet = CondStateRet
CondPlainRet /\ CondPlainRet = CondPlainRet
CondStateRet /\ _ = CondStateRet
_ /\ CondStateRet = CondStateRet

top :: Foldable f => f RetType -> RetType
top = foldl (/\) PlainRet 

getTopRetType :: (Foldable f, Functor f) => f PhoneActionData -> RetType
getTopRetType = top . fmap necessaryPhoneRet

-- Note that we are appending the options. This
-- is to ensure that they have the same order
-- as the input.
addPlainRet :: (Name -> (Guard, Exp)) -> ReturnClauses -> ReturnClauses
addPlainRet c rcs = rcs { plainRet = plainRet rcs ++ [c]}

addStateRet :: (Name -> Name -> (Guard, Exp)) -> ReturnClauses -> ReturnClauses
addStateRet c rcs = rcs { stateRet = stateRet rcs ++ [c]}

addCondPlainRet :: (Name -> Name -> (Guard, Exp)) -> ReturnClauses -> ReturnClauses
addCondPlainRet c rcs = rcs { condPlainRet = condPlainRet rcs ++ [c]}

addCondStateRet :: (Name -> Name -> Name -> (Guard, Exp)) -> ReturnClauses -> ReturnClauses
addCondStateRet c rcs = rcs { condStateRet = condStateRet rcs ++ [c]}

data PhoneActionData = PhoneActionData
  { padModify :: [ModifyStateX]
  , padChecks :: [CheckStateX]
  , padNexts  :: [Name -> Exp]
  } -- deriving (Show, Eq)

necessaryPhoneRet :: PhoneActionData -> RetType
necessaryPhoneRet (PhoneActionData [] [] []) = PlainRet
necessaryPhoneRet (PhoneActionData xs _  []) = StateRet
necessaryPhoneRet (PhoneActionData _  ys []) = StateRet
necessaryPhoneRet (PhoneActionData [] [] zs) = CondPlainRet
necessaryPhoneRet (PhoneActionData _  _  _ ) = CondStateRet

addPhoneActionClause :: (Quasi q, Quote q) => OutputNameDatabase -> OutputCase -> RetType -> [CharPatternItem] -> PhoneActionData -> ReturnClauses -> q ReturnClauses
addPhoneActionClause ond oc PlainRet cpi pad rcs = case pad of
  (PhoneActionData [] [] []) -> do
    expr <- createCaseExp oc cpi
    othw <- [| otherwise |]
    let rslt = \v -> (NormalG othw, AppE expr (VarE v)) 
    return $ addPlainRet rslt rcs
  _ -> do 
    qReportError $ "Couldn't create guard/clause (1) for output \"" ++ show cpi ++ "\"." 
    return rcs -- error
addPhoneActionClause ond oc StateRet cpi pad rcs = case pad of
  (PhoneActionData [] [] []) -> do
    expr <- createCaseExp oc cpi
    othw <- [| otherwise |]
    let rslt = \v s -> (NormalG othw, TupE [Just (AppE expr (VarE v)), Just $ VarE s]  ) 
    return $ addStateRet rslt rcs
  (PhoneActionData md [] []) -> do
    expr <- createCaseExp oc cpi
    othw <- [| otherwise |]
    func <- makeModify ond md 
    let rslt = \v s -> (NormalG othw, TupE [Just (AppE expr (VarE v)), Just $ func s])
    return $ addStateRet rslt rcs
  (PhoneActionData md cs []) -> do
    expr <- createCaseExp oc cpi
    func <- makeModify  ond md
    pred <- makeConfirm ond cs
    let rslt = \v s -> (NormalG $ pred s, TupE [Just (AppE expr (VarE v)), Just $ func s])
    return $ addStateRet rslt rcs
  _ -> do 
    qReportError $ "Couldn't create guard/clause (2) for output \"" ++ show cpi ++ "\"." 
    return rcs -- error
addPhoneActionClause ond oc CondPlainRet cpi pad rcs = case pad of
  (PhoneActionData [] [] []) -> do
    expr <- createCaseExp oc cpi
    othw <- [| otherwise |]
    let rslt = \v _ -> (NormalG $ othw, (AppE expr (VarE v))  ) 
    return $ addCondPlainRet rslt rcs
  (PhoneActionData [] [] ns) -> do
    expr <- createCaseExp oc cpi
    let pred x   = intersperseInfixEDef (VarE 'otherwise) (VarE '(&&)) (map ($ x) ns)
        rslt v n = (NormalG $ pred n, AppE expr (VarE v))
    return $ addCondPlainRet rslt rcs
  _ -> do 
    qReportError $ "Couldn't create guard/clause (3) for output \"" ++ show cpi ++ "\"." 
    return rcs -- error
addPhoneActionClause ond oc CondStateRet cpi pad rcs = case pad of
  (PhoneActionData [] [] []) -> do
    expr <- createCaseExp oc cpi
    othw <- [| otherwise |]
    let rslt = \v _ s -> (NormalG othw, TupE [Just (AppE expr (VarE v)), Just $ VarE s]  ) 
    return $ addCondStateRet rslt rcs
  (PhoneActionData md cs []) -> do
    expr <- createCaseExp oc cpi
    func <- makeModify  ond md
    prdc <- makeConfirm ond cs
    let rslt = \v _ s -> (NormalG $ prdc s, TupE [Just (AppE expr (VarE v)), Just $ func s])
    return $ addCondStateRet rslt rcs
  (PhoneActionData md cs ns) -> do
    expr <- createCaseExp oc cpi
    func  <- makeModify  ond md
    pred1 <- makeConfirm ond cs
    let pred2 x     = intersperseInfixEDef (VarE 'otherwise) (VarE '(&&)) (map ($ x) ns)
        pred3 n s   = andE (pred1 s) (pred2 n)
        rslt  v n s = (NormalG $ pred3 n s, TupE [Just (AppE expr (VarE v)), Just $ func s])
    return $ addCondStateRet rslt rcs


collectActionData :: OutputNameDatabase -> [PhoneResultActionX] -> Either String PhoneActionData
collectActionData ond [] = return $ PhoneActionData [] [] []
collectActionData ond ps = toEither $ do
  let (confirms', rst1) = partition isConfirmState ps
      (modifys' , rst2) = partition isModifyState  rst1
      (atEnds'  , rst3) = partition isAtEnd rst2
      (notEnds' , rst4) = partition isNotEnd rst3
      (checks'  , _rst) = partition isCheckNext rst4
      
      confirms = map (\(PRConfirmState zs) -> zs) confirms'
      modifys  = map (\(PRModifyState  zs) -> zs) modifys'
      atEnds   = map (\PRAtEnd  -> appliedTo 'isNothing) atEnds'
      notEnds  = map (\PRNotEnd -> appliedTo 'isJust) notEnds'
      checks   = map (\(PRCheckNext chk) -> chk) checks'
  
      checksX = mapMaybe (getLookups ond) checks

  return $ PhoneActionData modifys confirms (atEnds ++ notEnds ++ checksX)

appliedTo :: Name -> Name -> Exp
appliedTo func val = AppE (VarE func) (VarE val)

-- Again, uses `any` to lift boolean functions
-- over `Maybe`.
getLookups :: OutputNameDatabase -> PhoneFollow -> Maybe (Name -> Exp)
getLookups ond (PhoneFollowedByGroup grp) = do
  func <- M.lookup grp (ondGroups ond)
  return (\nom -> AppE (AppE (VarE 'any) (VarE func)) (VarE nom))
getLookups ond (PhoneFollowedByTrait trt) = do
  (func, mDict) <- M.lookup trt (ondTraits ond)
  case mDict of
    Nothing  -> return (\nom -> AppE (AppE (VarE 'any) (VarE func)) (VarE nom))
    (Just _) -> return (\nom -> AppE (VarE 'isJust) (InfixE (Just (VarE nom)) (VarE '(>>=)) (Just (VarE func))))  
getLookups ond (PhoneFollowedByTraitAt trt trtVal) = do
  (func, dict) <- M.lookup trt (ondValTraits ond)
  trtVal' <- M.lookup trtVal dict
  return (\nom -> InfixE (Just (VarE nom)) (VarE '(==)) (Just (AppE (ConE 'Just) (ConE trtVal'))))
getLookups ond (PhoneFollowedByAspect asp) = do
  func <- M.lookup asp (ondAspectChecks ond)
  return (\nom -> AppE (VarE 'isJust) (InfixE (Just (VarE nom)) (VarE '(>>=)) (Just (VarE func))))
getLookups ond (PhoneFollowedByAspectAt asp aspVal) = do
  (func, (_,dict)) <- M.lookup asp (ondAspects ond)
  aspVal' <- M.lookup aspVal dict
  return (\nom -> InfixE (Just (VarE nom)) (VarE '(==)) (Just (AppE (ConE 'Just) (ConE aspVal'))))
getLookups ond (PhoneFollowedByPhone str) = do
  Nothing -- don't know how to do these.



{-
data PhoneFollow
  = PhoneFollowedByGroup String
  | PhoneFollowedByTrait String
  | PhoneFollowedByTraitAt String String
  | PhoneFollowedByAspect String
  | PhoneFollowedByAspectAt String String
  | PhoneFollowedByPhone String
  deriving (Show, Eq, Ord)

-}

      
  -- return $ PhoneActionData [] [] []

-- PhoneResultActionX


-- makeReturn :: (QL q) => OutputNameDatabase -> S.Set PhoneResult -> q _
-- makeReturn

{-
makeReturnItem :: (Quote q, Quasi q) => OutputNameDatabase -> PhoneResult -> ReturnClauses -> q ReturnClauses
makeReturnItem ond (PhoneResult [] cPats ocs) rcs = do
  -- (v -> m r)
  thisExp <- createCaseExp ocs cPats
  othw <- [| otherwise |]
  let grd = (\v -> (NormalG othw, AppE thisExp (VarE v)))
  return $ addPlainRet grd rcs
makeReturnItem ond (PhoneResult )
-}

-- | Create an expression that can be applied to 
--   the list of cases to get the desired output.
createCaseExp :: (Quote q, Quasi q) => OutputCase -> [CharPatternItem] -> q Exp
createCaseExp ocs cpats = do 
  case ocs of
    OCMaj    -> [| \x -> if (getMaxCase x) == UpperCase then (return $(strQ $ plainCharPat cpats)) else (fail "Not (an) upper-case phoneme(s)") |]
    OCMin    -> [| \x -> if (getMaxCase x) /= UpperCase then (return $(strQ $ plainCharPat cpats)) else (fail  "Not (a) lower-case phoneme(s)") |]
    OCNull   -> [| \_ -> return $(strQ $ plainCharPat cpats) |]
    (OCDetect src outp) -> do
      -- okay...
      func1 <- case src of
        CSFirst -> pure 'getFirstCase
        CSLast  -> pure 'getLastCase
        CSHigh  -> pure 'getMaxCase
        CSLow   -> pure 'getMinCase
      (expr1, expr2) <- case outp of
        CATitle -> do
            e1 <- [| return $(strQ (firstUpperPat cpats)) |]
            e2 <- [| return $(strQ (allLowerPat   cpats)) |]
            return (e1,e2)
        CAAll -> do
            e1 <- [| return $(strQ (allUpperPat cpats)) |]
            e2 <- [| return $(strQ (allLowerPat cpats)) |]
            return (e1,e2)
        CAExactUpper -> do
            e1 <- [| return $(strQ (plainCharPat cpats)) |]
            e2 <- [| fail "Should be upper-case." |]
            return (e1,e2)
        CAExactLower -> do
            e1 <- [| fail "Should be lower-case." |]
            e2 <- [| return $(strQ (plainCharPat cpats)) |]
            return (e1,e2)
      [| \x -> if (( $(pure $ VarE func1) x) == UpperCase ) then $(pure expr1) else $(pure expr2) |]

strQ :: (Quote q) => String -> q Exp
strQ str = return (strE str)


plainCharPat :: [CharPatternItem] -> String
plainCharPat [] = ""
plainCharPat ((CasableChar   c):rst) = c : plainCharPat rst
plainCharPat ((UncasableChar c):rst) = c : plainCharPat rst

firstUpperPat :: [CharPatternItem] -> String
firstUpperPat [] = ""
firstUpperPat ((CasableChar   c):rst) = (toUpper c) : (T.unpack $ T.toLower $ T.pack $ plainCharPat rst)
firstUpperPat ((UncasableChar c):rst) = c : firstUpperPat rst

allLowerPat :: [CharPatternItem] -> String
allLowerPat = T.unpack . T.toLower . T.pack . plainCharPat

allUpperPat :: [CharPatternItem] -> String
allUpperPat = T.unpack . T.toUpper . T.pack . plainCharPat

-- | Convert a list of `ModifyStateX` into a
--   single record update expression.
makeModify :: (Quasi q, Quote q) => OutputNameDatabase -> [ModifyStateX] -> q (Name -> Exp)
makeModify _ [] = return $ \x -> VarE x
makeModify ond mss = do
  let mss' = map (makeModify1 ond &&& getModName) mss
  mss2 <- mapM checkNames mss'
  let mss3 = catMaybes mss2
  return $ \nom -> RecUpdE (VarE nom) mss3

makeConfirm :: (Quasi q, Quote q) => OutputNameDatabase -> [CheckStateX] -> q (Name -> Exp)
makeConfirm _ [] = return $ \_ -> VarE 'otherwise
makeConfirm ond css = do
  let css' = map (makeCheck1 ond &&& getChkName) css
  css2 <- mapM checkNames css'
  let css3 = catMaybes css2
  return $ \x -> intersperseInfixEDef (VarE 'otherwise) (VarE '(&&)) (map ($ x) css3)

checkNames :: (Quasi q, Quote q) => (Maybe a, String) -> q (Maybe a)
checkNames (Just fx, _) = return (Just fx)
checkNames (Nothing, v) = do
  qReportError $ "Encountered error with state \"" ++ v ++ "\"."
  return Nothing

getModName :: ModifyStateX -> String
getModName (ModifyStateBB str _) = str
getModName (ModifyStateVV str x) = str ++ ":" ++ x
getModName (ModifyStateVX str  ) = str

getChkName :: CheckStateX -> String
getChkName (CheckStateBB str _) = str
getChkName (CheckStateVB str _) = str
getChkName (CheckStateVV str x) = str ++ ":" ++ x


-- FieldExp = (Name, Exp)
makeModify1 :: OutputNameDatabase -> ModifyStateX -> Maybe FieldExp
makeModify1 ond (ModifyStateBB str bl) = do
  (recNom, _mpr) <- M.lookup str $ ondStates ond
  return (recNom, boolE bl)
makeModify1 ond (ModifyStateVX str) = do
  (recNom, _mpr) <- M.lookup str $ ondStates ond
  return (recNom, ConE 'Nothing)
makeModify1 ond (ModifyStateVV str val) = do
  (recNom, mpr) <- M.lookup str $ ondStates ond
  (_n, valDict) <- mpr
  conNom <- M.lookup val valDict
  return (recNom, AppE (ConE 'Just) (ConE conNom))


makeCheck1 :: OutputNameDatabase -> CheckStateX -> Maybe (Name -> Exp)
makeCheck1 ond (CheckStateBB str bl) = do
  (recNom, _mpr) <- M.lookup str $ ondStates ond
  let expr1 = \x -> AppE (VarE recNom) (VarE x)
  return $ if bl
    then expr1
    else (\x -> AppE (VarE 'not) (expr1 x))
makeCheck1 ond (CheckStateVB str bl) = do
  (recNom, _mpr) <- M.lookup str $ ondStates ond
  let expr1 = \x -> AppE (VarE recNom) (VarE x)
  return $ if bl
    then (\x -> AppE (VarE 'isJust)    (expr1 x))
    else (\x -> AppE (VarE 'isNothing) (expr1 x))
makeCheck1 ond (CheckStateVV str val) = do
  (recNom, mpr) <- M.lookup str $ ondStates ond
  (_n, valDict) <- mpr
  conNom <- M.lookup val valDict
  return $ \x -> InfixE (Just (AppE (VarE recNom) (VarE x))) (VarE '(==)) (Just (AppE (ConE 'Just) (ConE conNom)))
  


-- ondStates  :: M.Map String (Name, Maybe (Name, M.Map String Name))
{-
-- | The validated "Check-State" type.
data CheckStateX
  -- | Boolean check on a bool-state.
  = CheckStateBB String Bool
  -- | Value check on a value-state.
  | CheckStateVV String String
  -- | Boolean check on a value-state.
  | CheckStateVB String Bool
  deriving (Show, Eq, Ord)

data ModifyStateX
  -- | Change the value of a boolean state.
  = ModifyStateBB String Bool
  -- | Change the value of a value-state.
  | ModifyStateVV String String
  -- | Set a value-state to `Nothing`.
  | ModifyStateVX String
  deriving (Show, Eq, Ord)
-}

-- RecUpdE (UnboundVarE x) [(Ghci7.example1,LitE (IntegerL 12))]


{-

data CharPatternItem
  = CasableChar   Char -- ^ A single `Char`.
  | UncasableChar Char -- ^ A single uncasable `Char`.
  deriving (Show, Eq, Ord)

  ( CharCase(..)
  , getCase
  , getMaxCase
  , getMinCase
  , getFirstCase
  , getLastCase
  ) where

data OutputCase
  -- | Upper-Case Pattern only.
  = OCMaj
  -- | Lower-Case Pattern only.
  | OCMin
  -- | Uncasable Pattern. In this case, both cases
  --   will use the same value.
  | OCNull
  -- | Automatically create cases for this pattern.
  --   The parameters tell you how to transform
  --   the input case to the output case. 
  | OCDetect CaseSource CaseApply
  deriving (Show, Eq, Ord)

-- | Which phoneme of the input list to
--   use to determine the case of the
--   output.
data CaseSource
   -- | Use the case of the first casable character.
   = CSFirst
   -- | Use the case of the last casable character
   | CSLast
   -- | Use lower-case, unless all characters are
   --   upper-case
   | CSLow
   -- | Use upper-case, unless all characters are
   --   lower-case.
   | CSHigh
   deriving (Show, Eq, Ord)

-- | Which output character(s) to apply the
--   case to.
data CaseApply
   -- | Use Title case if the input is capitalised.
   = CATitle
   -- | Apply case to all characters.
   | CAAll
   -- | Use this pattern if you want this to
   --   match the upper-case and lower-case
   --   separately. Use this one for upper-case.
   | CAExactUpper
   -- | Use this pattern if you want this to
   --   match the upper-case and lower-case
   --   separately. Use this one for lower-case.
   | CAExactLower
   deriving (Show, Eq, Ord)

-}


{-

data PhoneResultActionX
  = PRConfirmState CheckStateX
  | PRModifyState  ModifyStateX
  | PRAtEnd
  | PRNotEnd
  | PRCheckNext PhoneFollow
  deriving (Show, Eq, Ord)

type PhoneResult = PhoneResultX [PhoneResultActionX]
-- type PhoneResult str = PhoneResultX str    [PhoneResultActionX]
-- type PhoneResultText = PhoneResultX T.Text [PhoneResultActionX]

data PhoneResultX a = PhoneResult
  { prPhoneConditions :: a
  , prPhoneOutput :: [CharPatternItem]
  , prOutputCase  :: OutputCase
  } deriving (Show, Eq)

-}

{-
data MatchResult m i v s r
  -- | There is only one option, which
  --   is to consume nothing and produce
  --   a value.
  = MatchReturn (MatchReturn m i v s r)
  -- | Continue matching the input using this
  --   function next. Typically run in the same
  --   way as the initial step (see above).
  | MatchContinue (i -> MatchResult m i v s r)
  -- | The `MatcherT` can either return here,
  --   or continue matching data from the input
  --   buffer. i.e. if the "Continue path" leads
  --   to a `MatchFail` result, then the input
  --   will be rewound, and the return value
  --   here will be run instead.
  | MatchOptions (MatchReturn m i v s r) (i -> MatchResult m i v s r)
  -- | Used for paths that aren't a valid match. 
  --   When writing a `MatchContinue` function, 
  --   you should use this together with a wildcard,
  --   e.g. 
  --
  --   @
  --   ... MatchContinue $ \case
  --     A -> ...
  --     ...
  --     _ -> MatchFail "Bad Path"
  --   @
  --
  --   So that the function isn't partial.
  | MatchFail String
-}

-- | Matching on return values. This is to
--   provide more functionaliy without having
--   to create multiple Constructors in `MatchResult`
--   for each kind of Return.
{-
data MatchReturn m i v s r
  -- | The usual return. The @v@ in the
  --   input a `Monoid` collected from 
  --   the input of the items parsed
  --   so far. If you want to ignore it,
  --   just use @PlainReturn (\_ -> pure x)@.
  = PlainReturn (v -> m r)
  | StateReturn (v -> s -> m (r,s))
  -- | Returning the results, but using
  --   the next value in the stream to
  --   determine what exactly should be
  --   returned. 
  | ConditionalReturn (v -> Maybe i -> m r)
  | ConditionalStateReturn (v -> Maybe i -> s -> m (r,s))
-}





