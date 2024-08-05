{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module Metamorth.Interpretation.Phonemes.TH
  -- * Main Types and Functions
  ( PhonemeDatabase(..)
  , PhonemeInformation(..)
  , producePhonemeDatabase
  -- * Other Functions/Types
  , producePropertyData
  , producePhonemeInventory
  , produceVariousData
  , produceVariousDecs
  , PropertyData(..)
  , PhonemeHierarchy(..)
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

-- import Data.Functor.Identity (runIdentity)

import Data.List.NonEmpty qualified as F (unzip)

import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.Tuple

import Data.Traversable (for)

import Data.Text qualified as T

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Haskell.TH.Ppr qualified as PP

import THLego.Helpers qualified as THL

import Metamorth.Interpretation.Phonemes.Types
  ( PhonemeParsingStructure(..)
  , PhonemeInventory(..)
  , PhonemeProperties(..)
  , lookupPhone
  )

import Data.Map.Strict       qualified as M
import Data.Map.Merge.Strict qualified as MM

import Metamorth.Helpers.Applicative
import Metamorth.Helpers.Monad
import Metamorth.Helpers.TH
import Metamorth.Helpers.Map
import Metamorth.Helpers.Q

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

-- | Since traits aren't embedded in the types themselves,
--   information about traits is limited to a function of
--   type @Phoneme -> TraitInfo@, or something like that.
data TraitData = TraitData
  { traitInfoName  :: Name
  , traitTypeTable :: M.Map String (Name, Type)
  , traitDefName   :: Name
  } deriving (Show, Eq)

produceVariousDecs :: PhonemeParsingStructure -> Q [Dec]
produceVariousDecs pps = do
  (_,decs) <- produceVariousData pps
  return decs

-- fmap (\((_,_,_,fncs),_) -> fncs) $ join $ runQ <$> produceVariousData <$> fromRight defaultPhonemeStructure <$> execPhonemeParser parsePhonemeFile <$> TIO.readFile "local/example1.thy"

-- ((PropertyData, M.Map String PhonemeHierarchy,M.Map String Name, M.Map (String, String) Name), [Dec])

{-
  :: IO
       ((PropertyData, M.Map String PhonemeHierarchy, M.Map String Name,M.Map (String, String) Name),
[Dec])
-}

-- | A type representing much of the information about an
--   individual phoneme. This type is intended to be used
--   as the value of a `M.Map`, i.e. @M.Map String `PhonemeInformation`@.
--
--   Using `fmap` or `M.map`, you can easily convert a `M.Map`
--   of this type to a `M.Map` of an individual field. e.g.
--
--   @ fmap phiPatternName :: M.Map String PhonemeInformation -> M.Map String Name @
data PhonemeInformation = PhonemeInformation
  -- | The name of the top-level Pattern Synonym.
  { phiPatternName :: Name
  -- | A list of the aspect options of the Phoneme.
  --   This is necessary to be able to build a 
  --   constructor.
  --
  --   e.g. Consider the following specification:
  -- 
  --   @
  --   aspect length : short long
  --   aspect nasal  : plain nasalised
  --   ...
  --   ====
  --   ...
  --   * vowel
  --     a : length nasal
  --   ...
  --   @
  --   
  --   Then the argument list for phoneme 'a'
  --   would look something like...
  --
  --   @
  --   [ M.fromList 
  --      [ ("long" , mkName "Long" )
  --      , ("short", mkName "Short")
  --      ]
  --   , M.fromList
  --      [ ("nasalised", mkName "Nasalised")
  --      , ("plain"    , mkName "Plain"    )
  --      ]
  --   ]
  --   @
  --
  , phiArgumentOptions :: [M.Map String Name]
  } deriving (Show, Eq)

makePhonemeInformation :: M.Map String Name -> M.Map String [M.Map String Name] -> M.Map String PhonemeInformation
makePhonemeInformation 
  = MM.merge 
      (MM.mapMissing     (\_ k1    -> PhonemeInformation k1 []))
      (MM.dropMissing)
      (MM.zipWithMatched (\_ k1 k2 -> PhonemeInformation k1 k2))

-- | A type containing everything that you'd need
--   from this module (I hope).
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
  -- | Functions for checking whether a `String` is
  --   a trait, and whether that trait is a value trait
  --   (@Just ...@) or a boolean trait (@Nothing@).
  , pdbTraitInformation :: M.Map String (Name, Maybe (Name, M.Map String Name))
  -- | Make an uncased expression an upper-case expression.
  , pdbMkMaj :: Exp -> Exp
  -- | Make an uncased expression a  lower-case expression.
  , pdbMkMin :: Exp -> Exp
  }

instance Show PhonemeDatabase where
  show x = 
    "PhonemeDatabase {pdbPropertyData = " <> show (pdbPropertyData x)
      <> ", pdbPhonemeInfo = "      <> show (pdbPhonemeInfo x)
      <> ", pdbTopPhonemeType = "   <> show (pdbTopPhonemeType x)
      <> ", pdbTopPhonemeType = "   <> show (pdbTopPhonemeType x)
      <> ", pdbWordTypeNames = "    <> show (pdbWordTypeNames x)
      <> ", pdbGroupMemberFuncs = " <> show (pdbGroupMemberFuncs x)
      <> ", pdbTraitInformation = " <> show (pdbTraitInformation x)
      <> ", pdbMkMaj = "            <> show (PP.ppr mkMajRep)
      <> ", pdbMkMin = "            <> show (PP.ppr mkMinRep)
      <> "}"
    where
      exprNom  = mkName "expr"
      exprPat  = VarP exprNom
      exprVar  = VarE exprNom
      mkMajRep = LamE [exprPat] (pdbMkMaj x exprVar)
      mkMinRep = LamE [exprPat] (pdbMkMin x exprVar)


-- combinePhoneMaps :: M.Map String PhonemeHierarchy -> M.Map String Name -> M.Map String PhonemeInformation
-- combinePhoneMaps = MM.merge MM.dropMissing

producePhonemeDatabase :: PhonemeParsingStructure -> Q (PhonemeDatabase, [Dec])
producePhonemeDatabase pps = do
  ((propData, _phoneDats, patNoms, _isFuncNames, mainTypeName, phoneGroups, traitFuncTree), theDecs) <- produceVariousData pps
  
  -- propData  :: PropertyData
  -- phoneDats :: M.Map String PhonemeHierarchy
  -- patNoms   :: M.Map String Name
  
  -- aspectTable  :: M.Map String (Name, (Name, M.Map String Name))

  -- = PhonemeProperties 
  --     { phAspects :: [String]
  --     , phTraits  :: [(String, Maybe String)]
  --     } deriving (Show, Eq)


  let thisMkMaj = id -- Temporary
      thisMkMin = id -- Temporary

      -- we want `M.Map String [M.Map String Name]`.
      phoneInv   = ppsPhonemeInventory pps
      phoneProps :: M.Map String (Maybe PhonemeProperties)
      phoneProps = M.mapWithKey (\k _ -> lookupPhone phoneInv k) patNoms
      phoneAsps  :: M.Map String (Maybe [String])
      phoneAsps  = M.map (fmap phAspects) phoneProps
      
      aspData :: M.Map String (Name, (Name, M.Map String Name))
      aspData    = aspectTable propData

  aspectListMap <- forWithKey phoneAsps $ \phoneName aspList ->
    case aspList of
      Nothing   -> return []
      (Just []) -> return []
      (Just aspList') -> do
        -- hmm...
        let rslt = for aspList' $ \thisStr -> do
              (_nom1, (_nom2, theMap)) <- lookupE thisStr aspData
              return theMap
        case rslt of
          (Left str) -> do 
            qReport True $ "Couldn't find aspect \"" ++ str ++ "\" for phoneme \"" ++ phoneName ++ "\"."
            return []
          (Right mp) -> return mp
  
  -- Combining the two maps...
  let phoneInfoMap = makePhonemeInformation patNoms aspectListMap

  wordTypeName  <- newName "CasedWord"
  wordConsName1 <- newName "WordPh"
  wordConsName2 <- newName "WordPunct"
  
  wordConsType1 <- [t| [ $(pure $ ConT mainTypeName) ] |]
  wordConsType2 <- [t| T.Text |]

  -- sumAdtDecDeriv :: Name -> [(Name, [Type])] -> [Type] -> Dec
  -- lookupPhone :: PhonemeInventory -> String -> Maybe PhonemeProperties
  -- lookupPhone (PhonemeSet   mp) phone = M.lookup phone mp
  -- lookupPhone (PhonemeGroup ps) phone = asum $ M.map (`lookupPhone` phone) ps
  
  -- fmap phiPatternName
  -- traitFuncTree :: Map String (Name, Type, Map String (Name -> Clause))
  -- makeTraitFunc :: M.Map String Name -> Type -> M.Map String (Name -> Clause) -> ([Clause], Bool)
  
  -- traitTable   :: M.Map String (Name, Maybe (Name, M.Map String Name))

  -- Getting the phoneme names...
  let phoneNames = fmap phiPatternName phoneInfoMap
      traitDicts = snd <$> traitTable propData
  
  -- Making the trait functions...
  traitDecsNames <- forWithKey traitFuncTree $ \trtStr (funcName, retType, clsMap) -> do
    funcType <- [t| $(pure (ConT mainTypeName)) -> $(pure retType) |]
    let funcSign = SigD funcName funcType
        (nowClauses, trtType) = makeTraitFunc phoneNames retType clsMap
        funcDefn = FunD funcName nowClauses
        trtCstr' = M.lookup trtStr traitDicts
        trtCstrs = join trtCstr'
    -- These errors shouldn't occur, but just in case they do,
    -- here are the warnings.
    when (isNothing trtCstr') $ do
      reportWarning $ "Issue with trait \"" ++ trtStr ++ "\" when creating \"is" ++ (dataName trtStr) ++ "\" function; can't find trait in dictionary (internal error)."
    when (trtType && (isNothing trtCstrs)) $ do
      reportWarning $ "Issue with trait \"" ++ trtStr ++ "\": trait type doesn't match type in dictionary (internal error 1)."
    when ((not trtType) && (isJust trtCstrs)) $ do
      reportWarning $ "Issue with trait \"" ++ trtStr ++ "\": trait type doesn't match type in dictionary (internal error 2)."
    return ([funcSign, funcDefn], (funcName, trtCstrs))

  let traitDecs  = fst <$> traitDecsNames
      traitNames = snd <$> traitDecsNames
      traitDecs' = concat $ M.elems traitDecs

  -- Classes already of type `Type`.
  eqC   <- [t|  Eq  |]
  showC <- [t| Show |]

  -- The newest type declarations.
  let wordTypeDecs = sumAdtDecDeriv wordTypeName [(wordConsName1, [wordConsType1]), (wordConsName2, [wordConsType2])] [eqC, showC]

  let finalDB = PhonemeDatabase
        { pdbPropertyData     = propData
        , pdbPhonemeInfo      = phoneInfoMap
        , pdbTopPhonemeType   = mainTypeName
        , pdbWordTypeNames    = (wordTypeName, (wordConsName1, wordConsName2))
        , pdbGroupMemberFuncs = M.mapMaybe id $ isGroupSubFuncs phoneGroups
        , pdbTraitInformation = traitNames
        , pdbMkMaj = thisMkMaj
        , pdbMkMin = thisMkMin
        }
  
  -- Return the actual stuff
  return (finalDB, wordTypeDecs : traitDecs' ++ theDecs)

makeTraitFunc :: M.Map String Name -> Type -> M.Map String (Name -> Clause) -> ([Clause], Bool)
makeTraitFunc patMap retType theMap 
  = ((M.elems $ M.mapMaybeWithKey (\str func -> func <$> M.lookup str patMap) theMap) ++ [emptyCase], hasVals)
  where
    emptyCase 
      | hasVals   = Clause [WildP] (NormalB nothingE) []
      | otherwise = Clause [WildP] (NormalB   falseE) []
    hasVals = isMaybeE retType

isMaybeE :: Type -> Bool
isMaybeE (AppT (ConT x) _) = x == ''Maybe
isMaybeE _ = False

-- | Produce most of the data generated by this module.
produceVariousData :: PhonemeParsingStructure -> Q ((PropertyData, M.Map String PhonemeHierarchy,M.Map String Name, M.Map (String, String) Name, Name, GroupProps, M.Map String (Name, Type, M.Map String (Name -> Clause))), [Dec])
produceVariousData pps = do
  propData <- producePropertyData pps

  let propDecs = propertyDecs propData
      phoneInv = ppsPhonemeInventory pps
  (phonDats, phonGrps, patNoms, isFuncNames, mainTypeName, traitFuncTree, phonDecs) <- producePhonemeInventory propData phoneInv
  -- let phonDecs = snd phonData
  --     phonDats = fst phonData
  return ((propData, phonDats, patNoms, isFuncNames, mainTypeName, phonGrps, traitFuncTree), (propDecs <> phonDecs))


-- Map String (Name, Type, Map String (Name -> Clause))

producePropertyData :: PhonemeParsingStructure -> Q PropertyData
producePropertyData pps = do
  (aspTab, trtTab, mtData, theDecs) <- producePropertyData' pps
  let trtData = forMap mtData $ \(nm, typs, defName) ->
        (TraitData nm typs defName)
  return $ PropertyData aspTab trtTab trtData theDecs


producePropertyData' :: PhonemeParsingStructure -> Q (M.Map String (Name, (Name, M.Map String Name)), M.Map String (Name, Maybe (Name, M.Map String Name)), Maybe (Name, M.Map String (Name, Type), Name), [Dec])
producePropertyData' pps = do
  let aspects = ppsPhonemeAspects pps
      traits  = ppsPhonemeTraits  pps
  
  -- Helpful Classes
  eqClass   <- [t| Eq |]
  ordClass  <- [t| Ord |]
  showClass <- [t| Show |]

  -- Note: It's okay if any aspects or traits
  -- use the same name for some of their options,
  -- since `newName` ensures that a new, unique
  -- name is generated
  -- aspMap :: Map String ((Name, (Name, Map String Name)), [Dec])
  aspMap <- forWithKey aspects $ \asp ops -> do
    aspName <- newName (dataName asp)
    aspRecN <- newName (varName  asp)
    opNames <- mapM (newName . dataName) $ fromSelfList ops -- :: Map String Name

    -- == data AspectName = Option1 | Option2 | Option3 ... deriving (Show, Eq)
    let aspDec = sumAdtDecDeriv aspName (map (,[]) $ M.elems opNames) [eqClass, ordClass]

    -- == instance Show AspectName where
    --      show Option1 = "option1"
    --      show Option2 = "option2"
    --      show Option3 = "option3"
    --      ...
    let aspShw = showSumInstance aspName (map swap $ M.toList opNames)
    -- aspShow <- [d| instance Show $(aspName) where show  |]

    return ((aspName, (aspRecN, opNames)), (aspDec : aspShw))
  
  let aspMap1 = fmap fst aspMap -- :: Map String (Name, (Name, Map String Name))
      aspMap2 = fmap snd aspMap -- :: Map String [Dec]
      aspDecs = concat $ M.elems aspMap2
  
  -- trtMap :: Map String ((Name, Maybe (Name, Map String Name)), Maybe [Dec] )
  trtMap <- forWithKey traits $ \trt ops -> do
    trtRecName <- newName trt
    -- trtTypeNames :: Maybe (Name, Map String Name)
    trtTypeNames <- case ops of
      [] -> return Nothing
      _s -> do
        trName  <- newName $ dataName trt
        opNames <- mapM (newName . dataName) $ fromSelfList ops
        return $ Just (trName,opNames)
    
    umOkay <- case trtTypeNames of
      Nothing    -> return $ Nothing
      (Just mps) -> return $ Just $ 
        (sumAdtDecDeriv (fst mps) (map (,[]) $ M.elems (snd mps)) [eqClass, ordClass])
        : (showSumInstance (fst mps) (map swap $ M.toList $ snd mps))
    return ((trtRecName,trtTypeNames), umOkay)
  
  -- To make the record type for Phoneme Traits...
  -- traitFields = M.map (fst . fst) trtMap
  traitRecTypeNameT <- newName "PhonemePropsT"
  traitRecTypeNameD <- newName "PhonemePropsD"
  let traitRecTypes = forMap trtMap $ \((trtRecName, mTrtInfo), _) -> case mTrtInfo of
        Nothing -> (trtRecName, ConT ''Bool)
        (Just (typeNom,_)) -> (trtRecName, maybeType $ ConT typeNom)
      aspctRecTypes = forMap aspMap1 $ \(aspTypeName, (aspRecName, _aspConsNames)) -> 
        (aspRecName, maybeType $ ConT aspTypeName)
      -- Since the parser disallows aspects and traits to have the same
      -- String/Name, the two maps should be disjoint. Thus, this type
      -- should work fine.
      propRecTypes = M.union traitRecTypes aspctRecTypes

  -- To be used later on...
  defRecordName <- newName "defaultPhonemeProps"

  -- Return the Type name of the Trait record type, along
  -- with the names of the fields.
  let trtRecOutput = if (M.null traitRecTypes)
        then Nothing
        else Just (traitRecTypeNameT, propRecTypes, defRecordName)

  -- Create the record type declaration.
  let trtRecordDec = case trtRecOutput of
        Nothing -> []
        (Just (_,prs,_)) -> [recordAdtDecDeriv traitRecTypeNameT traitRecTypeNameD (M.elems prs) [eqClass, showClass] ]
  

  -- (The type on the next line may be out of date)
  -- trtMap :: Map String ((Name, Maybe (Name, Map String Name)), Maybe [Dec] )
  let trtMap1 = fmap fst trtMap
      trtMap2 = fmap snd trtMap
      trtDecs = concat $ catMaybes $ M.elems trtMap2
  
  -- Making the "default" trait type.
  -- defRecordName <- newName "defaultPhonemeTraits"
  let defRecordSig = case trtRecOutput of
        Nothing  -> []
        (Just _) -> [SigD defRecordName (ConT traitRecTypeNameT)]
  
  -- A declaration of the default record value/function.
  -- e.g.
  --  > defaultPhonemeProps = PhonemeProps False Nothing Nothing False Nothing
  let defRecordDec = case trtRecOutput of
        Nothing  -> []
        (Just (_,xs,_)) -> [ValD (VarP defRecordName) (NormalB $ THL.multiAppE (ConE traitRecTypeNameD) (map (produceDefaultRecV . snd) $ M.elems xs) ) []]
  
  return (aspMap1, trtMap1, trtRecOutput, aspDecs <> trtDecs <> trtRecordDec <> defRecordSig <> defRecordDec)


-- | Should only be used for mapping from Types of the fields
--   of `defaultPhonemeTraits` to expressions.
produceDefaultRecV :: Type -> Exp
produceDefaultRecV (ConT x)
  | (x == ''Bool) = ConE 'False
  | otherwise     = error "Encountered a type that isn't Bool or (Maybe ...)"
produceDefaultRecV (AppT (ConT x) _) 
  | (x == ''Maybe) = ConE 'Nothing
  | otherwise      = error "Encountered a type that isn't Bool or (Maybe ...)"
produceDefaultRecV _ = error "Encountered a type that isn't Bool or (Maybe ...)"


-- :m + Metamorth.Interpretation.Phonemes.Parsing Metamorth.Interpretation.Phonemes.Parsing.Types Metamorth.Interpretation.Phonemes.TH Language.Haskell.TH Data.Either Language.Haskell.TH.Ppr Control.Monad
-- import Data.Text.IO qualified as TIO
-- join $ runQ <$> producePropertyData <$> fromRight defaultPhonemeStructure <$> execPhonemeParser parsePhonemeFile <$> TIO.readFile "local/example1.thy"

--  fmap ppr $ fmap propertyDecs $ join $ runQ <$> producePropertyData <$> fromRight defaultPhonemeStructure <$> execPhonemeParser parsePhonemeFile <$> TIO.readFile "local/example1.thy"
-- fmap ppr $ join $ runQ <$> produceVariousDecs <$> fromRight defaultPhonemeStructure <$> execPhonemeParser parsePhonemeFile <$> TIO.readFile "local/example1.thy"

data GroupProps = GroupProps
  { groupStringName :: String
  , isGroupFuncName :: Name
  -- all the lower-down "is<Subsubgroup>" functions.
  , isGroupSubFuncs :: M.Map String (Maybe Name)
  -- , groupType :: Type
  , phonemeConstructors :: M.Map String (([Type], Name), [Name])
  , isGroupBottom :: Bool
  -- 
  -- , isGroupFuncType :: Type
  } deriving (Show, Eq)

-- | To make phoneme groups work.
data PhonemeHierarchy
   = PhoneLeaf Name [Type]
   | PhoneNode Name (M.Map String PhonemeHierarchy)
   deriving (Show, Eq)

{-
-- | Lookup a value in a hierarchy.
lookupHierarchy :: (M.Map String PhonemeHierarchy) -> String -> Maybe (Name, [Type])
lookupHierarchy pmap str
  = asum $ M.map (\k v -> lookupHierarchy' k v str) pmap

lookupHierarchy' :: String -> PhonemeHierarchy -> String -> Maybe (Name, [Type])
lookupHierarchy' pstr (PhoneLeaf nom typs) str
  | pstr == str = Just (nom, typs)
  | otherwise   = Nothing
lookupHierarchy' _str (PhoneNode _ mp) str
  = lookupHierarchy mp str
-}

hierarchyName :: PhonemeHierarchy -> Name
hierarchyName (PhoneLeaf nm _) = nm
hierarchyName (PhoneNode nm _) = nm

makeLeaves :: (Ord k) => M.Map k (Name, [Type]) -> M.Map k PhonemeHierarchy
makeLeaves = M.map $ \(nm, typs) -> PhoneLeaf nm typs

makeNodes :: (Ord k) => M.Map k (Name, M.Map String (PhonemeHierarchy)) -> M.Map k PhonemeHierarchy
makeNodes  = M.map $ \(nm, nods) -> PhoneNode nm nods

-- What we need out of this:
--  * For each phoneme:
--    * A pattern synonym to cut through the nested layers
--    * An integer giving the number of arguments it has.

-- Note: the "cstrList" is just the list of constructors that
-- make the final constructor; e.g.
--
-- Letter (Consonant (Labial P))
-- would bde ["Letter", "Consonant", "Labial", "P"]
producePhonemePatterns :: Name -> M.Map String (([Type], Name), [Name]) -> Q (M.Map String (Name, [Dec]))
producePhonemePatterns topType mp = do
  -- Only create as many vars as needed.
  let maxLen = if (M.null mp) then 0 else maximum $ M.map (\((ts,_),_) -> length ts) mp
  necVars <- forM (take maxLen [(1 :: Int)..]) $ \n -> newName ("x" ++ (show n))
  forWithKey mp $ \phoneStr ((argList, baseConstr), cstrList) -> do
    -- hmm...
    patName <- newName $ (dataName phoneStr) <> "PhonePat"
    let patVars = take (length argList) necVars
        patSign = PatSynSigD patName $ THL.arrowChainT argList (ConT topType)
        patRslt = PatSynD patName (PrefixPatSyn patVars) ImplBidir (nestedConPat cstrList baseConstr (map VarP patVars))
    return (patName, [patSign, patRslt])

producePhonemeInventory :: PropertyData -> PhonemeInventory -> Q (M.Map String PhonemeHierarchy, GroupProps, M.Map String Name, M.Map (String, String) Name, Name, M.Map String (Name, Type, M.Map String (Name -> Clause)), [Dec])
producePhonemeInventory propData phi = do
  mainName <- newName "Phoneme"
  baseClauseMaps <- makeInitialClauseMap $ traitTable propData
  ((phoneHi, groupProps, decs), isGroupFuncsStuff) <- runStateT (producePhonemeInventory' "Phoneme" mainName propData phi) (PhoneInvState M.empty baseClauseMaps)
  patMap <- producePhonemePatterns mainName (phonemeConstructors groupProps)
  let patDecs = concat $ M.elems $ M.map snd patMap
      patNoms = M.map fst patMap

  return (phoneHi, groupProps, patNoms, pisMatrix isGroupFuncsStuff, mainName, pisClauses isGroupFuncsStuff, decs <> patDecs)

data PhoneInvState = PhoneInvState
  { pisMatrix  :: M.Map (String, String) Name
  , pisClauses :: M.Map String (Name, Type, M.Map String (Name -> Clause))
  }

modifyMatrix :: Monad m => (M.Map (String, String) Name -> M.Map (String, String) Name) -> StateT PhoneInvState m ()
modifyMatrix f = do
  x <- get
  put $! x {pisMatrix = f $! pisMatrix x }
--  Monad m => (s -> s) -> StateT s m ()

modifyClauses :: Monad m => String -> (M.Map String (Name -> Clause) -> M.Map String (Name -> Clause)) -> StateT PhoneInvState m ()
modifyClauses trtStr f = do
  x <- get 
  let clsMap = pisClauses x
      y = forAdjust trtStr clsMap $ \(funcNom, typ, thisMap) -> (funcNom, typ, f thisMap)
  put $! x {pisClauses = y}
  where
    forAdjust :: Ord k => k -> M.Map k a -> (a -> a) -> M.Map k a
    forAdjust k mp g = M.adjust g k mp

-- | Modify the clauses with a monadic action.
modifyClausesM :: Monad m => String -> (M.Map String (Name -> Clause) -> m (M.Map String (Name -> Clause))) -> StateT PhoneInvState m ()
modifyClausesM trtStr f = do
  st <- get 
  let clsMap = pisClauses st
      y = M.lookup trtStr clsMap
  -- forAdjust trtStr clsMap $ \(funcNom, typ, thisMap) -> (funcNom, typ, f thisMap)
  case y of
    Nothing -> return ()
    (Just (funcNom, typ, thisMap)) -> do
      thatMap <- lift $ f thisMap
      put $! st {pisClauses = M.insert trtStr (funcNom, typ, thatMap) clsMap}

-- | Modify the clauses with an action in `Q`. This version
--   reports a warning if one of the traits can't be found.
modifyClausesQ :: String -> (M.Map String (Name -> Clause) -> Q (M.Map String (Name -> Clause))) -> StateT PhoneInvState Q ()
modifyClausesQ trtStr f = do
  st <- get 
  let clsMap = pisClauses st
      y = M.lookup trtStr clsMap
  -- forAdjust trtStr clsMap $ \(funcNom, typ, thisMap) -> (funcNom, typ, f thisMap)
  case y of
    Nothing -> lift (reportWarning $ "modifyClausesQ : Couldn't find trait \"" ++ trtStr ++ "\".")
    (Just (funcNom, typ, thisMap)) -> do
      thatMap <- lift $ f thisMap
      put $! st {pisClauses = M.insert trtStr (funcNom, typ, thatMap) clsMap}


-- | Create the initial "empty" trait `Clause` map
--   from the trait table found in `PropertyData`.
makeInitialClauseMap :: M.Map String (Name, Maybe (Name, M.Map String Name)) -> Q (M.Map String (Name, Type, M.Map String (Name -> Clause)))
makeInitialClauseMap = M.traverseWithKey makeVal
  where
    makeVal :: String -> (Name, Maybe (Name, M.Map String Name)) -> Q (Name, Type, M.Map String (Name -> Clause))
    makeVal trtStr (_nom, Nothing) = do
      funcName <- newName $ "is" ++ dataName trtStr
      return (funcName, ConT ''Bool, M.empty)
    makeVal trtStr (_nom, Just (typNom, _conMap)) = do
      funcName <- newName $ "is" ++ dataName trtStr
      return (funcName, AppT (ConT ''Maybe) (ConT typNom), M.empty)


-- M.Map String (Name, Maybe (Name, M.Map String Name))
{-
createTraitClauses 
  :: M.Map String (Name -> Clause) -- accumulator
  -> String 
  -> Name
  -> Maybe (Name, M.Map String Name)
  -> String 
  -> PhonemeProperties
  -> Q (M.Map String (Name -> Clause))
createTraitClauses acc trtStr trtNom (Just (trtDNom, trtCstrs)) phoneNom phoneProps = do

forFoldM :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
-}

-- producePhonemeInventory' :: Name -> PropertyData -> PhonemeInventory -> Q (M.Map String PhonemeHierarchy, GroupProps, [Dec])
producePhonemeInventory' :: String -> Name -> PropertyData -> PhonemeInventory -> StateT PhoneInvState Q (M.Map String PhonemeHierarchy, GroupProps, [Dec])
producePhonemeInventory' _thisGrpStr nm propData (PhonemeSet mp) = do
  (mpX, grpProps, decs) <- lift $ producePhonemeSet propData nm mp
  -- Handle the trait thingies.
  -- Quite awkward, but I think it should work...
  forWithKey_ (traitTable propData) $ \trtStr (oldNom, mCstrs) -> do
    modifyClausesQ trtStr $ \clsMap -> forFoldM clsMap (M.assocs mp) $ \acc (phoneStr, phoneProps) -> do
      createTraitClauses acc trtStr oldNom mCstrs phoneStr phoneProps    

  return (makeLeaves mpX, grpProps, decs)
producePhonemeInventory' thisGrpStr nm propData (PhonemeGroup mp) = do
  -- rslts :: M.Map String (Name, ((M.Map String PhonemeHierarchy), [Dec]) )
  rslts <- forWithKey mp $ \str phi -> do
    subName <- lift $ newName $ dataName str
    (mpX, gProps, decs) <- producePhonemeInventory' str subName propData phi
    return (subName, (mpX, gProps, decs))
  
  let subDecs  = forMap rslts $ \(_,(_,_,dcls)) -> dcls
      subDecs' = concat $ M.elems subDecs
      subNoms  = M.map fst rslts
      -- I'm so confused...
      -- subRslts = makeNodes $ forMap rslts $ \(nom,(mpZ,gProps,_)) -> (nom,gProps,mpZ)
      subGrps :: M.Map String (Name, GroupProps)
      subGrps = forMap rslts $ \(subNom,(_,subGrpProps,_)) -> (subNom,subGrpProps)
      -- subGrps  = forMap rslts $ \(_,(theMap,_,_decs)) -> forMaybeMapWithKey theMap $ \str val -> case val of
      --   (PhoneLeaf _ _) -> Nothing
      --   (PhoneNode _grpNom grpProps _grpMap) -> Just grpProps

  
  -- The value to be passed to `sumAdtDecDeriv`
  -- i.e. M.Map String (Name,[Type])
  subPats <- forWithKey subNoms $ \str nom -> do
    -- Adding "Ph" to distinguish it a bit...
    sumName <- lift $ newName $ "Ph" <> (dataName str)
    return (sumName, [ConT nom])
  
  -- Create the isSubgroup_ThisGroup functions, etc...
  
  -- subGrpRslts :: M.Map String ((M.Map String Name, M.Map String ((Int, Name), [Name])), [Dec])
  subGrpRslts <- sequence $ forIntersectionWithKey subGrps subPats $ \subGrpString (_subGrpNom, subGrpProps) (conName,_) -> do
    -- hmm...
    let oneDownFnc  = isGroupFuncName subGrpProps
        oneDownFnc' = if (isGroupBottom subGrpProps) then Nothing else (Just oneDownFnc)
        downhillMap = M.insert subGrpString oneDownFnc' $ isGroupSubFuncs subGrpProps
        newPhoneConstrs = M.map (second (conName:)) $ phonemeConstructors subGrpProps
    
    -- This will be merged with other maps once returned to the upper level.
    -- newSubFuncs :: M.Map String (Name, [Dec])
    newSubFuncs <- forWithKey downhillMap $ \anotherGroupStr subFuncNom -> do
      -- remember, "nm" is actually the top-level name here.
      newFuncName <- lift $ newName $ "is" <> (dataName anotherGroupStr) <> "_" <> (thisGrpStr)
      newVar      <- lift $ newName "x"
      let newFuncSign = SigD newFuncName (THL.arrowChainT [ConT nm] (ConT ''Bool))
          -- produceSubMapClause constr (Just funcName) var
          -- newFuncBod1 = Clause [ConP subGrpNom [] [VarP newVar]] (NormalB (AppE (VarE subFuncNom) (VarE newVar))) []
          -- newFuncBod1 = produceSubMapClause subGrpNom subFuncNom newVar
          newFuncBod1 = produceSubMapClause conName subFuncNom newVar
          newFuncBod2 = Clause [WildP] (NormalB (ConE 'False)) []
          newFuncDefn = FunD newFuncName [newFuncBod1, newFuncBod2]
          newFuncDecl = [newFuncSign, newFuncDefn]
      modifyMatrix $ M.insert (thisGrpStr, anotherGroupStr) newFuncName
      return (Just newFuncName, newFuncDecl)
    
    let newSubFuncNames = M.map fst newSubFuncs
        newSubFuncDecls = concat $ M.elems $ M.map snd newSubFuncs
    
    -- Do I need to do more here?
    return ((newSubFuncNames, newPhoneConstrs), newSubFuncDecls)

  -- isGroupFuncName
  -- This is a mostly pointless function whose purpose
  -- is to make the recursive steps easier.
  igfName <- lift $ newName $ "is" <> (nameBase nm) <> "_" <> (nameBase nm)
  let groupStr = nameBase nm -- temp?
      igfSign  = SigD igfName (AppT (AppT ArrowT (ConT nm)) (ConT ''Bool))
      igfDecl  = FunD igfName [Clause [WildP] (NormalB (ConE 'True)) []]
      igfPrag  = PragmaD (InlineP igfName Inline FunLike AllPhases)
      igfDecls = [igfSign, igfDecl, igfPrag]

  let subGrpRslts1 = M.unions $ M.map (fst . fst) subGrpRslts
      subGrpRslts2 = M.unions $ M.map (snd . fst) subGrpRslts
      subGrpDecls  = fold   $ M.map snd subGrpRslts
      thisGroupProps = GroupProps
        { groupStringName     = groupStr
        , isGroupFuncName     = igfName
        , isGroupSubFuncs     = subGrpRslts1
        , phonemeConstructors = subGrpRslts2
        , isGroupBottom       = False
        }

-- 


  -- Also todo: add the (Phoneme -> PhonemeProps) function
  -- for this level of the code. 
  
  -- Also todo: add "is<Group>" functions for each level.

{-
  let _help :: M.Map String (GroupProps, [Dec])
      _help = subFuncs
      subDecls = concatMap snd $ M.elems subFuncs
      subGrpProps = M.map fst subFuncs
-}    

      

  -- Create a data type for this:
  -- (Also: Maybe make a custom `Show` instance
  --  that just shows the underlying value.)
  let newDecs   = sumAdtDecDeriv nm (M.elems subPats) [(ConT ''Eq), (ConT ''Ord), (ConT ''Show)]
      -- subRsltsX :: _
      subRsltsX = forMap rslts $ \(nom,(mpZ,_,_)) -> (nom,mpZ)
      subRslts  = makeNodes subRsltsX
  
  -- makeNodes :: (Ord k) => M.Map k (Name, M.Map String (PhonemeHierarchy)) -> M.Map k PhonemeHierarchy

  -- Temporary combining:

  return ((subRslts), thisGroupProps, [newDecs] <> subDecs' <> subGrpDecls <> igfDecls)

{-
data GroupProps = GroupProps
  { groupStringName :: String
  , isGroupFuncName :: Name
  , isGroupSubFuncs :: M.Map String Name
  , phonemeConstructors :: M.Map String ((Int, Name), [Name])
  } deriving (Show, Eq)

-}

-- newFuncBod1 = Clause [ConP subGrpNom [] [VarP newVar]] (NormalB (AppE (VarE subFuncNom) (VarE newVar))) []
-- produceSubMapClause' :: Bool -> Name -> Name -> Name -> Clause
-- produceSubMapClause' True  constr        _   _ = Clause [ConP constr [] [WildP]]    (NormalB (ConE 'True)) []
-- produceSubMapClause' False constr funcName var = Clause [ConP constr [] [VarP var]] (NormalB (AppE (VarE funcName) (VarE var))) []

produceSubMapClause :: Name -> Maybe Name -> Name -> Clause
produceSubMapClause constr         Nothing   _ = Clause [ConP constr [] [WildP]]    (NormalB (ConE 'True)) []
produceSubMapClause constr (Just funcName) var = Clause [ConP constr [] [VarP var]] (NormalB (AppE (VarE funcName) (VarE var))) []


{-
data PhonemeProperties
   = PhonemeProperties 
       { phAspects :: [String]
       , phTraits  :: [(String, Maybe String)]
       } deriving (Show, Eq)

, traitTable   :: M.Map String (Name, Maybe (Name, M.Map String Name))
-}

-- -> M.Map String (Name, Maybe (Name, M.Map String Name)) 

makePhoneWildPat :: Name -> PhonemeProperties -> Pat
makePhoneWildPat phoneNom phoneProps
  = ConP phoneNom [] ((phAspects phoneProps) $> WildP)

-- | Create the clauses for a function
--   that tells whether a specific phoneme
--   has a certain trait or not. Note that
--   we use (Name -> Clause) as the value
--   of the `M.Map`, since we may not know
--   whether we have the pattern synonym
--   name at this point in the construction.
createTraitClauses 
  :: M.Map String (Name -> Clause) -- accumulator
  -> String 
  -> Name
  -> Maybe (Name, M.Map String Name)
  -> String 
  -> PhonemeProperties
  -> Q (M.Map String (Name -> Clause))
createTraitClauses acc trtStr trtNom Nothing phoneNom phoneProps = do
  let srchVal = (trtStr, Nothing)
  if (srchVal `elem` (phTraits phoneProps))
    then return (M.insert phoneNom (\nom -> Clause [wildPat nom] (NormalB trueE) []) acc)
    else do 
      let mRslt = find altPred (phTraits phoneProps)
      case mRslt of
        Nothing -> return acc
        (Just (_,Nothing)) -> return acc -- already handled.
        (Just (_,Just rslt)) -> do
          qReportError $ "Trait \"" ++ trtStr ++ "\" is a boolean trait; having phoneme \"" ++ phoneNom ++ "\" set it to value \"" ++ rslt ++ "\" will not work." 
          return acc
  where 
    wildPat nom = makePhoneWildPat nom phoneProps
    altPred (theTrait, _) = theTrait == trtStr

createTraitClauses acc trtStr trtNom (Just (trtDNom, trtCstrs)) phoneNom phoneProps = do
  let mRslt = find altPred (phTraits phoneProps)
  case mRslt of
    Nothing -> return acc -- This phoneme doesn't use this trait.
    (Just (_,Nothing)) -> do -- This phoneme doesn't specify a value for this trait.
      reportError $ "Phoneme \"" ++ phoneNom ++ "\" doesn't specify a value for trait \"" ++ trtStr ++ "\"."
      return acc
    (Just (_,Just thisCstr)) -> do -- This phoneme DOES specifiy a value for this trait.
      let mMatch = M.lookup thisCstr trtCstrs
      case mMatch of
        Nothing -> do -- failed to find 
          reportError $ "Phoneme \"" ++ phoneNom ++ "\": Trait \"" ++ trtStr ++ "\" does not have a value known as \"" ++ thisCstr ++ "\"."
          return acc
        (Just cstrNom) -> do
          let thisClause nom = Clause [wildPat nom] (NormalB $ justE (ConE cstrNom)) []
          return (M.insert phoneNom thisClause acc)
  where
    wildPat nom = makePhoneWildPat nom phoneProps
    altPred (theTrait, _) = theTrait == trtStr

producePhonemeSet :: PropertyData -> Name -> M.Map String PhonemeProperties -> Q (M.Map String (Name, [Type]), GroupProps, [Dec])
producePhonemeSet propData subName phoneSet = do
  -- Need to have a way to supply the user-side
  -- name string to this function.

  -- The name to map from phonemes to traits
  -- STILL TODO.
  -- funcName <- newName ((nameBase subName) <> "_traits") -- changed how we do this


  -- isGroupFuncName
  -- This is a mostly pointless function whose purpose
  -- is to make the recursive steps easier.
  igfName <- newName $ "is" <> (nameBase subName) <> "_" <> (nameBase subName)
  let groupStr = nameBase subName -- temp?
      -- igfSign  = SigD igfName (AppT (AppT ArrowT (ConT subName)) (ConT ''Bool))
      -- igfDecl  = FunD igfName [Clause [WildP] (NormalB (ConE 'True)) []]
      -- igfPrag  = PragmaD (InlineP igfName Inline FunLike AllPhases)
      -- igfDecls = [igfSign, igfDecl, igfPrag]
  
  let aspTable = aspectTable propData
      aspTableNames = M.map fst aspTable

  -- The name of the data type for this set of phonemes.
  -- (Unneeded; `subName` is already the right name.)
  -- phonemeSetName <- newName ((nameBase subName) <> (dataName subName))

  -- phoneMap :: M.Map String (Name,[Type])
  phoneMap <- forWithKey phoneSet $ \phone props -> do
    phoneName <- newName $ dataName phone
    let phoneAsps     = phAspects props -- :: [String]
        phoneAspTypes = mapMaybe (\str -> ConT <$> M.lookup str aspTableNames) phoneAsps
    return (phoneName, phoneAspTypes) 
  
  let phoneDecs = sumAdtDecDeriv subName (M.elems phoneMap) [(ConT ''Eq), (ConT ''Ord)]
      phoneShowMap  = map (\(str,(nm,typ)) -> (nm,str,length typ)) (M.assocs phoneMap)
  
  phoneShowDecs <- showSumProdInstanceAlt subName phoneShowMap

  -- Getting the last few group properties...
  let phoneConstructors = forMap phoneMap $ \(phoneNom, argList) -> 
        ((argList, phoneNom), [])

  let groupProps = GroupProps
        { groupStringName = groupStr
        , isGroupFuncName = igfName
        , isGroupSubFuncs = M.empty
        , phonemeConstructors = phoneConstructors
        , isGroupBottom   = True
        }

  -- return (phoneMap, groupProps, phoneDecs : phoneShowDecs <> igfDecls)
  return (phoneMap, groupProps, phoneDecs : phoneShowDecs)

-- | Make a record update expression, to be used
--   when making the `phonemeProperties` function.
--   Note that this returns an `Exp`ression, *not* a
--   function declaration or `Clause`.
makeRecordUpdate :: Name -> M.Map String (Name, Maybe (Name, M.Map String Name)) -> M.Map String (Name,Type) -> PhonemeProperties -> Exp
makeRecordUpdate defPropName traitsInfo traitFields phoneProps
  = RecUpdE (VarE defPropName) phoneStuff'
  -- = forMap phone

  where
    -- phoneTraits :: [(String, Maybe String)] 
    -- phoneTraits = phTraits phoneProps
    phoneStuff :: [Maybe (Name, Exp)]
    phoneStuff = forMap (phTraits phoneProps) $ \(trtStr, trtOption) -> do
      (_trtName, mtrtOps) <- M.lookup trtStr traitsInfo
      let mtrtOps' = snd <$> mtrtOps
      trtValue <- case trtOption of
        Nothing    -> Just $ ConE 'True
        (Just opn) -> AppE (ConE 'Just) <$> (ConE <$> (M.lookup opn =<< mtrtOps'))
      trtField <- fst <$> M.lookup trtStr traitFields
      return (trtField, trtValue)
    phoneStuff' :: [(Name, Exp)]
    phoneStuff' = catMaybes phoneStuff



-- producePhonemeData :: PhonemeParsingStructure -> Q ([Dec], M.Map String Name)
-- producePhonemeData pss = do


-- :m + Metamorth.Interpretation.Phonemes.Parsing Metamorth.Interpretation.Phonemes.Parsing.Types Metamorth.Interpretation.Phonemes.TH Language.Haskell.TH Data.Either Control.Monad Metamorth.Helpers.IO
-- join $ runQ <$> producePropertyData <$> fromRight defaultPhonemeStructure <$> execPhonemeParser parsePhonemeFile <$> readFileUTF8 "local/example1.thy"
-- join $ runQ <$> producePhonemeDatabase <$> fromRight defaultPhonemeStructure <$> execPhonemeParser parsePhonemeFile <$> readFileUTF8 "examples/phonemes/example_inuktitut.thyt"

