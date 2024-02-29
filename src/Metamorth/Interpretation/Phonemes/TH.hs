{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Interpretation.Phonemes.TH
  ( producePropertyData
  , producePhonemeInventory
  , produceVariousData
  , produceVariousDecs
  , PropertyData(..)
  , PhonemeHierarchy(..)
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.Tuple

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

import THLego.Helpers qualified as THL

import Metamorth.Interpretation.Phonemes.Types
  ( PhonemeParsingStructure(..)
  , PhonemeInventory(..)
  , PhonemeProperties(..)
  )

import Data.Map.Strict qualified as M

import Metamorth.Helpers.TH
import Metamorth.Helpers.Map


-- | A type to make understanding the output
--   of `producePropertyData` easier.
data PropertyData = PropertyData
  { aspectTable  :: M.Map String (Name, (Name, M.Map String Name))
  , traitTable   :: M.Map String (Name, Maybe (Name, M.Map String Name))
  , traitData    :: Maybe TraitData
  , propertyDecs :: [Dec]
  } deriving (Show, Eq)

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


produceVariousData :: PhonemeParsingStructure -> Q ((PropertyData, M.Map String PhonemeHierarchy,M.Map String Name, M.Map (String, String) Name), [Dec])
produceVariousData pps = do
  propData <- producePropertyData pps
  let propDecs = propertyDecs propData
      phoneInv = ppsPhonemeInventory pps
  (phonDats, phonGrps, patNoms, isFuncNames, phonDecs) <- producePhonemeInventory propData phoneInv
  -- let phonDecs = snd phonData
  --     phonDats = fst phonData
  return ((propData, phonDats, patNoms, isFuncNames), (propDecs <> phonDecs))



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
      xs -> do
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
  traitRecTypeName <- newName "PhonemeProps"
  let traitRecTypes = forMap trtMap $ \((trtRecName, mTrtInfo), _) -> case mTrtInfo of
        Nothing -> (trtRecName, ConT ''Bool)
        (Just (typeNom,_)) -> (trtRecName, maybeType $ ConT typeNom)
      aspctRecTypes = forMap aspMap1 $ \(aspTypeName, (aspRecName, aspConsNames)) -> 
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
        else Just (traitRecTypeName, propRecTypes, defRecordName)

  -- Create the record type declaration.
  let trtRecordDec = case trtRecOutput of
        Nothing -> []
        (Just (_,prs,_)) -> [recordAdtDecDeriv traitRecTypeName (M.elems prs) [eqClass, showClass] ]
  

  -- (The type on the next line may be out of date)
  -- trtMap :: Map String ((Name, Maybe (Name, Map String Name)), Maybe [Dec] )
  let trtMap1 = fmap fst trtMap
      trtMap2 = fmap snd trtMap
      trtDecs = concat $ catMaybes $ M.elems trtMap2
  
  -- Making the "default" trait type.
  -- defRecordName <- newName "defaultPhonemeTraits"
  let defRecordSig = case trtRecOutput of
        Nothing  -> []
        (Just _) -> [SigD defRecordName (ConT traitRecTypeName)]
  
  -- A declaration of the default record value/function.
  -- e.g.
  --  > defaultPhonemeProps = PhonemeProps False Nothing Nothing False Nothing
  let defRecordDec = case trtRecOutput of
        Nothing  -> []
        (Just (_,xs,_)) -> [ValD (VarP defRecordName) (NormalB $ THL.multiAppE (ConE traitRecTypeName) (map (produceDefaultRecV . snd) $ M.elems xs) ) []]
  
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
  , phonemeConstructors :: M.Map String ((Int, Name), [Name])
  , isGroupBottom :: Bool
  -- 
  -- , isGroupFuncType :: Type
  } deriving (Show, Eq)

-- | To make phoneme groups work.
data PhonemeHierarchy
   = PhoneLeaf Name [Type]
   | PhoneNode Name (M.Map String PhonemeHierarchy)
   deriving (Show, Eq)

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

producePhonemePatterns :: M.Map String ((Int, Name), [Name]) -> Q (M.Map String (Name, Dec))
producePhonemePatterns mp = do
  -- Only create as many vars as needed.
  let maxLen = if (M.null mp) then 0 else maximum $ M.map (\((n,_),_) -> n) mp
  necVars <- replicateM maxLen $ newName "x"
  forWithKey mp $ \phoneStr ((numArgs, baseConstr), cstrList) -> do
    -- hmm...
    patName <- newName $ (dataName phoneStr) <> "PhonePat"
    let patVars = take numArgs necVars
        patRslt = PatSynD patName (PrefixPatSyn patVars) ImplBidir (nestedConPat cstrList baseConstr (map VarP patVars))
    return (patName, patRslt)

producePhonemeInventory :: PropertyData -> PhonemeInventory -> Q (M.Map String PhonemeHierarchy, GroupProps, M.Map String Name, M.Map (String, String) Name, [Dec])
producePhonemeInventory propData phi = do
  mainName <- newName "Phoneme"
  ((phoneHi, groupProps, decs), isGroupFuncsStuff) <- runStateT (producePhonemeInventory' "Phoneme" mainName propData phi) M.empty
  patMap <- producePhonemePatterns (phonemeConstructors groupProps)
  let patDecs = M.elems $ M.map snd patMap
      patNoms = M.map fst patMap


  return (phoneHi, groupProps, patNoms, isGroupFuncsStuff, decs <> patDecs)


-- producePhonemeInventory' :: Name -> PropertyData -> PhonemeInventory -> Q (M.Map String PhonemeHierarchy, GroupProps, [Dec])
producePhonemeInventory' :: String -> Name -> PropertyData -> PhonemeInventory -> StateT (M.Map (String, String) Name) Q (M.Map String PhonemeHierarchy, GroupProps, [Dec])
producePhonemeInventory' thisGrpStr nm propData (PhonemeSet mp) = do
  (mpX, grpProps, decs) <- lift $ producePhonemeSet propData nm mp
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

{-
data GroupProps = GroupProps
  { groupStringName :: String
  , isGroupFuncName :: Name
  -- all the lower-down "is<Subsubgroup>" functions.
  , isGroupSubFuncs :: M.Map String Name
  -- , groupType :: Type
  , phonemeConstructors :: M.Map String ((Int, Name), [Name])
  -- 
  } deriving (Show, Eq)
-}

  
  -- Create the isSubgroup_ThisGroup functions, etc...
  
  -- subGrpRslts :: M.Map String ((M.Map String Name, M.Map String ((Int, Name), [Name])), [Dec])
  subGrpRslts <- sequence $ forIntersectionWithKey subGrps subPats $ \subGrpString (subGrpNom, subGrpProps) (conName,_) -> do
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
          newFuncBod1 = produceSubMapClause subGrpNom subFuncNom newVar
          newFuncBod2 = Clause [WildP] (NormalB (ConE 'False)) []
          newFuncDefn = FunD newFuncName [newFuncBod1, newFuncBod2]
          newFuncDecl = [newFuncSign, newFuncDefn]
      modify $ M.insert (thisGrpStr, anotherGroupStr) newFuncName
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




  {-
  -- This is WAY too complicated...
  subFuncs <- sequence $ forIntersectionWithKey subPats subGrps $ \str (sumNom,typs) subGroupsMap -> do
    -- For the top-level stuff...
    funName <- newName $ "is" <> (dataName str) <> "_" <> (nameBase nm)
    let funType = THL.arrowChainT [ConT nm] (ConT ''Bool)
        funSign = SigD funName funType
        -- funMain = (FunD funName [Clause [ConP sumNom [] [WildP]] (NormalB (ConE 'True)) []])
        -- funElse = (FunD funName [Clause [WildP] (NormalB (ConE 'False)) []])
        funMain = Clause [ConP sumNom [] [WildP]] (NormalB (ConE 'True)) []
        funElse = Clause [WildP] (NormalB (ConE 'False)) []
        funDefn = (FunD funName [funMain, funElse])
    -- For the next level down stuff...
    lowerStuff <- forWithKey subGroupsMap $ \grpString grpProps -> do
      newVar  <- newName "x"
      let funTypeZ = THL.arrowChainT [ConT nm] (ConT ''Bool)
          
      subsubStuff <- forWithKey (isGroupSubFuncs grpProps) $ \strZ subsubName -> do
        funNameZ <- newName $ "is" <> (dataName strZ) <> "_" <> (nameBase nm)
        let funSignZ = SigD funNameZ funTypeZ
            funMainZ = Clause [ConP sumNom [] [VarP newVar]] (NormalB (AppE (VarE subsubName) (VarE newVar))) []
            funElseZ = Clause [WildP] (NormalB (ConE 'False)) []
            funDefnZ = [funSignZ, (FunD funNameZ [funMainZ, funElseZ])]
        return (funNameZ, funDefnZ)
      
      return subsubStuff
    
    let lowerStuff' = M.unions lowerStuff
        -- unsure what to insert... 
        lowerNames  = M.insert str funName $ M.map fst lowerStuff'
        lowerDecls  = concat $ M.elems $ M.map snd lowerStuff'
      
      -- (FunD nm 
      --   [ Clause [ConP sumNom [] [VarP newVar]] (NormalB (AppE (VarE funName23) (VarE newVar))) []
      --   , Clause [WildP] (NormalB (ConE 'False)) []]])

    let rsltGroupProps = GroupProps (nameBase nm) lowerNames funType

    return (rsltGroupProps, ([funSign, funDefn] <> lowerDecls))
    -}
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
produceSubMapClause' :: Bool -> Name -> Name -> Name -> Clause
produceSubMapClause' True  constr        _   _ = Clause [ConP constr [] [WildP]]    (NormalB (ConE 'True)) []
produceSubMapClause' False constr funcName var = Clause [ConP constr [] [VarP var]] (NormalB (AppE (VarE funcName) (VarE var))) []

produceSubMapClause :: Name -> (Maybe Name) -> Name -> Clause
produceSubMapClause constr         Nothing   _ = Clause [ConP constr [] [WildP]]    (NormalB (ConE 'True)) []
produceSubMapClause constr (Just funcName) var = Clause [ConP constr [] [VarP var]] (NormalB (AppE (VarE funcName) (VarE var))) []



producePhonemeSet :: PropertyData -> Name -> (M.Map String PhonemeProperties) -> Q (M.Map String (Name, [Type]), GroupProps, [Dec])
producePhonemeSet propData subName phoneSet = do
  -- Need to have a way to supply the user-side
  -- name string to this function.

  -- The name to map from phonemes to traits
  -- STILL TODO.
  funcName <- newName ((nameBase subName) <> "_traits")


  -- isGroupFuncName
  -- This is a mostly pointless function whose purpose
  -- is to make the recursive steps easier.
  igfName <- newName $ "is" <> (nameBase subName) <> "_" <> (nameBase subName)
  let groupStr = nameBase subName -- temp?
      igfSign  = SigD igfName (AppT (AppT ArrowT (ConT subName)) (ConT ''Bool))
      igfDecl  = FunD igfName [Clause [WildP] (NormalB (ConE 'True)) []]
      igfPrag  = PragmaD (InlineP igfName Inline FunLike AllPhases)
      igfDecls = [igfSign, igfDecl, igfPrag]
  
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
        ((length argList, phoneNom), [])

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
--   function declaration or clause.
makeRecordUpdate :: Name -> M.Map String (Name, Maybe (Name, M.Map String Name)) -> M.Map String (Name,Type) -> PhonemeProperties -> Exp
makeRecordUpdate defPropName traitsInfo traitFields phoneProps
  = RecUpdE (VarE defPropName) phoneStuff'
  -- = forMap phone

  where
    -- phoneTraits :: [(String, Maybe String)] 
    -- phoneTraits = phTraits phoneProps
    phoneStuff :: [Maybe (Name, Exp)]
    phoneStuff = forMap (phTraits phoneProps) $ \(trtStr, trtOption) -> do
      (trtName, mtrtOps) <- M.lookup trtStr traitsInfo
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


-- :m + Metamorth.Interpretation.Phonemes.Parsing Metamorth.Interpretation.Phonemes.Parsing.Types Metamorth.Interpretation.Phonemes.TH Language.Haskell.TH Data.Either Control.Monad
-- import Data.Text.IO qualified as TIO
-- join $ runQ <$> producePropertyData <$> fromRight defaultPhonemeStructure <$> execPhonemeParser parsePhonemeFile <$> TIO.readFile "local/example1.thy"


{-



ghci> data ExampleRec = ExampleRec { recField1 :: Bool, recField2 :: Int, recField3 :: Maybe Word}
ghci> [d| {exampleRec :: ExampleRec ; exampleRec = (ExampleRec False 0 Nothing)} |]
[SigD exampleRec_16 (ConT Ghci23.ExampleRec),ValD (VarP exampleRec_16) (NormalB (AppE (AppE (AppE (ConE Ghci23.ExampleRec) (ConE GHC.Types.False)) (LitE (IntegerL 0))) (ConE GHC.Maybe.Nothing))) []]

[| \x -> x {recField2 = 10} |]
LamE [VarP x_11] (RecUpdE (VarE x_11) [(Ghci2.recField2,LitE (IntegerL 10))])

-}


{-
[("harmony",(Harmony_0,fromList [("back",Back_1),("front",Front_2)]))]
,fromList 
  [("articulation",(articulation_3,Just (Articulation_4,fromList [("alveolar",Alveolar_5),("labial",Labial_6),("velar",Velar_7)])))
  ,("neutral",(neutral_8,Nothing))
  ]
,Just (PhonemeTraits_9, [(articulation_3,Articulation_4),(neutral_8,GHC.Types.Bool)])
, [DataD [] Harmony_0 [] Nothing [NormalC Back_1 [],NormalC Front_2 []] 
    [DerivClause Nothing [ConT GHC.Classes.Eq],DerivClause Nothing [ConT GHC.Classes.Ord]]
  , InstanceD Nothing [] (AppT (ConT GHC.Show.Show) (ConT Harmony_0)) 
      [FunD GHC.Show.show 
        [ Clause [ConP Back_1 [] []] (NormalB (LitE (StringL "back"))) []
        , Clause [ConP Front_2 [] []] (NormalB (LitE (StringL "front"))) []
        ]
      ]
  , DataD [] Articulation_4 [] Nothing [NormalC Alveolar_5 [],NormalC Labial_6 [],NormalC Velar_7 []] 
    [ DerivClause Nothing [ConT GHC.Classes.Eq],DerivClause Nothing [ConT GHC.Classes.Ord]]
    , InstanceD Nothing [] (AppT (ConT GHC.Show.Show) (ConT Articulation_4)) 
       [FunD GHC.Show.show 
         [Clause [ConP Alveolar_5 [] []] (NormalB (LitE (StringL "alveolar"))) []
         ,Clause [ConP Labial_6 [] []] (NormalB (LitE (StringL "labial"))) []
         ,Clause [ConP Velar_7 [] []] (NormalB (LitE (StringL "velar"))) []]]]
-}


