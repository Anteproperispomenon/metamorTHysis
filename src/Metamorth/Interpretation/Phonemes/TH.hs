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

import Data.Functor
import Data.Maybe
import Data.Tuple

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

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

produceVariousData :: PhonemeParsingStructure -> Q ((PropertyData, M.Map String PhonemeHierarchy), [Dec])
produceVariousData pps = do
  propData <- producePropertyData pps
  let propDecs = propertyDecs propData
      phoneInv = ppsPhonemeInventory pps
  phonData <- producePhonemeInventory propData phoneInv
  let phonDecs = snd phonData
      phonDats = fst phonData
  return ((propData, phonDats), (propDecs <> phonDecs))



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


-- :m + Metamorth.Interpretation.Phonemes.Parsing Metamorth.Interpretation.Phonemes.Parsing.Types Metamorth.Interpretation.Phonemes.TH Language.Haskell.TH Data.Either Language.Haskell.TH.Ppr
-- import Data.Text.IO qualified as TIO
-- join $ runQ <$> producePropertyData <$> fromRight defaultPhonemeStructure <$> execPhonemeParser parsePhonemeFile <$> TIO.readFile "local/example1.thy"

--  fmap ppr $ fmap propertyDecs $ join $ runQ <$> producePropertyData <$> fromRight defaultPhonemeStructure <$> execPhonemeParser parsePhonemeFile <$> TIO.readFile "local/example1.thy"
-- fmap ppr $ join $ runQ <$> produceVariousDecs <$> fromRight defaultPhonemeStructure <$> execPhonemeParser parsePhonemeFile <$> TIO.readFile "local/example1.thy"

data GroupProps = GroupProps
  { groupStringName :: String
  -- all the lower-down "is<Subsubgroup>" functions.
  , isGroupSubFuncs :: M.Map String Name
  , groupType :: Type
  -- , isGroupFuncName :: Name
  -- , isGroupFuncType :: Type
  } deriving (Show, Eq)

-- | To make phoneme groups work.
data PhonemeHierarchy
   = PhoneLeaf Name [Type]
   | PhoneNode Name GroupProps (M.Map String PhonemeHierarchy)
   deriving (Show, Eq)

hierarchyName :: PhonemeHierarchy -> Name
hierarchyName (PhoneLeaf nm _) = nm
hierarchyName (PhoneNode nm _ _) = nm

makeLeaves :: (Ord k) => M.Map k (Name, [Type]) -> M.Map k PhonemeHierarchy
makeLeaves = M.map $ \(nm, typs) -> PhoneLeaf nm typs

makeNodes :: (Ord k) => M.Map k (Name, GroupProps, M.Map String (PhonemeHierarchy)) -> M.Map k PhonemeHierarchy
makeNodes  = M.map $ \(nm, grpProps, nods) -> PhoneNode nm grpProps nods

producePhonemeInventory :: PropertyData -> PhonemeInventory -> Q (M.Map String PhonemeHierarchy, {-GroupProps,-} [Dec])
producePhonemeInventory propData phi = do
  mainName <- newName "Phoneme"
  (phoneHi, _groupProps, decs) <- producePhonemeInventory' mainName propData phi
  return (phoneHi, decs)

producePhonemeInventory' :: Name -> PropertyData -> PhonemeInventory -> Q (M.Map String PhonemeHierarchy, Maybe GroupProps, [Dec])
producePhonemeInventory' nm propData (PhonemeSet mp) = do
  (mpX, decs) <- producePhonemeSet propData nm mp
  return (makeLeaves mpX, Nothing, decs)
producePhonemeInventory' nm propData (PhonemeGroup mp) = do
  -- rslts :: M.Map String (Name, ((M.Map String PhonemeHierarchy), [Dec]) )
  rslts <- forWithKey mp $ \str phi -> do
    subName <- newName $ dataName str
    (mpX, gProps, decs) <- producePhonemeInventory' subName propData phi
    return (subName, (mpX, gProps, decs))
  
  let subDecs  = forMap rslts $ \(_,(_,_,dcls)) -> dcls
      subDecs' = concat $ M.elems subDecs
      subNoms  = M.map fst rslts
      -- I'm so confused...
      -- subRslts = makeNodes $ forMap rslts $ \(nom,(mpZ,gProps,_)) -> (nom,gProps,mpZ)
      subGrps :: M.Map String (M.Map String GroupProps)
      subGrps  = forMap rslts $ \(_,(theMap,_,_decs)) -> forMaybeMapWithKey theMap $ \str val -> case val of
        (PhoneLeaf _ _) -> Nothing
        (PhoneNode _grpNom grpProps _grpMap) -> Just grpProps

  
  -- The value to be passed to `sumAdtDecDeriv`
  -- i.e. M.Map String (Name,[Type])
  subPats <- forWithKey subNoms $ \str nom -> do
    -- Adding "Ph" to distinguish it a bit...
    sumName <- newName $ "Ph" <> (dataName str)
    return (sumName, [ConT nom])

{-
data GroupProps = GroupProps
  { groupStringName :: String
  -- all the lower-down "is<Subsubgroup>" functions.
  , isGroupSubFuncs :: M.Map String Name
  , groupType :: Type

  , isGroupFuncName :: Name
  , isGroupFuncType :: Type
  } deriving (Show, Eq)
-}


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
    
-- 


  -- Also todo: add the (Phoneme -> PhonemeProps) function
  -- for this level of the code. 
  
  -- Also todo: add "is<Group>" functions for each level.

  let _help :: M.Map String (GroupProps, [Dec])
      _help = subFuncs
      subDecls = concatMap snd $ M.elems subFuncs
      subGrpProps = M.map fst subFuncs
      

      

  -- Create a data type for this:
  -- (Also: Maybe make a custom `Show` instance
  --  that just shows the underlying value.)
  let newDecs  = sumAdtDecDeriv nm (M.elems subPats) [(ConT ''Eq), (ConT ''Ord), (ConT ''Show)]
      subRslts = makeNodes $ forIntersectionWithKey rslts subGrpProps $ \key (nom,(mpZ,_gProps,_)) grpPropers -> (nom,grpPropers,mpZ)
  
  -- Temporary combining:

  return ((subRslts), Nothing, [newDecs] <> subDecls <> subDecs')


producePhonemeSet :: PropertyData -> Name -> (M.Map String PhonemeProperties) -> Q (M.Map String (Name, [Type]), [Dec])
producePhonemeSet propData subName phoneSet = do
  -- The name to map from phonemes to traits
  funcName <- newName ((nameBase subName) <> "_traits")
  
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

  return (phoneMap, phoneDecs : phoneShowDecs)

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


