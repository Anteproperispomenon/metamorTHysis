{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Interpretation.Phonemes.TH
  ( producePropertyData

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
  { aspectTable  :: M.Map String (Name, M.Map String Name)
  , traitTable   :: M.Map String (Name, Maybe (Name, M.Map String Name))
  , traitData    :: Maybe TraitData
  , propertyDecs :: [Dec]
  } deriving (Show, Eq)

data TraitData = TraitData
  { traitInfoName  :: Name
  , traitTypeTable :: M.Map String (Name, Type)
  , traitDefName   :: Name
  } deriving (Show, Eq)

producePropertyData :: PhonemeParsingStructure -> Q PropertyData
producePropertyData pps = do
  (aspTab, trtTab, mtData, theDecs) <- producePropertyData' pps
  let trtData = forMap mtData $ \(nm, typs, defName) ->
        (TraitData nm typs defName)
  return $ PropertyData aspTab trtTab trtData theDecs


producePropertyData' :: PhonemeParsingStructure -> Q (M.Map String (Name, M.Map String Name), M.Map String (Name, Maybe (Name, M.Map String Name)), Maybe (Name, M.Map String (Name, Type), Name), [Dec])
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
  -- aspMap :: Map String ((Name, Map String Name), [Dec])
  aspMap <- forWithKey aspects $ \asp ops -> do
    aspName <- newName (dataName asp)
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

    return ((aspName, opNames), (aspDec : aspShw))
  
  let aspMap1 = fmap fst aspMap -- :: Map String (Name, Map String Name)
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
  traitRecTypeName <- newName "PhonemeTraits"
  let traitRecTypes = forMap trtMap $ \((trtRecName, mTrtInfo), _) -> case mTrtInfo of
        Nothing -> (trtRecName, ConT ''Bool)
        (Just (typeNom,_)) -> (trtRecName, maybeType $ ConT typeNom)

  -- To be used later on...
  defRecordName <- newName "defaultPhonemeTraits"

  -- Return the Type name of the Trait record type, along
  -- with the names of the fields.
  let trtRecOutput = if (M.null traitRecTypes)
        then Nothing
        else Just (traitRecTypeName, traitRecTypes, defRecordName)

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


-- :m + Metamorth.Interpretation.Phonemes.Parsing Metamorth.Interpretation.Phonemes.Parsing.Types Metamorth.Interpretation.Phonemes.TH Language.Haskell.TH Data.Either
-- import Data.Text.IO qualified as TIO
-- join $ runQ <$> producePropertyData <$> fromRight defaultPhonemeStructure <$> execPhonemeParser parsePhonemeFile <$> TIO.readFile "local/example1.thy"



producePhonemeInventory :: M.Map String Name -> M.Map String Name -> PhonemeInventory -> Q ()
producePhonemeInventory asps traits phi = do
  return ()


producePhonemeSet :: PropertyData -> Name -> (M.Map String PhonemeProperties) -> Q ()
producePhonemeSet propData subName phoneSet = do
  -- The name to map from phonemes to traits
  funcName <- newName ((nameBase subName) <> "_traits")
  

  phoneMap <- forWithKey phoneSet $ \phone props -> do
    phoneName <- newName $ dataName phone

    return (phoneName) -- temp

  return ()

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


