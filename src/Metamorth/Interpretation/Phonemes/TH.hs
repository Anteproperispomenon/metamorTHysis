{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Interpretation.Phonemes.TH
  ( producePropertyData

  ) where

import Control.Monad

import Data.Functor
import Data.Maybe

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


producePropertyData :: PhonemeParsingStructure -> Q (M.Map String (Name, M.Map String Name), M.Map String (Name, Maybe (Name, M.Map String Name)), [Dec])
producePropertyData pps = do
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
  aspMap <- forWithKey aspects $ \asp ops -> do
    aspName <- newName (dataName asp)
    opNames <- mapM (newName . dataName) $ fromSelfList ops

    let aspDec = sumAdtDecDeriv aspName (map (,[]) $ M.elems opNames) [eqClass, ordClass]
    -- aspShow <- [d| instance Show $(aspName) where show  |]

    return ((aspName, opNames), aspDec)
  
  let aspMap1 = fmap fst aspMap
      aspMap2 = fmap snd aspMap
  
  trtMap <- forWithKey traits $ \trt ops -> do
    trtRecName <- newName trt
    trtTypeNames <- case ops of
      [] -> return Nothing
      xs -> do
        trName  <- newName $ dataName trt
        opNames <- mapM (newName . dataName) $ fromSelfList ops
        return $ Just (trName,opNames)
    
    umOkay <- case trtTypeNames of
      Nothing    -> return $ Nothing
      (Just mps) -> return $ Just $ sumAdtDecDeriv (fst mps) (map (,[]) $ M.elems (snd mps)) [eqClass, ordClass] 
    return ((trtRecName,trtTypeNames), umOkay)
  
  let trtMap1 = fmap fst trtMap
      trtMap2 = fmap snd trtMap
      trtLst1 = catMaybes $ M.elems trtMap2
  



  return (aspMap1, trtMap1, M.elems aspMap2)
  

-- :m + Metamorth.Interpretation.Phonemes.Parsing Metamorth.Interpretation.Phonemes.Parsing.Types Metamorth.Interpretation.Phonemes.TH Language.Haskell.TH Data.Either
-- import Data.Text.IO qualified as TIO
-- join $ runQ <$> producePropertyData <$> fromRight defaultPhonemeStructure <$> execPhonemeParser parsePhonemeFile <$> TIO.readFile "local/example1.thy"



producePhonemeInventory :: M.Map String Name -> M.Map String Name -> PhonemeInventory -> Q ()
producePhonemeInventory asps traits phi = do
  return ()

-- producePhonemeData :: PhonemeParsingStructure -> Q ([Dec], M.Map String Name)
-- producePhonemeData pss = do
  

