module Metamorth.Interpretation.Output.TH.Constructors
  ( makePhoneConstructorPat
  ) where

-- help with Generating constructors from strings

import Metamorth.Interpretation.Output.Types

import Language.Haskell.TH

import Data.Map.Strict qualified as M

import Metamorth.Helpers.Either 

-- | Generate the pattern for pattern-matching
--   on a phoneme.
makePhoneConstructorPat :: M.Map String Name -> M.Map String [M.Map String Name] -> PhoneName -> Either [String] Pat
makePhoneConstructorPat nomMap aspMap phoneNom = do
  let phNom = pnName phoneNom
      phAsp = pnCons phoneNom
  conName <- eitherMaybe' (M.lookup phNom nomMap) ["Can't find phoneme name for \"" ++ phNom ++ "\"."]
  aspList <- eitherMaybe' (M.lookup phNom aspMap) ["Can't find phoneme constructor list for \"" ++ phNom ++ "\"."]
  subPats <- traverseAllEither (uncurry lkupAsp) (zip phAsp aspList)
  return $ ConP conName [] subPats
  where
    -- Check for wild cards.
    lkupAsp :: String -> (M.Map String Name) -> Either String Pat
    lkupAsp "*" _  = Right WildP
    lkupAsp "_" _  = Right WildP
    lkupAsp str mp = do
        rsltNom <- eitherMaybe' (M.lookup str mp) $ "Couldn't find aspect constructor: \"" ++ str ++ "\"."
        return $ ConP rsltNom [] []
