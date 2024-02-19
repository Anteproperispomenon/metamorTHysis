
module Metamorth.Interpretation.Phonemes.Types
  -- * Types
  ( PhonemeParsingStructure(..)
  , defaultPhonemeStructure
  , PhonemeInventory(..)
  , PhonemeProperties(..)
  , PhonemePropertiesRaw(..)
  -- , Property(..)
  , emptyPhonemeProps
  -- * State Modifiers
  , addAspect
  , addTrait
  -- * Helpers
  , validateProperties
  , validateRawProperty
  , validateRawProperties
  , lookupPhone
  , findProperty
  ) where

import Control.Applicative ((<|>), asum)

import Data.Either
import Data.List (find, null, partition)

import Data.Map.Strict qualified as M
import Data.Set        qualified as S



data PropertyOption
  = AspectOption String String
  | TraitOption  String (Maybe String)
  deriving (Show, Eq)

isAspectOption :: PropertyOption -> Bool
isAspectOption (AspectOption _ _) = True
isAspectOption _ = False

groupProperties :: [PropertyOption] -> PhonemeProperties
groupProperties props
  | (xs,ys) <- partition isAspectOption props
  = PhonemeProperties
      (map (\(AspectOption x y) -> (x,y)) xs)
      (map (\(TraitOption  x y) -> (x,y)) ys)

data PhonemeParsingStructure
   = PhonemeParsingStructure
      { ppsPhonemeInventory :: PhonemeInventory
      , ppsPhonemeAspects   :: M.Map String [String]
      , ppsPhonemeTraits    :: M.Map String [String]
      } deriving (Show, Eq)

defaultPhonemeStructure :: PhonemeParsingStructure
defaultPhonemeStructure
  = PhonemeParsingStructure
      (PhonemeSet M.empty)
      M.empty
      M.empty

-- | The set of phonemes, sorted into groups, along with
--   their properties.
data PhonemeInventory
  = PhonemeSet   (M.Map String PhonemeProperties) -- ^ The set of phonemes in this group/sub-group/etc...
  | PhonemeGroup (M.Map String PhonemeInventory ) -- ^ A set of groups (or sub-groups if already in a group).
  deriving (Show, Eq)

lookupPhone :: PhonemeInventory -> String -> Maybe PhonemeProperties
lookupPhone (PhonemeSet   mp) phone = M.lookup phone mp
lookupPhone (PhonemeGroup ps) phone = asum $ M.map (`lookupPhone` phone) ps

data PhonemeProperties
   = PhonemeProperties 
       { phAspects :: [(String, String)]
       , phTraits  :: [(String, Maybe String)]
       } deriving (Show, Eq)

emptyPhonemeProps :: PhonemeProperties
emptyPhonemeProps = PhonemeProperties { phAspects = [], phTraits = [] }

newtype PhonemePropertiesRaw
   = PhonemePropertiesRaw [(String, Maybe String)]
   deriving (Show, Eq)

-- | Validate that the properties of a specific phoneme
--   are valid with the property set of these phonemes.
validateProperties :: PhonemeParsingStructure -> String -> PhonemeProperties -> Either [String] ()
validateProperties pps phoneName props
  | aspctErrs <- lefts $ map (\pr -> validateAspect aspects phoneName pr) (phAspects props)
  , traitErrs <- lefts $ map (\pr -> validateTrait  traits  phoneName pr) (phTraits  props)
  , errs <- aspctErrs <> traitErrs
  = if (null errs) then (Right ()) else (Left errs)
  where
    aspects = ppsPhonemeAspects pps
    traits  = ppsPhonemeAspects pps

validateAspect :: M.Map String [String] -> String -> (String,String) -> Either String ()
validateAspect mp nom (aspct, val)
  | (Just options) <- M.lookup aspct mp
  =  if (val `elem` options)
       then (Right ())
       else (Left ("Phoneme \'" <> nom <> "\' has aspect \'" <> aspct <> "\' with unknown value \'" <> val <> "\'."))
  | otherwise = Left (("Phoneme \'" <> nom <> "\' has unknown aspect \'" <> aspct <> "\'."))

validateTrait :: M.Map String [String] -> String -> (String, Maybe String) -> Either String ()
validateTrait mp nom (trt, valx)
  | (Just options) <- M.lookup trt mp
  = case valx of
      Nothing -> if (null options) 
        then (Right ())
        else (Left $ "Phoneme \'" <> nom <> "\' has no specified option for trait \'" <> trt <> "\', which requires that an option be given.")
      (Just val) -> if (null options)
        then (Left $ "Phoneme \'" <> nom <> "\' specified an option for trait \'" <> trt <> "\' even though that trait does not take options.")
        else if (val `elem` options)
            then (Right ())
            else (Left  ("Phoneme \'" <> nom <> "\' has trait \'" <> trt <> "\' with unknown option \'" <> val <> "\'."))
  | otherwise = Left (("Phoneme \'" <> nom <> "\' has unknown trait \'" <> trt <> "\'."))

-- | Left  : aspect
--   Right : trait
findProperty :: PhonemeParsingStructure -> String -> Maybe (Either [String] [String])
findProperty pps prop 
  = (Left      <$> (M.lookup prop $ ppsPhonemeAspects pps))
    <|> (Right <$> (M.lookup prop $ ppsPhonemeTraits  pps))

validateRawProperty :: PhonemeParsingStructure -> String -> (String, Maybe String) -> Either String PropertyOption
validateRawProperty pps phoneName (prop, valx)
  -- options shouldn't be empty if validated earlier on.
  | (Just options) <- M.lookup prop (ppsPhonemeAspects pps)
  = case valx of
      Nothing -> (Left $ "Phoneme \'" <> phoneName <> "\' has no specified option for aspect \'" <> prop <> "\', which requires that an option be given.")
      (Just val) -> if (val `elem` options)
        then Right (AspectOption prop val)
        else Left  ("Phoneme \'" <> phoneName <> "\' has aspect \'" <> prop <> "\' with unknown value \'" <> val <> "\'.")
  | (Just options) <- M.lookup prop (ppsPhonemeAspects pps)
  = case valx of
      Nothing -> if (null options)
        then (Right (TraitOption prop Nothing))
        else (Left $ "Phoneme \'" <> phoneName <> "\' has no specified option for trait \'" <> prop <> "\', which requires that an option be given.")
      (Just val) -> if (null options)
        then (Left $ "Phoneme \'" <> phoneName <> "\' specified an option for trait \'" <> prop <> "\' even though that trait does not take options.")
        else if (val `elem` options)
            then (Right (TraitOption prop valx))
            else (Left  ("Phoneme \'" <> phoneName <> "\' has trait \'" <> prop <> "\' with unknown option \'" <> val <> "\'."))
  
  | otherwise = Left (("Phoneme \'" <> phoneName <> "\' has unknown property \'" <> prop <> "\'."))

validateRawProperties :: PhonemeParsingStructure -> String -> PhonemePropertiesRaw -> Either [String] PhonemeProperties
validateRawProperties pps phoneName (PhonemePropertiesRaw props)
  | null errs = Right (groupProperties rslts)
  | otherwise = Left errs
  where
    eiths = map (validateRawProperty pps phoneName) props
    errs  = lefts eiths
    rslts = rights eiths


--------------------------------
-- Updating the State

-- | Add an aspect to the phoneme state, giving an
--   error if there is already an aspect or trait
--   of the same name.
addAspect :: String -> [String] -> PhonemeParsingStructure -> Either String PhonemeParsingStructure
addAspect prop options pps
  | (Just x) <- findProperty pps prop
  = case x of
      (Left  _) -> Left $ "The aspect \'" <> prop <> "\' is declared more than once."
      (Right _) -> Left $ "Can't declare aspect \'" <> prop <> "\'; there is already a trait with that name." 
  | otherwise = Right $ pps {ppsPhonemeAspects = aspectMap'}
    where aspectMap' = M.insert prop options (ppsPhonemeAspects pps)

-- | Add an aspect to the phoneme state, giving an
--   error if there is already a trait or aspect
--   of the same name.
addTrait :: String -> [String] -> PhonemeParsingStructure -> Either String PhonemeParsingStructure
addTrait prop options pps
  | (Just x) <- findProperty pps prop
  = case x of
      (Left  _) -> Left $ "Can't declare trait \'" <> prop <> "\'; there is already an aspect with that name." 
      (Right _) -> Left $ "The trait \'" <> prop <> "\' is declared more than once."
  | otherwise = Right $ pps {ppsPhonemeTraits = traitMap'}
    where traitMap' = M.insert prop options (ppsPhonemeTraits pps)
