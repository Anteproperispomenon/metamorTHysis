
module Metamorth.Interpretation.Phonemes.Types
  ( PhonemeParsingState(..)
  , PhonemeInventory(..)
  , validateProperties
  , lookupPhone
  , findProperty
  ) where

import Control.Applicative ((<|>), asum)

import Data.Either
import Data.List (find, null)

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

data PhonemeParsingState
   = PhonemeParsingState
      { ppsPhonemeInventory :: PhonemeInventory
      , ppsPhonemeAspects   :: M.Map String [String]
      , ppsPhonemeTraits    :: M.Map String [String]

      } deriving (Show, Eq)

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

-- | Validate that the properties of a specific phoneme
--   are valid with the property set of these phonemes.
validateProperties :: PhonemeParsingState -> String -> PhonemeProperties -> Either [String] ()
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
findProperty :: PhonemeParsingState -> String -> Maybe (Either [String] [String])
findProperty pps prop 
  = (Left      <$> (M.lookup prop $ ppsPhonemeAspects pps))
    <|> (Right <$> (M.lookup prop $ ppsPhonemeTraits  pps))


