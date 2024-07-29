{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Metamorth.Interpretation.Output.Parsing.Types
  ( OutputParser
  , embedOutputParser
  , OutputParsingState(..)
  , runOnPhoneme
  , CharPatternRaw(..)
  , validateCharPattern
  , ImportProperty(..)
  , PhoneName(..)
  , PhonePatternRaw(..)
  , validatePhonePattern
  , showPPR
  , showPPRs
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS

import Data.Attoparsec.Text qualified as AT

import Data.Trie.Map qualified as TM

import Data.Char (ord, chr, isSpace, isLower, isAlpha)
import Data.List (intercalate)
import Data.Text qualified as T
import Data.Text (Text)

import Data.Maybe

import Data.Map.Strict qualified as M

import Data.Set qualified as S

import Metamorth.Helpers.Error
import Metamorth.Helpers.Error.RWS
import Metamorth.Helpers.Maybe
import Metamorth.Helpers.Char

import Metamorth.Interpretation.Output.Types
import Metamorth.Interpretation.Output.Types.Alt

import Metamorth.Interpretation.Shared.Types (ImportProperty(..))

--------------------------------
-- Types for Parsing

-- | A `AT.Parser` wrapped in an `RWST`.
-- 
--   * The reader holds the name of the phoneme(s) being processed.
--   * The writer is used to collect error messages
--   * The state will hold stateful info.
type OutputParser a = RWST String [ParserMessage] OutputParsingState AT.Parser a

embedOutputParser 
  :: S.Set String 
  -> M.Map String (Maybe (S.Set String)) 
  -> M.Map String (S.Set String)
  -> S.Set String
  -> OutputParser a 
  -> AT.Parser (a, OutputParsingState, [ParserMessage])
embedOutputParser grps trts asps phons prs 
  = runRWST prs "N/A" $ OutputParsingState
      { opsStateDictionary   = M.empty
      , opsGroupDictionary   = S.empty
      , opsGroupDictionary'  = grps
      , opsTraitDictionary   = M.empty
      , opsTraitDictionary'  = trts
      , opsAspectDictionary  = M.empty
      , opsAspectDictionary' = asps
      , opsPhoneDictionary   = phons
      , opsDefaultCasing     = OCNull
      , opsOutputTrie        = TM.empty
      }

-- | The State type for `OutputParser`.
data OutputParsingState = OutputParsingState
  -- | The state dictionary. This is updated as
  --   the parser parses the state declarations.
  { opsStateDictionary :: M.Map String (Bool, Maybe (S.Set String))
  -- | The Group "Dictionary". This is supplied by
  --   the phoneme parser when the output files
  --   are run.
  , opsGroupDictionary  :: S.Set String
  -- | The full group dictionary supplied by
  --   the phoneme code.
  , opsGroupDictionary' :: S.Set String
  -- | The Trait Dictionary. This is supplied by
  --   the phoneme parser when the output files
  --   are run. The `S.Set` contains the possible
  --   values for the trait, if relevant.
  , opsTraitDictionary  :: M.Map String (Maybe (S.Set String))
  -- | The full trait dictionary supplied by the driver.
  , opsTraitDictionary' :: M.Map String (Maybe (S.Set String))
  -- | The Aspect Dictionary. This is supplied by
  --   the phoneme parser when the output files
  --   are run. The `S.Set` contains the possible
  --   values for the trait.
  , opsAspectDictionary  :: M.Map String (S.Set String)
  , opsAspectDictionary' :: M.Map String (S.Set String)
  -- | The phoneme dictionary supplied by the
  --   phoneme data. Doesn't contain the names
  --   for each phoneme, since they aren't needed
  --   by the parser.
  , opsPhoneDictionary :: S.Set String
  , opsDefaultCasing   :: OutputCase
  -- | The main trie to be used for determining
  --   output.
  , opsOutputTrie      :: TM.TMap PhonePatternAlt (S.Set PhoneResult)
  -- Old version
  -- , opsOutputTrie      :: TM.TMap PhonePattern (M.Map OutputCase OutputPattern)
  } deriving (Show, Eq)

-- | Use this when running a function that might
--   error with a phoneme name. e.g.
--   
--   > parsePhonePat = do
--   >   phoneName <- getPhoneName
--   >   rslt <- runOnPhoneme phoneName $ checkForStuff
runOnPhoneme :: String -> OutputParser a -> OutputParser a
runOnPhoneme str = local (const str)

-- | Data type for declarations of the form
--   `import (aspect | group | trait) prop_name`.
-- Moved to "Metamorth.Interpretation.Shared.Types"
-- data ImportProperty
--   = ImportAspect String
--   | ImportGroup  String
--   | ImportTrait  String
--   deriving (Show, Eq, Ord)

-- | The parsed values on the left-hand side
--   of a phoneme pattern.
data PhonePatternRaw
  = PhoneNameR PhoneName
  | PhoneAtStartR
  | PhoneNotStartR
  | PhoneAtEndR
  | PhoneNotEndR
  | PhoneFollowedByGroupR String
  | PhoneFollowedByTraitR String
  | PhoneFollowedByTraitAtR String String
  | PhoneFollowedByAspectR String
  | PhoneFollowedByAspectAtR String String
  | PhoneFollowedByPhoneR String
  | PhoneValStateR String (Either String Bool)
  deriving (Show, Eq)

pattern PhoneFollowR :: PhonePatternRaw
pattern PhoneFollowR <- (convertFollow -> (Just _))

-- | Fast way to match a phoneme patterns that
--   have to go at the start of an expression.
pattern PhoneStartR :: PhonePatternRaw
pattern PhoneStartR <- ((\x -> x == PhoneAtStartR || x == PhoneNotStartR) -> True)

-- | Fast way to match a phoneme patterns that
--   have to go at the end of an expression.
pattern PhoneEndR :: PhonePatternRaw
pattern PhoneEndR <- ((\x -> x == PhoneAtEndR || x == PhoneNotEndR) -> True)

convertFollow :: PhonePatternRaw -> Maybe PhoneFollow
convertFollow (PhoneFollowedByGroupR str)          = Just $ PhoneFollowedByGroup str
convertFollow (PhoneFollowedByTraitR str)          = Just $ PhoneFollowedByTrait str
convertFollow (PhoneFollowedByTraitAtR str1 str2)  = Just $ PhoneFollowedByTraitAt str1 str2
convertFollow (PhoneFollowedByAspectR str)         = Just $ PhoneFollowedByAspect str
convertFollow (PhoneFollowedByAspectAtR str1 str2) = Just $ PhoneFollowedByAspectAt str1 str2
convertFollow (PhoneFollowedByPhoneR str)          = Just $ PhoneFollowedByPhone str
convertFollow _ = Nothing 

convertState :: PhonePatternRaw -> Maybe CheckState
convertState (PhoneValStateR str (Left val)) = Just $ CheckStateV str val
convertState (PhoneValStateR str (Right bl)) = Just $ CheckStateB str bl
convertState _ = Nothing

-- | Convert a raw phoneme pattern list into a list of `PhonePattern`s.
validatePhonePattern :: M.Map String (Maybe (S.Set String)) -> [PhonePatternRaw] -> Either String [PhonePattern]
validatePhonePattern mps praws = validatePhonePatternE mps pfs pss prest2
  where
    (pfs, prest1) = partitionMaybe convertFollow praws
    (pss, prest2) = partitionMaybe convertState  prest1

-- Running on the first element of a pattern...
validatePhonePatternE :: M.Map String (Maybe (S.Set String)) -> [PhoneFollow] -> [CheckState] -> [PhonePatternRaw] -> Either String [PhonePattern]
validatePhonePatternE _ _ _ [] = Left "Can't have an empty phoneme pattern."
validatePhonePatternE _ _ _ [PhoneStartR] = Left "Can't have a pattern that's just a start."
validatePhonePatternE _ _ _ (PhoneEndR:_) = Left "Can't start a pattern with an end."
validatePhonePatternE _ _ _ [PhoneStartR, PhoneEndR] = Left "Can't have a pattern that's just a start and an end."
validatePhonePatternE mp pfs pps (PhoneAtStartR  :xs) = (PhoneAtStart :) <$> validatePhonePatternE' mp pfs pps xs
validatePhonePatternE mp pfs pps (PhoneNotStartR :xs) = (PhoneNotStart:) <$> validatePhonePatternE' mp pfs pps xs
validatePhonePatternE mp pfs pps ((PhoneNameR pn):xs) = do
  -- Errors on the first failing name
  pps' <- traverse (validateCheckState mp) pps
  ((PhonemeName pps' pn):) <$> validatePhonePatternEX pfs xs
validatePhonePatternE _ _ _ (PhoneFollowR:_) = Left "Got a `PhoneFollow` (1) even though it should already have been filtered out."
validatePhonePatternE _ _ _ (x:_) = Left $ "Got an unexpected PhonePat (1) : \"" ++ show x ++ "\"."


-- Run after encountering a non-PlainCharR element...
validatePhonePatternE' :: M.Map String (Maybe (S.Set String)) -> [PhoneFollow] -> [CheckState] -> [PhonePatternRaw] -> Either String [PhonePattern]
validatePhonePatternE' _ [] [] [] = Left "Can't have an empty phoneme pattern."
validatePhonePatternE' _ _  _  [] = Left "Can't have an empty phoneme pattern."
validatePhonePatternE' _ _ _ (PhoneStartR:_) = Left "Can't have a start pattern in the middle of a pattern."
validatePhonePatternE' _ _ _ (PhoneEndR:_)   = Left "Can't have an end pattern in the middle of a pattern."
validatePhonePatternE' mp pfs pps ((PhoneNameR pn):xs) = do
  -- Errors on the first failing name
  pps' <- traverse (validateCheckState mp) pps
  ((PhonemeName pps' pn):) <$> validatePhonePatternEX pfs xs
validatePhonePatternE' _ _ _ (PhoneFollowR:_) = Left "Got a `PhoneFollow` (2) even though it should already have been filtered out."
validatePhonePatternE' _ _ _ (x:_) = Left $ "Got an unexpected PhonePat (2) : \"" ++ show x ++ "\"."

-- Run once a PlainCharR element has been encountered.
validatePhonePatternEX :: [PhoneFollow] -> [PhonePatternRaw] -> Either String [PhonePattern]
validatePhonePatternEX []  [] = Right []
validatePhonePatternEX pfs [] = Right [PhoneFollow pfs]
validatePhonePatternEX pfs ((PhoneNameR pn):xs) = ((PhonemeName [] pn):) <$> validatePhonePatternEX pfs xs
validatePhonePatternEX _ (PhoneStartR:_) = Left "Can't have a start pattern in the middle of a pattern."
validatePhonePatternEX [] [PhoneAtEndR]  = Right [PhoneAtEnd]
validatePhonePatternEX [] [PhoneNotEndR] = Right [PhoneNotEnd]
validatePhonePatternEX _f (PhoneEndR:_) = Left $ "Can't have both follow-up patterns and end-patterns."
validatePhonePatternEX _ (PhoneFollowR:_) = Left "Got a `PhoneFollow` (3) even though it should already have been filtered out."
validatePhonePatternEX _ (x:_) = Left $ "Got an unexpected PhonePat (3) : \"" ++ show x ++ "\"."

-- | Show a `PhonePatternRaw` in a shortened form.
showPPR :: PhonePatternRaw -> String
showPPR (PhoneNameR (PhoneName pn pc))
  | null pc   = pn
  | otherwise = pn ++ "[" ++ (intercalate "," pc) ++ "]"
showPPR PhoneAtStartR  = "^"
showPPR PhoneNotStartR = "%"
showPPR PhoneAtEndR    = "$"
showPPR PhoneNotEndR   = "&"
showPPR (PhoneFollowedByGroupR  str) = "G>" ++ str
showPPR (PhoneFollowedByTraitR  str) = "T>" ++ str
showPPR (PhoneFollowedByAspectR str) = "A>" ++ str
showPPR (PhoneFollowedByTraitAtR  str str') = "T>" ++ str ++ "=" ++ str'
showPPR (PhoneFollowedByAspectAtR str str') = "A>" ++ str ++ "=" ++ str'
showPPR (PhoneFollowedByPhoneR str) = "P>" ++ str
showPPR (PhoneValStateR st (Left val)) = "@" ++ st ++ "=" ++ val
showPPR (PhoneValStateR st (Right True )) = "@" ++ st ++ "=" ++ "on"
showPPR (PhoneValStateR st (Right False)) = "@" ++ st ++ "=" ++ "off"

showPPRs :: [PhonePatternRaw] -> String
showPPRs [] = ""
showPPRs xs = unwords (map showPPR xs)
-- unwords == (intercalate " ")

-- | The Raw data for a single "Character"
--   on the RHS of a pattern.
data CharPatternRaw
  = PlainCharR Char   -- ^ A single `Char`.
  -- | CharClassR String -- ^ Any member of a class from the header.
  -- | WordStartR        -- ^ The start of a word.
  -- | WordEndR          -- ^ The end of a word.
  -- | NotStartR         -- ^ NOT the start of a word.
  -- | NotEndR           -- ^ NOT the end of a word.
  -- | ValStateR String (Either String Bool) -- ^ Check that the state is a certain value.
  | SetStateR String (Either String Bool) -- ^ Set the state to a certain value.
  deriving (Show, Eq, Ord)

getPlainChar :: CharPatternRaw -> Maybe CharPatternItem
getPlainChar (PlainCharR c)
  | isCasable c = Just $ CasableChar   c
  | otherwise   = Just $ UncasableChar c
getPlainChar _ = Nothing

getPlainState :: CharPatternRaw -> Maybe ModifyState
getPlainState (SetStateR str (Left val)) = Just $ ModifyStateV str val
getPlainState (SetStateR str (Right bl)) = Just $ ModifyStateB str bl
getPlainState _ = Nothing

validateCharPattern :: M.Map String (Maybe (S.Set String)) -> [CharPatternRaw] -> Either String CharPattern
validateCharPattern mp cps = do
  let (sts, rst) = partitionMaybe getPlainState cps
      itms       = mapMaybe getPlainChar rst
  sts' <- traverse (validateModifyState mp) sts
  return $ CharPattern itms sts'


