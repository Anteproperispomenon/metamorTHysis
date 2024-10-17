{-# LANGUAGE PatternSynonyms #-}

-- Since this value will be used by the CLI
-- program, we can't define it directly. However,
-- we can use Pattern Synonyms...

module Metamorth.ForOutput.Quasi.Types
  ( ExtraLanguageDetails 
  , pattern ExtraLanguageDetails
  , eldLanguageName
  , eldOrthDescs
  ) where

import Data.Map.Strict qualified as M

type ExtraLanguageDetails = (Maybe String, M.Map String String) -- , Bool)
 
-- | Details about the language that
--   don't fit nicely into one piece
--   or another.
pattern ExtraLanguageDetails :: Maybe String -> M.Map String String -> ExtraLanguageDetails
pattern ExtraLanguageDetails { eldLanguageName, eldOrthDescs } = (eldLanguageName, eldOrthDescs)

-- pattern ExtraLanguageDetails :: Maybe String -> M.Map String String -> Bool -> ExtraLanguageDetails
-- pattern ExtraLanguageDetails { eldLanguageName, eldOrthDescs, eldIsCased } = (eldLanguageName, eldOrthDescs, eldIsCased)



