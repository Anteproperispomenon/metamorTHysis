module Metamorth.Interpretation.Output.Types
  ( OutputCase(..)
  , CaseSource(..)
  , CaseApply(..)
  ) where

data OutputPattern = OutputPattern
  { opCasedness :: OutputCase
  } deriving (Show, Eq)

-- | The case of an output pattern.
--   **NOTE**: this does NOT mean that
--   the pattern produced will be a
--   certain case, but rather that the
--   matched pattern only accepts upper-case
--   etc... patterns.
data OutputCase
  -- | Upper-Case Pattern only.
  = OCMaj
  -- | Lower-Case Pattern only.
  | OCMin
  -- | Uncasable Pattern. In this case, both cases
  --   will use the same value.
  | OCNull
  -- | Automatically create cases for this pattern.
  --   The parameters tell you how to transform
  --   the input case to the output case. 
  | OCDetect CaseSource CaseApply
  | OCDetectIndividual
  deriving (Show, Eq)

-- | Which phoneme of the input list to
--   use to determine the case of the
--   output.
data CaseSource
   -- | Use the case of the first casable character.
   = CSFirst
   -- | Use the case of the last casable character
   | CSLast
   -- | Use lower-case, unless all characters are
   --   upper-case
   | CSLow
   -- | Use upper-case, unless all characters are
   --   lower-case.
   | CSHigh
   deriving (Show, Eq)

-- | Which output character(s) to apply the
--   case to.
data CaseApply
   -- | Use Title case if the input is capitalised.
   = CATitle
   -- | Apply case to all characters.
   | CAAll
   -- | 
   deriving (Show, Eq)

