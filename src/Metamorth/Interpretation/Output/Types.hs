module Metamorth.Interpretation.Output.Types
  ( OutputCase(..)
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
  --   If the first casable value is upper-case, then
  --   the 
  | OCDetectTitle
  -- | Automatically create cases for this pattern.
  --   Use the case value of the last phoneme.
  | OCDetectFirst
  -- | Automatically create cases for this pattern.
  --   Use the case value of the last phoneme.
  | OCDetectLast
  -- | Automatically create cases for this pattern.
  --   Go for upper-case unless there aren't any
  --   upper-case phonemes.
  | OCDetectHigh
  -- | Automatically create cases for this pattern.
  --   Go for lower-case unless there aren't any
  --   lower-case phonemes.
  | OCDetectLow
  -- | Automatically detect each phoneme individually.
  --   Only works when each phoneme has a corresponding
  --   (list of) character(s).
  | OCDetectIndividual
  deriving (Show, Eq)
