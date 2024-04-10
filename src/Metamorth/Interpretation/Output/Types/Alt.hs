{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Metamorth.Interpretation.Output.Types.Alt
  ( PhonePatternAlt(..)
  , PhoneResultActionX(..)
  , PhoneResult
  , PhoneResultX(..)
  ) where

-- Alternate forms of types.

-- import Data.Functor.Compose

import Data.Ord (Down(..))

import Metamorth.Helpers.Ord

import Metamorth.Interpretation.Output.Types

-- | Alternate form of `PhonePatternX` that
--   instead puts state and follow data into
--   the value instead of the key.
data PhonePatternAlt
  = PhonemeNameZ PhoneName
  | PhoneAtStartZ
  | PhoneNotStartZ
  -- | PhoneAtEndZ
  -- | PhoneNotEndZ
  deriving (Show, Eq)    

data PhoneResultActionX
  = PRConfirmState CheckStateX
  | PRModifyState  ModifyStateX
  | PRAtEnd
  | PRNotEnd
  | PRCheckNext [String] -- i.e. must be one of the strings in question.
  deriving (Show, Eq, Ord)

type PhoneResult = PhoneResultX PhoneResultActionX

data PhoneResultX a = PhoneResult
  { prPhoneOutput :: String
  , prPhoneConditions :: a
  } deriving (Show, Eq)

deriving instance {-# OVERLAPPING #-} (Ord b) => Ord (PhoneResultX (Down b))

deriving via (PhoneResultX (Down b)) instance {-# OVERLAPPABLE #-} (Ord b) => Ord (PhoneResultX b)

-- We want this instance to be chosen over plain @b@, but
-- *not* over @Down b@.
-- deriving via (PhoneResultX (Compose Down SizeOrdList b))
deriving via (PhoneResultX (Down (SizeOrdList b))) 
  instance {-# OVERLAPS #-} (Ord b) => Ord (PhoneResultX [b])


