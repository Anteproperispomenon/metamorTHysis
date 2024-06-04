module Metamorth.Helpers.String 
  ( toLowerT
  ) where

import Data.Text qualified as T

-- | Run `T.toLower` on a `String`
--   by `T.pack`ing it and `T.unpack`ing it.
toLowerT :: String -> String
toLowerT = T.unpack . T.toLower . T.pack

