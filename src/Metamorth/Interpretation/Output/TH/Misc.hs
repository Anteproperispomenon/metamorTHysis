module Metamorth.Interpretation.Output.TH.Misc
  -- * Lookup a value from a `M.Map` in `Q`.
  -- ** Using `Show`-able values
  ( qLookupWarn
  , qLookupError
  , qLookupFail
  -- ** Using `String`s
  , qLookupWarnS
  , qLookupErrorS
  , qLookupFailS
  ) where

import Control.Monad

import Data.Maybe

import Data.Map.Strict qualified as M

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

import Metamorth.Helpers.Q

-- | Try looking up a value, reporting a warning
--   if not present.
qLookupWarn :: (Quasi q, Ord k, Show k) => k -> M.Map k a -> q (Maybe a)
qLookupWarn k mp = do
  let mVal = M.lookup k mp
  when (isNothing mVal) $ qReportWarning $ "Can't find key: \"" ++ show k ++ "\"."
  return mVal

-- | Try looking up a value, reporting a error
--   if not present.
qLookupError :: (Quasi q, Ord k, Show k) => k -> M.Map k a -> q (Maybe a)
qLookupError k mp = do
  let mVal = M.lookup k mp
  when (isNothing mVal) $ qReportError $ "Can't find key: \"" ++ show k ++ "\"."
  return mVal

-- | Try looking up a value, failing if not present.
qLookupFail :: (Quasi q, Ord k, Show k) => k -> M.Map k a -> q a
qLookupFail k mp = do
  let mVal = M.lookup k mp
  case mVal of
    Nothing  -> fail $ "Can't find key: \"" ++ show k ++ "\"."
    (Just v) -> return v

-- | Try looking up a value, reporting a warning
--   if not present.
qLookupWarnS :: (Quasi q) => String -> M.Map String a -> q (Maybe a)
qLookupWarnS k mp = do
  let mVal = M.lookup k mp
  when (isNothing mVal) $ qReportWarning $ "Can't find key: \"" ++ k ++ "\"."
  return mVal

-- | Try looking up a value, reporting a error
--   if not present.
qLookupErrorS :: (Quasi q) => String -> M.Map String a -> q (Maybe a)
qLookupErrorS k mp = do
  let mVal = M.lookup k mp
  when (isNothing mVal) $ qReportError $ "Can't find key: \"" ++ k ++ "\"."
  return mVal

-- | Try looking up a value, failing if not present.
qLookupFailS :: (Quasi q) => String -> M.Map String a -> q a
qLookupFailS k mp = do
  let mVal = M.lookup k mp
  case mVal of
    Nothing  -> fail $ "Can't find key: \"" ++ k ++ "\"."
    (Just v) -> return v

