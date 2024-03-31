{-|
Module      : Metamorth.Helpers.Error.RWS
Description : RWST Helpers for Error Types
Copyright   : (c) David Wilson, 2024
License     : BSD-3

Simple helpers for using `Metamorth.Helpers.Error`
with the `RWST` monad transformer.

-}

module Metamorth.Helpers.Error.RWS
  ( mkError
  , mkErrors
  , tellError
  , tellErrors
  , warn
  , warns
  , message
  , messages
  ) where

import Control.Monad.Trans.RWS.CPS

import Metamorth.Helpers.Error

-- | `tell` a single warning.
warn :: Monad m => String -> RWST r [ParserMessage] s m ()
warn str = tell [ParserWarning str]

-- | `tell` multiple warnings.
warns :: Monad m => [String] -> RWST r [ParserMessage] s m ()
warns strs = tell $ map ParserWarning strs

-- | `tell` a single message.
message :: Monad m => String -> RWST r [ParserMessage] s m ()
message str = tell [ParserMessage str]

-- | `tell` multiple messages.
messages :: Monad m => [String] -> RWST r [ParserMessage] s m ()
messages strs = tell $ map ParserMessage strs

-- | `tell` a single error.
mkError :: Monad m => String -> RWST r [ParserMessage] s m ()
mkError str = tell [ParserError str]

-- | `tell` multiple errors.
mkErrors :: Monad m => [String] -> RWST r [ParserMessage] s m ()
mkErrors strs = tell $ map ParserError strs

-- | `tell` a single error.
tellError :: Monad m => String -> RWST r [ParserMessage] s m ()
tellError = mkError
{-# INLINE tellError #-}

-- | `tell` multiple errors.
tellErrors :: Monad m => [String] -> RWST r [ParserMessage] s m ()
tellErrors = mkErrors
{-# INLINE tellErrors #-}