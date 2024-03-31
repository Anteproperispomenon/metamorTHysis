{-|
Module      : Metamorth.Helpers.Error.Writer
Description : WriterT Helpers for Error Types
Copyright   : (c) David Wilson, 2024
License     : BSD-3

Simple helpers for using `Metamorth.Helpers.Error`
with the `WriterT` monad transformer.

-}

module Metamorth.Helpers.Error.Writer
  ( mkError
  , mkErrors
  , tellError
  , tellErrors
  , warn
  , warns
  , message
  , messages
  ) where

import Control.Monad.Trans.Writer.CPS

import Metamorth.Helpers.Error

-- | `tell` a single warning.
warn :: Monad m => String -> WriterT [ParserMessage] m ()
warn str = tell [ParserWarning str]

-- | `tell` multiple warnings.
warns :: Monad m => [String] -> WriterT [ParserMessage] m ()
warns strs = tell $ map ParserWarning strs

-- | `tell` a single message.
message :: Monad m => String -> WriterT [ParserMessage] m ()
message str = tell [ParserMessage str]

-- | `tell` multiple messages.
messages :: Monad m => [String] -> WriterT [ParserMessage] m ()
messages strs = tell $ map ParserMessage strs

-- | `tell` a single error.
mkError :: Monad m => String -> WriterT [ParserMessage] m ()
mkError str = tell [ParserError str]

-- | `tell` multiple errors.
mkErrors :: Monad m => [String] -> WriterT [ParserMessage] m ()
mkErrors strs = tell $ map ParserError strs

-- | `tell` a single error.
tellError :: Monad m => String -> WriterT [ParserMessage] m ()
tellError = mkError
{-# INLINE tellError #-}

-- | `tell` multiple errors.
tellErrors :: Monad m => [String] -> WriterT [ParserMessage] m ()
tellErrors = mkErrors
{-# INLINE tellErrors #-}
