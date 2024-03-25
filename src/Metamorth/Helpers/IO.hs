{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Metamorth.Helpers.IO
Description : Simple Encoding-Specific IO Functions
License     : BSD-3

This module just contains some variants of
`readFile`, `writeFile`, etc... that are 
specified to specific encoding schemes,
mostly UTF-8. These functions are equivalent
to ones in `Data.Text.IO`, but specialised
to specific encodings; i.e. they all return/use
values of type `T.Text`.

Note: Since version 2.1, the @text@ package contains 
a module `Data.Text.IO.Utf8`. The UTF-8 functions
in this module are effectively equivalent to the
functions in that module, but these ones don't
require the newest version of @text@.

-}


module Metamorth.Helpers.IO
  -- * UTF-8
  -- ** File-at-a-time operations
  ( readFileUTF8
  , readFileUtf8
  , writeFileUTF8
  , writeFileUtf8
  , appendFileUTF8
  , appendFileUtf8
  -- ** Operations on Handles
  , hGetContentsUTF8
  , hGetContentsUtf8
  , hGetLineUTF8
  , hGetLineUtf8
  , hPutStrUTF8
  , hPutStrUtf8
  , hPutStrLnUTF8
  , hPutStrLnUtf8
  -- * UTF-16 Little-Endian
  -- ** File-at-a-time operations
  , readFileUTF16LE
  , writeFileUTF16LE
  , appendFileUTF16LE
  -- ** Operations on Handles
  , hGetContentsUTF16LE
  , hGetLineUTF16LE
  , hPutStrUTF16LE
  , hPutStrLnUTF16LE
  -- * UTF-16 Big-Endian
  -- ** File-at-a-time operations
  , readFileUTF16BE
  , writeFileUTF16BE
  , appendFileUTF16BE
  -- ** Operations on Handles
  , hGetContentsUTF16BE
  , hGetLineUTF16BE
  , hPutStrUTF16BE
  , hPutStrLnUTF16BE
  -- * UTF-32 Little-Endian
  -- ** File-at-a-time operations
  , readFileUTF32LE
  , writeFileUTF32LE
  , appendFileUTF32LE
  -- ** Operations on Handles
  , hGetContentsUTF32LE
  , hGetLineUTF32LE
  , hPutStrUTF32LE
  , hPutStrLnUTF32LE
  -- * UTF-32 Big-Endian
  -- ** File-at-a-time operations
  , readFileUTF32BE
  , writeFileUTF32BE
  , appendFileUTF32BE
  -- ** Operations on Handles
  , hGetContentsUTF32BE
  , hGetLineUTF32BE
  , hPutStrUTF32BE
  , hPutStrLnUTF32BE
  ) where

import Control.Monad

import Data.ByteString qualified as BS

import Data.Text (Text)
import Data.Text.Encoding qualified as TE

import Control.Exception (evaluate)

import System.IO

----------------------------------------------------------------
-- NOTE: The majority of these functions are taken right
-- from Data.Text.IO.Utf8, albeit renamed. The UTF-16 and
-- UTF-32 are also based on the functions from
-- Data.TExt.IO.Utf8, but with different encodings.

----------------------------------------------------------------
-- UTF-8 Functions

decodeUtf8IO :: BS.ByteString -> IO Text
decodeUtf8IO = evaluate . TE.decodeUtf8

-- | The `readFileUTF8` function reads a file and returns the contents of
--   the file as `Text`.  The entire file is read strictly, as with
--   'getContents'.
readFileUTF8 :: FilePath -> IO Text
readFileUTF8 = decodeUtf8IO <=< BS.readFile

-- | Synonym for `readFileUTF8`.
readFileUtf8 :: FilePath -> IO Text
readFileUtf8 = readFileUTF8
{-# INLINE readFileUtf8 #-}

-- | Write some `Text to a file.  The file is truncated to zero length
-- before writing begins.
writeFileUTF8 :: FilePath -> Text -> IO ()
writeFileUTF8 fp = BS.writeFile fp . TE.encodeUtf8

-- | Synonym for `writeFileUTF8`.
writeFileUtf8 :: FilePath -> Text -> IO ()
writeFileUtf8 = writeFileUTF8
{-# INLINE writeFileUtf8 #-}

-- | Write some `Text` to the end of a file.
appendFileUTF8 :: FilePath -> Text -> IO ()
appendFileUTF8 fp = BS.appendFile fp . TE.encodeUtf8

-- | Synonym for `appendFileUTF8`.
appendFileUtf8 :: FilePath -> Text -> IO ()
appendFileUtf8 = appendFileUTF8
{-# INLINE appendFileUtf8 #-}

-- | Read the remaining contents of a 'Handle' as a string.
hGetContentsUTF8 :: Handle -> IO Text
hGetContentsUTF8 = decodeUtf8IO <=< BS.hGetContents

-- | Read the remaining contents of a 'Handle' as a string.
hGetContentsUtf8 :: Handle -> IO Text
hGetContentsUtf8 = hGetContentsUTF8
{-# INLINE hGetContentsUtf8 #-}

-- | Read a single line from a handle.
hGetLineUTF8 :: Handle -> IO Text
hGetLineUTF8 = decodeUtf8IO <=< BS.hGetLine

-- | Synonym for `hGetLineUTF8`.
hGetLineUtf8 :: Handle -> IO Text
hGetLineUtf8 = hGetLineUTF8
{-# INLINE hGetLineUtf8 #-}

-- | Write a string to a handle.
hPutStrUTF8 :: Handle -> Text -> IO ()
hPutStrUTF8 h = BS.hPutStr h . TE.encodeUtf8

-- | Synonym for `hPutStrUTF8`.
hPutStrUtf8 :: Handle -> Text -> IO ()
hPutStrUtf8 = hPutStrUTF8
{-# INLINE hPutStrUtf8 #-}

-- | Write a string to a handle, followed by a newline.
hPutStrLnUTF8 :: Handle -> Text -> IO ()
hPutStrLnUTF8 h t = hPutStrUTF8 h t >> BS.hPutStr h "\n"

-- | Write a string to a handle, followed by a newline.
hPutStrLnUtf8 :: Handle -> Text -> IO ()
hPutStrLnUtf8 = hPutStrLnUTF8
{-# INLINE hPutStrLnUtf8 #-}

----------------------------------------------------------------
-- UTF-16 Little Endian Functions

decodeUtf16LEIO :: BS.ByteString -> IO Text
decodeUtf16LEIO = evaluate . TE.decodeUtf16LE

-- | The `readFileUTF16LE` function reads a file and returns the contents of
--   the file as `Text`.  The entire file is read strictly, as with
--   'getContents'.
readFileUTF16LE :: FilePath -> IO Text
readFileUTF16LE = decodeUtf16LEIO <=< BS.readFile

-- | Write some `Text to a file.  The file is truncated to zero length
-- before writing begins.
writeFileUTF16LE :: FilePath -> Text -> IO ()
writeFileUTF16LE fp = BS.writeFile fp . TE.encodeUtf16LE

-- | Write some `Text` to the end of a file.
appendFileUTF16LE :: FilePath -> Text -> IO ()
appendFileUTF16LE fp = BS.appendFile fp . TE.encodeUtf16LE

-- | Read the remaining contents of a 'Handle' as a string.
hGetContentsUTF16LE :: Handle -> IO Text
hGetContentsUTF16LE = decodeUtf16LEIO <=< BS.hGetContents

-- | Read a single line from a handle.
hGetLineUTF16LE :: Handle -> IO Text
hGetLineUTF16LE = decodeUtf16LEIO <=< BS.hGetLine

-- | Write a string to a handle.
hPutStrUTF16LE :: Handle -> Text -> IO ()
hPutStrUTF16LE h = BS.hPutStr h . TE.encodeUtf16LE

-- | Write a string to a handle, followed by a newline.
hPutStrLnUTF16LE :: Handle -> Text -> IO ()
hPutStrLnUTF16LE h t = hPutStrUTF16LE h t >> hPutStrUTF16LE h "\n"

----------------------------------------------------------------
-- UTF-16 Big Endian Functions

decodeUtf16BEIO :: BS.ByteString -> IO Text
decodeUtf16BEIO = evaluate . TE.decodeUtf16BE

-- | The `readFileUTF16BE` function reads a file and returns the contents of
--   the file as `Text`.  The entire file is read strictly, as with
--   'getContents'.
readFileUTF16BE :: FilePath -> IO Text
readFileUTF16BE = decodeUtf16BEIO <=< BS.readFile

-- | Write some `Text to a file.  The file is truncated to zero length
-- before writing begins.
writeFileUTF16BE :: FilePath -> Text -> IO ()
writeFileUTF16BE fp = BS.writeFile fp . TE.encodeUtf16BE

-- | Write some `Text` to the end of a file.
appendFileUTF16BE :: FilePath -> Text -> IO ()
appendFileUTF16BE fp = BS.appendFile fp . TE.encodeUtf16BE

-- | Read the remaining contents of a 'Handle' as a string.
hGetContentsUTF16BE :: Handle -> IO Text
hGetContentsUTF16BE = decodeUtf16BEIO <=< BS.hGetContents

-- | Read a single line from a handle.
hGetLineUTF16BE :: Handle -> IO Text
hGetLineUTF16BE = decodeUtf16BEIO <=< BS.hGetLine

-- | Write a string to a handle.
hPutStrUTF16BE :: Handle -> Text -> IO ()
hPutStrUTF16BE h = BS.hPutStr h . TE.encodeUtf16BE

-- | Write a string to a handle, followed by a newline.
hPutStrLnUTF16BE :: Handle -> Text -> IO ()
hPutStrLnUTF16BE h t = hPutStrUTF16BE h t >> hPutStrUTF16BE h "\n"

----------------------------------------------------------------
-- UTF-32 Little Endian Functions

decodeUtf32LEIO :: BS.ByteString -> IO Text
decodeUtf32LEIO = evaluate . TE.decodeUtf32LE

-- | The `readFileUTF32LE` function reads a file and returns the contents of
--   the file as `Text`.  The entire file is read strictly, as with
--   'getContents'.
readFileUTF32LE :: FilePath -> IO Text
readFileUTF32LE = decodeUtf32LEIO <=< BS.readFile

-- | Write some `Text to a file.  The file is truncated to zero length
-- before writing begins.
writeFileUTF32LE :: FilePath -> Text -> IO ()
writeFileUTF32LE fp = BS.writeFile fp . TE.encodeUtf32LE

-- | Write some `Text` to the end of a file.
appendFileUTF32LE :: FilePath -> Text -> IO ()
appendFileUTF32LE fp = BS.appendFile fp . TE.encodeUtf32LE

-- | Read the remaining contents of a 'Handle' as a string.
hGetContentsUTF32LE :: Handle -> IO Text
hGetContentsUTF32LE = decodeUtf32LEIO <=< BS.hGetContents

-- | Read a single line from a handle.
hGetLineUTF32LE :: Handle -> IO Text
hGetLineUTF32LE = decodeUtf32LEIO <=< BS.hGetLine

-- | Write a string to a handle.
hPutStrUTF32LE :: Handle -> Text -> IO ()
hPutStrUTF32LE h = BS.hPutStr h . TE.encodeUtf32LE

-- | Write a string to a handle, followed by a newline.
hPutStrLnUTF32LE :: Handle -> Text -> IO ()
hPutStrLnUTF32LE h t = hPutStrUTF32LE h t >> hPutStrUTF32LE h "\n"

----------------------------------------------------------------
-- UTF-32 Big Endian Functions

decodeUtf32BEIO :: BS.ByteString -> IO Text
decodeUtf32BEIO = evaluate . TE.decodeUtf32BE

-- | The `readFileUTF32BE` function reads a file and returns the contents of
--   the file as `Text`.  The entire file is read strictly, as with
--   'getContents'.
readFileUTF32BE :: FilePath -> IO Text
readFileUTF32BE = decodeUtf32BEIO <=< BS.readFile

-- | Write some `Text to a file.  The file is truncated to zero length
-- before writing begins.
writeFileUTF32BE :: FilePath -> Text -> IO ()
writeFileUTF32BE fp = BS.writeFile fp . TE.encodeUtf32BE

-- | Write some `Text` to the end of a file.
appendFileUTF32BE :: FilePath -> Text -> IO ()
appendFileUTF32BE fp = BS.appendFile fp . TE.encodeUtf32BE

-- | Read the remaining contents of a 'Handle' as a string.
hGetContentsUTF32BE :: Handle -> IO Text
hGetContentsUTF32BE = decodeUtf32BEIO <=< BS.hGetContents

-- | Read a single line from a handle.
hGetLineUTF32BE :: Handle -> IO Text
hGetLineUTF32BE = decodeUtf32BEIO <=< BS.hGetLine

-- | Write a string to a handle.
hPutStrUTF32BE :: Handle -> Text -> IO ()
hPutStrUTF32BE h = BS.hPutStr h . TE.encodeUtf32BE

-- | Write a string to a handle, followed by a newline.
hPutStrLnUTF32BE :: Handle -> Text -> IO ()
hPutStrLnUTF32BE h t = hPutStrUTF32BE h t >> hPutStrUTF32BE h "\n"

