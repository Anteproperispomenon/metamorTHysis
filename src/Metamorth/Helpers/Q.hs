module Metamorth.Helpers.Q
  -- * Lifting from `Q`
  ( QL(..)
  , newTopName
  -- * Other Functions
  , qDebugNotice
  , qDebugNoticeUnflushed
  , qFlushErr
  , qFlushOut
  , isOpChar
  , isSpecChar
  , notNameChar
  , isNameChar
  , opChars
  , getLastName
  , qReportError
  , qReportWarning
  ) where

import Language.Haskell.TH.Syntax (Quasi, Quote, qReport, Q, Name, newName, qRunIO)

import System.IO (stderr, stdout, hPutStrLn, hFlush)

-- In the future, will extend this function
-- or similar function to include unicode
-- symbols.
isOpChar :: Char -> Bool
isOpChar x = 
    (x == '!')
      || (x == '#')
      || (x == '$')
      || (x == '%')
      || (x == '&')
      || (x == '*')
      || (x == '+')
      || (x == '.')
      || (x == '/')
      || (x == '<')
      || (x == '=')
      || (x == '>')
      || (x == '?')
      || (x == '@')
      || (x == '\\')
      || (x == '^')
      || (x == '|')
      || (x == '-')
      || (x == '~')
      || (x == ':')

opChars :: String 
opChars = "!#$%&*+./<=>?@\\^|-~:"

isSpecChar :: Char -> Bool
isSpecChar x =
    (x == '[')
      || (x == ']')
      || (x == '(')
      || (x == ')')
      || (x == '{')
      || (x == '}')
      || (x == ';')

notNameChar :: Char -> Bool
notNameChar x = (isSpecChar x) || (isOpChar x)

isNameChar :: Char -> Bool
isNameChar x = not $ notNameChar x

-- | Split a `String` after the last module separator.
--   Note that this is not the same as splitting after
--   the last dot, since the last dot could be part
--   of an operator `Name`.
getLastName :: String -> (String, String)
getLastName str
  | null str = ("", "")
  -- Most Words...
  | null strPfx
  = ("", strSfx)
  -- See whether the whole string is an operator...
  | (c:_) <- str
  , isOpChar c
  = ("", str)
  -- See whether the suffix is a standard word...
  | (c:_) <- strSfx
  , (not $ isOpChar c)
  = (strPfx, strSfx)
  -- Qualified Operators
  | (not $ null opSfx)
  = (opPfx, opSfx)
  -- May need to add some other guards...
  | otherwise = (strPfx, strSfx)

  -- This function could stand to be improved
  -- in general.
  where
    (strPfx, strSfx) = getLastName' str
    altPfx = takeWhile (\x -> (x == '.') || (not $ isOpChar x))
    (opPfx, opSfx) = getLastOp str

getLastName' :: String -> (String, String)
getLastName' "" = ("","")
getLastName' str
  -- Note the reversed order in the result;
  -- (x, y) -> (reverse y, reverse x)
  | (rpfx, rsfx) <- break (== '.') rstr
  = (reverse rsfx, reverse rpfx)
  where rstr = reverse str

getLastOp :: String -> (String, String)
getLastOp str
  | null rsfx = ("", reverse rpfx)
  -- Move the "first" dot from the
  -- suffix to the prefix, as in
  -- `Data.Bits..&.`.
  | (c:cs) <- reverse rpfx
  , c == '.'
  = (reverse ('.':rsfx), cs)
  | otherwise = (reverse rsfx, reverse rpfx)

  where 
    (rpfx, rsfx) = span (isOpChar) (reverse str)

qReportError :: (Quasi q) => String -> q ()
qReportError = qReport True

qReportWarning :: (Quasi q) => String -> q ()
qReportWarning = qReport False

qDebugNoticeUnflushed :: (Quasi q) => String -> q ()
qDebugNoticeUnflushed str = qRunIO $ hPutStrLn stderr str

-- | Write a message to `stderr` as the 
--   program is compiling. Useful if compilation
--   is hanging.
qDebugNotice :: (Quasi q) => String -> q ()
qDebugNotice str = qRunIO $ do
  hPutStrLn stderr str
  hFlush stderr

qFlushErr :: (Quasi q) => q ()
qFlushErr = qRunIO $ hFlush stderr

qFlushOut :: (Quasi q) => q ()
qFlushOut = qRunIO $ hFlush stdout

-- | Types that can be lifted from the Q.
--   This is similar to the `MonadIO` class,
--   but over `Q` instead of `IO`.
class (Quasi q, Quote q) => QL q where
  -- | Lift a computation from `Q` into a monad
  --   with `Q` as the base.
  fromQ :: Q a -> q a

-- Base instance
instance QL Q where
  fromQ f = f

-- | Make a new `Name` without any
--   prefixes or suffixes. Useful when
--   you want to create a name that will
--   be exported.
newTopName :: (QL q) => String -> q Name
newTopName str = fromQ $ newName str
