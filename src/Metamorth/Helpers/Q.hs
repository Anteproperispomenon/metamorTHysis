module Metamorth.Helpers.Q
  ( isOpChar
  , opChars
  , getLastName
  , qReportError
  , qReportWarning
  ) where

import Language.Haskell.TH.Syntax (Quasi, qReport)

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
  | (c:cs) <- strSfx
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

