{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

{-|
Module      : Metamorth.Helpers.QP
Description : Simple variant of the Q monad
Copyright   : (c) David Wilson, 2024
License     : BSD-3

This modudle provides a simple wrapper over
@`ReaderT` `String` `Q`@ that prepends a
`String` to any generated names. This makes
it easy to run multiple instances of the
same code generation that would otherwise
generate the same names each time.

-}

module Metamorth.Helpers.QP
 ( QP
 , runQP
 , runQP2
 , liftQP
 , qaNewName
 , qaPlainNewName
 ) where


import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Fail
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Data.Coerce (coerce)

import Data.Char 
import Data.List (span, break, dropWhile)

-- | @QP@ is a `ReaderT` over `Q` that automatically adds
--   prefices to Names created with `newName` or `qNewName`.
--   To run it, just use `runQP` or `runQP2`. e.g.
--
--   @
--   makeFunc :: String -> QP Dec
--   makeFunc myName = do
--     theName <- newName myName
--     [d| $(pure $ VarP theName) = \x -> x * x |]
--   
--   createFunc :: String -> String -> Q Dec
--   createFunc pfx myName = runQP pfx $ makeFunc myName
--   
--   @
newtype QP a = QP { getQP :: ReaderT (String, String) Q a}
  deriving newtype 
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFix
    , MonadFail
    -- , Semigroup
    -- , Monoid
    )

-- | Lift an operation in `Q` to one in `QP`.
--   Note that if the action calls `newName`,
--   it won't add the prefix to it.
liftQP :: Q a -> QP a
liftQP action = QP $ lift action

-- | The main way to run a QP function.
--   Note that it will fail if you try
--   to feed it a prefix that doesn't
--   start with an ASCII letter. Numbers,
--   punctuation, etc... will fail.
--   However, you don't have to worry
--   about ensuring that the `String` is
--   upper-case or lower-case, since the
--   `qaNewName` function will automatically
--   handle conversions. However, empty
--   strings are accepted.
runQP :: String -> QP a -> Q a
runQP []  qa = do
  reportWarning "Running a QP action with the empty string."  
  runReaderT (getQP qa) ([], [])
runQP str@(c:cs) qa
  -- Prefix must work for 
  | ((isAsciiUpper c) || (isAsciiLower c)) = runReaderT (getQP qa) (str, [])
  | otherwise = fail $ "Can't run QP with a prefix that doesn't start with an ASCII letter; given \"" <> str <> "\"."

-- | Like `runQP`, but also specifies a prefix
--   for infix operators. If you aren't planning
--   to define any infix operators, or don't want
--   them to use a prefix, just use `runQP` instead.
runQP2 :: String -> String -> QP a -> Q a
runQP2 [] []  qa = do
  reportWarning "Running a QP action with two empty strings."
  runReaderT (getQP qa) ([], [])
runQP2 [] infPre qa
  | (all isOpChar infPre) = runReaderT (getQP qa) ([], infPre)
  | otherwise = fail $ "Can't run QP with an operator prefix that has disallowed characters; given \"" ++ infPre ++ "\"."
runQP2 str@(c:_) [] qa
  -- Prefix must work for 
  | ((isAsciiUpper c) || (isAsciiLower c)) = runReaderT (getQP qa) (str, [])
  | otherwise = fail $ "Can't run QP with a prefix that doesn't start with an ASCII letter; given \"" <> str <> "\"."
runQP2 str@(c:_) infPre qa
  | (((isAsciiUpper c) || (isAsciiLower c)) && (all isOpChar infPre))
  = runReaderT (getQP qa) (str, infPre)
  -- str works but infPre doesn't:
  | ((isAsciiUpper c) || (isAsciiLower c))
  = fail $ "Can't run QP with an operator prefix that has disallowed characters; given \"" ++ infPre ++ "\"."
  -- infPre works but str doesn't
  | (all isOpChar infPre)
  = fail $ "Can't run QP with a prefix that doesn't start with an ASCII letter; given \"" <> str <> "\"."
  -- Both fail
  | otherwise
  = fail $ "Trying to run QP with failing prefices; given \"" ++ str ++ "\" and \"" ++ infPre ++ "\"."


-- | Create a new `Name` without the
--   designated prefix. In case you
--   don't want to include the prefix.
qaPlainNewName :: String -> QP Name
qaPlainNewName str = QP $ lift $ newName str

-- | The main function to create a new `Name`
--   with a prefix. You don't actually have
--   to use this function directly; `newName` 
--   and `qNewName` just call this function.
qaNewName :: String -> QP Name
qaNewName []  = QP $ do
  lift $ reportError "Trying to create a Name from the empty string."
  -- Not adding the prefix since there's no way
  -- to know which kind of name is needed.
  lift $ newName ""
qaNewName str
  = QP $ do
    (pref, pref') <- ask
    let (strModule, strName) = getLastName str
    case strName of
      [] -> lift $ do
        reportError $ "Trying to create invalid Name: \"" ++ str ++ "\"."
        newName str
      (c:cs)
        | isOpChar c -> lift $ do
            case (all isOpChar cs) of
              False -> do 
                reportError $ "Trying to create operator Name with letters and/or disallowed symbols: \"" ++ str ++ "\"."
                newName str
              True  -> newName (strModule ++ (addPrefixO pref' strName))
        | otherwise  -> do
            case pref of
              -- Just use the same string when prefix is empty.
              [] -> lift $ newName str
              _  -> do
                -- Add the prefix to the string.
                let (newStr, gotError) = addPrefixW pref strName
                when gotError $ lift $ reportWarning $ "Encountered unusual Name in QP : \"" <> str <> "\"."
                lift $ newName (strModule ++ newStr)

-- Add a prefix to a word (not an operator).
addPrefixW :: String -> String -> (String, Bool)
addPrefixW pfx "" = ("", False) -- already covered
addPrefixW pfx str@(c:_)
  | (isAsciiLower c) = (makeLower1 pfx ++ str, False)
  | (isAsciiUpper c) = (makeUpper1 pfx ++ str, False)
  | (isLowerCase  c) = (makeLower1 pfx ++ str, True)
  | (isUpperCase  c) = (makeUpper1 pfx ++ str, True)
  | otherwise        = (makeLower1 pfx ++ str, True)

makeLower1 :: String -> String
makeLower1 "" = ""
makeLower1 (c:cs)
  = (toLower c) : cs

makeUpper1 :: String -> String
makeUpper1 "" = ""
makeUpper1 (c:cs)
  = (toUpper c) : cs

addPrefixO :: String -> String -> String
addPrefixO [] str = str
-- retaining the colon partway through.
addPrefixO prf@(x:_) str@(':':_) 
  | (x == ':') = prf ++ str
  | otherwise  = ':' : (prf ++ str)
addPrefixO prf str
  = (dropWhile (== ':') prf) ++ str -- remove ALL colons.

-- | Split a `String` after the last dot.
getLastName :: String -> (String, String)
getLastName "" = ("","")
getLastName str
  -- Note the reversed order in the result;
  -- (x, y) -> (reverse y, reverse x)
  | (rpfx, rsfx) <- break (== '.') rstr
  = (reverse rsfx, reverse rpfx)
  where rstr = reverse str

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

instance Quasi QP where
  qNewName = qaNewName
  qRecover m1 m2           = QP $ do
    val <- ask
    lift $ qRecover (runReaderT (getQP m1) val) (runReaderT (getQP m2) val)
  qReport bl x             = QP $ lift $ qReport bl x
  qLookupName bl str       = QP $ lift $ qLookupName bl str
  qReify nm                = QP $ lift $ qReify nm
  qReifyFixity nm          = QP $ lift $ qReifyFixity nm
  qReifyType nm            = QP $ lift $ qReifyType nm
  qReifyInstances nm typs  = QP $ lift $ qReifyInstances nm typs
  qReifyRoles nm           = QP $ lift $ qReifyRoles nm
  qReifyAnnotations lkup   = QP $ lift $ qReifyAnnotations lkup
  qReifyModule modu        = QP $ lift $ qReifyModule modu
  qReifyConStrictness nm   = QP $ lift $ qReifyConStrictness nm
  qLocation                = QP $ lift $ qLocation
  qRunIO action            = QP $ lift $ qRunIO action
  qGetPackageRoot          = QP $ lift $ qGetPackageRoot
  qAddDependentFile fp     = QP $ lift $ qAddDependentFile fp
  qAddTempFile fp          = QP $ lift $ qAddTempFile fp
  qAddTopDecls decs        = QP $ lift $ qAddTopDecls decs
  qAddForeignFilePath l fp = QP $ lift $ qAddForeignFilePath l fp
  qAddModFinalizer action  = QP $ lift $ qAddModFinalizer action
  qAddCorePlugin str       = QP $ lift $ qAddCorePlugin str
  qGetQ                    = QP $ lift $ qGetQ
  qPutQ val                = QP $ lift $ qPutQ val
  qIsExtEnabled ext        = QP $ lift $ qIsExtEnabled ext
  qExtsEnabled             = QP $ lift $ qExtsEnabled
  qPutDoc dloc str         = QP $ lift $ qPutDoc dloc str
  qGetDoc dloc             = QP $ lift $ qGetDoc dloc

instance Quote QP where
  newName = qaNewName

instance (Semigroup a) => Semigroup (QP a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (QP a) where
  mempty = pure mempty

