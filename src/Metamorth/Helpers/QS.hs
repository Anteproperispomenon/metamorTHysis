{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

{-|
Module      : Metamorth.Helpers.QS
Description : Simple variant of the Q monad
Copyright   : (c) David Wilson, 2024
License     : BSD-3

This modudle provides a simple wrapper over
@`ReaderT` `String` `Q`@ that appends a
`String` to any generated names. This makes
it easy to run multiple instances of the
same code generation that would otherwise
generate the same names each time.

-}

module Metamorth.Helpers.QS
 ( QS
 , runQS
 , runQS2
 , liftQS
 , qsNewName
 , qsPlainNewName
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

-- | @QS@ is a `ReaderT` over `Q` that automatically adds
--   suffixes to Names created with `newName` or `qNewName`.
--   To run it, just use `runQS` or `runQS2`. e.g.
--
--   @
--   makeFunc :: String -> QS Dec
--   makeFunc myName = do
--     theName <- newName myName
--     [d| $(pure $ VarP theName) = \x -> x * x |]
--   
--   createFunc :: String -> String -> Q Dec
--   createFunc pfx myName = runQS pfx $ makeFunc myName
--   
--   @
newtype QS a = QS { getQS :: ReaderT (String, String) Q a}
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

-- | Lift an operation in `Q` to one in `QS`.
--   Note that if the action calls `newName`,
--   it won't add the prefix to it.
liftQS :: Q a -> QS a
liftQS action = QS $ lift action

-- | The main way to run a QS function.
--   Note that it will fail if you try
--   to feed it a prefix that doesn't
--   start with an ASCII letter. Numbers,
--   punctuation, etc... will fail.
--   However, you don't have to worry
--   about ensuring that the `String` is
--   upper-case or lower-case, since the
--   `qsNewName` function will automatically
--   handle conversions. However, empty
--   strings are accepted.
runQS :: String -> QS a -> Q a
runQS []  qs = do
  reportWarning "Running a QS action with the empty string."  
  runReaderT (getQS qs) ([], [])
runQS str@(c:cs) qs
  -- Should add better conditions on the allowed `Char`s.
  = runReaderT (getQS qs) (str, [])

-- | Like `runQS`, but also specifies a prefix
--   for infix operators. If you aren't planning
--   to define any infix operators, or don't want
--   them to use a prefix, just use `runQS` instead.
runQS2 :: String -> String -> QS a -> Q a
runQS2 [] []  qs = do
  reportWarning "Running a QS action with two empty strings."
  runReaderT (getQS qs) ([], [])
runQS2 [] infPre qs
  | (all isOpChar infPre) = runReaderT (getQS qs) ([], infPre)
  | otherwise = fail $ "Can't run QS with an operator suffix that has disallowed characters; given \"" ++ infPre ++ "\"."
runQS2 str [] qs
  = runReaderT (getQS qs) (str, [])
-- runQS2 str@(c:_) [] qs
  -- | ((isAsciiUpper c) || (isAsciiLower c)) = runReaderT (getQS qs) (str, [])
  -- | otherwise = fail $ "Can't run QS with a prefix that doesn't start with an ASCII letter; given \"" <> str <> "\"."
runQS2 str infPre qs
  -- Might want to add better checks for the suffix.
  | (all isOpChar infPre)
  = runReaderT (getQS qs) (str, infPre)
  | otherwise
  = fail $ "Can't run QS with an operator prefix that has disallowed characters; given \"" ++ infPre ++ "\"."


-- | Create a new `Name` without the
--   designated suffix. In case you
--   don't want to include the suffix.
qsPlainNewName :: String -> QS Name
qsPlainNewName str = QS $ lift $ newName str

-- | The main function to create a new `Name`
--   with a prefix. You don't actually have
--   to use this function directly; `newName` 
--   and `qNewName` just call this function.
qsNewName :: String -> QS Name
qsNewName []  = QS $ do
  lift $ reportError "Trying to create a Name from the empty string."
  -- Not adding the prefix since there's no way
  -- to know which kind of name is needed.
  lift $ newName ""
qsNewName str
  = QS $ do
    (suff, suff') <- ask
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
              True  -> newName (strModule ++ (addSuffixO suff' strName))
        | otherwise  -> do
            case suff of
              -- Just use the same string when prefix is empty.
              [] -> lift $ newName str
              _  -> do
                -- Add the prefix to the string.
                let newStr = addSuffixW suff strName
                lift $ newName (strModule ++ newStr)

-- Add a prefix to a word (not an operator).
addSuffixW :: String -> String -> String
addSuffixW _fx "" = ""
addSuffixW sfx str = str ++ sfx

{-
makeLower1 :: String -> String
makeLower1 "" = ""
makeLower1 (c:cs)
  = (toLower c) : cs

makeUpper1 :: String -> String
makeUpper1 "" = ""
makeUpper1 (c:cs)
  = (toUpper c) : cs
-}

addSuffixO :: String -> String -> String
addSuffixO []  str = str
addSuffixO sfx str = str ++ sfx

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

instance Quasi QS where
  qNewName = qsNewName
  qRecover m1 m2           = QS $ do
    val <- ask
    lift $ qRecover (runReaderT (getQS m1) val) (runReaderT (getQS m2) val)
  qReport bl x             = QS $ lift $ qReport bl x
  qLookupName bl str       = QS $ lift $ qLookupName bl str
  qReify nm                = QS $ lift $ qReify nm
  qReifyFixity nm          = QS $ lift $ qReifyFixity nm
  qReifyType nm            = QS $ lift $ qReifyType nm
  qReifyInstances nm typs  = QS $ lift $ qReifyInstances nm typs
  qReifyRoles nm           = QS $ lift $ qReifyRoles nm
  qReifyAnnotations lkup   = QS $ lift $ qReifyAnnotations lkup
  qReifyModule modu        = QS $ lift $ qReifyModule modu
  qReifyConStrictness nm   = QS $ lift $ qReifyConStrictness nm
  qLocation                = QS $ lift $ qLocation
  qRunIO action            = QS $ lift $ qRunIO action
  qGetPackageRoot          = QS $ lift $ qGetPackageRoot
  qAddDependentFile fp     = QS $ lift $ qAddDependentFile fp
  qAddTempFile fp          = QS $ lift $ qAddTempFile fp
  qAddTopDecls decs        = QS $ lift $ qAddTopDecls decs
  qAddForeignFilePath l fp = QS $ lift $ qAddForeignFilePath l fp
  qAddModFinalizer action  = QS $ lift $ qAddModFinalizer action
  qAddCorePlugin str       = QS $ lift $ qAddCorePlugin str
  qGetQ                    = QS $ lift $ qGetQ
  qPutQ val                = QS $ lift $ qPutQ val
  qIsExtEnabled ext        = QS $ lift $ qIsExtEnabled ext
  qExtsEnabled             = QS $ lift $ qExtsEnabled
  qPutDoc dloc str         = QS $ lift $ qPutDoc dloc str
  qGetDoc dloc             = QS $ lift $ qGetDoc dloc

instance Quote QS where
  newName = qsNewName

instance (Semigroup a) => Semigroup (QS a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (QS a) where
  mempty = pure mempty

