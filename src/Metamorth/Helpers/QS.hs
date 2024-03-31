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
 -- * Main Monad
 ( QS
 , QST
 , runQS
 , runQS2
 -- * Additional Helpers
 , extendSuffix
 , extendSuffix2
 , liftQS
 , qsNewName
 , qsPlainNewName
 ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
-- import Control.Monad.Fail

-- import Data.Coerce (coerce)

-- import Data.Char 
-- import Data.List (span, break, dropWhile)

import Metamorth.Helpers.Q

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
type QS = QST Q
-- Need to write it like this and not `type QS a = QST Q a`, since
-- that would cause problems when using it with Monad transformers.

-- | @QST` is a variant of `QS` that works over any
--   instance of `Quasi` and `Quote`.
--   Note that `runQS` and `runQS2` work on both
--   `QS` and `QST`.
newtype QST q a = QS { getQS :: ReaderT (String, String) q a}
  deriving newtype 
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFix
    , MonadFail
    , MonadTrans
    -- , Semigroup
    -- , Monoid
    )

-- | Lift an operation in `Q` to one in `QS`.
--   Note that if the action calls `newName`,
--   it won't add the suffix to it.
liftQS :: (Quote q, Quasi q) => q a -> QST q a
liftQS action = QS $ lift action

-- | The main way to run a QS function.
--   Note that it will fail if you try
--   to feed it a suffix that doesn't
--   start with an ASCII letter. Numbers,
--   punctuation, etc... will fail.
--   However, you don't have to worry
--   about ensuring that the `String` is
--   upper-case or lower-case, since the
--   `qsNewName` function will automatically
--   handle conversions. However, empty
--   strings are accepted.
runQS :: (Quasi q, Quote q) => String -> QST q a -> q a
runQS []  qs = do
  qReportWarning "Running a QS action with the empty string."  
  runReaderT (getQS qs) ([], [])
runQS str qs
  -- Should add better conditions on the allowed `Char`s.
  | any notNameChar str = do
      qReportError $ "Trying to run a QS block with improper suffix: \"" ++ str ++ "\"."
      runReaderT (getQS qs) (filter isNameChar str, [])
  | otherwise = runReaderT (getQS qs) (str, [])

-- | Like `runQS`, but also specifies a suffix
--   for infix operators. If you aren't planning
--   to define any infix operators, or don't want
--   them to use a suffix, just use `runQS` instead.
runQS2 :: (Quasi q, Quote q) => String -> String -> QST q a -> q a
runQS2 [] []  qs = do
  qReportWarning "Running a QS action with two empty strings."
  runReaderT (getQS qs) ([], [])
runQS2 [] infPre qs
  | (all isOpChar infPre) = runReaderT (getQS qs) ([], infPre)
  | otherwise = do 
      qReportError $ "Can't run QS with an operator suffix that has disallowed characters; given \"" ++ infPre ++ "\"."
      runReaderT (getQS qs) ([], filter isOpChar infPre)
runQS2 str [] qs
  | (all isNameChar str) = runReaderT (getQS qs) (str, [])
  | otherwise = do 
      qReportError $ "Trying to run a QS block with improper suffix: \"" ++ str ++ "\"."
      runReaderT (getQS qs) (filter isNameChar str, [])

runQS2 str infPre qs
  -- Might want to add better checks for the suffix.
  | (all isOpChar infPre) && (all isNameChar str)
  = runReaderT (getQS qs) (str, infPre)
  | (all isOpChar infPre) = do
      qReportError $ "Trying to run a QS block with improper Name suffix: \"" ++ str ++ "\"."
      runReaderT (getQS qs) (filter isNameChar str, infPre)
  | (all isNameChar str) = do
      qReportError $ "Can't run QS with an operator suffix that has disallowed characters; given \"" ++ infPre ++ "\"."
      runReaderT (getQS qs) (str, filter isOpChar infPre)
  | otherwise = do
      qReportError $ "Can't run QS with an name suffix that has disallowed characters; given \""     ++ str    ++ "\"."
      qReportError $ "Can't run QS with an operator suffix that has disallowed characters; given \"" ++ infPre ++ "\"."
      runReaderT (getQS qs) (filter isNameChar str, filter isOpChar infPre)

-- | A faster way of running `runQS` in another `QST` block.
--   This uses `local` from `Control.Monad.Trans.Reader` internally.
--
--   Note that there is one significant difference between `extendSuffix`
--   and `runQS`: `qsPlainNewName` will act differently between the two.
--   Using `qsPlainNewName` in a runQS block will only remove the
--   latest suffix, while `running it in an `extendSuffix` block will
--   remove any extra suffixes added with `extendSuffix`. e.g.
--   
--   @
--   thisAction :: (`QL` q) => String -> q Name
--   thisAction str = extendSuffix "_asdf" $ qsPlainNewName str
--
--   thatAction :: (`QL` q) => String -> q Name
--   thatAction str = runQS "_asdf" $ qsPlainNewName str
--
--   runActions :: String -> Q [Name]
--   runActions str = do
--     name1 <- runQS "_zxcv" $ thisAction str
--     name2 <- runQS "_zxcv" $ thatAction str
--     return [name1, name2]
--   @
--
--   Running @runActions "hello"@ would produce the result
--   @["hello", "hello_zxcv"]@.
extendSuffix :: (Quasi q, Quote q) => String -> QST q a -> QST q a
extendSuffix str (QS action) 
  | (any notNameChar str) = do
      qReportWarning $ "Trying to extend suffix with disallowed characters: \"" ++ str ++ "\"."
      QS $ local (\(x,y) -> (x ++ (filter isNameChar str), y)) action
  | otherwise = QS $ local (\(x,y) -> (x ++ str,y)) action

-- | A faster way of running `runQS2` in another `QST` block.
--   This uses `local` from `Control.Monad.Trans.Reader` internally.
--
--   Note that the same difference between `extendSuffix` and `runQS`
--   applies to this function and `runQS2`, except that it applies
--   to both suffixes here.
extendSuffix2 :: (Quasi q, Quote q) => String -> String -> QST q a -> QST q a
extendSuffix2 str1 str2 (QS action)
  | (any (not . isOpChar) str2) && (any notNameChar str1) = do
      qReportWarning $ "Trying to extend name suffix with disallowed characters: \"" ++ str1 ++ "\"."
      qReportWarning $ "Trying to extend operator suffix with disallowed characters: \"" ++ str2 ++ "\"."
      QS $ local (\(x,y) -> (x ++ (filter isNameChar str1), y ++ (filter isOpChar str2))) action
  | (any (not . isOpChar) str2) = do
      qReportWarning $ "Trying to extend operator suffix with disallowed characters: \"" ++ str2 ++ "\"."
      QS $ local (\(x,y) -> (x ++ str1, y ++ (filter isOpChar str2))) action
  | (any notNameChar str1) = do
      qReportWarning $ "Trying to extend name suffix with disallowed characters: \"" ++ str1 ++ "\"."
      QS $ local (\(x,y) -> (x ++ (filter isNameChar str1), y ++ str2)) action
  | otherwise = QS $ local (\(x,y) -> (x ++ str1,y ++ str2)) action

-- | Create a new `Name` without the
--   designated suffix. In case you
--   don't want to include the suffix.
--
--   Note that this function only goes
--   one level up; if you want to generate
--   a top-level name, use `newTopName`
--   instead.
qsPlainNewName :: (Quote q) => String -> QST q Name
qsPlainNewName str = QS $ lift $ newName str

-- | The main function to create a new `Name`
--   with a suffix. You don't actually have
--   to use this function directly; `newName` 
--   and `qNewName` just call this function.
qsNewName :: (Quasi q) => String -> QST q Name
qsNewName []  = QS $ do
  lift $ qReportError "Trying to create a Name from the empty string."
  -- Not adding the suffix since there's no way
  -- to know which kind of name is needed.
  lift $ qNewName ""
qsNewName str
  = QS $ do
    (suff, suff') <- ask
    let (strModule, strName) = getLastName str
    case strName of
      [] -> lift $ do
        qReportError $ "Trying to create invalid Name: \"" ++ str ++ "\"."
        qNewName str
      (c:cs)
        | isOpChar c -> lift $ do
            case (all isOpChar cs) of
              False -> do 
                qReportError $ "Trying to create operator Name with letters and/or disallowed symbols: \"" ++ str ++ "\"."
                qNewName str
              True  -> qNewName (strModule ++ (addSuffixO suff' strName))
        | (all isNameChar strName)  -> do
            case suff of
              -- Just use the same string when suffix is empty.
              [] -> lift $ qNewName str
              _  -> do
                -- Add the suffix to the string.
                let newStr = addSuffixW suff strName
                lift $ qNewName (strModule ++ newStr)
        | otherwise -> lift $ do
            qReportError $ "Trying to create function Name with special characters: \"" ++ str ++ "\"."
            let newStr = addSuffixW suff strName
            qNewName (strModule ++ newStr)

-- Add a suffix to a word (not an operator).
addSuffixW :: String -> String -> String
addSuffixW _fx "" = ""
addSuffixW sfx str = str ++ sfx

addSuffixO :: String -> String -> String
addSuffixO []  str = str
addSuffixO sfx str = str ++ sfx


instance (Quasi q) => Quasi (QST q) where
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

instance (Quasi q, Quote q) => Quote (QST q) where
  newName = qsNewName

instance (Semigroup a, Applicative q) => Semigroup (QST q a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Applicative q) => Monoid (QST q a) where
  mempty = pure mempty

-- Lifting from the bottom:
instance (QL q) => QL (QST q) where
  fromQ f = QS $ lift $ fromQ f

