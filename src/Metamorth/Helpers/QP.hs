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
 -- * Main Monad
 ( QP
 , QPT
 , runQP
 , runQP2
 -- * Additional Helpers
 , liftQP
 , qpNewName
 , qpPlainNewName
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

import Data.Char 
-- import Data.List (span, break, dropWhile)

import Metamorth.Helpers.Q

-- | @QP@ is a `ReaderT` over `Q` that automatically adds
--   prefices to Names created with `newName` or `qNewName`.
--   To run it, just use `runQP` or `runQP2`. e.g.
--
--   @
--   makeFunc :: String -> QP Dec
--   makeFunc myName = do
--     theName <- newName myName
--     [d| $(pure $ VarP theName) = \\x -> x * x |]
--   
--   createFunc :: String -> String -> Q Dec
--   createFunc pfx myName = runQP pfx $ makeFunc myName
--   
--   @
type QP = QPT Q
-- Need to write it like this and not `type QP a = QPT Q a`, since
-- that would cause problems when using it with Monad transformers.

-- | @QPT@ is a variant of `QP` that works over any
--   instance of `Quasi` and `Quote`.
--   Note that `runQP` and `runQP2` work on both
--   `QP` and `QPT`.
newtype QPT q a = QP { getQP :: ReaderT (String, String) q a}
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

-- | Lift an operation in `Q` to one in `QP`.
--   Note that if the action calls `newName`,
--   it won't add the prefix to it.
liftQP :: (Quote q, Quasi q) => q a -> QPT q a
liftQP action = QP $ lift action

-- | The main way to run a QP function.
--   Note that it will fail if you try
--   to feed it a prefix that doesn't
--   start with an ASCII letter. Numbers,
--   punctuation, etc... will fail.
--   However, you don't have to worry
--   about ensuring that the `String` is
--   upper-case or lower-case, since the
--   `qpNewName` function will automatically
--   handle conversions. However, empty
--   strings are accepted.
runQP :: (Quasi q, Quote q) => String -> QPT q a -> q a
runQP []  qp = do
  qReportWarning "Running a QP action with the empty string."  
  runReaderT (getQP qp) ([], [])
runQP str@(c:_cs) qp
  -- Prefix must work for 
  | ((isAsciiUpper c) || (isAsciiLower c)) = runReaderT (getQP qp) (str, [])
  | otherwise = fail $ "Can't run QP with a prefix that doesn't start with an ASCII letter; given \"" <> str <> "\"."

-- | Like `runQP`, but also specifies a prefix
--   for infix operators. If you aren't planning
--   to define any infix operators, or don't want
--   them to use a prefix, just use `runQP` instead.
runQP2 :: (Quasi q, Quote q) => String -> String -> QPT q a -> q a
runQP2 [] []  qp = do
  qReportWarning "Running a QP action with two empty strings."
  runReaderT (getQP qp) ([], [])
runQP2 [] infPre qp
  | (all isOpChar infPre) = runReaderT (getQP qp) ([], infPre)
  | otherwise = fail $ "Can't run QP with an operator prefix that has disallowed characters; given \"" ++ infPre ++ "\"."
runQP2 str@(c:_) [] qp
  -- Prefix must work for 
  | ((isAsciiUpper c) || (isAsciiLower c)) = runReaderT (getQP qp) (str, [])
  | otherwise = fail $ "Can't run QP with a prefix that doesn't start with an ASCII letter; given \"" <> str <> "\"."
runQP2 str@(c:_) infPre qp
  | (((isAsciiUpper c) || (isAsciiLower c)) && (all isOpChar infPre))
  = runReaderT (getQP qp) (str, infPre)
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
--
--   Note that this function only goes
--   one level up; if you want to generate
--   a top-level name, use `newTopName`
--   instead.
qpPlainNewName :: (Quote q) => String -> QPT q Name
qpPlainNewName str = QP $ lift $ newName str

-- | The main function to create a new `Name`
--   with a prefix. You don't actually have
--   to use this function directly; `newName` 
--   and `qNewName` just call this function.
qpNewName :: (Quasi q) => String -> QPT q Name
qpNewName []  = QP $ do
  lift $ qReportError "Trying to create a Name from the empty string."
  -- Not adding the prefix since there's no way
  -- to know which kind of name is needed.
  lift $ qNewName ""
qpNewName str
  = QP $ do
    (pref, pref') <- ask
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
              True  -> qNewName (strModule ++ (addPrefixO pref' strName))
        | otherwise  -> do
            case pref of
              -- Just use the same string when prefix is empty.
              [] -> lift $ qNewName str
              _  -> do
                -- Add the prefix to the string.
                let (newStr, gotError) = addPrefixW pref strName
                when gotError $ lift $ qReportWarning $ "Encountered unusual Name in QP : \"" <> str <> "\"."
                lift $ qNewName (strModule ++ newStr)



-- Add a prefix to a word (not an operator).
addPrefixW :: String -> String -> (String, Bool)
addPrefixW _pfx "" = ("", False) -- already covered
addPrefixW  pfx str@(c:_)
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

instance (Quasi q) => Quasi (QPT q) where
  qNewName = qpNewName
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

instance (Quote q, Quasi q) => Quote (QPT q) where
  newName = qpNewName

instance (Semigroup a, Applicative q) => Semigroup (QPT q a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Applicative q) => Monoid (QPT q a) where
  mempty = pure mempty

-- Lifting from the bottom:
instance (QL q) => QL (QPT q) where
  fromQ f = QP $ lift $ fromQ f

