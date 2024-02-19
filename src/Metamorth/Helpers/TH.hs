module Metamorth.Helpers.TH
  ( dataName
  , sumAdtDecDeriv
  ) where

import Data.Char

import Language.Haskell.TH.Syntax

import THLego.Helpers (sumCon)

-- May be a better way to do this.
dataName :: String -> String
dataName "" = ""
dataName (x:xs) = (toUpper x : xs)

sumAdtDecDeriv :: Name -> [(Name, [Type])] -> [Type] -> Dec
sumAdtDecDeriv a b ders =
  DataD [] a [] Nothing (fmap (uncurry sumCon) b) (map (\cl -> DerivClause Nothing [cl]) ders)