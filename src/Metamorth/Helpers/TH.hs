{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Helpers.TH
  ( dataName
  , varName
  , maybeType
  , sumAdtDecDeriv
  , recordAdtDecDeriv
  , showSumInstance
  , showSumProdInstance
  , showSumProdInstanceAlt
  , intersperseInfixRE
  , nestedConPat
  , groupCaseGuards
  , forMap
  , first
  , second
  -- * Basic helpers
  , stringExp
  , charE
  ) where

import Control.Monad

import Data.Char
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty(..))

import Data.List (intersperse, partition)

import Language.Haskell.TH.Syntax

import THLego.Helpers (sumCon, fieldBang, intersperseInfixE)

import GHC.Show qualified as GHCShow

maybeType :: Type -> Type
maybeType typ = AppT (ConT ''Maybe) typ

-- | Variant of `fmap` but with the arguments
--   flipped. Meant for use in code such as
-- 
--   > forMap myList $ \(x,y) -> ...
forMap :: (Functor f) => f a -> (a -> b) -> f b
forMap m f = fmap f m
{-# INLINE forMap #-}

-- | Stolen from Control.Arrow, but
--   specialised to Functions.
first :: (a -> b) -> (a, a') -> (b, a')
first f (x,y) = (f x, y)
{-# INLINE second #-}

-- | Stolen from Control.Arrow, but
--   specialised to Functions.
second :: (a' -> b) -> (a, a') -> (a, b)
second f (x,y) = (x, f y)
{-# INLINE first #-}

-- May be a better way to do this.
dataName :: String -> String
dataName "" = ""
dataName (x:xs) = (toUpper x : xs)

varName :: String -> String
varName "" = ""
varName (x:xs) = (toLower x : xs)

sumAdtDecDeriv :: Name -> [(Name, [Type])] -> [Type] -> Dec
sumAdtDecDeriv a b ders =
  DataD [] a [] Nothing (fmap (uncurry sumCon) b) [DerivClause Nothing ders]

recordAdtDecDeriv :: Name -> [(Name, Type)] -> [Type] -> Dec
recordAdtDecDeriv typeName fields ders =
  DataD [] typeName [] Nothing [con] [DerivClause Nothing ders]
  where
    con =
      RecC typeName (fmap (\(fieldName, fieldType) -> (fieldName, fieldBang, fieldType)) fields)

stringExp :: String -> Exp
stringExp = LitE . StringL

-- | Create a `Show` instance for a simple sum
--   type where each constructor is represented
--   by a given `String`.
showSumInstance :: Name -> [(Name, String)] -> [Dec]
showSumInstance typeName prs = 
  [InstanceD Nothing [] (AppT (ConT ''Show) (ConT typeName) )
    [ FunD 'show $ map (\(nm,str) -> Clause [ConP nm [] []] (NormalB (LitE (StringL str))) []) prs]
  ]

-- | Create `Show` instances for a "sum of products" type.
--   i.e. types that look like:
-- 
--   > data Example = Ex1 Int Bool String | Ex2 Bool Word8 | Ex3 Int Int
-- 
--   The resulting `Show` instance will look something like this:
--   
--   > instance Show Example where
--   >   show (Ex1 x1 x2 x3) = "Ex1 " <> (show x1) <> " " <> (show x2) <> " " (show x3)
--   >   show (Ex2 x1 x2)    = "Ex2 " <> (show x1) <> " " <> (show x2)
--   >   show (Ex3 x1 x2)    = "Ex3 " <> (show x1) <> " " <> (show x2)
--
--   ...but with the value of the first string (i.e. "Ex1" etc...) 
--   defined by the user (the second value in each triple).
showSumProdInstance :: Name -> [(Name, String, Int)] -> Q [Dec]
showSumProdInstance typeName prs = do
  clauses <- mapM showSumProdClause prs
  return 
    [ InstanceD Nothing [] (AppT (ConT ''Show) (ConT typeName))
        [ FunD 'show clauses ]
    ]

-- | Like `showSumProdInstance`, but with a different way of turning
--   the sum/product type into a string. Again, for a type like
-- 
--   > data Example = Ex1 Int Bool String | Ex2 Bool Word8 | Ex3 Int Int
-- 
--   ... the resulting `Show` instance would look like...
--
--   > instance `Show` Example where
--   >   show (Ex1 x1 x2 x3) = "Ex1[" <> (show x1) <> " " <> (show x2) <> " " <> show x3 <> "]"
--   >   show (Ex2 x1 x2)    = "Ex2[" <> (show x1) <> " " <> (show x2) <> "]"
--   >   show (Ex3 x1 x2)    = "Ex3[" <> (show x1) <> " " <> (show x2) <> "]"
showSumProdInstanceAlt :: Name -> [(Name, String, Int)] -> Q [Dec]
showSumProdInstanceAlt typeName prs = do
  clauses <- mapM showSumProdClauseAlt prs
  return 
    [ InstanceD Nothing [] (AppT (ConT ''Show) (ConT typeName))
        [ FunD 'show clauses ]
    ]

showSumProdClause :: (Name, String, Int) -> Q Clause
showSumProdClause (phoneName, phoneString, n) 
  | (n <= 0) = return $ Clause [ConP (phoneName) [] []] (NormalB (LitE (StringL phoneString))) []
  | otherwise = do
      -- Generate the variable names
      phoneVars <- replicateM n (newName "x")
      let varps   = map VarP phoneVars
          vares   = map VarE phoneVars
          clauseP = [ConP phoneName [] varps]
          spc = LitE (StringL " ")
      return $ Clause clauseP 
        (NormalB (intersperseInfixE (VarE '(<>)) ((LitE (StringL (phoneString <> " "))) :| (intersperse spc (map (AppE (VarE 'show)) vares))  )  )) []

showSumProdClauseAlt :: (Name, String, Int) -> Q Clause
showSumProdClauseAlt (phoneName, phoneString, n) 
  | (n <= 0) = return $ Clause [ConP (phoneName) [] []] (NormalB (LitE (StringL phoneString))) []
  | otherwise = do
      -- Generate the variable names
      phoneVars <- replicateM n (newName "x")
      let varps   = map VarP phoneVars
          vares   = map VarE phoneVars
          clauseP = [ConP phoneName [] varps]
          spc  = LitE (StringL " ")
          endB = LitE (StringL "]")
      return $ Clause clauseP 
        (NormalB (intersperseInfixE (VarE '(<>)) ((LitE (StringL (phoneString <> "["))) :| ((intersperse spc (map (AppE (VarE 'show)) vares)) <> [endB])  )  )) []

nestedConPat :: [Name] -> Name -> [Pat] -> Pat
nestedConPat [] nom pats     = ConP nom [] pats
nestedConPat (c:cs) nom pats = ConP   c [] [nestedConPat cs nom pats]

-- | Essentially the same as `THLego.Helpers.intersperseInfixRE`, but
--   with a right-fold instead of a left-fold. For example, this means
--   that 
--
--   @
--   intersperseInfixRE (||) [x,y,z,w]
--   == (x || (y || (z || w)))
--   @
-- 
--   Instead of:
-- 
--   @
--   intersperseInfixE  (||) [x,y,z,w]
--   == (((x || y) || z) || w)
--   @
intersperseInfixRE :: Exp -> NonEmpty Exp -> Exp
intersperseInfixRE op =
  foldr1 (\l r -> InfixE (Just l) op (Just r))

-- | Group guarded values in a case-expression.
--   Useful if the code you generate has multiple
--   cases for the same pattern, e.g.
--
--   @
--   case c of 
--     'a' | (val1 == z && val2 == y) -> ...
--     ...
--     'a' | (val1 == z) -> ...
--     ...
--     'a' | (val3 == r) -> ...
--     ...
--     'a' -> ...
--   @
--
--   This function should group together such cases
--   into something like
--
--   @
--   case c of
--     'a' | (val1 == z && val2 == y) -> ...
--         | (val1 == z)              -> ...
--         | (val3 == r)              -> ...
--         | otherwise                -> ...
--   @
--
--   At present, it will only group together conditions
--   with @where@ statements if the `Decs` of the @where@
--   statement are identical.
--
--   Note that this function runs in O(n^2).
groupCaseGuards :: Exp -> Exp
groupCaseGuards (CaseE expr mtchs) = CaseE expr (groupCaseGuards' mtchs)
groupCaseGuards expr = expr

groupCaseGuards' :: [Match] -> [Match]
groupCaseGuards' []  = []
groupCaseGuards' [m] = [m]
groupCaseGuards' (m@(Match ptrn bdy dcs):mtchs)
  | null sameMatches = m : (groupCaseGuards' mtchs)
  | otherwise = (Match ptrn (GuardedB newGuards) dcs) : (groupCaseGuards' restMatches)
  where
    checkMatch (Match ptrn' _bdy dcs') = (ptrn == ptrn') && (dcs' == dcs)
    (sameMatches, restMatches) = partition checkMatch mtchs
    newGuards = concatMap (guardifyBody . getBody) (m:sameMatches)
    
    -- Modifying the remaining Matches
    getBody :: Match -> Body
    getBody (Match _ bdy _) = bdy
    otherwiseE :: Exp
    otherwiseE = VarE 'otherwise
    guardifyBody :: Body -> [(Guard, Exp)]
    guardifyBody (GuardedB grds) = grds
    guardifyBody (NormalB  expr) = [(NormalG otherwiseE,expr)]

------------------------------------------------
-- More basic functions

charE :: Char -> Exp
charE = LitE . CharL 


-- [ConP Data.Either.Right [] [VarP x_1]]

-- [d| {asdf :: Either () Int -> Maybe Int ; asdf (Left _) = Nothing ; asdf (Right x) = Just x } |]

-- 

{-
[InstanceD Nothing [] (AppT (ConT GHC.Show.Show) (ConT Ghci4.ABC)) 
  [FunD GHC.Show.show 
    [Clause [ConP Ghci4.XYZ  [] []] (NormalB (LitE (StringL "xyz" ))) []
    ,Clause [ConP Ghci4.ZXCV [] []] (NormalB (LitE (StringL "zxcv"))) []
    ,Clause [ConP Ghci4.TPT  [] []] (NormalB (LitE (StringL "tpt" ))) []
    ,Clause [ConP Ghci4.A73  [] []] (NormalB (LitE (StringL "a73" ))) []
    ]
  ]
]
-}

{-
[InstanceD Nothing [] (AppT (ConT GHC.Show.Show) (ConT Ghci6.QWERT)) 
  [FunD GHC.Show.show 
    [Clause [ConP Ghci6.ASDF [] []] (NormalB (LitE (StringL "asdf"))) []
    ,Clause [ConP Ghci6.YUIOP [] [VarP x_2]] (NormalB (InfixE (Just (LitE (StringL "yuiop "))) (VarE GHC.Base.<>) (Just (AppE (VarE GHC.Show.show) (VarE x_2))))) []
    ,Clause [ConP Ghci6.MMMM [] []] (NormalB (LitE (StringL "mmmm"))) []
    ]
  ]
]
-}

{-
ghci> data ABC = XYZ | ZXCV | TPT | A73
ghci> [d| instance Show ABC where {show XYZ = "xyz" ; show ZXCV = "zxcv" ; show TPT = "tpt" ; show A73 = "a73"} |]

data ABC = XYZ | ZXCV | TPT | A73
data QWERT = ASDF | YUIOP ABC | MMMM

[d| instance Show QWERT where {show ASDF = "asdf" ; show (YUIOP x) = "yuiop " <> show x; show MMMM = "mmmm"} |]

[d| instance Show QWERT where {show ASDF = "asdf" ; show (YUIOP x) = "yuiop " <> show x; show MMMM = "mmmm"} |]
[InstanceD Nothing [] (AppT (ConT GHC.Show.Show) (ConT Ghci6.QWERT)) [FunD GHC.Show.show [Clause [ConP Ghci6.ASDF [] []] (NormalB (LitE (StringL "asdf"))) [],Clause [ConP Ghci6.YUIOP [] [VarP x_2]] (NormalB (InfixE (Just (LitE (StringL "yuiop "))) (VarE GHC.Base.<>) (Just (AppE (VarE GHC.Show.show) (VarE x_2))))) [],Clause [ConP Ghci6.MMMM [] []] (NormalB (LitE (StringL "mmmm"))) []]]]

-}
