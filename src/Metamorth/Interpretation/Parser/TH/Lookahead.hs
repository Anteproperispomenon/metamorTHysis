{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Interpretation.Parser.TH.Lookahead
  ( createLookahead
  , createMultiLookahead
  , createMultiLookahead2
  ) where

import Control.Applicative
import Control.Monad

import Data.Attoparsec.Text qualified as AT

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Metamorth.Helpers.Parsing

import Metamorth.Interpretation.Parser.Types

import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty(..))

import Metamorth.Helpers.TH

import Control.Monad.Trans.State.Strict qualified as St

createLookahead :: (Quote q) => {-[ModifyStateX]-} Exp -> Name -> (Exp -> Exp) -> Exp -> q Exp
createLookahead stModLamb funcName rsltCheck rslt = do
  -- let x = 0
  zNom <- newName "z"
  okay <- 
    [| do
         -- hmm...
         { newRslt <- lookAheadSX $(pure stModLamb) $ do
           { $(pure $ VarP zNom) <- NE.head <$> $(pure $ VarE funcName) 
           ; return $ $(pure $ rsltCheck (VarE zNom))
           }
         ; if newRslt then (return $(pure $ rslt)) else (fail "Failed a lookahead")
         }
    |]
  return okay

createMultiLookahead :: (Quote q) => {-[ModifyStateX]-} Exp -> Name -> [(Exp -> Exp, Exp)] -> Maybe Exp -> q Exp
createMultiLookahead stModLamb funcName rsltChecks otherRslt = do
  -- let x = 0
  zNom <- newName "z"
  okay <- 
    [| do
         -- hmm...
         -- Don't need the Bool for the casedness, since we're starting
         -- a new phoneme. The Bool for casedness only applies when
         -- running a parser midway through parsing a single phoneme.
         { $(pure $ VarP zNom) <- lookAheadSX $(pure stModLamb) $ NE.head <$> $(pure $ VarE funcName) 
         ; $(pure $ MultiIfE (ifBlocks zNom))
         }
    |]
  return ()
  return okay
  where
    otherRslt'
      | (Just oRslt) <- otherRslt
      = if (stModLamb == (VarE 'id))
        then AppE (VarE 'return) oRslt
        else InfixE (Just (AppE (VarE 'St.modify') stModLamb)) (VarE '(>>)) (Just (AppE (VarE 'return) oRslt))
      -- = [| return $(pure oRslt) |]
      | otherwise
      =  AppE (VarE 'fail) (LitE (StringL "Could not find a lookahead."))
      -- = [| fail "Could not find a lookahead." |]
    ifBlocks' :: Name -> [(Guard, Exp)]
    ifBlocks' nom = forMap rsltChecks $ \(expFunc, outp) ->
      ( NormalG $ expFunc (VarE nom)
      , AppE (VarE 'return) outp
      )
    ifBlocks :: Name -> [(Guard, Exp)]
    ifBlocks nom = ifBlocks' nom ++ [(NormalG $ VarE 'otherwise, otherRslt')]
    
-- Warning: Needs to modify state!
-- Also: What if there are multiple lookaheads that
-- make different moddifications to the state(s)?

-- MultiIfE [(Guard, Exp)]
-- data Guard = NormalG Exp | PatG [Stmt]

-- | For use when you have multiple possible
--   lookaheads that have different state modifications.
createMultiLookahead2 :: (Quote q) => Name -> [(Exp, [(Exp -> Exp, Exp)])] -> Maybe (Exp, Exp) -> q Exp
createMultiLookahead2 funcName modCases otherRslt = do
  rsltList <- forM modCases $ \(stModLamb, rsltChecks) -> 
    createMultiLookahead stModLamb funcName rsltChecks Nothing
  return $ intersperseInfixRE (VarE '(<|>)) (snocNE rsltList otherRslt')
  where
    otherRslt'
      | Just (oRslt, stMod) <- otherRslt
      = if (stMod == (VarE 'id))
        then AppE (VarE 'return) oRslt
        else InfixE (Just (AppE (VarE 'St.modify') stMod)) (VarE '(>>)) (Just (AppE (VarE 'return) oRslt))
      | otherwise
      =  AppE (VarE 'fail) (LitE (StringL "Could not find a lookahead."))

snocNE :: [a] -> a -> NonEmpty a
snocNE []     y = y :| []
snocNE (x:xs) y = x :| (xs ++ [y])

{-
lookAheadSX :: (s -> s) -> (State.StateT s AT.Parser a) -> (State.StateT s AT.Parser a)
lookAheadSX f prs = do
  st <- State.get
  lift $ AC.lookAhead (State.evalStateT prs (f $! st))
-}

-- intersperseInfixRE (VarE '(<|>))

