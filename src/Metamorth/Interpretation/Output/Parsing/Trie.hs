module Metamorth.Interpretation.Output.Parsing.Trie
  ( addOutputPattern
  ) where

import Control.Monad.Trans.RWS.CPS

import Data.Functor (($>))
import Data.Maybe

import Metamorth.Interpretation.Output.Parsing.Types
import Metamorth.Interpretation.Output.Types

import Data.Trie.Map qualified as TM

import Data.Map.Strict qualified as M

import Metamorth.Helpers.Error
import Metamorth.Helpers.Error.RWS


-- okay :: ([PhonePattern], OutputPattern) -> TM.TMap PhonePattern OutputPattern -> TM.TMap PhonePattern OutputPattern
-- okay (pp, op) tmap = insertOrig pp op tmap

insertOrig :: Ord c => [c] -> a -> TM.TMap c a -> TM.TMap c a
insertOrig cs a = TM.revise (fromMaybe a) cs

insertOrigM' :: (Monad m, Ord c) => [c] -> a -> TM.TMap c a -> ([c] -> a -> a -> m ()) -> m (TM.TMap c a)
insertOrigM' cs a tmap f = do
  let x = TM.lookup cs tmap
  case x of
    Nothing  -> return $ insertOrig cs a tmap
    (Just z) -> (f cs a z) $> tmap

-- | Insert an element into a `TM.TMap`, running a
--   monadic action if the element is already present
--   in the trie.
insertOrigM :: (Monad m, Ord c) => [c] -> a -> TM.TMap c a -> (a -> m ()) -> m (TM.TMap c a)
insertOrigM cs a tmap f = do
  let x = TM.lookup cs tmap
  case x of
    Nothing  -> return $ insertOrig cs a tmap
    (Just z) -> (f z) $> tmap

insertOrigMapM :: (Monad m, Ord c) => c -> a -> M.Map c a -> (a -> m ()) -> m (M.Map c a)
insertOrigMapM key val mp action = do
  -- okay
  let x = M.lookup key mp
  case x of
    Nothing  -> return $ M.insert key val mp
    (Just z) -> action z $> mp

modifyNodeM :: (Monad m, Ord c) => [c] -> (Maybe a -> m a) -> TM.TMap c a -> m (TM.TMap c a)
modifyNodeM cs f tmap = do
  let x = TM.lookup cs tmap
  rslt <- f x
  return $ TM.insert cs rslt tmap

modifyNodeM' :: (Monad m, Ord c) => [c] -> TM.TMap c a -> (Maybe a -> m a) -> m (TM.TMap c a)
modifyNodeM' cs tmap f = modifyNodeM cs f tmap


-- addOutputPattern :: ([PhonePattern], OutputPattern) -> OutputParser ()
-- addOutputPattern pr@(pp, op) = do
--   tmap  <- gets opsOutputTrie
--   tmap' <- insertOrigM pp op tmap $ \origVal -> do
--     warn $ "Can't insert pair " ++ show pr ++ "; key already used for \"" ++ show origVal ++ "\"."
--   modify $ \x -> x {opsOutputTrie = tmap'}

addOutputPattern :: ([PhonePattern], OutputPattern) -> OutputParser ()
addOutputPattern pr@(pp, op) = do
  tmap  <- gets opsOutputTrie
  tmap' <- modifyNodeM' pp tmap $ \case
     Nothing   -> return $ M.singleton (opCasedness op) op
     (Just mp) -> insertOrigMapM (opCasedness op) op mp $ \origVal ->
        warn $ "Can't insert pair " ++ show pr ++ "; key already used for \"" ++ show origVal ++ "\"."
  modify $ \x -> x {opsOutputTrie = tmap'}


