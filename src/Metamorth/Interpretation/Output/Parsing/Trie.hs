module Metamorth.Interpretation.Output.Parsing.Trie
  ( addOutputPattern
  ) where

import Control.Monad.Trans.RWS.CPS

import Data.Functor (($>))
import Data.Maybe

import Metamorth.Interpretation.Output.Parsing.Types
import Metamorth.Interpretation.Output.Types

import Data.Trie.Map qualified as TM

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


addOutputPattern :: ([PhonePattern], OutputPattern) -> OutputParser ()
addOutputPattern pr@(pp, op) = do
  tmap  <- gets opsOutputTrie
  tmap' <- insertOrigM pp op tmap $ \origVal -> do
    warn $ "Can't insert pair " ++ show pr ++ "; key already used for \"" ++ show origVal ++ "\"."
  modify $ \x -> x {opsOutputTrie = tmap'}


