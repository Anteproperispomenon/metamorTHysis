module Metamorth.Interpretation.Output.Parsing.Trie
  ( addOutputPattern
  , addOutputPatternPlain
  , insertNodeS
  ) where

import Control.Monad.Trans.RWS.CPS

import Data.Functor (($>))
import Data.Maybe

import Metamorth.Interpretation.Output.Parsing.Types
import Metamorth.Interpretation.Output.Types
import Metamorth.Interpretation.Output.Types.Alt

import Data.Trie.Map qualified as TM

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

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

insertNodeS :: (Ord c, Ord p) => [c] -> p -> TM.TMap c (S.Set p) -> TM.TMap c (S.Set p)
insertNodeS cs p = TM.revise ins cs
  where
    ins Nothing   = S.singleton p
    ins (Just st) = S.insert p st

-- addOutputPattern :: ([PhonePattern], OutputPattern) -> OutputParser ()
-- addOutputPattern pr@(pp, op) = do
--   tmap  <- gets opsOutputTrie
--   tmap' <- insertOrigM pp op tmap $ \origVal -> do
--     warn $ "Can't insert pair " ++ show pr ++ "; key already used for \"" ++ show origVal ++ "\"."
--   modify $ \x -> x {opsOutputTrie = tmap'}

addOutputPatternPlain :: ([PhonePattern], OutputPattern) -> TM.TMap PhonePatternAlt (S.Set PhoneResult) -> TM.TMap PhonePatternAlt (S.Set PhoneResult)
addOutputPatternPlain prOld = insertNodeS pp op
  where (pp,op) = renewOutputPattern prOld

addOutputPattern :: ([PhonePattern], OutputPattern) -> OutputParser ()
addOutputPattern prOld = do
  let (pp, op) = renewOutputPattern prOld
  tmap <- gets opsOutputTrie
  let tmap' = insertNodeS pp op tmap
  modify' $ \x -> x {opsOutputTrie = tmap'}
  
  -- tmap' <- modifyNodeM' pp tmap $ \case
  --    Nothing   -> return $ M.singleton (opCasedness op) op
  --    (Just mp) -> insertOrigMapM (opCasedness op) op mp $ \origVal ->
  --       warn $ "Can't insert pair " ++ show pr ++ "; key already used for \"" ++ show origVal ++ "\"."
  -- modify $ \x -> x {opsOutputTrie = tmap'}

modify' :: (Monad m) => (s -> s) -> RWST r w s m ()
modify' f = do
  s <- get
  put $! f s

-- Example Patterns

-- ([PhonemeName [] "K", PhonemeName [] "A"], (OutputPattern (CharPattern [(CasableChar 'k'), (CasableChar 'a')] []) OCNull))
-- ([PhonemeName [] "K", PhonemeName [] "E"], (OutputPattern (CharPattern [(CasableChar 'k'), (CasableChar 'e')] []) OCNull))



