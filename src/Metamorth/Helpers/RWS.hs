module Metamorth.Helpers.RWS
  -- * Re-Ordered Runners
  -- ** Non-Monadic
  ( rwsRun
  , rwsEval
  , rwsExec
  -- ** Monadic
  , rwsRunT
  , rwsEvalT
  , rwsExecT
  -- * Extra Functions
  , modify'
  ) where

import Control.Monad.Trans.RWS.CPS qualified as RWS

-- | Like `RWS.runRWS`, but opposite argument order.
rwsRun :: Monoid w => r -> s -> RWS.RWS r w s a -> (a, s, w)
rwsRun rdr st op = RWS.runRWS op rdr st
{-# INLINE rwsRun #-}

-- | Like `RWS.evalRWS`, but opposite argument order.
rwsEval :: Monoid w => r -> s -> RWS.RWS r w s a -> (a,w)
rwsEval rdr st op = RWS.evalRWS op rdr st
{-# INLINE rwsEval #-}

-- | Like `RWS.execState`, but opposite argument order.
rwsExec :: Monoid w => r -> s -> RWS.RWS r w s a -> (s, w)
rwsExec rdr st op = RWS.execRWS op rdr st
{-# INLINE rwsExec #-}

-- | Like `RWS.runRWST`, but opposite argument order.
rwsRunT :: Monoid w => r -> s -> RWS.RWST r w s m a -> m (a, s, w)
rwsRunT rdr st op = RWS.runRWST op rdr st
{-# INLINE rwsRunT #-}

-- | Like `RWS.evalRWST`, but opposite argument order.
rwsEvalT :: (Monad m, Monoid w) => r -> s -> RWS.RWST r w s m a -> m (a,w)
rwsEvalT rdr st op = RWS.evalRWST op rdr st
{-# INLINE rwsEvalT #-}

-- | Like `RWS.execRWST`, but opposite argument order.
rwsExecT :: (Monad m, Monoid w) => r -> s -> RWS.RWST r w s m a -> m (s, w)
rwsExecT rdr st op = RWS.execRWST op rdr st
{-# INLINE rwsExecT #-}

-- | Strict variant of `RWS.modify`.
modify' :: (Monad m) => (s -> s) -> RWS.RWST r w s m ()
modify' f = do
  s <- RWS.get
  RWS.put $! f s
