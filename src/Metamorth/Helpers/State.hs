module Metamorth.Helpers.State
  ( stateRun
  , stateEval
  , stateExec
  , stateRunT
  , stateEvalT
  , stateExecT
  ) where

import Control.Monad.Trans.State.Strict qualified as State

-- | Like `State.runState`, but opposite argument order.
stateRun :: s -> State.State s a -> (a, s)
stateRun st op = State.runState op st
{-# INLINE stateRun #-}

-- | Like `State.evalState`, but opposite argument order.
stateEval :: s -> State.State s a -> a
stateEval st op = State.evalState op st
{-# INLINE stateEval #-}

-- | Like `State.execState`, but opposite argument order.
stateExec :: s -> State.State s a -> s
stateExec st op = State.execState op st
{-# INLINE stateExec #-}


-- | Like `State.runState`, but opposite argument order.
stateRunT :: Monad m => s -> State.StateT s m a -> m (a, s)
stateRunT st op = State.runStateT op st
{-# INLINE stateRunT #-}

-- | Like `State.evalState`, but opposite argument order.
stateEvalT :: Monad m => s -> State.StateT s m a -> m a
stateEvalT st op = State.evalStateT op st
{-# INLINE stateEvalT #-}

-- | Like `State.execState`, but opposite argument order.
stateExecT :: Monad m => s -> State.StateT s m a -> m s
stateExecT st op = State.execStateT op st
{-# INLINE stateExecT #-}
