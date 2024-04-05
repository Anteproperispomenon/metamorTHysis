module Metamorth.ForOutput.Monad.Matcher.Result
  ( MatchResult(..)
  ) where

-- | How to match lists in a `MatcherT`.
data MatchResult m i v r
  -- | There is only one option, which
  --   is to consume nothing and produce
  --   a value.
  = MatchReturn (v -> m r)
  | MatchContinue (i -> MatchResult m i v r)
  | MatchOptions (v -> m r) (i -> MatchResult m i v r)
  | MatchFail (m String)
