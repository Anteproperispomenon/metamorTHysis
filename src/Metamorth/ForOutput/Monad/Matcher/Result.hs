module Metamorth.ForOutput.Monad.Matcher.Result
  ( MatchResult(..)
  , MatchReturn(..)
  ) where

-- | How to match lists in a `MatcherT`.
data MatchResult m i v r
  -- | There is only one option, which
  --   is to consume nothing and produce
  --   a value.
  = MatchReturn (MatchReturn m i v r)
  | MatchContinue (i -> MatchResult m i v r)
  | MatchOptions (MatchReturn m i v r) (i -> MatchResult m i v r)
  | MatchFail (m String)

-- | Matching on return values.
data MatchReturn m i v r
  = PlainReturn (v -> m r)
  | ConditionalReturn (v -> Maybe i -> m r)
