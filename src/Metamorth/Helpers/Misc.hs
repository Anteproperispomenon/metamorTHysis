module Metamorth.Helpers.Misc
  ( repeatUntilEq
  ) where

-- | Repeatedly perform an operation @f@ on a
--   value @x@ until @f x == x@. To prevent
--   this function from running forever, there
--   is a maximum number of attempts.
repeatUntilEq :: (Eq a) => Int -> (a -> a) -> a -> a
repeatUntilEq n f x
  | (n <= 0)   = x
  | (f x) == x = x
  | otherwise = repeatUntilEq (n-1) f (f x)
