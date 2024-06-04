module Metamorth.Helpers.Applicative
  ( (<%>)
  ) where

-- | Simplified version of @f <*> (pure x)@.
(<%>) :: Applicative f => f (a -> b) -> a -> f b    
f <%> x = f <*> (pure x)

infixl 4 <%>