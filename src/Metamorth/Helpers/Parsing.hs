module Metamorth.Helpers.Parsing
  ( skipHoriz
  , skipHorizontalSpace
  , skipHoriz1
  , skipHorizontalSpace1

  , takeIdentifier
  , isFollowId
  , consProd
  
  , forParseOnly
  ) where

import Data.Attoparsec.Text qualified as AT

import Data.Functor
import Data.Text qualified as T
import Data.Char (isAlphaNum)


skipHoriz :: AT.Parser ()
skipHoriz = AT.skipWhile AT.isHorizontalSpace

skipHorizontalSpace :: AT.Parser ()
skipHorizontalSpace = skipHoriz

skipHoriz1 :: AT.Parser ()
skipHoriz1 = AT.satisfy AT.isHorizontalSpace *> skipHoriz

skipHorizontalSpace1 :: AT.Parser ()
skipHorizontalSpace1 = skipHoriz1

-- | Meant for parsing identifiers where the
--   acceptable set of characters for the 
--   first character is different from the
--   acceptable set of the rest of the chars.
takeIdentifier :: (Char -> Bool) -> (Char -> Bool) -> AT.Parser T.Text
takeIdentifier p1 p2 = T.cons <$> AT.satisfy p1 <*> AT.takeWhile p2

-- | Common predicate to use for second argument
--   of `takeIdentifier`.
isFollowId :: Char -> Bool
isFollowId x = isAlphaNum x || (x == '_') || (x == '-') || (x == '\'')

forParseOnly :: T.Text -> AT.Parser a -> Either String a
forParseOnly txt prs = AT.parseOnly prs txt

-- | "Consume and produce". i.e. consume any `Char`,
--   followed by outputting the specified value.
consProd :: a -> AT.Parser a
consProd x = AT.anyChar $> x
