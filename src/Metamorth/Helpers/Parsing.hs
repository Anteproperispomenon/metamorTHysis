module Metamorth.Helpers.Parsing
  -- * Skipping White Space
  ( skipHoriz
  , skipHorizontalSpace
  , skipHoriz1
  , skipHorizontalSpace1

  -- * Parsing identifiers
  , takeIdentifier
  , isFollowId
  , consProd
  
  -- * Re-ordered Functions
  , forParseOnly
  
  -- * Combinators lifted over `StateT`
  , lookAheadS
  , lookAheadS'

  -- * Re-Exports
  , (AT.<?>)
  ) where

import Data.Attoparsec.Text       qualified as AT
import Data.Attoparsec.Combinator qualified as AC

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict qualified as State

import Data.Functor
import Data.Text qualified as T
import Data.Char (isAlphaNum)

-- | Skip horizontal spaces until a non-space
--   or non-horizontal space is encountered.
skipHoriz :: AT.Parser ()
skipHoriz = AT.skipWhile AT.isHorizontalSpace

-- | Synonym for `skipHoriz`.
skipHorizontalSpace :: AT.Parser ()
skipHorizontalSpace = skipHoriz

-- | Skip horizontal spaces like `skipHoriz`, but
--   ensures at least one horizontal space is
--   encountered. Very useful to use between
--   identifiers, e.g.
--
--   @`AT.sepBy1` (`takeIdentifier` `Data.Char.isLower` `isFollowId`) `skipHoriz1`@
--
skipHoriz1 :: AT.Parser ()
skipHoriz1 = AT.satisfy AT.isHorizontalSpace *> skipHoriz

-- | Synonym for `skipHoriz1`.
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

-- | The same as `AT.parseOnly` but with a different argument order.
forParseOnly :: T.Text -> AT.Parser a -> Either String a
forParseOnly txt prs = AT.parseOnly prs txt

-- | "Consume and produce". i.e. consume any `Char`,
--   followed by outputting the specified value.
consProd :: a -> AT.Parser a
consProd x = AT.anyChar $> x

-- | Like `AC.lookAhead`, but lifted to work
--   over states. This version also doesn't
--   return the state calculated during the 
--   previewed action. Note that the state of the 
--   resulting computation is "rewound" back to 
--   where it started. If you're looking for
--   a version that does return the state
--   of the previewed action, see `lookAheadS'`.
lookAheadS :: (State.StateT s AT.Parser a) -> (State.StateT s AT.Parser a)
lookAheadS prs = do
  st <- State.get
  lift $ AC.lookAhead (State.evalStateT prs st)

-- | Like `AC.lookAhead`, but lifted to work
--   over states. This version also returns
--   the state calculated during the previewed
--   action. Note that the state of the main
--   computation is "rewound" back to where it
--   started.
lookAheadS' :: (State.StateT s AT.Parser a) -> (State.StateT s AT.Parser (a,s))
lookAheadS' prs = do
  st <- State.get
  lift $ AC.lookAhead (State.runStateT prs st)

