module Metamorth.Helpers.Parsing
  -- * Skipping White Space
  ( skipHoriz
  , skipHorizontalSpace
  , skipHoriz1
  , skipHorizontalSpace1
  , parseEndComment

  -- * Parsing identifiers
  , takeIdentifier
  , isFollowId
  , isFileId
  , consProd
  
  -- * More Combinators
  , many_
  , some_
  , many'_
  , some'_

  -- * Re-ordered Functions
  , forParseOnly
  
  -- * Combinators lifted over `State.StateT`
  , lookAheadS
  , lookAheadS'

  -- * Combinators lifted over `RWS.RWST`
  , lookAheadRWS
  , lookAheadEvalRWS
  , lookAheadTellRWS
  , lookAheadTellRWS'

  -- * Other Parsers
  , parseFileName

  -- * Re-Exports
  , (AT.<?>)
  ) where

import Data.Attoparsec.Text       qualified as AT
import Data.Attoparsec.Combinator qualified as AC

import Control.Applicative
import Control.Monad

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict qualified as State
import Control.Monad.Trans.RWS.CPS      qualified as RWS

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

-- | Like `isFollowId`, but meant for filenames.
isFileId :: Char -> Bool
isFileId x = isAlphaNum x || (x == '_') || (x == '-') || (x == '.')

-- | Parse a file name that may be in quotations.
parseFileName :: AT.Parser T.Text
parseFileName = do
  c <- AT.peekChar'
  case c of
    '\"' -> parseFileName1 AT.<?> "Encountered bad character before closing quotation mark"
    _    -> parseFileName2 AT.<?> "Couldn't parse file name"

parseFileName1 :: AT.Parser T.Text
parseFileName1 = do
  _ <- AT.char '\"'
  txt <- AT.takeWhile1 chk
  _ <- AT.char '\"'
  return txt
  where
    chk c = (isAlphaNum c) || (c == '_') || (c == '-') || (c == '/') || (c == '\\') || (c == '.') || (c == ' ')

parseFileName2 :: AT.Parser T.Text
parseFileName2 = AT.takeWhile $ \c -> (isAlphaNum c) || (c == '_') || (c == '-') || (c == '/') || (c == '\\') || (c == '.')

-- | Parse the end of line, possibly preceded by a comment.
parseEndComment :: AT.Parser ()
parseEndComment = do
  skipHoriz
  AT.option () $ do
    _ <- AT.char '#'
    AT.skipWhile (\x -> x /= '\n' && x /= '\r')
  AT.endOfLine


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

-- | Lift `AC.lookAhead` over `RWS.RWST`, returning
--   the value, state, and writer of the computation.
lookAheadRWS :: (Monoid w) => (RWS.RWST r w s AT.Parser a) -> (RWS.RWST r w s AT.Parser (a, s, w))
lookAheadRWS prs = do
  st <- RWS.get
  rd <- RWS.ask
  lift $ AC.lookAhead (RWS.runRWST prs rd st)

-- | Lift `AC.lookAhead` over `RWS.RWST`, only returning
--   the output of the look-ahead.
lookAheadEvalRWS :: (Monoid w) => (RWS.RWST r w s AT.Parser a) -> (RWS.RWST r w s AT.Parser a)
lookAheadEvalRWS prs = do
  (a, _s, _w) <- lookAheadRWS prs
  return a

-- | Lift `AC.lookAhead` over `RWS.RWST`, writing the
--   writer output to the main writer, and returning
--   output but not the state.
lookAheadTellRWS :: (Monoid w) => (RWS.RWST r w s AT.Parser a) -> (RWS.RWST r w s AT.Parser a)
lookAheadTellRWS prs = do
  (a, _s, w) <- lookAheadRWS prs
  RWS.tell w
  return a

-- | Like `lookAheadRWS`, but instead of returning
--   the writer's output, it writes it to the main
--   writer.
lookAheadTellRWS' :: (Monoid w) => (RWS.RWST r w s AT.Parser a) -> (RWS.RWST r w s AT.Parser (a,s))
lookAheadTellRWS' prs = do
  (a, s, w) <- lookAheadRWS prs
  RWS.tell w
  return (a,s)

many_ :: Alternative f => f a -> f ()
many_ f = many f $> ()

some_ :: Alternative f => f a -> f ()
some_ f = some f $> ()

many'_ :: MonadPlus m => m a -> m ()
many'_ f = AT.many' f $> ()

some'_ :: MonadPlus m => m a -> m ()
some'_ f = AT.many1' f $> ()
