module Metamorth.Interaction.Quasi.Parser.Helpers
  -- * Low-level helpers
  ( countHoriz
  , consumeEndComment
  , parseBool
  -- * Parsing Items in Lists
  , parseQuoteString
  , parseQuoteText
  , parseUnquoteString
  , parseUnquoteText
  -- * Parsing Lists Themselves
  , parseKeySep
  , parseListSep
  -- * Parsing Indents
  , findIndent
  , findIndentQQ1
  , indentedTo
  , indentedToQQ1
  ) where

import Data.Functor

import Control.Applicative

import Control.Monad
import Control.Monad.Trans.Class

import Data.Attoparsec.Text       qualified as AT
import Data.Attoparsec.Combinator qualified as AC

import Data.Text qualified as T

import Data.Char

import Metamorth.Helpers.Parsing

import Metamorth.Interaction.Quasi.Parser.Types

-- | Consume and count horizontal spaces. Tabs 
--   are treated as two spaces.
countHoriz :: AT.Parser Int
countHoriz = countHoriz' 0

countHoriz' :: Int -> AT.Parser Int
countHoriz' n = do
  c <- AT.peekChar
  case c of
    (Just '\t') -> do
        _ <- AT.anyChar
        countHoriz' (n+2)
    (Just ' ') -> do
        _ <- AT.anyChar
        countHoriz' (n+1)
    _ -> return n

-- | Variant of the one from "Metamorth.Helpers.Parsing"
--   that also accepts @'--'@ as the start of a comment.
consumeEndComment :: AT.Parser ()
consumeEndComment = do
  skipHoriz
  AT.choice [choice1, choice2, choice3]
  where
    choice1 = AT.endOfLine
    choice2 = do
      _ <- "--"
      AT.skipWhile (\x -> x /= '\n' && x /= '\r')
      AT.endOfLine
    choice3 = do
      _ <- AT.char '#'
      AT.skipWhile (\x -> x /= '\n' && x /= '\r')
      AT.endOfLine

-- | Simple `String` version that doesn't treat
--   backslashes any differently.
--   Note that this doesn't work on quotations
--   that have linebreaks in them.
parseQuoteString :: AT.Parser String
parseQuoteString = T.unpack <$> parseQuoteText

-- | Simple `T.Text` version that doesn't treat
--   backslashes any differently.
--   Note that this doesn't work on quotations
--   that have linebreaks in them.
parseQuoteText :: AT.Parser T.Text
parseQuoteText = do
  _ <- AT.char '\"'
  txt <- AT.takeWhile (\x -> x /= '\"' && x /= '\n' && x /= '\r')
  _ <- AT.char '\"'
  return txt

-- | Parsing a String that doesn't have spaces,
--   separators, or quotation marks.
parseUnquoteString :: AT.Parser String
parseUnquoteString = T.unpack <$> parseUnquoteText

-- | Parsing `T.Text` that doesn't have spaces,
--   separators, or quotation marks.
parseUnquoteText :: AT.Parser T.Text
parseUnquoteText = AT.takeWhile (\x -> not (isSpace x) && x /= '\"' && x /= ',' && x /= ';' && x /= '|')

-- | Parse the separator between a key and its value.
--   Can be either @':'@ or @'='@.
parseKeySep :: AT.Parser ()
parseKeySep = do
  skipHoriz
  _ <- AT.satisfy (\x -> x == ':' || x == '=')
  skipHoriz
  return ()

-- | Parse a separator between two items in a list.
--   It can be one of @','@, @';'@, @'|'@, or just @' '@.
--   Any of these options can be surrounded by extra spaces
--   or tabs. Note that this function is tricky to write
--   properly, so check to make sure this function is
--   working properly when using it.
parseListSep :: AT.Parser ()
parseListSep = parseListSepX <|> parseListSepY
  
-- For when an explicit separator occurs right
-- after the previous code
parseListSepX :: AT.Parser ()
parseListSepX = do
  _ <- AT.satisfy (\x -> x == ',' || x == ';' || x == '|')
  skipHoriz
  return ()

parseListSepY :: AT.Parser ()
parseListSepY = do
  skipHoriz1
  _ <- optional parseListSepX
  return ()

-- | Either runs a parser indented by a certain
--   number of spaces (followed by a comment), or 
--   parses a comment.
indentedTo :: Int -> AT.Parser a -> AT.Parser (Maybe a)
indentedTo n prs = (consumeEndComment $> Nothing) <|> do
  x <- countHoriz
  -- Might want to rethink how failure occurs...
  when (x /= n) $ do
    txt <- AT.takeWhile (\c -> c /= '\n' && c /= '\r')
    fail $ "The following line is incorrectly indented:\n\"" ++ (T.unpack txt) ++ "\""
  rslt <- prs
  consumeEndComment
  return $ Just rslt

-- | Either runs a parser indented by a certain
--   number of spaces (followed by a comment), or 
--   parses a comment.
indentedToQQ1 :: Int -> ParserQQ1 a -> ParserQQ1 (Maybe a)
indentedToQQ1 n prs = (liftQQ1 consumeEndComment $> Nothing) <|> do
  x <- liftQQ1 countHoriz
  case x of
    0 -> fail "End of this Orthography"
    z | z == n -> do
      rslt <- prs
      liftQQ1 consumeEndComment
      return $ Just rslt
    _ -> do
      txt <- liftQQ1 $ AC.lookAhead $ AT.takeWhile (\c -> c /= '\n' && c /= '\r')
      lift $ tellError $ "The following line is incorrectly indented:\n\"" ++ (T.unpack txt) ++ "\""
      return Nothing

findIndent :: AT.Parser a -> AT.Parser (Int, a)
findIndent prs = do
  AT.skipMany consumeEndComment
  n <- countHoriz
  x <- prs
  consumeEndComment
  return (n,x)
  
findIndentQQ1 :: ParserQQ1 a -> ParserQQ1 (Int, a)
findIndentQQ1 prs = do
  liftQQ1 $ AT.skipMany consumeEndComment
  n <- liftQQ1 countHoriz
  x <- prs
  liftQQ1 consumeEndComment
  return (n,x)

parseBool :: AT.Parser Bool
parseBool = do
  txt <- AT.takeWhile isAlpha
  case (checkBoolString txt) of
    (Just True)  -> return True
    (Just False) -> return False
    Nothing      -> fail $ "Couldn't parse string as boolean: \"" ++ T.unpack txt ++ "\"." 

checkBoolString :: T.Text -> Maybe Bool
checkBoolString txt
  | (txt' == "yes" || txt' == "y" || txt' == "t" || txt' == "true"  || txt' == "on" ) = Just True
  | (txt' == "no"  || txt' == "n" || txt' == "f" || txt' == "false" || txt' == "off") = Just False
  | otherwise = Nothing
  where txt' = T.toLower txt

