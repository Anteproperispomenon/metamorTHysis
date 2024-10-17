-- These are just functions originally from "Metamorth.Interaction.Quasi.Parser.Helpers"
-- that don't depend on "Metamorth.Interaction.Quasi.Parser.Types"

module Metamorth.Interaction.Quasi.Parser.Helpers.Low
  -- * Low-level helpers
  ( countHoriz
  , consumeEndComment
  , parseBool
  , (<|?>)
  -- * Parsing Items in Lists
  , parseQuoteString
  , parseQuoteText
  , parseUnquoteString
  , parseUnquoteText
  , parseQuoteLineText
  , parseQuoteLineString
  , parseUnquoteLineText
  , parseUnquoteLineString
  -- * Parsing Lists Themselves
  , parseKeySep
  , parseListSep
  -- * Parsing Indents
  , findIndent
  , indentedTo
  -- * Other Helpers
  , quotedList
  ) where

import Data.Functor

import Control.Applicative

import Control.Monad
import Control.Monad.Trans.Class

import Data.Attoparsec.Text       qualified as AT
import Data.Attoparsec.Combinator qualified as AC

import Data.Map.Strict qualified as M

import Data.Text qualified as T

import Data.Char
import Data.Maybe

import Metamorth.Helpers.Parsing
import Metamorth.Helpers.String

import Text.Printf

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

-- | More complex `T.Text` version that treats 
--   backslashes differently.
--   Note that this **DOES** work on quotations
--   that have linebreaks in them.
parseQuoteLineText :: AT.Parser T.Text
parseQuoteLineText = do
  _ <- AT.char '\"'
  parseQuoteLineText'

parseQuoteLineText' :: AT.Parser T.Text
parseQuoteLineText' = do
  txt <- AT.takeWhile (\x -> x /= '\\' && x /= '\n' && x /= '\r' && x /= '\"')
  z <- AT.peekChar
  case z of
    Nothing -> return txt
    (Just '\n') -> return txt
    (Just '\r') -> return txt
    (Just '\"') -> do
      _ <- AT.anyChar
      return txt
    (Just '\\') -> do
      _ <- AT.anyChar
      w <- AT.peekChar
      case w of
        Nothing -> return (txt `T.snoc` '\\')
        (Just 'n') -> do
          _ <- AT.anyChar
          ((txt `T.snoc` '\n') <>) <$> parseQuoteLineText'
        (Just '\\') -> do
          _ <- AT.anyChar
          ((txt `T.snoc` '\\') <>) <$> parseQuoteLineText'
        (Just '\"') -> do
          _ <- AT.anyChar
          ((txt `T.snoc` '\"') <>) <$> parseQuoteLineText'
        (Just '#') -> do
          c <- AT.anyChar
          ((txt `T.snoc` c) <>) <$> parseQuoteLineText'
        (Just '-') -> do
          c <- AT.anyChar
          ((txt `T.snoc` c) <>) <$> parseQuoteLineText'
        (Just _) -> do
          ((txt `T.snoc` '\\') <>) <$> parseQuoteLineText'
    (Just _) -> return txt
          
-- | More complex `String` version that treats 
--   backslashes differently.
--   Note that this **DOES** work on quotations
--   that have linebreaks in them.
parseQuoteLineString :: AT.Parser String
parseQuoteLineString = T.unpack <$> parseQuoteLineText

-- | Parsing a String that doesn't have spaces,
--   separators, or quotation marks.
parseUnquoteString :: AT.Parser String
parseUnquoteString = T.unpack <$> parseUnquoteText

-- | Parsing `T.Text` that doesn't have spaces,
--   separators, or quotation marks.
parseUnquoteText :: AT.Parser T.Text
parseUnquoteText = AT.takeWhile (\x -> not (isSpace x) && x /= '\"' && x /= ',' && x /= ';' && x /= '|')

-- | Parsing unquoted text for a description. This
--   is a bit more complicated, since we need to keep
--   backslashes in mind.
parseUnquoteLineText :: AT.Parser T.Text
parseUnquoteLineText = T.dropWhileEnd isSpace <$> parseUnquoteLineText'

-- So that we can run an operation on the
-- resulting `T.Text` only once. 
parseUnquoteLineText' :: AT.Parser T.Text
parseUnquoteLineText' = do
  txt <- AT.takeWhile (\x -> x /= '#' && x /= '\\' && x /= '-' && x /= '\n' && x /= '\r')
  z <- AT.peekChar
  case z of
    Nothing     -> return txt
    (Just '#' ) -> return txt
    (Just '\n') -> return txt
    (Just '\r') -> return txt
    (Just '\\') -> do
      _ <- AT.anyChar  -- consume the backslash
      x <- AT.peekChar
      case x of
        Nothing -> return (txt `T.snoc` '\\')
        (Just 'n') -> do 
          _ <- AT.anyChar
          ((txt `T.snoc` '\n') <>) <$> parseUnquoteLineText'
        (Just '#') -> do
          c <- AT.anyChar
          ((txt `T.snoc` c) <>) <$> parseUnquoteLineText'
        (Just '\\') -> do
          c <- AT.anyChar
          ((txt `T.snoc` c) <>) <$> parseUnquoteLineText'
        -- Allow any number of hyphens.
        (Just '-') -> do
          hyp <- AT.takeWhile (== '-')
          ((txt <> hyp) <>) <$> parseUnquoteLineText'
        (Just _) -> do
          -- Just proceed with the next character as normal.
          ((txt `T.snoc` '\\') <>) <$> parseUnquoteLineText'
    (Just '-') -> do
      c <- AC.lookAhead (optional "--")
      case c of
        Nothing -> do
          hyp <- AT.anyChar
          ((txt `T.snoc` hyp) <>) <$> parseUnquoteLineText'
        (Just _) -> return txt
    _ -> return txt

parseUnquoteLineString :: AT.Parser String
parseUnquoteLineString = T.unpack <$> parseUnquoteLineText

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

findIndent :: AT.Parser a -> AT.Parser (Int, a)
findIndent prs = do
  AT.skipMany consumeEndComment
  n <- countHoriz
  x <- prs
  consumeEndComment
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

quotedList :: [String] -> String
quotedList []  = "[]"
quotedList [x] = '\"' : x ++ "\""
quotedList [x,y] = '\"' : x ++ "\" and \"" ++ y ++ "\""
quotedList (x:xs) = '\"' : x ++ "\", " ++ (quotedList' xs)

quotedList' :: [String] -> String
quotedList' []  = "[]"
quotedList' [x] = '\"' : x ++ "\""
quotedList' [x,y] = '\"' : x ++ "\", and \"" ++ y ++ "\""
quotedList' (x:xs) = '\"' : x ++ "\", " ++ (quotedList' xs)

-- | A simple way to determine which of two 
--   options was chosen, where you don't care
--   about the value of the two options.
(<|?>) :: Alternative f => f a -> f b -> f Bool
op1 <|?>  op2 = (op1 $> True) <|> (op2 $> False)

infixl 3 <|?>
