{-|
Module      : Metamorth.Interpretation.Parser.Parsing.Expr
Description : Boolean Tree Parsing
Copyright   : (c) David Wilson, 2024
License     : BSD-3

This module is for parsing expressions into
the boolean trees seen in "Metamorth.Interpretation.Parser.Parsing.Expr".

-}

module Metamorth.Interpretation.Parser.Parsing.Expr
  ( parseBoolean
  , parseParens
  , parseBooleanExpr
  , parseBooleanExpr'
  ) where

import Metamorth.Interpretation.Parser.Parsing.Boolean

import Data.Attoparsec.Text       qualified as AT
import Data.Attoparsec.Combinator qualified as AC

import Control.Monad

import Control.Monad.Combinators
import Control.Monad.Combinators.Expr

import Metamorth.Helpers.Parsing

parseOr :: AT.Parser (Boolean2 a -> Boolean2 a -> Boolean2 a)
parseOr = do
  skipHoriz
  AT.char '|'
  c <- AT.peekChar
  -- Allow for double bars...
  case c of
    (Just '|') -> void AT.anyChar
    _ -> return ()
  skipHoriz
  return OrB2

parseAnd :: AT.Parser (Boolean2 a -> Boolean2 a -> Boolean2 a)
parseAnd = do
  skipHoriz
  _ <- AT.char '&'
  c <- AT.peekChar
  -- Allow for double ampersands...
  case c of
    (Just '&') -> void AT.anyChar
    _ -> return ()
  skipHoriz
  return OrB2

parseNot :: AT.Parser (Boolean2 a -> Boolean2 a)
parseNot = do
  skipHoriz
  _ <- AT.satisfy (\x -> x == '!' || x == '~')
  return NotB2

-- | One of the main ways to call the
--   parser for the Boolean Tree. This
--   one does not require parentheses around
--   expressions.
parseBoolean :: AT.Parser a -> AT.Parser (Boolean2 a)
parseBoolean termParser = makeExprParser
  (parseParens termParser)
  [ [ Prefix parseNot ]
  , [ InfixR parseOr  ]
  , [ InfixR parseAnd ]
  ]

-- | One of the main ways to call the parser for
--   the boolean tree. This one **does** require
--   parentheses around expressions, but not around
--   individual terms. It still requires them around
--   single @not@ expressions.
parseParens :: AT.Parser a -> AT.Parser (Boolean2 a)
parseParens termParser = (between (AT.char '(') (AT.char ')') (parseBoolean termParser))
   <|> (PlainB2 <$> termParser)

parseNotOnly :: AT.Parser a -> AT.Parser (Boolean2 a)
parseNotOnly termParser = do
  _ <- AT.satisfy (\x -> x == '!' || x == '~')
  NotB2 <$> parseParens termParser

-- | One of the main ways to call the parser for
--   the boolean tree. This one **does** require
--   parentheses around expressions, but not around
--   individual terms. Unlike `parseParens`, it also
--   does not require them around individual @not@
--   expressions. 
--
--   Also note that when encountering an expression 
--   such as
--
--     @ !2|5 @
--
--   this function will *not* fail, and instead leave 
--   the parser position between the @2@ and the @|@. 
--   This can be a problem, since the program won't
--   realise that it's midway through an expression.
parseBooleanExpr' :: AT.Parser a -> AT.Parser (Boolean2 a)
parseBooleanExpr' termParser
  = parseParens termParser <|> parseNotOnly termParser

-- | One of the main ways to call the parser for
--   the boolean tree. This one **does** require
--   parentheses around expressions, but not around
--   individual terms. Unlike `parseParens`, it also
--   does not require them around individual @not@
--   expressions. 
--
--   This function also (hopefully) avoids the pitfalls
--   of `parseBooleanExpr'` by looking ahead to see
--   whether the incoming input looks like an expression.
parseBooleanExpr :: AT.Parser a -> AT.Parser (Boolean2 a)
parseBooleanExpr termParser = do
  expr <- parseBooleanExpr' termParser
  rslt <- AC.lookAhead $ do
    skipHoriz
    c <- AT.peekChar
    -- Note: Maybe return more of the following result
    -- to make troubleshooting easier.
    case c of
      (Just '|') -> return True
      (Just '&') -> return True
      _ -> return False
  
  when rslt $ fail "Expression not enclosed in parentheses."

  return expr

