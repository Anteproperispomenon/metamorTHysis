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
  -- * Fixing up Orthographies
  , verifyAndFillInOrthographies
  , verifyOrthographies
  , fillInOrthographies
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

import Metamorth.Interaction.Quasi.Parser.Types

import Text.Printf

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
      liftQQ1 consumeEndComment -- Maybe?
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

--------------------------------
-- Separating out values

-- | Check that the orthography set doesn't have any
--   errors, and fill in any missing data holes.
verifyAndFillInOrthographies :: [OrthographyDetails] -> ParserQQ [OrthographyDetails]
verifyAndFillInOrthographies odts = do
  odSet <- verifyOrthographies odts
  fillInOrthographies odSet odts

data OrthographyDetailsSet = OrthographyDetailsSet
  { odsName :: MultiSet String -- or T.Text
  -- | All input files for parsing.
  , odsInputFiles  :: MultiSet FilePath
  -- | The names of the input and output functions.
  , odsFuncName   :: MultiSet String
  -- | The suffixes used for internal functions.
  , odsSuffix     :: MultiSet String
  -- | The strings used to identify this parser
  --   for the CLI interface.
  , odsCLINames   :: MultiSet String
  } deriving (Show, Eq)

addToSetWM :: (Ord a) => Maybe a -> MultiSet a -> (a -> String) -> ParserQQ (MultiSet a)
addToSetWM Nothing st _ = return st
addToSetWM (Just x) st ppr = do
  let ovr = x `melem` st
  if ovr
    then tellWarning (ppr x) $> (minsert x st)
    else return $ minsert x st

addToSetEM :: (Ord a) => Maybe a -> MultiSet a -> (a -> String) -> ParserQQ (MultiSet a)
addToSetEM Nothing st _ = return st
addToSetEM (Just x) st ppr = do
  let ovr = x `melem` st
  if ovr
    then tellError (ppr x) $> (minsert x st)
    else return $ minsert x st

addToSetW :: (Ord a) => a -> MultiSet a -> (a -> String) -> ParserQQ (MultiSet a)
addToSetW x st ppr = do
  let ovr = x `melem` st
  if ovr
    then tellWarning (ppr x) $> (minsert x st)
    else return $ minsert x st

addsToSetW :: (Ord a) => [a] -> MultiSet a -> ([a] -> String) -> ParserQQ (MultiSet a)
addsToSetW xs st ppr = do
  let ovr = filter (`melem` st) xs
  case ovr of
    [] -> return $ foldr minsert st xs
    _  -> tellWarning (ppr ovr) $> (foldr minsert st xs)

addToSetE :: (Ord a) => a -> MultiSet a -> (a -> String) -> ParserQQ (MultiSet a)
addToSetE x st ppr = do
  let ovr = x `melem` st
  if ovr
    then tellError (ppr x) $> (minsert x st)
    else return $ minsert x st

addsToSetE :: (Ord a) => [a] -> MultiSet a -> ([a] -> String) -> ParserQQ (MultiSet a)
addsToSetE xs st ppr = do
  let ovr = filter (`melem` st) xs
  case ovr of
    [] -> return $ foldr minsert st xs
    _  -> tellError (ppr ovr) $> (foldr minsert st xs)

extraSuffixes :: [String]
extraSuffixes = map (\n -> printf "_prs%03d" n) [(1 :: Int)..]

extraNames :: String -> [String]
extraNames nom = nom : map (\n -> printf (nom ++ "%03d") n) [(1 :: Int)..]

-- | The automatically generated function names.
starterFunctions :: MultiSet String
starterFunctions = M.fromAscList [("convertOrthography",1),("convertOrthographyBS",1),("convertOrthographyLazy",1)]

verifyOrthographies :: [OrthographyDetails] -> ParserQQ OrthographyDetailsSet
verifyOrthographies [] = return $ OrthographyDetailsSet M.empty M.empty starterFunctions M.empty M.empty
verifyOrthographies od = verifyOrthographies' (OrthographyDetailsSet M.empty M.empty starterFunctions M.empty M.empty) od

verifyOrthographies' :: OrthographyDetailsSet -> [OrthographyDetails] -> ParserQQ OrthographyDetailsSet
verifyOrthographies' ods [] = return ods
verifyOrthographies' ods (od:rst) = do
  ods1  <- addToSetE  (odName od) (odsName ods) $ \ nom ->
    "The orthography name \"" ++ nom ++ "\" is used more than once."
  ods2  <- addToSetWM (odInputFile  od) (odsInputFiles ods) $ \fp ->
    "The parser specification file \"" ++ fp ++ "\" is used more than once."
  ods2' <- addToSetWM (odOutputFile od) ods2 $ \fp ->
    "The output specification file \"" ++ fp ++ "\" is used more than once."
  ods3  <- addToSetWM (odInputName  od) (odsFuncName ods) $ \nom ->
    "The function name \"" ++ nom ++ "\" is used more than once."
  ods3' <- addToSetWM (odOutputName od) ods3 $ \nom ->
    "The function name \"" ++ nom ++ "\" is used more than once."
  ods4  <- addToSetWM (odInSuffix od) (odsSuffix ods) $ \suf ->
    "The suffix \"" ++ suf ++ "\" is used more than once."
  ods4' <- addToSetWM (odOutSuffix od) ods4 $ \suf ->
    "The suffix \"" ++ suf ++ "\" is used more than once."
  ods5  <- addsToSetW (odCLINames od) (odsCLINames ods) $ \noms ->
    "The CLI name(s) " ++ quotedList noms ++ "are already in use."
  let ods' = OrthographyDetailsSet ods1 ods2' ods3' ods4' ods5
  verifyOrthographies' ods' rst

-- | Fill in missing necessary details.
fillInOrthographies :: OrthographyDetailsSet -> [OrthographyDetails] -> ParserQQ [OrthographyDetails]
fillInOrthographies _ds [] = return []
fillInOrthographies ods dts = fillInOrthographies' (filter (`mnotElem` sfxs) extraSuffixes) ods dts
  where sfxs = odsSuffix ods

fillInOrthographies' :: [String] -> OrthographyDetailsSet -> [OrthographyDetails] -> ParserQQ [OrthographyDetails]
fillInOrthographies' _ _ [] = return []
fillInOrthographies' (sfx1:sfx2:sfxs) ods (od:rst) = do
  let inSfx   = maybe sfx1 (\x -> if isDuplicate x sfxMS then sfx1 else x) (odInSuffix  od)
      outSfx  = maybe sfx2 (\x -> if isDuplicate x sfxMS then sfx2 else x) (odOutSuffix od)

      inName  = maybe defParseName  (\x -> if isDuplicate x fncMS then defParseName  else x) (odInputName  od)
      outName = maybe defOutputName (\x -> if isDuplicate x fncMS then defOutputName else x) (odOutputName od)

      newOd = OrthographyDetails
                { odName = odName od
                , odInputFile  = odInputFile  od
                , odOutputFile = odOutputFile od
                , odUnifyBranches = odUnifyBranches od
                , odGroupGuards   = odGroupGuards   od
                , odCheckStates   = odCheckStates   od
                , odInputName     = Just inName
                , odOutputName    = Just outName
                , odInSuffix      = Just inSfx
                , odOutSuffix     = Just outSfx
                , odCLINames      = odCLINames od
                }

  (newOd:) <$> fillInOrthographies' sfxs ods rst
  where
    sfxMS = odsSuffix ods
    fncMS = odsFuncName ods
    defParseName  = head $ filter (`mnotElem` fncMS) (extraNames ("parse"  ++ odName od))
    defOutputName = head $ filter (`mnotElem` fncMS) (extraNames ("output" ++ odName od))
-- Unreachable pattern to quash warnings.
fillInOrthographies' _ _ _ = return []

{-
  { odName :: String -- or T.Text
  , odInputFile  :: Maybe FilePath
  , odOutputFile :: Maybe FilePath
  -- | Whether to unify branches for parser.
  , odUnifyBranches :: Maybe Bool
  -- | Whether to group guards for parser.
  , odGroupGuards   :: Maybe Bool
  -- | Whether to check states for parser.
  , odCheckStates   :: Maybe Bool
  -- | The name of the input parser function.
  , odInputName     :: Maybe String
  -- | The name of the output function.
  , odOutputName    :: Maybe String
  -- | The suffix used for internal parser functions.
  , odInSuffix      :: Maybe String
  -- | The suffix used for internal output functions.
  , odOutSuffix     :: Maybe String
  -- | The strings used to identify this parser
  --   for the CLI interface.
  , odCLINames      :: [String]
  } deriving (Show, Eq)

  { odsName :: MultiSet String -- or T.Text
  -- | All input files for parsing.
  , odsInputFiles  :: MultiSet FilePath
  -- | The names of the input and output functions.
  , odsFuncName   :: MultiSet String
  -- | The suffixes used for internal functions.
  , odsSuffix     :: MultiSet String
  -- | The strings used to identify this parser
  --   for the CLI interface.
  , odsCLINames   :: MultiSet String
-}

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

--------------------------------
-- Multi-Sets

type MultiSet a = M.Map a Int

melem :: (Ord a) => a -> MultiSet a -> Bool
melem = M.member

mnotElem :: (Ord a) => a -> MultiSet a -> Bool
mnotElem = M.notMember

minsert :: (Ord a) => a -> MultiSet a -> MultiSet a
minsert x ms = M.insertWith (+) x 1 ms

isDuplicate :: (Ord a) => a -> MultiSet a -> Bool
isDuplicate x ms
  | (Just n) <- M.lookup x ms
  = n > 1
  | otherwise = False
