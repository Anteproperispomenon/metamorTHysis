{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Metamorth.Interpretation.Parser.TH
Description : Generating the Parsers for Orthographies
Copyright   : (c) David Wilson, 2024
License     : BSD-3

This is the module that probably does the most(?) 
complicated part of the process. 

An obvious(?) way to generate a set of parser
functions is to just make a Trie out of the
Phoneme Patterns, and then just make a function
at each branch. The problem with this method
is that you'll get a lot of duplicate functions.
e.g. If "ts" and "ʦ" (U+02A6) are both acceptable
ways to represent the same phoneme path, then we'd
wind up with something like:

@
  ╔═════╗
  ║Start║
  ╚══╦══╝
     ║
  ╔══╩══╗
 ┌╨┐   ┌╨┐
 │t│   │ʦ│
 └╥┘   └╥┘
  ║     ║
╔═╩═╗   ║
║St1║   ║
╚═╦═╝   ║
  ║     ║
 ┌╨┐    ║
 │s│    ║  
 └╥┘    ║  
  ║     ║
╔═╩═╗ ╔═╩═╗  
║St2║ ║St3║
╚═╦═╝ ╚═╦═╝
  ║     ║
@

... where St2 and St3 would generate two different
functions, even though it would make more sense to
use the same state in those two spots, like so:

@
  ╔═════╗
  ║Start║
  ╚══╦══╝
     ║
  ╔══╩══╗
 ┌╨┐   ┌╨┐
 │t│   │ʦ│
 └╥┘   └╥┘
  ║     ║
╔═╩═╗   ║
║St1║   ║
╚═╦═╝   ║
  ║     ║
 ┌╨┐    ║
 │s│    ║  
 └╥┘    ║  
  ║     ║
╔═╩═════╩═╗  
║   St2   ║
╚════╦════╝
     ║
@

This can be accomplished by using `unifyPaths`
from "Metamorth.Helpers.Trie". It annotates each
branch of a `TM.TMap`/Trie with a label such that
two branches with the same label can be assumed
to be the same state. Thus only one function
needs to be generated for either state, reducing
the total number of generated functions.

-}


module Metamorth.Interpretation.Parser.TH
  ( setupTrie'
  , pathifyTrie
  , tempTester
  , makeGuards
  , exampleInfo
  , constructFunctions
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS

import Data.Attoparsec.Text qualified as AT

import Data.Bifunctor qualified as Bi

import Data.Foldable

import Data.Traversable

import Data.Char
import Metamorth.Helpers.Char
import Metamorth.Helpers.Either
import Metamorth.Helpers.List

import Data.Either
import Data.Maybe

import Data.Text qualified as T -- ?

import Data.Map.Strict qualified as M

import Data.Set qualified as S

import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty(..))

import Data.Trie.Map qualified as TM
import Metamorth.Helpers.Trie

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Haskell.TH.Ppr qualified as PP

import THLego.Helpers

import Metamorth.Interpretation.Parser.Types
import Metamorth.Interpretation.Parser.Parsing.Types
import Metamorth.Helpers.TH


-- How to check for the end of a word:
-- Create a bool function that checks
-- whether a character is any of the
-- `Char`s that can start a (midword) 
-- phoneme. Then, just run
-- @ do 
--     x <- AT.peekChar 
--     case x of 
--       Nothing -> pure phone
--       (Just z) -> if (isLetter z)
--                    then (fail "Not at word's end")
--                    else (pure phone) 
-- @

----------------------------------------------------------------
-- Main Constructor
----------------------------------------------------------------


makeTheParser 
  :: M.Map String Name                  -- ^ A `M.Map` from `String`s to Pattern Synonym `Name`s.
  -> M.Map String ([M.Map String Name]) -- ^ A list of `M.Map`s for the constructors of each argument of the Phoneme.
  -> (Exp -> Exp)                       -- ^ How to convert a Pattern synonym to an upper-case character.
  -> (Exp -> Exp)                       -- ^ How to convert a Pattern synonym to an lower-case character.
  -> ()
makeTheParser _ _ _ _ = ()

{-
x <- AT.peekChar'
case x of
  c1 -> AT.anyChar 
  c2 -> AT.anyChar



-}

----------------------------------------------------------------
-- Top-Level Function Generator
----------------------------------------------------------------

makeTrieAnnNames :: TM.TMap CharPattern (TrieAnnotation, Maybe a) -> Q (M.Map TrieAnnotation Name)
makeTrieAnnNames theTrie = do
  let anns = map fst $ TM.elems theTrie
  prs <- for anns $ \ann -> do
    annName <- annFuncName ann
    return (ann, annName)
  return $ M.fromList prs


{-
createWordStartFunction 
  :: Name
  -> Name
  -> StaticParserInfo
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (MultiPhoneName, Caseness))
  -> RWST () [String] s Q ([Dec], M.Map TrieAnnotation Bool)
createWordStartFunction blName peekName spi theTrie = do
  grdName <- lift $ newName "c"

  
  where
    subTries = map (second (first fromJust)) $ getSubTries theTrie
-}
{-
getSubTries
  :: Ord c => TM.TMap c a -> [(c, (Maybe a, TM.TMap c a))]
-}

-- | Kinda hard to explain...
createCasesForStart :: Name -> StaticParserInfo -> CharPattern -> TrieAnnotation -> Maybe (MultiPhoneName, Caseness) -> Either [String] ((Either (Guard, Exp) [Match]), Bool)
createCasesForStart grdName spi cp trieAnn@(TrieAnn _) _mph = case cp of
  -- hmm
  (PlainChar c) -> do
    funcName <- eitherMaybe' (M.lookup trieAnn funcMap) ["Couldn't find a function name for \"" <> show trieAnn <> "\"."]
    if (isCasable c) 
      then Right (Right [Match (LitP (CharL c)) (NormalB $ consumerFuncE (boolE $ isTupper c) funcName ) []], True)
      else Right (Right [Match (LitP (CharL c)) (NormalB $ consumerFuncX funcName) []], False)
  (CharOptCase c) -> do
    funcName <- eitherMaybe' (M.lookup trieAnn funcMap) ["Couldn't find a function name for \"" <> show trieAnn <> "\"."]
    let xs = getCases c
        zs = map (\c -> (c, isTupper c)) xs
    if (any isCasable xs)
      then do 
        zqr <- return $ forMap zs $ \(theChar, theCase) ->
          (Match (LitP (CharL theChar)) (NormalB $ consumerFuncE (boolE theCase) funcName) [])
        return (Right zqr, True)
      else do 
        zqr <- return $ forMap xs $ \theChar ->
          (Match (LitP (CharL theChar)) (NormalB $ consumerFuncX funcName) [])
        return (Right zqr, False)
  (CharClass cn) -> do
    funcName  <- eitherMaybe' (M.lookup trieAnn funcMap) ["Couldn't find a function name for \"" <> show trieAnn <> "\"."]
    (classFunc, cs) <- eitherMaybe' (M.lookup cn classMap) ["Couldn't find function name for class \"" <> cn <> "\"."]
    let checkExp = AppE (VarE classFunc) (VarE grdName)
        boolExp  = AppE (VarE 'isUpperCase) (VarE grdName)
    if (any isCasable cs)
      then Right (Left (NormalG checkExp, consumerFuncE boolExp funcName), True )
      else Right (Left (NormalG checkExp, consumerFuncX         funcName), False)
  WordStart -> Left ["Shouldn't encounter a \"WordStart\" when using this function."]
  WordEnd   -> Left ["Shouldn't encounter a \"WordEnd\" when using this function."]
      
  where
    funcMap  = spiAnnotationMap spi
    classMap = spiClassMap spi
--------------------
-- For Leaf Nodes --
--------------------
-- createCasesForStart 
--  :: Name 
--  -> StaticParserInfo 
--  -> CharPattern 
--  -> TrieAnnotation 
--  -> Maybe (MultiPhoneName, Caseness) 
--  -> Either [String] ((Either (Guard, Exp) [Match]), Bool)
createCasesForStart grdName spi cp TrieLeaf mph = case cp of
  -- hmm
  (PlainChar c) -> do
    -- funcName <- eitherMaybe' (M.lookup trieAnn funcMap) ("Couldn't find a function name for \"" <> show trieAnn <> "\".")
    (mphX, cs) <- eitherMaybe' mph ["Couldn't lookup value for parsed character: \"" <> [c] <> "\"."]
    mulExprs <- phoneNamePatterns constrMap aspMaps mphX
    if (isCasable c) 
      then Right (Right [Match (LitP (CharL c)) (NormalB $ consumerRetA mkMaj mkMin c       cs mulExprs ) []], True )
      else Right (Right [Match (LitP (CharL c)) (NormalB $ consumerRetB mkMaj mkMin grdName cs mulExprs ) []], False)
  (CharOptCase c) -> do
    -- funcName <- eitherMaybe' (M.lookup trieAnn funcMap) ["Couldn't find a function name for \"" <> show trieAnn <> "\"."]
    (mphX, cs) <- eitherMaybe' mph ["Couldn't lookup value for parsed character: \"" <> [c] <> "\"."]
    mulExprs <- phoneNamePatterns constrMap aspMaps mphX
    let xs = getCases c
        zs = map (\c -> (c, isTupper c)) xs
    if (any isCasable xs)
      then do 
        zqr <- return $ forMap zs $ \(theChar, theCase) ->
          (Match (LitP (CharL theChar)) (NormalB $ consumerRetA mkMaj mkMin theChar cs mulExprs) [])
        return (Right zqr, True)
      else do 
        zqr <- return $ forMap xs $ \theChar ->
          (Match (LitP (CharL theChar)) (NormalB $ consumerRetB mkMaj mkMin grdName cs mulExprs) [])
        return (Right zqr, False)
  (CharClass cn) -> do
    -- funcName  <- eitherMaybe' (M.lookup trieAnn funcMap) ["Couldn't find a function name for \"" <> show trieAnn <> "\"."]
    (classFunc, zs) <- eitherMaybe' (M.lookup cn classMap) ["Couldn't find function name for class \"" <> cn <> "\"."]
    (mphX, cs) <- eitherMaybe' mph ["Couldn't lookup value for parsed class: \"" <> cn <> "\"."]
    mulExprs <- phoneNamePatterns constrMap aspMaps mphX
    let checkExp = AppE (VarE classFunc) (VarE grdName)
        boolExp  = AppE (VarE 'isUpperCase) (VarE grdName)
    -- can probably just reduce to one expression.
    if (any isCasable zs)
      then Right (Left (NormalG checkExp, consumerRetB mkMaj mkMin grdName cs mulExprs), True ) -- need to change, maybe?
      else Right (Left (NormalG checkExp, consumerRetB mkMaj mkMin grdName cs mulExprs), False)
  WordStart -> Left ["Shouldn't encounter a \"WordStart\" when using this function."]
  WordEnd   -> Left ["Shouldn't encounter a \"WordEnd\" when using this function."]
      
  where
    classMap = spiClassMap spi
    mkMaj = spiMkMaj spi
    mkMin = spiMkMin spi
    constrMap = spiConstructorMap spi
    aspMaps   = spiAspectMaps spi


{-
phoneNamePatterns :: M.Map String Name -> (M.Map String ([M.Map String Name])) -> MultiPhoneName -> Either [String] (NonEmpty Exp)
consumerRetA
  :: (Exp -> Exp) -- ^ A function to make upper-case constructors.
  -> (Exp -> Exp) -- ^ A function to make lower-case constructors
  -> Char         -- ^ The next peeked `Char`.
  -> Caseness     -- ^ The `Caseness` of the Phoneme.
  -> MulExp       -- ^ The uncased value of this phoneme.
  -> Exp          -- ^ The resulting expression.
consumerRetA mkMaj mkMin c cs exprs = infixCont anyCharE (phonemeRet mkMaj mkMin c cs Nothing exprs)

consumerRetB
  :: (Exp -> Exp) -- ^ A function to make upper-case constructors.
  -> (Exp -> Exp) -- ^ A function to make lower-case constructors
  -> Name         -- ^ The name of the peeked `Char`. (not in a maybe)
  -> Caseness     -- ^ The `Caseness` of the Phoneme.
  -> MulExp       -- ^ The uncased value of this phoneme.
  -> Exp          -- ^ The resulting expression.
consumerRetB mkMaj mkMin nom cs exprs = infixCont anyCharE (phonemeRetZB mkMaj mkMin cs nom exprs)

-}


-- | Split a trie into those that start with
--   `WordStart` and those that don't.
splitTrie 
  ::  TM.TMap CharPattern (TrieAnnotation, Maybe a) 
  -> (TM.TMap CharPattern (TrieAnnotation, Maybe a), TM.TMap CharPattern (TrieAnnotation, Maybe a))
splitTrie trie
  = (startTrie, notStartTrie)
  where
    (_, startTrie) = TM.match     [WordStart] trie
    notStartTrie   = deleteBranch  WordStart  trie


----------------------------------------------------------------
-- Function Generators
----------------------------------------------------------------

-- TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneName, Caseness))

-- | Data type containing all the static info
--   that'll be used for generating parsers.
data StaticParserInfo = StaticParserInfo
  -- | A `M.Map` from `TrieAnnotation`s to function `Name`s, 
  --   where the function `Name` refers to the function
  --   that corresponds to that node in the `TM.TMap`.
  { spiAnnotationMap  :: M.Map TrieAnnotation Name
  -- | A `M.Map` from phoneme `String`s to Constructor/Pattern Synonym `Name`s.
  , spiConstructorMap :: M.Map String Name
  -- | A `M.Map` from phoneme `String`s to the constructors of the
  --   arguments (i.e. aspects) of the constructor of that Phoneme. 
  --   Each argument is represented by a `M.Map` from `String`s to
  --   `Name`s, where each `Name` refers to a constructor itself.
  --
  --   For example, let's say you have the following phoneme and
  --   aspect definitions:
  --
  --   > aspect release : plain labial palatal
  --   > aspect voice   : voiceless voiced ejective
  --   >
  --   > ====
  --   > 
  --   > ...
  --   > k : voice release
  --   > ...
  --
  --   Then the entry for @k@ in @spiAspectMaps` would be
  --   something like...
  --   
  --   > ("k", 
  --   >   [ fromList [("plain", "Plain"), ("labial", "Labial"), ("palatal", "Palatal")]
  --   >   , fromList [("voiceless", "Voiceless"), ("voiced", "Voiced"), ("ejective", "Ejective")]
  --   >   ]
  --   >  )
  , spiAspectMaps     :: M.Map String [M.Map String Name]
  -- | A `M.Map` from class `String` names to function `Name`s.
  --   These functions will have the type @`Char` -> `Bool`@,
  --   and just be simple tests of whether the `Char` is one
  --   of the members of the class. e.g.
  --
  --   > isApost :: Char -> Bool
  --   > isApost x = (x == '\'') || (x == '`') || (x == '\x313')
  --   
  --   etc...
  , spiClassMap       :: M.Map String (Name,[Char])
  -- | A function to turn a `Phoneme` expression/value into
  --   a upper-case value. This is represented as a function
  --   for more flexibility. This should be a very simple function.
  --   For uncased orthographies, it should just be @`id`@ or the
  --   same as @`spiMkMin`@. For cased orthographies, it should 
  --   just be a function like:
  --
  --   > mkMaj :: Exp -> Exp
  --   > mkMaj expr = AppE (ConE upperName) expr
  --
  --   where @upperName` is the `Name` for the Upper-case constructor.
  , spiMkMaj          :: (Exp -> Exp)
  -- | Same as `spiMkMaj`, but for lower-case instead.
  , spiMkMin          :: (Exp -> Exp)
  -- | The `Name` of a function that checks whether a `Char`
  --   is *NOT* one of the characters that can start a phoneme
  --   mid-word. If a peeked `Char` satisfies this predicate,
  --   then we know we are at the end of a word.
  , spiEndWordFunc    :: Name
  -- | The `Name` of the `Type` used for phonemes.
  , spiPhoneTypeName  :: Name
  }

instance Eq StaticParserInfo where
  x == y
    = ((spiAnnotationMap x) == (spiAnnotationMap y))
      && ((spiConstructorMap x) == (spiConstructorMap y))
      && ((spiAspectMaps x) == (spiAspectMaps y))
      && ((spiClassMap x) == (spiClassMap y))
      && ((spiMkMaj' x) == (spiMkMaj' y))
      && ((spiMkMin' x) == (spiMkMin' y))
      && ((spiEndWordFunc x) == (spiEndWordFunc y))
      && ((spiPhoneTypeName x) == (spiPhoneTypeName y))
    where
      spiMkMaj' z = (spiMkMaj z) (ConE (mkName "Example"))
      spiMkMin' z = (spiMkMin z) (ConE (mkName "Example"))

instance Show StaticParserInfo where
  show x =
    "StaticParserInfo {spiAnnotationMap = " <> show (spiAnnotationMap x)
      <> ", spiConstructorMap = " <> show (spiConstructorMap x)
      <> ", spiAspectMaps = "     <> show (spiAspectMaps x)
      <> ", spiClassMap = "       <> show (spiClassMap x)
      <> ", spiMkMaj = "          <> show (PP.ppr mkMajRep)
      <> ", spiMkMin = "          <> show (PP.ppr mkMinRep)
      <> ", spiEndWordFunc = "    <> show (spiEndWordFunc x)
      <> ", spiPhoneTypeName = "  <> show (spiPhoneTypeName x)
      <> "}"
    where
      exprNom  = mkName "expr"
      exprPat  = VarP exprNom
      exprVar  = VarE exprNom
      mkMajRep = LamE [exprPat] (spiMkMaj x exprVar)
      mkMinRep = LamE [exprPat] (spiMkMin x exprVar)

-- | To make re-writing code easier.
type MultiPhoneName = NonEmpty PhoneName

-- | To make re-writing code easier.
type MulExp = NonEmpty Exp


annFuncName :: TrieAnnotation -> Q Name
annFuncName ann = newName $ varName $ show ann

eitherToRWST :: (Monoid w, Monad m) => Either w a -> RWST r w s m (Maybe a)
eitherToRWST (Left err) = tell err >> return Nothing
eitherToRWST (Right  x) = return $ Just x

{-
createCasesForStart 
  :: Name  -- guardName
  -> StaticParserInfo 
  -> CharPattern 
  -> TrieAnnotation 
  -> Maybe (MultiPhoneName, Caseness) 
  -> Either [String] ((Either (Guard, Exp) [Match]), Bool)
-}

constructFunctions 
  :: StaticParserInfo
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (MultiPhoneName, Caseness))
  -> Q (Either [String] [Dec])
constructFunctions spi trie = do
  blName   <- newName "isCharMaj"
  peekName <- newName "peekedChar"
  grdName  <- newName "c"
  topFuncName <- newName "mainParse"
  let rslts = forMap subTries $ \(cp, ((tann, mval), subTrie )) -> createCasesForStart grdName spi cp tann mval 
  case (liftEitherList rslts) of
    (Left errs)    -> return (Left $ concat errs)
    (Right rsltsX) -> do
      let rsltsY = map (\case {(Left x, y) -> Left (x,y) ; (Right x, y) -> Right (x,y)}) rsltsX
          (grds, mtchs) = partitionEithers  rsltsY
          grdBody   = GuardedB $ map fst grds
          grdMatch  = if (null grds) then [] else [Match (VarP grdName) grdBody []]
          finalMat  = Match WildP (NormalB $ AppE (VarE 'fail) (strE "No matches found.")) []
          matches   = (concatMap fst mtchs) ++ grdMatch ++ [finalMat]
          caseStuff = CaseE (VarE peekName) matches
      mainType <- [t| Char -> AT.Parser (NonEmpty $(return $ ConT phoneType) ) |]
      let mainSign = SigD topFuncName mainType
          mainDec  = FunD topFuncName [Clause [VarP peekName] (NormalB caseStuff) []]
      -- okay, the hard part
      (moreDecs, errs) <- evalRWST' () (S.empty, M.empty) $ forM subTries $ \(cp, ((tann, mval), subTrie )) -> do 
        let thisbl = True -- TEMPORARY
        constructFunctions' blName peekName thisbl spi subTrie [cp] tann mval
      let moreDecs' = concat moreDecs
          errs'     = errs
      case errs' of
        [] -> return (Right (mainSign:mainDec:moreDecs'))
        xs -> return (Left xs)
      
  where
    subTries  = map (second (first fromJust)) $ getSubTries trie
    phoneType = spiPhoneTypeName spi
    evalRWST' rdr st act = evalRWST act rdr st


constructFunctions'
  :: Name -- ^ The name generated to be used as the bool argument
  -> Name -- ^ The name generated to be used as the char argument
  -> Bool
  -> StaticParserInfo
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (MultiPhoneName, Caseness))
  -> [CharPattern]
  -> TrieAnnotation
  -> Maybe (MultiPhoneName, Caseness)
  -> RWST () [String] (S.Set TrieAnnotation, M.Map TrieAnnotation Bool) Q [Dec] -- ?
constructFunctions' _ _ _ _ _ _ TrieLeaf _ = return []
constructFunctions' blName peekName isCased spi theTrie cPats trieAnn thisVal = do
  -- uh...
  rslts <- eitherToRWST $ constructGuards mbl peekName trieAnn thisVal theTrie spi cPats
  case rslts of
    Nothing -> return []
    (Just (bod, conts)) -> do
      -- funcNom <- lift $ annFuncName trieAnn
      funcNom <- do
        let mrslt = M.lookup trieAnn funcMap
        case mrslt of
          (Just x) -> return x
          Nothing  -> lift $ annFuncName trieAnn
      modify $ first  $ S.insert trieAnn
      modify $ second $ M.insert trieAnn isCased
      moreDecs <- for conts $ \( tval@(nextAnn, nextVal) , nextCased, nextPat, nextTrie ) -> do
        curMemSet <- gets fst
        xrslts <- if (nextAnn `S.member` curMemSet)
          then (return [])
          else do
            -- hmm...
            constructFunctions' blName peekName nextCased spi nextTrie (cPats ++ [nextPat]) nextAnn nextVal
        return xrslts
      -- okay, convert the body to decs...
      
      
      -- now, do the signature...
      -- mbool <- lift $ [t| Maybe Bool |]
      -- mchar <- lift $ [t| Maybe Char |]
      let phonType = ConT $ spiPhoneTypeName spi
          phonType' = AppT (ConT ''NonEmpty) phonType
          funcType = arrowChainT margs (parserT' phonType')
          funcSign = SigD funcNom funcType
          funcDec  = FunD funcNom [Clause pargs bod []]
      return (funcSign:funcDec:concat moreDecs)

  where
    mbl = if isCased then (Just blName) else Nothing
    mbool = AppT (ConT ''Maybe) (ConT ''Bool)
    mchar = AppT (ConT ''Maybe) (ConT ''Char)
    pbool = VarP blName
    pchar = VarP peekName
    margs = if isCased then [mbool,mchar] else [mchar]
    pargs = if isCased then [pbool,pchar] else [pchar]
    funcMap = spiAnnotationMap spi


{-
[( (TrieAnnotation, Maybe (MultiPhoneName, Caseness))
 , Bool
 , CharPattern
 , TM.TMap CharPattern (TrieAnnotation, Maybe (MultiPhoneName, Caseness))
 )
 ]
-}

constructGuards 
  :: Maybe Name 
  -> Name 
  -> TrieAnnotation
  -> Maybe (MultiPhoneName, Caseness)
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (MultiPhoneName, Caseness)) -- ^ This node in the Trie.
  -> StaticParserInfo -- ^ Constant info about the parser.
  -> [CharPattern]
  -> Either [String] (Body, [((TrieAnnotation, Maybe (MultiPhoneName, Caseness)), Bool, CharPattern, TM.TMap CharPattern (TrieAnnotation, Maybe (MultiPhoneName, Caseness)))])
constructGuards mbl charVarName trieAnn mTrieVal theTrie spi charPat
  = makeGuards
      mbl
      charVarName
      trieAnn
      mTrieVal
      theTrie
      (spiAnnotationMap spi)
      (spiMkMaj spi)
      (spiMkMin spi)
      (spiConstructorMap spi)
      (spiAspectMaps spi)
      (spiClassMap spi)
      (spiEndWordFunc spi)
      charPat

makeGuards
  :: Maybe Name                    -- ^ The name of the boolean variable.
  -> Name                          -- ^ The name of the peeked char variable.
  -> TrieAnnotation                -- ^ This node's Annotation
  -> (Maybe (MultiPhoneName, Caseness)) -- ^ This node's output value.
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (MultiPhoneName, Caseness)) -- ^ This node in the Trie.
  -> M.Map TrieAnnotation Name     -- ^ Mapping from `TrieAnnotation`s to function names.
  -> (Exp -> Exp)                  -- ^ Maj-maker
  -> (Exp -> Exp)                  -- ^ Min-maker
  -> (M.Map String Name)           -- ^ Map for patterns/constructors.
  -> (M.Map String ([M.Map String Name])) -- ^ Map for constructors of sub-elements.
  -> (M.Map String (Name, [Char])) -- ^ Map for class function names.
  -> Name                          -- ^ Name of the "end of word" function.
  -> [CharPattern]                 -- ^ The `CharPattern` leading up to this point.
  -> Either [String] (Body, [((TrieAnnotation, Maybe (MultiPhoneName, Caseness)), Bool, CharPattern, TM.TMap CharPattern (TrieAnnotation, Maybe (MultiPhoneName, Caseness)))])
makeGuards mbl@(Just blName) charVarName trieAnn mTrieVal theTrie funcMap mkMaj mkMin patMap aspMaps classMap endWordFunc precPatrn = do
  (grds, subs) <- fmap unzip $ Bi.first concat $ liftEitherList $ map mkRslts subTries
  lstGrd       <- finalRslt
  return (GuardedB $ (map (first NormalG) grds) ++ [lstGrd], catMaybes subs)
  
  where
    -- Since TrieAnnotation should always be present, it should
    -- be safe to use `fromJust`.
    subTries = map (second (first fromJust)) $ getSubTries theTrie

    -- This is using the `Either` monad.
    mkRslts = \(chrP,((ann, mPhone), thisSubTrie)) -> do
      -- (phnNom, phnArgs) <- eitherMaybe' mPhone ("Couldn't fi")
      -- cstr <- eitherMaybe' (M.lookup )
      case (ann, mPhone) of
        (TrieLeaf, mph) -> do 
          (pnom, cs) <- eitherMaybe' mph ["Found a leaf that doesn't have a return value; pattern is \"" <> (ppCharPats $ precPatrn ++ [chrP]) <> "\"."]
          -- cstrPat <- eitherMaybe' (M.lookup (pnName pnom) patMap) ("Can't find constructor for phoneme: \"" <> (pnName pnom) <> "\".")
          cstrExps   <- phoneNamePatterns patMap aspMaps pnom
          guardThing <- Bi.first (:[]) $ charPatternGuard classMap endWordFunc chrP charVarName
          return (( guardThing , consumerRet' mkMaj mkMin chrP cs mbl cstrExps), Nothing)
        -- (Trie)
        -- [((TrieAnnotation, Maybe (PhoneName, Caseness)), Bool, TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneName, Caseness)))]
        elm@(tnn@(TrieAnn _), mph) -> do
          nextFunc <- eitherMaybe' (M.lookup tnn funcMap) ["Couldn't find function for pattern: \"" <> (ppCharPats $ precPatrn ++ [chrP]) <> "\"."]
          guardThing <- Bi.first (:[]) $ charPatternGuard classMap endWordFunc chrP charVarName
          -- idk...
          let expVal = consumerFunc' blName nextFunc
          -- The True is since the next function expects a 
          return ((guardThing, expVal),Just (elm, True, chrP, thisSubTrie))

    -- This guard matches @peekedChar == `Nothing`@, or when
    -- none of the possible matches are correct.
    finalRslt = otherwiseG <$> case mTrieVal of
      Nothing -> return $ AppE (VarE 'fail) (LitE (StringL $ "Couldn't find a match for pattern: \"" ++ (ppCharPats precPatrn) ++ "\"."))
      (Just (pnom, cs)) -> do 
        cstrExp    <- phoneNamePatterns patMap aspMaps pnom
        return $ phonemeRet' mkMaj mkMin cs mbl cstrExp
makeGuards Nothing charVarName trieAnn mTrieVal theTrie funcMap mkMaj mkMin patMap aspMaps classMap endWordFunc precPatrn = do
  (grds, subs) <- fmap unzip $ Bi.first concat $ liftEitherList $ map mkRslts subTries
  lstGrd       <- finalRslt
  return (GuardedB $ (map (first NormalG) grds) ++ [lstGrd], catMaybes subs)
  where
    -- Since TrieAnnotation should always be present, it should
    -- be safe to use `fromJust`.
    subTries = map (second (first fromJust)) $ getSubTries theTrie
    mkRslts = \(chrP, ((ann, mPhone), thisSubTrie)) -> do
      case (ann, mPhone) of
        (TrieLeaf, mph) -> do
          (pnom, cs) <- eitherMaybe' mph ["Found a leaf that doesn't have a return value; pattern is \"" <> (ppCharPats $ precPatrn ++ [chrP]) <> "\"."]
          cstrExp    <- phoneNamePatterns patMap aspMaps pnom
          guardThing <- Bi.first (:[]) $ charPatternGuard classMap endWordFunc chrP charVarName
          let xRslt = consumerRetX mkMaj mkMin chrP cs charVarName cstrExp
          return ((guardThing,xRslt), Nothing)
        elm@(tnn@(TrieAnn _), mph) -> do
          nextFunc <- eitherMaybe' (M.lookup tnn funcMap) ["Couldn't find function for pattern: \"" <> (ppCharPats $ precPatrn ++ [chrP]) <> "\"."]
          (guardThing, isCased) <- Bi.first (:[]) $ charPatternGuard' classMap endWordFunc chrP charVarName
          -- If cased, allow... stuff... to happen.
          if isCased
            then do
              let rsltX = consumerFuncE (AppE (liftPred 'isUpperCase) (VarE charVarName)) nextFunc
              return ((guardThing, rsltX), Just (elm, True, chrP, thisSubTrie))
            else do
              -- uh...
              let rsltX = consumerFuncX nextFunc
              return ((guardThing, rsltX), Just (elm, False, chrP, thisSubTrie))
    
    finalRslt = otherwiseG <$> case mTrieVal of
      Nothing -> return $ AppE (VarE 'fail) (LitE (StringL $ "Couldn't find a match for pattern: \"" ++ (ppCharPats precPatrn) ++ "\"."))
      (Just (pnom, cs)) -> do
        cstrExp <- phoneNamePatterns patMap aspMaps pnom
        return $ phonemeRetZ mkMaj mkMin cs charVarName cstrExp
{-

consumerRetX mkMaj mkMin (PlainChar   c) cs peekName rslt

phonemeRet'
  :: (Exp -> Exp) -- ^ A function to make upper-case constructors.
  -> (Exp -> Exp) -- ^ A function to make lower-case constructors
  -> Caseness     -- ^ The `Caseness` of the Phoneme.
  -> (Maybe Name) -- ^ The name of the variable with the caseness value.
  -> Exp          -- ^ The uncased value of this phoneme.
  -> Exp          -- ^ The resulting expression.

charPatternGuard :: (M.Map String Name) -> Name -> CharPattern -> Name -> Either String Exp

consumerRet
  :: (Exp -> Exp) -- ^ A function to make upper-case constructors.
  -> (Exp -> Exp) -- ^ A function to make lower-case constructors
  -> Char         -- ^ The `Char` in question.
  -> Caseness     -- ^ The `Caseness` of the Phoneme.
  -> (Maybe Name) -- ^ The name of the variable with the caseness value.
  -> Exp          -- ^ The uncased value of this phoneme.
  -> Exp          -- ^ The resulting expression.

phoneNamePattern :: M.Map String Name -> (M.Map String ([M.Map String Name])) -> PhoneName -> Either [String] Exp

getSubTries :: (Ord c) => TM.TMap c a -> [(c,(Maybe a, TM.TMap c a))]

(TrieAnnotation, Maybe (PhoneName, Caseness))

eqJustChar :: Name -> Char -> Exp
eqJustChar mbVar theChar
  = InfixE (Just (VarE mbVar)) (VarE '(==)) (Just (AppE (ConE 'Just) (LitE (CharL theChar))))

otherwiseG :: Exp -> (Guard, Exp)
otherwiseG exp1 = (NormalG (VarE 'otherwise), exp1)

-}



----------------------------------------------------------------
-- Expression Generators
----------------------------------------------------------------


-- | Create the function
--   @myFunc = AT.anyChar >>= nextFunc _@
--   to be used after finding a matching `Char`
--   after using `AT.peekChar`.
consumerFunc :: Char -> Name -> Exp
consumerFunc c nom
  | (isTupper c)        = infixCont anyCharE (infixBind peekCharE (AppE (VarE nom)  trueE))
  -- | (not $ isCasable c) = infixCont anyCharE (infixBind peekCharE (VarE nom))
  | (isLowerCase c)     = infixCont anyCharE (infixBind peekCharE (AppE (VarE nom) falseE))
  | otherwise           = infixCont anyCharE (infixBind peekCharE (VarE nom))

-- | Uh... hmm...
-- 
--   Like `consumerFunc`, but where you already
--   have the `Name` of the caseness variable.
consumerFunc' :: Name -> Name -> Exp
consumerFunc' caseVal funcName
  = infixCont anyCharE (infixBind peekCharE (AppE (VarE funcName) (VarE caseVal)))

-- | Like `consumerFunc'`, but where you are using
--   an expression instead of a variable.
consumerFuncE :: Exp -> Name -> Exp
consumerFuncE caseExp funcName
  = infixCont anyCharE (infixBind peekCharE (AppE (VarE funcName) caseExp))

-- | For when you still can't say whether
--   it's cased or not.
consumerFuncX :: Name -> Exp
consumerFuncX funcName
  = infixCont anyCharE (infixBind peekCharE (VarE funcName))

{-
data CharPattern
  = PlainChar Char   -- ^ A single `Char`.
  | CharOptCase Char -- ^ Any case of a `Char`.
  | CharClass String -- ^ Any member of a class from the header.
  | WordStart        -- ^ The start of a word.
  | WordEnd          -- ^ The end of a word.
  deriving (Show, Eq, Ord)
-}

-- phonemeRet mkMaj mkMin c cs (Just blName) rslts

-- | I hate these functions so much. I keep having to make more.
--
--   This one is for the first character of a word.
consumerRetA
  :: (Exp -> Exp) -- ^ A function to make upper-case constructors.
  -> (Exp -> Exp) -- ^ A function to make lower-case constructors
  -> Char         -- ^ The next peeked `Char`.
  -> Caseness     -- ^ The `Caseness` of the Phoneme.
  -> MulExp       -- ^ The uncased value of this phoneme.
  -> Exp          -- ^ The resulting expression.
consumerRetA mkMaj mkMin c cs exprs = infixCont anyCharE (phonemeRet mkMaj mkMin c cs Nothing exprs)

consumerRetB
  :: (Exp -> Exp) -- ^ A function to make upper-case constructors.
  -> (Exp -> Exp) -- ^ A function to make lower-case constructors
  -> Name         -- ^ The name of the peeked `Char`. (not in a maybe)
  -> Caseness     -- ^ The `Caseness` of the Phoneme.
  -> MulExp       -- ^ The uncased value of this phoneme.
  -> Exp          -- ^ The resulting expression.
consumerRetB mkMaj mkMin nom cs exprs = infixCont anyCharE (phonemeRetZB mkMaj mkMin cs nom exprs)

-- phonemeRetZB mkMaj mkMin cs peekName rslts


consumerRetX
  :: (Exp -> Exp) -- ^ A function to make upper-case constructors.
  -> (Exp -> Exp) -- ^ A function to make lower-case constructors
  -> CharPattern  -- ^ The `Char` in question.
  -> Caseness     -- ^ The `Caseness` of the Phoneme.
  -> Name         -- ^ The `Name` of the next peeked `Char`.
  -> MulExp       -- ^ The uncased value of this phoneme.
  -> Exp          -- ^ The resulting expression.
consumerRetX mkMaj mkMin (PlainChar   c) cs peekName rslt = infixCont anyCharE (phonemeRetZ mkMaj mkMin cs peekName rslt)
consumerRetX mkMaj mkMin (CharOptCase c) cs peekName rslt = infixCont anyCharE (phonemeRetZ mkMaj mkMin cs peekName rslt)
consumerRetX mkMaj mkMin (CharClass _cl) cs peekName rslt = infixCont anyCharE (phonemeRetZ mkMaj mkMin cs peekName rslt)
consumerRetX mkMaj mkMin WordEnd         cs peekName rslt = (phonemeRetZ mkMaj mkMin cs peekName rslt)
consumerRetX mkMaj mkMin WordStart       cs peekName rslt = (phonemeRetZ mkMaj mkMin cs peekName rslt)


-- | For when the next node is a `AnnLeaf`... I think.
consumerRet'
  :: (Exp -> Exp) -- ^ A function to make upper-case constructors.
  -> (Exp -> Exp) -- ^ A function to make lower-case constructors
  -> CharPattern  -- ^ The `Char` in question.
  -> Caseness     -- ^ The `Caseness` of the Phoneme.
  -> (Maybe Name) -- ^ The name of the variable with the caseness value.
  -> MulExp       -- ^ The uncased value of this phoneme.
  -> Exp          -- ^ The resulting expression.
consumerRet' mkMaj mkMin (PlainChar   c) cs mblName rslt = infixCont anyCharE (phonemeRet  mkMaj mkMin c cs mblName rslt)
consumerRet' mkMaj mkMin (CharOptCase c) cs mblName rslt = infixCont anyCharE (phonemeRet  mkMaj mkMin c cs mblName rslt)
consumerRet' mkMaj mkMin (CharClass _cl) cs mblName rslt = infixCont anyCharE (phonemeRet' mkMaj mkMin   cs mblName rslt)
consumerRet' mkMaj mkMin WordEnd         cs mblName rslt = (phonemeRet' mkMaj mkMin cs mblName rslt)
consumerRet' mkMaj mkMin WordStart       cs mblName rslt = (phonemeRet' mkMaj mkMin cs mblName rslt)

-- | Like `consumerFunc`, but just returns
--   a single phoneme instead of continuing
--   to parse.
consumerRet
  :: (Exp -> Exp) -- ^ A function to make upper-case constructors.
  -> (Exp -> Exp) -- ^ A function to make lower-case constructors
  -> Char         -- ^ The `Char` in question.
  -> Caseness     -- ^ The `Caseness` of the Phoneme.
  -> (Maybe Name) -- ^ The name of the variable with the caseness value.
  -> MulExp       -- ^ The uncased value of this phoneme.
  -> Exp          -- ^ The resulting expression.
consumerRet mkMaj mkMin c cs mblName rslts = infixCont anyCharE (phonemeRet mkMaj mkMin c cs mblName rslts)

-- | For when you still don't have a clue
--   whether this phoneme is upper or lower.
phonemeRetZ
  :: (Exp -> Exp) -- ^ mkMaj
  -> (Exp -> Exp) -- ^ mkMin
  -> Caseness     -- ^ Caseness
  -> Name         -- ^ Name of the next peekedChar
  -> MulExp       -- ^ Uncased expression
  -> Exp          -- ^ Result
phonemeRetZ mkMaj mkMin cs peekName rslts
  | cs == CMaj = AppE ret $ formMulExp $ forMap rslts $ \rslt -> (mkMaj rslt)
  | cs == CMin = AppE ret $ formMulExp $ forMap rslts $ \rslt -> (mkMin rslt)
  | otherwise  = condCaseExp (AppE (liftPred 'isUpperCase) (VarE peekName)) mkMaj mkMin rslts
  where ret = VarE 'return

-- | Like `phonemeRetZ`, but where the peeked
--   var isn't contained in a `Maybe`.
phonemeRetZB
  :: (Exp -> Exp) -- ^ mkMaj
  -> (Exp -> Exp) -- ^ mkMin
  -> Caseness     -- ^ Caseness
  -> Name         -- ^ Name of the next peekedChar
  -> MulExp       -- ^ Uncased expression
  -> Exp          -- ^ Result
phonemeRetZB mkMaj mkMin cs peekName rslts
  | cs == CMaj = AppE ret $ formMulExp $ forMap rslts $ \rslt -> (mkMaj rslt)
  | cs == CMin = AppE ret $ formMulExp $ forMap rslts $ \rslt -> (mkMin rslt)
  | otherwise  = condCaseExp (AppE (VarE 'isUpperCase) (VarE peekName)) mkMaj mkMin rslts
  where ret = VarE 'return


-- | Like `consumerRet`, but doesn't
--   consume a character first. To be
--   used when you can return without
--   consuming. e.g. if @ts@ and @t@
--   are both valid single phonemes,
--   but @te@ is treated as @t + e@,
--   then you'd want to use this after
--   consuming @t@ but seeing that the
--   next character is @e@.
phonemeRet 
  :: (Exp -> Exp) -- ^ A function to make upper-case constructors.
  -> (Exp -> Exp) -- ^ A function to make lower-case constructors
  -> Char         -- ^ The `Char` in question.
  -> Caseness     -- ^ The `Caseness` of the Phoneme.
  -> (Maybe Name) -- ^ The name of the variable with the caseness value.
  -> MulExp       -- ^ The uncased value of this phoneme.
  -> Exp          -- ^ The resulting expression.
phonemeRet mkMaj mkMin c cs (Just blName) rslts
  | (cs == CMaj) = AppE ret $ formMulExp $ forMap rslts $ \rslt -> (mkMaj rslt)
  | (cs == CMin) = AppE ret $ formMulExp $ forMap rslts $ \rslt -> (mkMin rslt)
  | otherwise    = condCase blName mkMaj mkMin rslts
  where
    ret  = VarE 'return
phonemeRet mkMaj mkMin c cs Nothing rslts
  | (isUp && cs == CDep) = AppE ret $ formMulExp $ forMap rslts $ \rslt -> (mkMaj rslt)
  | (cs == CDep)         = AppE ret $ formMulExp $ forMap rslts $ \rslt -> (mkMin rslt)
  | (cs == CMaj)         = AppE ret $ formMulExp $ forMap rslts $ \rslt -> (mkMaj rslt)
  | otherwise            = AppE ret $ formMulExp $ forMap rslts $ \rslt -> (mkMin rslt)
  where
    isUp = isTupper c
    ret  = VarE 'return

phonemeRet'
  :: (Exp -> Exp) -- ^ A function to make upper-case constructors.
  -> (Exp -> Exp) -- ^ A function to make lower-case constructors
  -> Caseness     -- ^ The `Caseness` of the Phoneme.
  -> (Maybe Name) -- ^ The name of the variable with the caseness value.
  -> MulExp       -- ^ The uncased value of this phoneme.
  -> Exp          -- ^ The resulting expression.
phonemeRet' mkMaj mkMin cs (Just blName) rslts
  | (cs == CMaj) = AppE ret $ formMulExp $ forMap rslts $ \rslt -> (mkMaj rslt)
  | (cs == CMin) = AppE ret $ formMulExp $ forMap rslts $ \rslt -> (mkMin rslt)
  | otherwise    = condCase blName mkMaj mkMin rslts
  where
    ret  = VarE 'return
phonemeRet' mkMaj mkMin cs Nothing rslts
  | (cs == CDep) = AppE ret $ formMulExp $ forMap rslts $ \rslt -> (mkMin rslt)
  | (cs == CMaj) = AppE ret $ formMulExp $ forMap rslts $ \rslt -> (mkMaj rslt)
  | otherwise    = AppE ret $ formMulExp $ forMap rslts $ \rslt -> (mkMin rslt)
  where
    ret  = VarE 'return

condCase :: Name -> (Exp -> Exp) -> (Exp -> Exp) -> MulExp -> Exp
condCase blName mkMaj mkMin exprs
  | (fmap mkMaj exprs == fmap mkMin exprs) = AppE ret $ formMulExp $ forMap exprs $ \expr -> (mkMaj expr)
  | otherwise = 
      CondE 
        (VarE blName) 
        (AppE ret $ formMulExp $ forMap exprs $ \expr -> mkMaj expr) 
        (AppE ret $ formMulExp $ forMap exprs $ \expr -> mkMin expr)
  where
    ret = VarE 'return

condCaseExp :: Exp -> (Exp -> Exp) -> (Exp -> Exp) -> MulExp -> Exp
condCaseExp blExpr mkMaj mkMin exprs
  | (fmap mkMaj exprs == fmap mkMin exprs) = formMulExp $ fmap (\expr -> AppE ret (mkMaj expr)) exprs
  | otherwise = 
    CondE 
      blExpr 
      (AppE ret $ formMulExp $ forMap exprs $ \expr -> mkMaj expr) 
      (AppE ret $ formMulExp $ forMap exprs $ \expr -> mkMin expr)
  where
    ret = VarE 'return

----------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------

{-
data ParserParsingState = ParserParsingState
  { ppsClassDictionary :: M.Map String (S.Set Char)
  , ppsPhonemePatterns :: M.Map PhoneName [PhonemePattern]
  } deriving (Show, Eq)

parseOrthographyFile :: AT.Parser (HeaderData, ParserParsingState, [String])

data PhonemePattern = PhonemePattern 
  { isUpperPP  :: Caseness      -- ^ Is this pattern upper-case?
  , charPatsPP :: [CharPattern] -- ^ The pattern of `Char`s for this phoneme.
  } deriving (Show, Eq)




-}

-- :m + Metamorth.Interpretation.Parser.Parsing Metamorth.Interpretation.Parser.Types Data.Either Control.Monad Metamorth.Interpretation.Parser.TH Metamorth.Interpretation.Parser.Parsing.Types
-- :set -XTemplateHaskell
-- import Data.Text.IO qualified as TIO
-- import Data.Attoparsec.Text qualified as AT
-- setupTrie' <$> ppsPhonemePatterns <$> (tempTester (\(_,x,_) -> x)) =<< AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample1.thyp"
-- (tempTester (\(_,x,_) -> setupTrie' $ ppsPhonemePatterns $ x)) =<< AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample1.thyp"
-- 
-- fmap (pathifyTrie . ppsPhonemePatterns) $ (tempTester (\(_,x,_) -> x)) =<< AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample2.thyp"

-- (constructFunctions exampleInfo) =<< (fmap (pathifyTrie . ppsPhonemePatterns) $ (tempTester (\(_,x,_) -> x)) =<< AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample2.thyp")
-- (runQ . (constructFunctions exampleInfo)) =<< (fmap (pathifyTrie . ppsPhonemePatterns) $ (tempTester (\(_,x,_) -> x)) =<< AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample2.thyp")
-- :m + Language.Haskell.TH Language.Haskell.TH.Syntax Language.Haskell.TH.Ppr

-- 
-- 
-- fmap (ppr . (fromRight [])) $ (runQ . (constructFunctions exampleInfo)) =<< (fmap (pathifyTrie . ppsPhonemePatterns) $ (tempTester (\(_,x,_) -> x)) =<< AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample2.thyp")


pathifyTrie :: M.Map MultiPhoneName [PhonemePattern] -> TM.TMap CharPattern (TrieAnnotation, Maybe (MultiPhoneName, Caseness))
pathifyTrie = unifyPaths . setupTrie'

setupTrie' :: M.Map MultiPhoneName [PhonemePattern] -> TM.TMap CharPattern (MultiPhoneName, Caseness)
setupTrie' phonePats = TM.fromList $ concatMap phoneStuff $ M.toList phonePats

  where
    phoneStuff :: (MultiPhoneName, [PhonemePattern]) -> [([CharPattern], (MultiPhoneName, Caseness))]
    phoneStuff (pname, phPats) = forMap phPats $ \thisPat ->
      (charPatsPP thisPat, (pname, isUpperPP thisPat))

-- | Temp function to test functions in REPL.
tempTester :: (a -> b) -> Either String a -> IO b
tempTester f (Right x) = return $ f x
tempTester f (Left st) = fail $ st

{-


-}

----------------------------------------------------------------
-- Pre-Constructed Expressions
----------------------------------------------------------------

-- | Create the expression
--   @ exp1 >>= exp2 @
infixBind :: Exp -> Exp -> Exp
infixBind exp1 exp2 = InfixE (Just exp1) (VarE '(>>=)) (Just exp2)

infixCont :: Exp -> Exp -> Exp
infixCont exp1 exp2 = InfixE (Just exp1) (VarE '(>>))  (Just exp2)

-- | One expression to be used in case I decide
--   to change the type to @StateT State Parser a@.
--   In which case I can change this to @lift AT.anyChar@.
anyCharE :: Exp
anyCharE = VarE 'AT.anyChar

peekCharE :: Exp
peekCharE = VarE 'AT.peekChar

peekCharE' :: Exp
peekCharE' = VarE 'AT.peekChar'

trueE :: Exp
trueE = ConE 'True

falseE :: Exp
falseE = ConE 'False

boolE :: Bool -> Exp
boolE True  = trueE
boolE False = falseE

parserT :: Type
parserT = ConT ''AT.Parser

parserT' :: Type -> Type
parserT' typ = AppT parserT typ


eqJustChar :: Name -> Char -> Exp
eqJustChar mbVar theChar
  = InfixE (Just (VarE mbVar)) (VarE '(==)) (Just (AppE (ConE 'Just) (LitE (CharL theChar))))

otherwiseG :: Exp -> (Guard, Exp)
otherwiseG exp1 = (NormalG (VarE 'otherwise), exp1)

orE :: Exp -> Exp -> Exp
orE x y = InfixE (Just x) (VarE '(||)) (Just y)

anyE :: [Exp] -> Exp
anyE xs  = case (NE.nonEmpty xs) of
  Nothing   -> falseE -- since False is the identity of "or".
  (Just ys) -> intersperseInfixRE (VarE '(||)) ys

-- | Lift the name of a predicate to one
--   that works on @Maybe a@.
liftPred :: Name -> Exp
liftPred funcName = (AppE (VarE 'any) (VarE funcName))

strE :: String -> Exp
strE str = LitE (StringL str)

-- | Convert a `MulExp` into a plain `Exp`
--   by making it a `NonEmpty`.
formMulExp :: MulExp -> Exp
formMulExp (expr :| [])
  = InfixE (Just expr) (ConE '(:|)) (Just (ConE '[]))
formMulExp (expr :| xs)
  = InfixE (Just expr) (ConE '(:|)) (Just (ListE xs))

-- | Lifting `phoneNamePattern` to work over `NonEmpty`.
--   Actually combining the expressions will be done
--   at a later step.
phoneNamePatterns :: M.Map String Name -> (M.Map String ([M.Map String Name])) -> MultiPhoneName -> Either [String] (NonEmpty Exp)
phoneNamePatterns patMap patConMap phoneNames
  = Bi.first fold $ liftEitherNonEmpty $ fmap (phoneNamePattern patMap patConMap) phoneNames
  -- fold :: (NonEmpty [String]) -> [String]

-- | Construct a constructor/pattern synonym
--   for a specific Phoneme Name.
phoneNamePattern :: M.Map String Name -> (M.Map String ([M.Map String Name])) -> PhoneName -> Either [String] Exp
phoneNamePattern patMap _patConMap (PhoneName nom []) = do
  patNom <- eitherMaybe' (M.lookup nom patMap) ["Couldn't find phoneme: \"" <> nom <> "\"."]
  return $ ConE patNom
phoneNamePattern patMap patConMap (PhoneName nom ps) = do
  patNom <- eitherMaybe' (M.lookup nom    patMap) ["Couldn't find phoneme: \"" <> nom <> "\"."]
  patSub <- eitherMaybe' (M.lookup nom patConMap) ["Couldn't find phoneme properties: \"" <> nom <> "\"."]
  -- This way it'll collect all errors instead of just the first one.
  opts <- liftEitherList $ withZip ps patSub $ \aspectOption aspectMap ->
    eitherMaybe' (M.lookup aspectOption aspectMap) ("Couldn't find aspect option \"" <> aspectOption <> "\" for phoneme \"" <> nom <> "\".")
  return $ multiAppE (ConE patNom) (map ConE opts)

-- | For constructing the guard part of 
--   a body.
charPatternGuard :: (M.Map String (Name,[Char])) -> Name -> CharPattern -> Name -> Either String Exp
charPatternGuard _classMap _endWordFunc (PlainChar c)   charVarName = Right (eqJustChar charVarName c)
charPatternGuard _classMap _endWordFunc (CharOptCase c) charVarName = Right (anyE (map (eqJustChar charVarName) (getCases c)))
charPatternGuard  classMap _endWordFunc (CharClass cnm) charVarName = do
  (funcName, _) <- eitherMaybe' (M.lookup cnm classMap) ("Couldn't find class name: \"" <> cnm <> "\".")
  return $ AppE (VarE funcName) (VarE charVarName)
charPatternGuard _classMap _endWordFunc WordStart _charVarName = Left $ "Can't have a 'WordStart' makrer in the middle of a Word."
charPatternGuard _classMap  endWordFunc WordEnd    charVarName = return $ AppE (VarE endWordFunc) (VarE charVarName)

charPatternGuard' :: (M.Map String (Name,[Char])) -> Name -> CharPattern -> Name -> Either String (Exp, Bool)
charPatternGuard' _classMap _endWordFunc (PlainChar   c) charVarName = Right (eqJustChar charVarName c, isCasable c)
charPatternGuard' _classMap _endWordFunc (CharOptCase c) charVarName = Right (anyE (map (eqJustChar charVarName) (getCases c)), isCasable c)
charPatternGuard'  classMap _endWordFunc (CharClass cnm) charVarName = do
  (funcName, chrs) <- eitherMaybe' (M.lookup cnm classMap) ("Couldn't find class name: \"" <> cnm <> "\".")
  return (AppE (VarE funcName) (VarE charVarName), any isCasable chrs)
charPatternGuard' _classMap _endWordFunc WordStart _charVarName = Left $ "Can't have a 'WordStart' makrer in the middle of a Word."
charPatternGuard' _classMap  endWordFunc WordEnd    charVarName = return (AppE (VarE endWordFunc) (VarE charVarName), False)


{-

data CharPattern
  = PlainChar Char   -- ^ A single `Char`.
  | CharOptCase Char -- ^ Any case of a `Char`.
  | CharClass String -- ^ Any member of a class from the header.
  | WordStart        -- ^ The start of a word.
  | WordEnd          -- ^ The end of a word.
  deriving (Show, Eq, Ord)

-}

exampleInfo :: StaticParserInfo
exampleInfo
  = StaticParserInfo
      -- hmm...
      (M.fromList $ forMap [1..100]   $ \n -> (TrieAnn n, mkName ("trieAnn_" ++ show n)))
      (M.fromList $ forMap ['a'..'z'] $ \c -> ([c], mkName $ dataName [c]))
      (M.fromList $ forMap ['a'..'z'] $ \c -> ([c], []))
      (M.empty) -- class Map (empty for now)
      id
      id
      (mkName "notSomeChar")
      (mkName "Phoneme")



{-
data StaticParserInfo = StaticParserInfo
  -- | A `M.Map` from `TrieAnnotation`s to function `Name`s, 
  --   where the function `Name` refers to the function
  --   that corresponds to that node in the `TM.TMap`.
  { spiAnnotationMap  :: M.Map TrieAnnotation Name
  -- | A `M.Map` from phoneme `String`s to Constructor/Pattern Synonym `Name`s.
  , spiConstructorMap :: M.Map String Name
  -- | A `M.Map` from phoneme `String`s to the constructors of the
  --   arguments (i.e. aspects) of the constructor of that Phoneme. 
  --   Each argument is represented by a `M.Map` from `String`s to
  --   `Name`s, where each `Name` refers to a constructor itself.
  --
  --   For example, let's say you have the following phoneme and
  --   aspect definitions:
  --
  --   > aspect release : plain labial palatal
  --   > aspect voice   : voiceless voiced ejective
  --   >
  --   > ====
  --   > 
  --   > ...
  --   > k : voice release
  --   > ...
  --
  --   Then the entry for @k@ in @spiAspectMaps` would be
  --   something like...
  --   
  --   > ("k", 
  --   >   [ fromList [("plain", "Plain"), ("labial", "Labial"), ("palatal", "Palatal")]
  --   >   , fromList [("voiceless", "Voiceless"), ("voiced", "Voiced"), ("ejective", "Ejective")]
  --   >   ]
  --   >  )
  , spiAspectMaps     :: M.Map String [M.Map String Name]
  -- | A `M.Map` from class `String` names to function `Name`s.
  --   These functions will have the type @`Char` -> `Bool`@,
  --   and just be simple tests of whether the `Char` is one
  --   of the members of the class. e.g.
  --
  --   > isApost :: Char -> Bool
  --   > isApost x = (x == '\'') || (x == '`') || (x == '\x313')
  --   
  --   etc...
  , spiClassMap       :: M.Map String (Name,[Char])
  -- | A function to turn a `Phoneme` expression/value into
  --   a upper-case value. This is represented as a function
  --   for more flexibility. This should be a very simple function.
  --   For uncased orthographies, it should just be @`id`@ or the
  --   same as @`spiMkMin`@. For cased orthographies, it should 
  --   just be a function like:
  --
  --   > mkMaj :: Exp -> Exp
  --   > mkMaj expr = AppE (ConE upperName) expr
  --
  --   where @upperName` is the `Name` for the Upper-case constructor.
  , spiMkMaj          :: (Exp -> Exp)
  -- | Same as `spiMkMaj`, but for lower-case instead.
  , spiMkMin          :: (Exp -> Exp)
  -- | The `Name` of a function that checks whether a `Char`
  --   is *NOT* one of the characters that can start a phoneme
  --   mid-word. If a peeked `Char` satisfies this predicate,
  --   then we know we are at the end of a word.
  , spiEndWordFunc    :: Name
  -- | The `Name` of the `Type` used for phonemes.
  , spiPhoneTypeName  :: Name
  }


-}


