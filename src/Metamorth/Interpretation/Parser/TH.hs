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
  ) where

import Control.Monad

import Data.Attoparsec.Text qualified as AT

import Data.Bifunctor qualified as Bi

import Data.Char
import Metamorth.Helpers.Char
import Metamorth.Helpers.Either
import Metamorth.Helpers.List

import Data.Maybe

import Data.Text qualified as T -- ?

import Data.Map.Strict qualified as M

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
  --   >   [ fromList [("plain", "Plain"), ("labial","Labial"), ("palatal","Palatal")]
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
  -- unnecessary; can just use "any".
  -- | The `Name` of the function that is just `isUpperCase`
  --   lifted to `Maybe` values.
  -- , spiIsUpperCaseM   :: Name
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
      -- && ((spiIsUpperCaseM x) == (spiIsUpperCaseM y))
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
      -- <> ", spiIsUpperCaseM = "   <> show (spiIsUpperCaseM x)
      <> "}"
    where
      exprNom  = mkName "expr"
      exprPat  = VarP exprNom
      exprVar  = VarE exprNom
      mkMajRep = LamE [exprPat] (spiMkMaj x exprVar)
      mkMinRep = LamE [exprPat] (spiMkMin x exprVar)
      

makeGuards
  :: Maybe Name                    -- ^ The name of the boolean variable.
  -> Name                          -- ^ The name of the peeked char variable.
  -> TrieAnnotation                -- ^ This node's Annotation
  -> (Maybe (PhoneName, Caseness)) -- ^ This node's output value.
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneName, Caseness)) -- ^ This node in the Trie.
  -> M.Map TrieAnnotation Name     -- ^ Mapping from `TrieAnnotation`s to function names.
  -> (Exp -> Exp)                  -- ^ Maj-maker
  -> (Exp -> Exp)                  -- ^ Min-maker
  -> (M.Map String Name)           -- ^ Map for patterns/constructors.
  -> (M.Map String ([M.Map String Name])) -- ^ Map for constructors of sub-elements.
  -> (M.Map String (Name, [Char])) -- ^ Map for class function names.
  -> Name                          -- ^ Name of the "end of word" function.
  -- -> Name                          -- ^ The `isUpperMaybe` function.
  -> [CharPattern]                 -- ^ The `CharPattern` leading up to this point.
  -> Either [String] (Body, [((TrieAnnotation, Maybe (PhoneName, Caseness)), Bool, TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneName, Caseness)))])
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
          cstrExp    <- phoneNamePattern patMap aspMaps pnom
          guardThing <- Bi.first (:[]) $ charPatternGuard classMap endWordFunc chrP charVarName
          return (( guardThing , consumerRet' mkMaj mkMin chrP cs mbl cstrExp), Nothing)
        -- (Trie)
        -- [((TrieAnnotation, Maybe (PhoneName, Caseness)), Bool, TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneName, Caseness)))]
        elm@(tnn@(TrieAnn _), mph) -> do
          nextFunc <- eitherMaybe' (M.lookup tnn funcMap) ["Couldn't find function for pattern: \"" <> (ppCharPats $ precPatrn ++ [chrP]) <> "\"."]
          guardThing <- Bi.first (:[]) $ charPatternGuard classMap endWordFunc chrP charVarName
          -- idk...
          let expVal = consumerFunc' blName nextFunc
          -- The True is since the next function expects a 
          return ((guardThing, expVal),Just (elm, True, thisSubTrie))

    -- This guard matches @peekedChar == `Nothing`@, or when
    -- none of the possible matches are correct.
    finalRslt = otherwiseG <$> case mTrieVal of
      Nothing -> return $ AppE (VarE 'fail) (LitE (StringL $ "Couldn't find a match for pattern: \"" ++ (ppCharPats precPatrn) ++ "\"."))
      (Just (pnom, cs)) -> do 
        cstrExp    <- phoneNamePattern patMap aspMaps pnom
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
          cstrExp    <- phoneNamePattern patMap aspMaps pnom
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
              return ((guardThing, rsltX), Just (elm, True, thisSubTrie))
            else do
              -- uh...
              let rsltX = consumerFuncX nextFunc
              return ((guardThing, rsltX), Just (elm, False, thisSubTrie))
    
    finalRslt = otherwiseG <$> case mTrieVal of
      Nothing -> return $ AppE (VarE 'fail) (LitE (StringL $ "Couldn't find a match for pattern: \"" ++ (ppCharPats precPatrn) ++ "\"."))
      (Just (pnom, cs)) -> do
        cstrExp <- phoneNamePattern patMap aspMaps pnom
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

consumerRetX
  :: (Exp -> Exp) -- ^ A function to make upper-case constructors.
  -> (Exp -> Exp) -- ^ A function to make lower-case constructors
  -> CharPattern  -- ^ The `Char` in question.
  -> Caseness     -- ^ The `Caseness` of the Phoneme.
  -> Name         -- ^ The `Name` of the next peeked `Char`.
  --- > Name         -- ^ The `Name` of the @isUpperCaseM` function.
  -> Exp          -- ^ The uncased value of this phoneme.
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
  -> Exp          -- ^ The uncased value of this phoneme.
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
  -> Exp          -- ^ The uncased value of this phoneme.
  -> Exp          -- ^ The resulting expression.
consumerRet mkMaj mkMin c cs mblName rslt = infixCont anyCharE (phonemeRet mkMaj mkMin c cs mblName rslt)

-- | For when you still don't have a clue
--   whether this phoneme is upper or lower.
phonemeRetZ
  :: (Exp -> Exp) -- ^ mkMaj
  -> (Exp -> Exp) -- ^ mkMin
  -> Caseness     -- ^ Caseness
  -> Name         -- ^ Name of the next peekedChar
  -- -> Name         -- ^ Name of the @isUpperCaseM@ function.
  -> Exp          -- ^ Uncased expression
  -> Exp          -- ^ Result
phonemeRetZ mkMaj mkMin cs peekName rslt
  | cs == CMaj = AppE ret (mkMaj rslt)
  | cs == CMin = AppE ret (mkMin rslt)
  | otherwise  = condCaseExp (AppE (liftPred 'isUpperCase) (VarE peekName)) mkMaj mkMin rslt
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
  -> Exp          -- ^ The uncased value of this phoneme.
  -> Exp          -- ^ The resulting expression.
phonemeRet mkMaj mkMin c cs (Just blName) rslt
  | (cs == CMaj) = AppE ret (mkMaj rslt)
  | (cs == CMin) = AppE ret (mkMin rslt)
  | otherwise    = condCase blName mkMaj mkMin rslt
  where
    ret  = VarE 'return
phonemeRet mkMaj mkMin c cs Nothing rslt
  | (isUp && cs == CDep) = AppE ret (mkMaj rslt)
  | (cs == CDep)         = AppE ret (mkMin rslt)
  | (cs == CMaj)         = AppE ret (mkMaj rslt)
  | otherwise            = AppE ret (mkMin rslt)
  where
    isUp = isTupper c
    ret  = VarE 'return

phonemeRet'
  :: (Exp -> Exp) -- ^ A function to make upper-case constructors.
  -> (Exp -> Exp) -- ^ A function to make lower-case constructors
  -> Caseness     -- ^ The `Caseness` of the Phoneme.
  -> (Maybe Name) -- ^ The name of the variable with the caseness value.
  -> Exp          -- ^ The uncased value of this phoneme.
  -> Exp          -- ^ The resulting expression.
phonemeRet' mkMaj mkMin cs (Just blName) rslt
  | (cs == CMaj) = AppE ret (mkMaj rslt)
  | (cs == CMin) = AppE ret (mkMin rslt)
  | otherwise    = condCase blName mkMaj mkMin rslt
  where
    ret  = VarE 'return
phonemeRet' mkMaj mkMin cs Nothing rslt
  | (cs == CDep) = AppE ret (mkMin rslt)
  | (cs == CMaj) = AppE ret (mkMaj rslt)
  | otherwise    = AppE ret (mkMin rslt)
  where
    ret  = VarE 'return

condCase :: Name -> (Exp -> Exp) -> (Exp -> Exp) -> Exp -> Exp
condCase blName mkMaj mkMin expr
  | (mkMaj expr == mkMin expr) = AppE ret (mkMaj expr)
  | otherwise                  = CondE (VarE blName) (AppE ret (mkMaj expr)) (AppE ret (mkMin expr))
  where
    ret = VarE 'return

condCaseExp :: Exp -> (Exp -> Exp) -> (Exp -> Exp) -> Exp -> Exp
condCaseExp blExpr mkMaj mkMin expr
  | (mkMaj expr == mkMin expr) = AppE ret (mkMaj expr)
  | otherwise = CondE blExpr (AppE ret (mkMaj expr)) (AppE ret (mkMin expr))
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

-- :m + Metamorth.Interpretation.Parser.Parsing Metamorth.Interpretation.Parser.Types Data.Either Control.Monad Metamorth.Interpretation.Parser.TH
-- :set -XTemplateHaskell
-- import Data.Text.IO qualified as TIO
-- import Data.Attoparsec.Text qualified as AT
-- setupTrie' <$> ppsPhonemePatterns <$> (tempTester (\(_,x,_) -> x)) =<< AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample1.thyp"
-- (tempTester (\(_,x,_) -> setupTrie' $ ppsPhonemePatterns $ x)) =<< AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample1.thyp"
-- 

-- :m + Language.Haskell.TH Language.Haskell.TH.Syntax Language.Haskell.TH.Ppr


pathifyTrie :: M.Map PhoneName [PhonemePattern] -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneName, Caseness))
pathifyTrie = unifyPaths . setupTrie'

setupTrie' :: M.Map PhoneName [PhonemePattern] -> TM.TMap CharPattern (PhoneName, Caseness)
setupTrie' phonePats = TM.fromList $ concatMap phoneStuff $ M.toList phonePats

  where
    phoneStuff :: (PhoneName, [PhonemePattern]) -> [([CharPattern], (PhoneName, Caseness))]
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




