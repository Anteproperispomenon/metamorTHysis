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
  -- * Parser Generation
  ( makeTheParser
  , ParserOptions(..)
  , defParserOptions
  , defParserOptions'
  -- * Output Types
  , StaticParserInfo(..)
  -- * Testing Helpers
  , testTheParser
  , testTheParserE
  , exampleInfo
  , exampleInfo2
  , exampleInfo3
  , tempTester
  -- * Debug Functions
  , setupTrie'
  , pathifyTrie
  , makeGuards
  , constructFunctions
  , createStateDecs
  , constructFunctionsBothX
  , makeClassDec
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS

import Control.Monad.Trans.State.Strict qualified as State

import Data.Attoparsec.Text qualified as AT

import Data.Bifunctor qualified as Bi

import Data.Foldable
-- import Data.Functor

import Data.Traversable

import Data.Char
import Metamorth.Helpers.Char
import Metamorth.Helpers.Either
import Metamorth.Helpers.List
import Metamorth.Helpers.QS

import Data.Unique
import Data.Either
import Data.Maybe

import Data.Text    qualified as T -- ?
import Data.Text.IO qualified as TIO

import Data.Map.Strict qualified as M
import Metamorth.Helpers.Map

import Data.Set qualified as S

import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty(..))

import Data.Trie.Map qualified as TM
import Metamorth.Helpers.Trie

import Data.Tuple

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Haskell.TH.Ppr qualified as PP

import THLego.Helpers

import Metamorth.Interpretation.Parser.Parsing (parseOrthographyFile)
import Metamorth.Interpretation.Parser.Types
import Metamorth.Interpretation.Parser.Parsing.Types
import Metamorth.Interpretation.Parser.Parsing.Trie (generaliseStateTrie)
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

testTheParserE
  :: StaticParserInfo
  -> String -- File Path
  -> IO ([Dec], (StaticParserInfo, Name))
testTheParserE spi fp = do
  theFile <- TIO.readFile fp
  let eParseRslt = AT.parseOnly parseOrthographyFile theFile
  (x,y,z) <- case eParseRslt of
    (Left err) -> fail $ "Couldn't parse input: " ++ err
    -- (HeaderData, ParserParsingState, [String])
    (Right (_hdr, pps, errStrings, warnStrings)) -> do
      case errStrings of
        [] -> return ()
        _s -> do
          putStrLn "Encountered errors while parsing patterns:"
          mapM_ putStrLn errStrings
      case warnStrings of
        [] -> return ()
        _s -> do
          putStrLn "Encountered warnings while parsing patterns:"
          mapM_ putStrLn warnStrings
      runQ
        ( makeTheParser
            (spiConstructorMap spi)
            (spiAspectMaps spi)
            (spiPhoneTypeName spi)
            (spiMkMaj spi)
            (spiMkMin spi)
            (spiWordTypeNames spi)
            pps
            defParserOptions'
        )
  return (x, (y,z))

testTheParser
  :: StaticParserInfo
  -> String -- File Path
  -> IO ([Dec], (StaticParserInfo, Name))
testTheParser spi fp = do
  theFile <- TIO.readFile fp
  let eParseRslt = AT.parseOnly parseOrthographyFile theFile
  (x,y,z) <- case eParseRslt of
    (Left err) -> fail $ "Couldn't parse input: " ++ err
    -- (HeaderData, ParserParsingState, [String])
    (Right (_hdr, pps, _someStrings, _warnStrings)) -> do
      runQ
        ( makeTheParser
            (spiConstructorMap spi)
            (spiAspectMaps spi)
            (spiPhoneTypeName spi)
            (spiMkMaj spi)
            (spiMkMin spi)
            (spiWordTypeNames spi)
            pps
            defParserOptions'
        )
  return (x,(y,z))

-- | Options for how to procede when
--   constructing the parser.
data ParserOptions = ParserOptions
  -- | Whether to unify intermediate
  --   parsing branches (See the top
  --   of this module for an example).
  --   This should be fine most of the
  --   time, but it may cause problems
  --   with compilation if you have some
  --   weird casing going on.
  { poUnifyBranches :: Bool
  -- | Whether to group guards on the same
  --   pattern together, instead of only
  --   having one guard per pattern. This
  --   should work fine in almost any case.
  , poGroupGuards   :: Bool
  -- | Try fixing the states so that you
  --   don't have an issue where a more
  --   general pattern gets rejected when
  --   in a more specific state. You'll 
  --   almost certainly want this on if
  --   you use states at all.
  , poCheckStates   :: Bool
  -- | A `String` representing what you
  --   want the main function name to be.
  , poMainFuncName  :: String
  -- | The suffix appended to names generated
  --   for this parser
  , poNameSuffix    :: String
  } deriving (Show, Eq)

defParserOptions' :: ParserOptions
defParserOptions' = ParserOptions
  { poUnifyBranches = True
  , poGroupGuards   = True
  , poCheckStates   = True
  , poMainFuncName  = "theActualParser"
  , poNameSuffix    = "_test"
  }

-- | Needs a `String` for the suffix
defParserOptions :: String -> ParserOptions
defParserOptions sfx = ParserOptions
  { poUnifyBranches = True
  , poGroupGuards   = True
  , poCheckStates   = True
  , poMainFuncName  = "theActualParser"
  , poNameSuffix    = sfx
  }

-- | Construct the actual parser code.
makeTheParser
  :: M.Map String Name                -- ^ A `M.Map` from `String`s to Pattern Synonym `Name`s.
  -> M.Map String [M.Map String Name] -- ^ A list of `M.Map`s for the constructors of each argument of the Phoneme.
  -> Name                             -- ^ The name of the type of Phonemes.
  -> (Exp -> Exp)                     -- ^ How to convert a Pattern synonym to an upper-case character.
  -> (Exp -> Exp)                     -- ^ How to convert a Pattern synonym to an lower-case character.
  -> (Name, (Name, Name))             -- ^ The type/data constructors for the word/punct type.
  -> ParserParsingState               -- ^ The data from parsing the specification.
  -> ParserOptions                    -- ^ Parser 
  -> Q ([Dec], StaticParserInfo, Name)
makeTheParser phoneMap aspectMap phoneName mkMaj mkMin wordDataNames pps pops
  = runQS (poNameSuffix pops) $ makeTheParser' phoneMap aspectMap phoneName mkMaj mkMin wordDataNames pps pops

-- | Construct the actual parser code.
makeTheParser'
  :: M.Map String Name                -- ^ A `M.Map` from `String`s to Pattern Synonym `Name`s.
  -> M.Map String [M.Map String Name] -- ^ A list of `M.Map`s for the constructors of each argument of the Phoneme.
  -> Name                             -- ^ The name of the type of Phonemes.
  -> (Exp -> Exp)                     -- ^ How to convert a Pattern synonym to an upper-case character.
  -> (Exp -> Exp)                     -- ^ How to convert a Pattern synonym to an lower-case character.
  -> (Name, (Name, Name))             -- ^ The type/data constructors for the word/punct type.
  -> ParserParsingState               -- ^ The data from parsing the specification.
  -> ParserOptions                    -- ^ Parser 
  -> QS ([Dec], StaticParserInfo, Name)
makeTheParser' phoneMap aspectMap phoneName mkMaj mkMin wordDataNames pps pops = do
  let classDictX = ppsClassDictionary pps
      stateDictX = ppsStateDictionary pps
      phonePats  = ppsPhonemePatterns pps
      -- mainTrie  = if (poGroupGuards pops) then (pathifyTrie phonePats) else (dontPathifyTrie phonePats)
      mainTrie   = setupTrie pops phonePats
  (stDecs, stateTypeName, stateConsName, defStateName, newStateDict) <- createStateDecs "StateType" "StateCons" stateDictX
  (clDecs, newClassDict) <- makeClassDecs classDictX
  -- Getting the annotation map...
  let annots' = map fst $ TM.elems mainTrie
      annots  = nubSort annots'
  annNames <- fmap M.fromList $ forM annots $ \ann -> do
    nom <- annFuncName ann
    return (ann, nom)

  (charPredDecs, (punctName, isEndName, noEndName)) <- makeWordEndFunctions mainTrie newClassDict

  let spi = StaticParserInfo
              { spiAnnotationMap   = annNames
              , spiConstructorMap  = phoneMap
              , spiAspectMaps      = aspectMap
              , spiClassMap        = newClassDict
              , spiMkMaj           = mkMaj
              , spiMkMin           = mkMin
              , spiEndWordFunc     = isEndName
              , spiNotEndWordFunc  = noEndName
              , spiIsPunctFunc     = punctName
              , spiPhoneTypeName   = phoneName
              , spiStateTypeName   = stateTypeName
              , spiStateConsName   = stateConsName
              , spiDefStateName    = defStateName
              , spiStateDictionary = newStateDict
              , spiWordTypeNames   = wordDataNames
              }

  eFuncRslt <- constructFunctionsBothX spi (poGroupGuards pops) mainTrie
  (funcDecs, nm1, nm2) <- case eFuncRslt of
    (Left errs) -> do
      qReport True (intercalate "\n" errs)
      return ([], mkName "funcErr1", mkName "funcErr2")
    (Right (okay, (x1,x2))) -> return  (okay, x1, x2)

  (epDecs, finalName) <- makeEntryPoint spi nm1 nm2 (poMainFuncName pops)

  return (concat [epDecs, stDecs, clDecs, charPredDecs, funcDecs], spi, finalName)

{-
  :: StaticParserInfo
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness))
  -> Q (Either [String] ([Dec],(Name, Name)))
constructFunctionsBothX spi trie

fmap (\(x,_,_,_) -> ppr x) $ 
  join (fmap 
         (runQ . (createStateDecs "StateType") . ppsStateDictionary) 
         $ (tempTester (\(_,x,_) -> x)) =<< AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample8.thyp")

fmap 
  (ppr . (fromRight [])) 
  $ (runQ . (fmap (fmap fst) . constructFunctionsBothX exampleInfo)) =<< (fmap 
      (pathifyTrie . ppsPhonemePatterns) 
      $ (tempTester (\(_,x,_) -> x)) =<< AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample3.thyp")

x <- AT.peekChar'
case x of
  c1 -> AT.anyChar 
  c2 -> AT.anyChar

constructFunctions 
  :: StaticParserInfo
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (MultiPhoneName, Caseness))
  -> Q (Either [String] [Dec])

data ParserParsingState = ParserParsingState
  { ppsClassDictionary :: M.Map String (S.Set Char)
  , ppsStateDictionary :: M.Map String (Maybe (S.Set String))
  , ppsPhonemePatterns :: M.Map PhoneResult [PhonemePattern]
  } deriving (Show, Eq)



-}


----------------------------------------------------------------
-- Creating information from parsed data
----------------------------------------------------------------

-- | Create the state types and produce a `M.Map` of
--   information about the types.
createStateDecs :: String -> String -> M.Map String (Maybe (S.Set String)) -> QS ([Dec], Name, Name, Name, (M.Map String (Name, Maybe (Name, M.Map String Name))))
createStateDecs stateTypeString stateConsString theMap = do
  -- Will use the same `Name` for both the type
  -- constructor and the data constructor.
  stateTypeName <- newName $ dataName stateTypeString
  stateConsName <- newName $ dataName stateConsString
  rslt1 <- forWithKey theMap $ \stateStr mSet -> do
    recFieldName <- newName $ varName stateStr
    case mSet of
      Nothing -> return (recFieldName, Nothing)
      -- Generate the names for the constructors of the subtype.
      (Just theSet) -> do
        stateValTypeName <- newName $ dataName stateStr
        rslt2 <- for (S.toAscList theSet) $ \valStr -> do
          -- Remember: return this as a list, not a map.
          constrName <- newName $ dataName valStr
          return (valStr, constrName)
        -- Hopefully this is safe...
        let rsltMap = M.fromAscList rslt2
        return (recFieldName, Just (stateValTypeName, rsltMap))
  -- Okay, back to the main level.
  -- Now, we have to convert this map into Decs...
  -- Remember recordAdtDecDeriv :: Name -> [(Name, Type)]   -> [Type] -> Dec
  -- and      sumAdtDecDeriv    :: Name -> [(Name, [Type])] -> [Type] -> Dec
  -- and      showSumInstance   :: Name -> [(Name, String)] -> [Dec]

  -- We don't really need the `String` right here, so...
  let mp1 = M.elems rslt1
  -- Creating the sub-Decs and the values to be
  -- fed into `recordAdtDecDeriv`.
  let decsInfo = forMap mp1 $ \(nom, mCons) -> case mCons of
        Nothing -> ((nom, ConT ''Bool), ([], falseE))
        (Just (typeCstrName, thisMap)) ->
          let showMap = map swap $ M.assocs thisMap
              typeMap = map (second (const [])) showMap
              theDecs = sumAdtDecDeriv  typeCstrName typeMap [ConT ''Eq, ConT ''Ord, ConT ''Enum ]
              showDec = showSumInstance typeCstrName showMap
          in ((nom, AppT (ConT ''Maybe) (ConT typeCstrName)), ((theDecs : showDec), nothingE))

  -- Okay, now the final dec...
  let (recTypes, restDecs') = unzip decsInfo
      (restDecs, defValues) = unzip restDecs'
      recTypeDec = recordAdtDecDeriv stateTypeName stateConsName recTypes [ConT ''Eq, ConT ''Ord, ConT ''Show]

  -- Setting up the default value declaration...
  defValueName <- newName $ "def" ++ stateConsString
  let defRecordVal = multiAppE (ConE stateConsName) defValues
      defValueSign = SigD defValueName (ConT stateTypeName)
      defValueDefn = ValD (VarP defValueName) (NormalB defRecordVal) []
      defValueDecs = [defValueSign, defValueDefn]

  return ((recTypeDec:(defValueDecs ++ concat restDecs)), stateTypeName, stateConsName, defValueName, rslt1)

-- fmap (\(x,_,_,_) -> ppr x) $ join (fmap (runQ . (createStateDecs "StateType") . ppsStateDictionary) $ (tempTester (\(_,x,_) -> x)) =<< AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample8.thyp")


----------------------------------------------------------------
-- Top-Level Function Generator
----------------------------------------------------------------

makeTrieAnnNames :: TM.TMap CharPattern (TrieAnnotation, Maybe a) -> QS (M.Map TrieAnnotation Name)
makeTrieAnnNames theTrie = do
  let anns = map fst $ TM.elems theTrie
  prs <- for anns $ \ann -> do
    annName <- annFuncName ann
    return (ann, annName)
  return $ M.fromList prs

makeClassDec :: Name -> Name -> [Char] -> [Dec]
makeClassDec xvar nom chrSet
  = [ SigD nom (AppT (AppT ArrowT (ConT ''Char)) (ConT ''Bool))
    , FunD nom [Clause [VarP xvar] (NormalB $ anyE (map mkBool chrSet)) []]
    , PragmaD (InlineP nom Inline FunLike AllPhases)
    ]
  where
    mkBool :: Char -> Exp
    mkBool = eqChar xvar

-- M.Map String (Name,[Char])

makeClassDecs :: M.Map String (S.Set Char) -> QS ([Dec], M.Map String (Name,[Char]))
makeClassDecs sdict = do
  xvar <- newName "x"
  -- The output value
  rsltDict <- forWithKey sdict $ \str theSet -> do
    funcName <- newName $ "is" ++ (dataName str)
    let charList = S.toAscList theSet
    return (funcName, charList)
  let decData = M.elems rsltDict
      classDecs = map (uncurry $ makeClassDec xvar) decData
  return (concat classDecs, rsltDict)

-- TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness))

-- | Create the functions that check about the end of a word.
makeWordEndFunctions
  -- | The main trie of the parser
  :: TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness))
  -> M.Map String (Name,[Char])
  -> QS ([Dec], (Name, Name, Name))
makeWordEndFunctions theTrie classDec = do
  let startTrie = snd $ TM.match [WordStart] theTrie
      notStTrie = snd $ TM.match [NotStart ] theTrie
      rstOfTrie = deleteBranch NotStart $ deleteBranch WordStart theTrie
      getOptions :: CharPattern -> [Char]
      getOptions (PlainChar   _ c) = [c]
      getOptions (CharOptCase _ c) = getCases c
      getOptions (CharClass   _ c) = joinListMaybe $ fmap snd $ M.lookup c classDec
      getOptions _ = []

      -- The chars from each of the three categories.
      startChars = map charE $ nubSort $ concatMap getOptions $ getFirstSteps startTrie
      notStChars = map charE $ nubSort $ concatMap getOptions $ getFirstSteps notStTrie
      rstOfChars = map charE $ nubSort $ concatMap getOptions $ getFirstSteps rstOfTrie

  punctExp <- [| \x -> $(pure $ allNeqE (startChars ++ rstOfChars) (VarE 'x)) |]
  isEndExp <- [| \x -> $(pure $ allNeqE (notStChars ++ rstOfChars) (VarE 'x)) |]
  noEndExp <- [| \x -> $(pure $ anyEqE  (notStChars ++ rstOfChars) (VarE 'x)) |]

  punctName <- newName "isPunctChar"
  isEndName <- newName "isEndOfWord"
  noEndName <- newName "notEndOfWord"

  funcSigns <- [t| Char -> Bool |]

  let punctSign = SigD punctName funcSigns
      isEndSign = SigD isEndName funcSigns
      noEndSign = SigD noEndName funcSigns

      punctDec  = FunD punctName [Clause [] (NormalB punctExp) []]
      isEndDec  = FunD isEndName [Clause [] (NormalB isEndExp) []]
      noEndDec  = FunD noEndName [Clause [] (NormalB noEndExp) []]

      theDecs = [punctSign, punctDec, isEndSign, isEndDec, noEndSign, noEndDec]

  return (theDecs,(punctName, isEndName, noEndName))
  where
    joinListMaybe :: Maybe [a] -> [a]
    joinListMaybe (Just xs) = xs
    joinListMaybe _ = []

makeEntryPoint :: StaticParserInfo -> Name -> Name -> String -> QS ([Dec], Name)
makeEntryPoint spi startWordFunc restWordFunc finalFuncStr = do
  let defStateVal     = VarE (spiDefStateName spi)

      startWordFuncQE = pure $ VarE startWordFunc
      restWordFuncQE  = pure $ VarE restWordFunc

      punctWordQE     = pure $ VarE $ spiIsPunctFunc spi

      (wordType, (wordCstr, puncCstr)) = spiWordTypeNames spi

      wordTypeQT = pure $ ConT wordType
      wordCstrQE = pure $ ConE wordCstr
      puncCstrQE = pure $ ConE puncCstr


  -- Building these up one at a time...
  funcExp1 <- [| (liftA2 (:)) $startWordFuncQE (AT.many' $restWordFuncQE) |] -- collect a lisst of phones...
  funcExp2 <- [| concatMap NE.toList <$> $(pure funcExp1) |] -- concat the results...
  funcExp3 <- [| State.evalStateT $(pure funcExp2) $(pure defStateVal) |] -- run them with the state...
  funcExp4 <- [| $wordCstrQE <$>  $(pure funcExp3) |] -- store in the proper type.

  -- Now, the other parsers...
  -- If using `AT.takeWhile` instead, it would always succeed without consuming.
  puncExp  <- [| $puncCstrQE <$> AT.takeWhile1 $punctWordQE |]
  altrExp  <- [| ($puncCstrQE . T.singleton) <$> AT.anyChar |]

  -- The final parser...
  finalExp <- [| AT.many1 ($(pure funcExp4) <|> $(pure puncExp) <|> $(pure altrExp)) |]
  finalFuncName <- newName finalFuncStr

  -- The declaration...
  parserType <- [t| AT.Parser [ $wordTypeQT ] |]

  let parserSign = SigD finalFuncName parserType
      parserDefn = FunD finalFuncName [Clause [] (NormalB finalExp) []]

  return ([parserSign, parserDefn], finalFuncName)


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

checkStateExps :: M.Map String (Name, Maybe (Name, M.Map String Name)) -> [CheckStateX] -> Name -> Either [String] (Maybe Exp)
-}

-- | Creates the cases for the initial parser function.
createCasesForStart :: Name -> Name -> StaticParserInfo -> CharPattern -> TrieAnnotation -> Maybe (PhoneResult, Caseness) -> Either [String] ((Either (Guard, Exp) [Match]), Bool)
createCasesForStart grdName stateValName spi cp trieAnn@(TrieAnn _) _mph = case cp of
  -- hmm
  {-
  (PlainChar [] c) -> do
    funcName <- eitherMaybe' (M.lookup trieAnn funcMap) ["Couldn't find a function name for \"" <> show trieAnn <> "\"."]
    if (isCasable c) 
      then Right (Right [Match (LitP (CharL c)) (NormalB $ consumerFuncE (boolE $ isTupper c) funcName ) []], True)
      else Right (Right [Match (LitP (CharL c)) (NormalB $ consumerFuncX funcName) []], False)
  -}
  (PlainChar chks c) -> do
    funcName <- eitherMaybe' (M.lookup trieAnn funcMap) ["Couldn't find a function name for \"" <> show trieAnn <> "\"."]
    mgrdExp  <- (checkStateExps sdict chks stateValName)
    if (isCasable c)
      then Right (Right [Match (LitP (CharL c)) (maybeGuard mgrdExp $ consumerFuncE (boolE $ isTupper c) funcName ) []], True)
      else Right (Right [Match (LitP (CharL c)) (maybeGuard mgrdExp $ consumerFuncX funcName) []], False)
  (CharOptCase chks c) -> do
    funcName <- eitherMaybe' (M.lookup trieAnn funcMap) ["Couldn't find a function name for \"" <> show trieAnn <> "\"."]
    mgrdExp  <- (checkStateExps sdict chks stateValName)
    let xs = getCases c
        zs = map (\q -> (q, isTupper q)) xs
    if (any isCasable xs)
      then do
        zqr <- return $ forMap zs $ \(theChar, theCase) ->
          (Match (LitP (CharL theChar)) (maybeGuard mgrdExp $ consumerFuncE (boolE theCase) funcName) [])
        return (Right zqr, True)
      else do
        zqr <- return $ forMap xs $ \theChar ->
          (Match (LitP (CharL theChar)) (maybeGuard mgrdExp $ consumerFuncX funcName) [])
        return (Right zqr, False)
  (CharClass chks cn) -> do
    funcName  <- eitherMaybe' (M.lookup trieAnn funcMap)   ["Couldn't find a function name for \"" <> show trieAnn <> "\"."]
    (classFunc, cs) <- eitherMaybe' (M.lookup cn classMap) ["Couldn't find function name for class \"" <> cn <> "\"."]
    mgrdExp  <- (checkStateExps sdict chks stateValName)
    let checkExp  = AppE (VarE classFunc) (VarE grdName)
        boolExp   = AppE (VarE 'isUpperCase) (VarE grdName)
        checkExp' = case mgrdExp of
          Nothing -> checkExp
          (Just guardExp) -> andE checkExp guardExp
    if (any isCasable cs)
      then Right (Left (NormalG checkExp', consumerFuncE boolExp funcName), True )
      else Right (Left (NormalG checkExp', consumerFuncX         funcName), False)
  WordStart -> Left ["Shouldn't encounter a \"WordStart\" when using this function."]
  WordEnd   -> Left ["Shouldn't encounter a \"WordEnd\" when using this function."]
  NotStart  -> Left ["Shouldn't encounter a \"NotStart\" when using this function."]
  NotEnd    -> Left ["Shouldn't encounter a \"NotEnd\" when using this function."]

  where
    funcMap  = spiAnnotationMap spi
    classMap = spiClassMap spi
    sdict    = spiStateDictionary spi
--------------------
-- For Leaf Nodes --
--------------------
-- createCasesForStart 
--  :: Name 
--  -> Name
--  -> StaticParserInfo 
--  -> CharPattern 
--  -> TrieAnnotation 
--  -> Maybe (MultiPhoneName, Caseness) 
--  -> Either [String] ((Either (Guard, Exp) [Match]), Bool)
createCasesForStart grdName stateValName spi cp TrieLeaf mph = case cp of
  -- hmm
  (PlainChar chks c) -> do
    -- funcName <- eitherMaybe' (M.lookup trieAnn funcMap) ("Couldn't find a function name for \"" <> show trieAnn <> "\".")
    (mphZ, cs) <- eitherMaybe' mph ["Couldn't lookup value for parsed character: \"" <> [c] <> "\"."]
    let (PhoneResult mphX stateMods) = mphZ
    mgrdExp  <- (checkStateExps sdict chks stateValName)
    mulExprs <- phoneNamePatterns constrMap aspMaps mphX
    stateModifier <- modifyStateExps sdict stateMods
    if (isCasable c)
      then Right (Right [Match (LitP (CharL c)) (maybeGuard mgrdExp $ stateModifier $ consumerRetA mkMaj mkMin c       cs mulExprs ) []], True )
      else Right (Right [Match (LitP (CharL c)) (maybeGuard mgrdExp $ stateModifier $ consumerRetB mkMaj mkMin grdName cs mulExprs ) []], False)
  (CharOptCase chks c) -> do
    -- funcName <- eitherMaybe' (M.lookup trieAnn funcMap) ["Couldn't find a function name for \"" <> show trieAnn <> "\"."]
    (mphZ, cs) <- eitherMaybe' mph ["Couldn't lookup value for parsed character: \"" <> [c] <> "\"."]
    let (PhoneResult mphX stateMods) = mphZ
    mgrdExp  <- (checkStateExps sdict chks stateValName)
    mulExprs <- phoneNamePatterns constrMap aspMaps mphX
    stateModifier <- modifyStateExps sdict stateMods
    let xs = getCases c
        zs = map (\q -> (q, isTupper q)) xs
    if (any isCasable xs)
      then do
        zqr <- return $ forMap zs $ \(theChar, _theCase) ->
          (Match (LitP (CharL theChar)) (maybeGuard mgrdExp $ stateModifier $ consumerRetA mkMaj mkMin theChar cs mulExprs) [])
        return (Right zqr, True)
      else do
        zqr <- return $ forMap xs $ \theChar ->
          (Match (LitP (CharL theChar)) (maybeGuard mgrdExp $ stateModifier $ consumerRetB mkMaj mkMin grdName cs mulExprs) [])
        return (Right zqr, False)
  (CharClass chks cn) -> do
    -- funcName  <- eitherMaybe' (M.lookup trieAnn funcMap) ["Couldn't find a function name for \"" <> show trieAnn <> "\"."]
    (classFunc, zs) <- eitherMaybe' (M.lookup cn classMap) ["Couldn't find function name for class \"" <> cn <> "\"."]
    (mphZ, cs) <- eitherMaybe' mph ["Couldn't lookup value for parsed class: \"" <> cn <> "\"."]
    let (PhoneResult mphX stateMods) = mphZ
    mgrdExp  <- checkStateExps sdict chks stateValName
    mulExprs <- phoneNamePatterns constrMap aspMaps mphX
    stateModifier <- modifyStateExps sdict stateMods
    let checkExp = AppE (VarE classFunc) (VarE grdName)
        -- boolExp  = AppE (VarE 'isUpperCase) (VarE grdName)
        -- checkExp' = case mgrdExp of
        --   Nothing -> checkExp
        --   (Just guardExp) -> andE checkExp guardExp
    -- can probably just reduce to one expression.
    if (any isCasable zs)
      then Right (Left (NormalG checkExp, stateModifier $ consumerRetB mkMaj mkMin grdName cs mulExprs), True ) -- need to change, maybe?
      else Right (Left (NormalG checkExp, stateModifier $ consumerRetB mkMaj mkMin grdName cs mulExprs), False)
  WordStart -> Left ["Shouldn't encounter a \"WordStart\" when using this function."]
  WordEnd   -> Left ["Shouldn't encounter a \"WordEnd\" when using this function."]
  NotStart  -> Left ["Shouldn't encounter a \"NotStart\" when using this function."]
  NotEnd    -> Left ["Shouldn't encounter a \"NotEnd\" when using this function."]

  where
    classMap = spiClassMap spi
    mkMaj = spiMkMaj spi
    mkMin = spiMkMin spi
    constrMap = spiConstructorMap spi
    aspMaps   = spiAspectMaps spi
    sdict     = spiStateDictionary spi

maybeGuard :: (Maybe Exp) -> Exp -> Body
maybeGuard Nothing        expr = NormalB  expr
maybeGuard (Just grdExpr) expr = GuardedB [(NormalG grdExpr, expr)]

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
  -> ((TM.TMap CharPattern (TrieAnnotation, Maybe a), TM.TMap CharPattern (TrieAnnotation, Maybe a)), TM.TMap CharPattern (TrieAnnotation, Maybe a))
splitTrie trie
  = ((startTrie, notStartTrie), restTrie)
  where
    (_, startTrie)    = TM.match     [WordStart] trie
    (_, notStartTrie) = TM.match     [NotStart]  trie
    restTrie = deleteBranch NotStart $ deleteBranch WordStart trie


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
  { spiAnnotationMap   :: M.Map TrieAnnotation Name
  -- | A `M.Map` from phoneme `String`s to Constructor/Pattern Synonym `Name`s.
  , spiConstructorMap  :: M.Map String Name
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
  , spiAspectMaps      :: M.Map String [M.Map String Name]
  -- | A `M.Map` from class `String` names to function `Name`s.
  --   These functions will have the type @`Char` -> `Bool`@,
  --   and just be simple tests of whether the `Char` is one
  --   of the members of the class. e.g.
  --
  --   > isApost :: Char -> Bool
  --   > isApost x = (x == '\'') || (x == '`') || (x == '\x313')
  --   
  --   etc...
  , spiClassMap        :: M.Map String (Name,[Char])
  -- | A function to turn a `Phoneme` expression/value into
  --   an upper-case value. This is represented as a function
  --   for more flexibility. This should be a very simple function.
  --   For uncased orthographies, it should just be @`id`@ or the
  --   same as @`spiMkMin`@. For cased orthographies, it should 
  --   just be a function like:
  --
  --   > mkMaj :: Exp -> Exp
  --   > mkMaj expr = AppE (ConE upperName) expr
  --
  --   where @upperName` is the `Name` for the Upper-case constructor.
  , spiMkMaj           :: (Exp -> Exp)
  -- | Same as `spiMkMaj`, but for lower-case instead.
  , spiMkMin           :: (Exp -> Exp)
  -- | The `Name` of a function that checks whether a `Char`
  --   is *NOT* one of the characters that can start a phoneme
  --   mid-word. If a peeked `Char` satisfies this predicate,
  --   then we know we are at the end of a word.
  , spiEndWordFunc     :: Name
  -- | The `Name` of a function that checks whether a `Char`
  --   *is* one of the characters that can start a phoneme
  --   mid-word. If a peeked `Char` satisfies this predicate,
  --   then we know we are *NOT* at the end of a word.
  , spiNotEndWordFunc  :: Name
  -- | The function to be used while consuming non-grapheme
  --   characters.
  , spiIsPunctFunc     :: Name
  -- | The `Name` of the `Type` used for phonemes.
  , spiPhoneTypeName   :: Name
  -- | The `Name` of the type of the state value.
  , spiStateTypeName   :: Name
  -- | The `Name` of the data constructor of the state value.
  , spiStateConsName   :: Name
  -- | The `Name` of the default value for the state value.
  , spiDefStateName    :: Name
  -- | The State information dictionary. This is
  --   implemented as a `M.Map` from `String`s to
  --   `Name`s of the argument in the State type.
  --   If the state type has multiple possible
  --   values, then there's also a `M.Map` from
  --   `String`s to `Name`s of constructors.
  , spiStateDictionary :: M.Map String (Name, Maybe (Name, M.Map String Name))
  -- | The `Name`s of the `Word` Type and the
  --   `Name`s of its constructors. The first
  --   constructor is for sequences of phonemes,
  --   the second is for punctuation etc...
  , spiWordTypeNames   :: (Name, (Name, Name))
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
      && ((spiNotEndWordFunc x) == (spiNotEndWordFunc y))
      && ((spiIsPunctFunc x) == (spiIsPunctFunc y))
      && ((spiPhoneTypeName x) == (spiPhoneTypeName y))
      && ((spiStateTypeName x) == (spiStateTypeName y))
      && ((spiStateConsName x) == (spiStateConsName y))
      && ((spiDefStateName x) == (spiDefStateName y))
      && ((spiStateDictionary x) == (spiStateDictionary y))
      && ((spiWordTypeNames x) == (spiWordTypeNames y))
    where
      spiMkMaj' z = (spiMkMaj z) (ConE (mkName "Example"))
      spiMkMin' z = (spiMkMin z) (ConE (mkName "Example"))

instance Show StaticParserInfo where
  show x =
    "StaticParserInfo {spiAnnotationMap = " <> show (spiAnnotationMap x)
      <> ", spiConstructorMap = "  <> show (spiConstructorMap x)
      <> ", spiAspectMaps = "      <> show (spiAspectMaps x)
      <> ", spiClassMap = "        <> show (spiClassMap x)
      <> ", spiMkMaj = "           <> show (PP.ppr mkMajRep)
      <> ", spiMkMin = "           <> show (PP.ppr mkMinRep)
      <> ", spiEndWordFunc = "     <> show (spiEndWordFunc x)
      <> ", spiNotEndWordFunc = "  <> show (spiNotEndWordFunc x)
      <> ", spiIsPunctFunc = "     <> show (spiIsPunctFunc x)
      <> ", spiPhoneTypeName = "   <> show (spiPhoneTypeName x)
      <> ", spiStateTypeName = "   <> show (spiStateTypeName x)
      <> ", spiStateConsName = "   <> show (spiStateConsName x)
      <> ", spiDefStateName = "    <> show (spiDefStateName x)
      <> ", spiStateDictionary = " <> show (spiStateDictionary x)
      <> ", spiWordTypeNames = "   <> show (spiWordTypeNames x)
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


annFuncName :: TrieAnnotation -> QS Name
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

-- splitTrie 

constructFunctionsBothX
  :: StaticParserInfo
  -> Bool
  -- -> [CharPattern]
  -- -> (S.Set TrieAnnotation, M.Map TrieAnnotation Bool)
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness))
  -> QS (Either [String] ([Dec],(Name, Name)))
constructFunctionsBothX spi bl trie
  = constructFunctionsBoth spi bl trie "startParser" "restParser"


constructFunctionsBoth
  :: StaticParserInfo
  -> Bool
  -- -> [CharPattern]
  -- -> (S.Set TrieAnnotation, M.Map TrieAnnotation Bool)
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness))
  -> String
  -> String
  -> QS (Either [String] ([Dec],(Name, Name)))
constructFunctionsBoth spi bl trie funNomStrt funNomRst = do
  let ((trieS, trieNS), trie2) = splitTrie trie
  -- Construct the 
  eRslt1 <- constructFunctionsSB bl spi [WordStart] (S.empty, M.empty) trieS
  case eRslt1 of
    (Left errs) -> return $ Left errs
    (Right (decs1, st1, nom1)) -> do
      eRslt2 <- constructFunctionsSB bl spi [NotStart] st1 trieNS
      case eRslt2 of
        (Left errs) -> return $ Left errs
        (Right (decs2, st2, nom2)) -> do
          eRslt3 <- constructFunctionsSB bl spi [] st2 trie2
          case eRslt3 of
            (Left errs) -> return $ Left errs
            (Right (decs3, _st3, nom3)) -> do
              -- Now, create the function that merges those two.
              -- combinedFuncExp <- [| $(peekCharQ') >>= \pkc -> ( ( $(return $ VarE nom1) pkc) <|> ( $(return $ VarE nom3) pkc) ) |]
              -- followFuncExp   <- [| $(peekCharQ') >>= \pkc -> ( ( $(return $ VarE nom2) pkc) <|> ( $(return $ VarE nom3) pkc) ) |]
              -- These functions provide the state to the `WordStart` function, even though it's unnecessary,
              -- since the state is reset back to "all off" right at the beginning of a word.
              -- The reason to include it is to simplify the function generation.
              combinedFuncExp <-
                [| do { st <- State.get ; pkc <- $(peekCharQ') ; ($(return $ VarE nom1) st pkc) <|> ( $(return $ VarE nom3) st pkc) } |]
              followFuncExp <-
                [| do { st <- State.get ; pkc <- $(peekCharQ') ; ($(return $ VarE nom2) st pkc) <|> ( $(return $ VarE nom3) st pkc) } |]
              let phoneType = spiPhoneTypeName spi
                  stateType = spiStateTypeName spi
                  -- stateCons = spiStateConsName spi
              functionType    <- [t| $(parserTQ (ConT stateType)) (NonEmpty $(return $ ConT phoneType)) |]
              -- Might want to check these strings are valid...
              funNom1 <- newName $ varName funNomStrt
              funNom2 <- newName $ varName funNomRst
              let func1Dec = FunD funNom1 [Clause [] (NormalB combinedFuncExp) []]
                  func2Dec = FunD funNom2 [Clause [] (NormalB followFuncExp  ) []]
                  func1Sig = SigD funNom1 functionType
                  func2Sig = SigD funNom2 functionType
                  theseDecs = [func1Sig, func1Dec, func2Sig, func2Dec]
              return $ Right (theseDecs ++ decs1 ++ decs2 ++ decs3, (funNom1, funNom2))


constructFunctionsPat
  :: StaticParserInfo
  -> [CharPattern]
  -> (S.Set TrieAnnotation, M.Map TrieAnnotation Bool)
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness))
  -> QS (Either [String] ([Dec],Name))
constructFunctionsPat spi cps stVals trie
  = fmap getIt <$> constructFunctionsS spi cps stVals trie
  where getIt (decs,_,nom) = (decs,nom)

constructFunctions
  :: StaticParserInfo
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness))
  -> QS (Either [String] ([Dec], Name))
constructFunctions spi trie
  = fmap getIt <$> constructFunctionsS spi [] (S.empty, M.empty) trie
  where getIt (decs,_,nom) = (decs,nom)

constructFunctionsB
  :: Bool
  -> StaticParserInfo
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness))
  -> QS (Either [String] ([Dec], Name))
constructFunctionsB bl spi trie
  = fmap getIt <$> (constructFunctionsSB bl) spi [] (S.empty, M.empty) trie
  where getIt (decs,_,nom) = (decs,nom)

constructFunctionsS
  :: StaticParserInfo
  -> [CharPattern]
  -> (S.Set TrieAnnotation, M.Map TrieAnnotation Bool)
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness))
  -> QS (Either [String] ([Dec],(S.Set TrieAnnotation, M.Map TrieAnnotation Bool), Name))
constructFunctionsS = constructFunctionsSB True

constructFunctionsSB
  :: Bool
  -> StaticParserInfo
  -> [CharPattern]
  -> (S.Set TrieAnnotation, M.Map TrieAnnotation Bool)
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness))
  -> QS (Either [String] ([Dec],(S.Set TrieAnnotation, M.Map TrieAnnotation Bool), Name))
constructFunctionsSB toReduce spi xcp stVals trie = do
  uniqInt  <- qRunIO $ hashUnique <$> newUnique
  blName   <- newName "isCharMaj"
  peekName <- newName "peekedChar"
  grdName  <- newName "c"
  stateNom <- newName "parseState"
  topFuncName <- newName ("mainParse" <> (show uniqInt))
  let rslts = forMap subTries $ \(cp, ((tann, mval), subTrie )) -> createCasesForStart grdName stateNom spi cp tann mval
  case (liftEitherList rslts) of
    (Left errs)    -> return (Left $ concat errs)
    (Right rsltsX) -> do
      let rsltsY = map (\case {(Left x, y) -> Left (x,y) ; (Right x, y) -> Right (x,y)}) rsltsX
          -- Get the bool results of rsltsX.
          rsltsZ = map snd rsltsX
          -- Zip the bool results with stuff.
          subTriesX = zip subTries rsltsZ
          -- Partition the class matches from the char matches.
          (grds, mtchs) = partitionEithers  rsltsY
          grdBody   = GuardedB $ map fst grds
          -- Matching for class types
          grdMatch  = if (null grds) then [] else [Match (VarP grdName) grdBody []]
          -- The otherwise clause.
          finalMat  = Match WildP (NormalB $ AppE (VarE 'fail) (strE "No matches found.")) []
          -- The resulting case expression.
          matches   = (concatMap fst mtchs) ++ grdMatch ++ [finalMat]
          casePrime = CaseE (VarE peekName) matches
          caseStuff = if toReduce then (groupCaseGuards casePrime) else casePrime
          stateType = spiStateTypeName spi
          -- stateCons = spiStateConsName spi
      mainType <- [t| $(return $ ConT stateType) -> Char -> ( $(parserTQ (ConT stateType)) (NonEmpty $(return $ ConT phoneType)) ) |]
      let mainSign = SigD topFuncName mainType
          mainDec  = FunD topFuncName [Clause [VarP stateNom, VarP peekName] (NormalB caseStuff) []]
      -- okay, the hard part
      (moreDecs, stt, errs) <- runRWST' () stVals $ forM subTriesX $ \((cp, ((tann, mval), subTrie )),thisbl) -> do
        -- let thisbl = True -- TEMPORARY
        constructFunctions' blName peekName thisbl spi subTrie (xcp ++ [cp]) tann mval
      let moreDecs' = concat moreDecs
          errs'     = errs
      case errs' of
        [] -> return (Right ((mainSign:mainDec:moreDecs'),stt,topFuncName))
        xs -> return (Left xs)

  where
    subTries  = map (second (first fromJust)) $ getSubTries trie
    phoneType = spiPhoneTypeName spi
    -- evalRWST' rdr st act = evalRWST act rdr st
    runRWST'  rdr st act = runRWST  act rdr st


constructFunctions'
  :: Name -- ^ The name generated to be used as the bool argument
  -> Name -- ^ The name generated to be used as the char argument
  -> Bool
  -> StaticParserInfo
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness))
  -> [CharPattern]
  -> TrieAnnotation
  -> Maybe (PhoneResult, Caseness)
  -> RWST () [String] (S.Set TrieAnnotation, M.Map TrieAnnotation Bool) QS [Dec] -- ?
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
          statType = ConT $ spiStateTypeName spi
          funcType = arrowChainT margs (parserT' statType phonType')
          funcSign = SigD funcNom funcType
          funcDec  = FunD funcNom [Clause pargs bod []]
      return (funcSign:funcDec:concat moreDecs)

  where
    mbl = if isCased then (Just blName) else Nothing
    -- mbool = AppT (ConT ''Maybe) (ConT ''Bool)
    mbool = (ConT ''Bool)
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
  -> Maybe (PhoneResult, Caseness)
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness)) -- ^ This node in the Trie.
  -> StaticParserInfo -- ^ Constant info about the parser.
  -> [CharPattern]
  -> Either [String] (Body, [((TrieAnnotation, Maybe (PhoneResult, Caseness)), Bool, CharPattern, TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness)))])
constructGuards mbl charVarName trieAnn mTrieVal theTrie spi = makeGuards
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
      (spiNotEndWordFunc spi)
      (spiStateDictionary spi)

makeGuards
  :: Maybe Name                    -- ^ The name of the boolean variable.
  -> Name                          -- ^ The name of the peeked char variable.
  -> TrieAnnotation                -- ^ This node's Annotation
  -> (Maybe (PhoneResult, Caseness)) -- ^ This node's output value.
  -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness)) -- ^ This node in the Trie.
  -> M.Map TrieAnnotation Name     -- ^ Mapping from `TrieAnnotation`s to function names.
  -> (Exp -> Exp)                  -- ^ Maj-maker
  -> (Exp -> Exp)                  -- ^ Min-maker
  -> (M.Map String Name)           -- ^ Map for patterns/constructors.
  -> (M.Map String ([M.Map String Name])) -- ^ Map for constructors of sub-elements.
  -> (M.Map String (Name, [Char])) -- ^ Map for class function names.
  -> Name                          -- ^ Name of the "end of word" function.
  -> Name                          -- ^ Name of the "not end of word" function.
  -> M.Map String (Name, Maybe (Name, M.Map String Name)) -- ^ The state dictionary
  -> [CharPattern]                 -- ^ The `CharPattern` leading up to this point.
  -> Either [String] (Body, [((TrieAnnotation, Maybe (PhoneResult, Caseness)), Bool, CharPattern, TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness)))])
makeGuards mbl@(Just blName) charVarName trieAnn mTrieVal theTrie funcMap mkMaj mkMin patMap aspMaps classMap endWordFunc notEndWordFunc sdict precPatrn = do
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
          (prslt, cs) <- eitherMaybe' mph ["Found a leaf that doesn't have a return value; pattern is \"" <> (ppCharPats $ precPatrn ++ [chrP]) <> "\"."]
          -- cstrPat <- eitherMaybe' (M.lookup (pnName pnom) patMap) ("Can't find constructor for phoneme: \"" <> (pnName pnom) <> "\".")
          -- modifyStateExps :: M.Map String (Name, Maybe (Name, M.Map String Name)) -> [ModifyStateX] -> Either [String] (Exp -> Exp)
          let pnom = prPhonemes  prslt
              pmod = prStateMods prslt
          cstrExps   <- phoneNamePatterns patMap aspMaps pnom
          guardThing <- Bi.first (:[]) $ charPatternGuard classMap endWordFunc notEndWordFunc chrP charVarName
          resultModifier <- modifyStateExps sdict pmod
          return (( guardThing , resultModifier $ consumerRet' mkMaj mkMin chrP cs mbl cstrExps), Nothing)
        -- (Trie)
        -- [((TrieAnnotation, Maybe (PhoneName, Caseness)), Bool, TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneName, Caseness)))]
        elm@(tnn@(TrieAnn _), mph) -> do
          nextFunc <- eitherMaybe' (M.lookup tnn funcMap) ["Couldn't find function for pattern: \"" <> (ppCharPats $ precPatrn ++ [chrP]) <> "\"."]
          guardThing <- Bi.first (:[]) $ charPatternGuard classMap endWordFunc notEndWordFunc chrP charVarName
          -- idk...
          let expVal = consumerFunc' blName nextFunc
          -- The True is since the next function expects a 
          return ((guardThing, expVal),Just (elm, True, chrP, thisSubTrie))

    -- This guard matches @peekedChar == `Nothing`@, or when
    -- none of the possible matches are correct.
    finalRslt = otherwiseG <$> case mTrieVal of
      Nothing -> return $ AppE (VarE 'fail) (LitE (StringL $ "Couldn't find a match for pattern: \"" ++ (ppCharPats precPatrn) ++ "\"."))
      (Just (prslt, cs)) -> do
        let pnom = prPhonemes  prslt
            pmod = prStateMods prslt
        cstrExp        <- phoneNamePatterns patMap aspMaps pnom
        resultModifier <- modifyStateExps sdict pmod
        return $ resultModifier $ phonemeRet' mkMaj mkMin cs mbl cstrExp
makeGuards Nothing charVarName trieAnn mTrieVal theTrie funcMap mkMaj mkMin patMap aspMaps classMap endWordFunc notEndWordFunc sdict precPatrn = do
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
          (prslt, cs) <- eitherMaybe' mph ["Found a leaf that doesn't have a return value; pattern is \"" <> (ppCharPats $ precPatrn ++ [chrP]) <> "\"."]
          let pnom = prPhonemes  prslt
              pmod = prStateMods prslt
          cstrExp    <- phoneNamePatterns patMap aspMaps pnom
          guardThing <- Bi.first (:[]) $ charPatternGuard classMap endWordFunc notEndWordFunc chrP charVarName
          resultModifier <- modifyStateExps sdict pmod
          let xRslt = consumerRetX mkMaj mkMin chrP cs charVarName cstrExp
          return ((guardThing,resultModifier xRslt), Nothing)
        elm@(tnn@(TrieAnn _), mph) -> do
          nextFunc <- eitherMaybe' (M.lookup tnn funcMap) ["Couldn't find function for pattern: \"" <> (ppCharPats $ precPatrn ++ [chrP]) <> "\"."]
          (guardThing, isCased) <- Bi.first (:[]) $ charPatternGuard' classMap endWordFunc notEndWordFunc chrP charVarName
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
      (Just (prslt, cs)) -> do
        -- TODO : Addd in the state stuff
        let pnom = prPhonemes  prslt
            pmod = prStateMods prslt
        cstrExp <- phoneNamePatterns patMap aspMaps pnom
        resultModifier <- modifyStateExps sdict pmod
        return $ resultModifier $ phonemeRetZ mkMaj mkMin cs charVarName cstrExp
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
consumerRetX mkMaj mkMin (PlainChar   _ c) cs peekName rslt = infixCont anyCharE (phonemeRetZ mkMaj mkMin cs peekName rslt)
consumerRetX mkMaj mkMin (CharOptCase _ c) cs peekName rslt = infixCont anyCharE (phonemeRetZ mkMaj mkMin cs peekName rslt)
consumerRetX mkMaj mkMin (CharClass _ _cl) cs peekName rslt = infixCont anyCharE (phonemeRetZ mkMaj mkMin cs peekName rslt)
consumerRetX mkMaj mkMin WordEnd         cs peekName rslt = (phonemeRetZ mkMaj mkMin cs peekName rslt)
consumerRetX mkMaj mkMin WordStart       cs peekName rslt = (phonemeRetZ mkMaj mkMin cs peekName rslt)
consumerRetX mkMaj mkMin NotEnd          cs peekName rslt = (phonemeRetZ mkMaj mkMin cs peekName rslt)
consumerRetX mkMaj mkMin NotStart        cs peekName rslt = (phonemeRetZ mkMaj mkMin cs peekName rslt)


-- | For when the next node is a `AnnLeaf`... I think.
consumerRet'
  :: (Exp -> Exp) -- ^ A function to make upper-case constructors.
  -> (Exp -> Exp) -- ^ A function to make lower-case constructors
  -> CharPattern  -- ^ The `Char` in question.
  -> Caseness     -- ^ The `Caseness` of the Phoneme.
  -> (Maybe Name) -- ^ The name of the variable with the caseness value.
  -> MulExp       -- ^ The uncased value of this phoneme.
  -> Exp          -- ^ The resulting expression.
consumerRet' mkMaj mkMin (PlainChar   _ c) cs mblName rslt = infixCont anyCharE (phonemeRet  mkMaj mkMin c cs mblName rslt)
consumerRet' mkMaj mkMin (CharOptCase _ c) cs mblName rslt = infixCont anyCharE (phonemeRet  mkMaj mkMin c cs mblName rslt)
consumerRet' mkMaj mkMin (CharClass _ _cl) cs mblName rslt = infixCont anyCharE (phonemeRet' mkMaj mkMin   cs mblName rslt)
consumerRet' mkMaj mkMin WordEnd         cs mblName rslt = (phonemeRet' mkMaj mkMin cs mblName rslt)
consumerRet' mkMaj mkMin WordStart       cs mblName rslt = (phonemeRet' mkMaj mkMin cs mblName rslt)
consumerRet' mkMaj mkMin NotEnd          cs mblName rslt = (phonemeRet' mkMaj mkMin cs mblName rslt)
consumerRet' mkMaj mkMin NotStart        cs mblName rslt = (phonemeRet' mkMaj mkMin cs mblName rslt)

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

-- Found an error here:
condCaseExp :: Exp -> (Exp -> Exp) -> (Exp -> Exp) -> MulExp -> Exp
condCaseExp blExpr mkMaj mkMin exprs
  -- | (fmap mkMaj exprs == fmap mkMin exprs) = formMulExp $ fmap (\expr -> AppE ret (mkMaj expr)) exprs
  | (fmap mkMaj exprs == fmap mkMin exprs) = returnExp $ formMulExp $ fmap (\expr -> mkMaj expr) exprs
  | otherwise =
    CondE
      blExpr
      (returnExp $ formMulExp $ forMap exprs $ \expr -> mkMaj expr)
      (returnExp $ formMulExp $ forMap exprs $ \expr -> mkMin expr)

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

-- fmap (ppr . (fromRight [])) $ (runQ . (fmap (fmap fst) . constructFunctionsBothX exampleInfo)) =<< (fmap (pathifyTrie . ppsPhonemePatterns) $ (tempTester (\(_,x,_) -> x)) =<< AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample3.thyp")

-- fmap (ppr . (fromRight [])) $ (runQ . (fmap (fmap fst) . constructFunctionsBothX exampleInfo)) =<< (fmap (pathifyTrie . ppsPhonemePatterns) $ (tempTester (\(_,x,_) -> x)) =<< AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample4.thyp")

-- 
-- 
-- fmap (ppr . (fromRight [])) $ (runQ . (constructFunctions exampleInfo)) =<< (fmap (pathifyTrie . ppsPhonemePatterns) $ (tempTester (\(_,x,_) -> x)) =<< AT.parseOnly parseOrthographyFile <$> TIO.readFile "local/parseExample2.thyp")

-- | Setup the trie according to the `ParserOptions`.
setupTrie :: ParserOptions -> M.Map PhoneResult [PhonemePattern] -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness))
setupTrie pops theMap
  | (poUnifyBranches pops) = unifyPaths   initialTrie'
  | otherwise              = annotateTrie initialTrie'
  where
    initialTrie  = setupTrie' theMap
    initialTrie' = if (poCheckStates pops) then (generaliseStateTrie initialTrie) else initialTrie


-- dontPathifyTrie :: M.Map PhoneResult [PhonemePattern] -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness))
-- dontPathifyTrie = annotateTrie . setupTrie'

pathifyTrie :: M.Map PhoneResult [PhonemePattern] -> TM.TMap CharPattern (TrieAnnotation, Maybe (PhoneResult, Caseness))
pathifyTrie = unifyPaths . setupTrie'

setupTrie' :: M.Map PhoneResult [PhonemePattern] -> TM.TMap CharPattern (PhoneResult, Caseness)
setupTrie' phonePats = TM.fromList $ concatMap phoneStuff $ M.toList phonePats

  where
    phoneStuff :: (PhoneResult, [PhonemePattern]) -> [([CharPattern], (PhoneResult, Caseness))]
    phoneStuff (pname, phPats) = forMap phPats $ \thisPat ->
      (charPatsPP thisPat, (pname, isUpperPP thisPat))

-- | Temp function to test functions in REPL.
tempTester :: (a -> b) -> Either String a -> IO b
tempTester f (Right x) = return $ f x
tempTester _ (Left st) = fail $ st

-- , spiStateTypeName   :: Name
-- , spiStateDictionary :: M.Map String (Name, Maybe (Name, M.Map String Name))

-- | Create a boolean expression to test whether the state has a certain value.
checkStateExp :: M.Map String (Name, Maybe (Name, M.Map String Name)) -> Name -> CheckStateX -> Either String Exp
checkStateExp sdict vnom (CheckStateBB str bl) = do
  -- Using a partial match here since it's already been validated.
  (recNom, mNothing) <- eitherMaybe' (M.lookup str sdict) $ "Couldn't find state \"" <> str <> "\"."
  case mNothing of
    (Just _) -> Left "checkStateExp: internal error 1"
    Nothing  -> do
      let setFunc = if bl then id else (\expr -> AppE (VarE 'not) expr)
      -- setFunc <$> [| $(pure $ VarE recnom) $(pure $ VarE vnom)  |]
      return $ setFunc $ AppE (VarE recNom) (VarE vnom)
checkStateExp sdict vnom (CheckStateVB str bl ) = do
  -- Using a partial match here since it's already been validated.
  (recNom, mJust) <- eitherMaybe' (M.lookup str sdict) $ "Couldn't find state \"" <> str <> "\"."
  case mJust of
    Nothing  -> Left "checkStateExp: internal error 2"
    (Just _) -> do
      let setFunc = if bl then (\expr -> AppE (VarE 'isJust) expr) else (\expr -> AppE (VarE 'isNothing) expr)
      -- setFunc <$> [| $(pure $ VarE recnom) $(pure $ VarE vnom)  |]
      return $ setFunc $ AppE (VarE recNom) (VarE vnom)
checkStateExp sdict vnom (CheckStateVV str val) = do
  (recNom, mJust) <- eitherMaybe' (M.lookup str sdict) $ "Couldn't find state \"" <> str <> "\"."
  case mJust of
    Nothing -> Left "checkStateExp: internal error 3"
    (Just (_typName, nextMap)) -> do
      valNom <- eitherMaybe' (M.lookup val nextMap) $ "Couldn't find state value \"" <> val <> "\" for state \"" <> str <> "\"."
      -- eqJustCon :: Exp -> Name -> Exp
      return $ eqJustCon (AppE (VarE recNom) (VarE vnom)) valNom

-- | A function that groups check state predicates into one predicate.
checkStateExps :: M.Map String (Name, Maybe (Name, M.Map String Name)) -> [CheckStateX] -> Name -> Either [String] (Maybe Exp)
checkStateExps _sdict [] _ = return Nothing
checkStateExps sdict chks varNom = do
  subExps <- liftEitherList (map (checkStateExp sdict varNom) chks)
  return $ Just $ allE subExps

-- | These are meant to be performed all at once, so 
--   just returning the `FieldExp` here. Once you
--   get them all, just do something like
--   @ LamE [VarP varNom] (RecUpdE (VarE varNom) fieldExps) @
modifyStateExp :: M.Map String (Name, Maybe (Name, M.Map String Name)) -> ModifyStateX -> Either String FieldExp
modifyStateExp sdict (ModifyStateBB str bl) = do
  (recNom, mNothing) <- eitherMaybe' (M.lookup str sdict) $ "Couldn't find state \"" <> str <> "\"."
  case mNothing of
    (Just _) -> Left "modifyStateExp: internal error 1"
    Nothing  -> return (recNom, boolE bl)
modifyStateExp sdict (ModifyStateVX str) = do
  (recNom, mJust) <- eitherMaybe' (M.lookup str sdict) $ "Couldn't find state \"" <> str <> "\"."
  case mJust of
    Nothing  -> Left "modifyStateExp: internal error 2"
    (Just _) -> return (recNom, nothingE)
modifyStateExp sdict (ModifyStateVV str val) = do
  (recNom, mJust) <- eitherMaybe' (M.lookup str sdict) $ "Couldn't find state \"" <> str <> "\"."
  case mJust of
    Nothing -> Left "modifyStateExp: internal error 3"
    (Just (_typNom,nextMap)) -> do
      valNom <- eitherMaybe' (M.lookup val nextMap) $ "Couldn't find state value \"" <> val <> "\" for state \"" <> str <> "\"."
      return (recNom, justE $ ConE valNom)
  -- LamE [VarP x_0] (RecUpdE (VarE x_0) [(Ghci1.recField1,LitE (IntegerL 12))])

-- | A function that takes the results of `modifyStateExp` and
--   runs them on multiple `ModifyStateX` instructions.
modifyStateExps :: M.Map String (Name, Maybe (Name, M.Map String Name)) -> [ModifyStateX] -> Either [String] (Exp -> Exp)
modifyStateExps _sdict [] = Right id
modifyStateExps  sdict mods = do
  fieldExps <- liftEitherList (map (modifyStateExp sdict) mods)
  let xvar = mkName "x"
      lamb = LamE [VarP xvar] (RecUpdE (VarE xvar) fieldExps)
  return $ \expr -> infixCont (AppE (VarE 'State.modify') lamb) expr


----------------------------------------------------------------
-- Pre-Constructed Expressions
----------------------------------------------------------------

-- | One expression to be used in case I decide
--   to change the type to @StateT State Parser a@.
--   In which case I can change this to @lift AT.anyChar@.
anyCharE :: Exp
anyCharE = AppE (VarE 'lift) (VarE 'AT.anyChar)

peekCharE :: Exp
peekCharE = AppE (VarE 'lift) (VarE 'AT.peekChar)

peekCharE' :: Exp
peekCharE' = AppE (VarE 'lift) (VarE 'AT.peekChar')

peekCharQ :: Quote q => q Exp
peekCharQ  = [| lift AT.peekChar  |]

peekCharQ' :: Quote q => q Exp
peekCharQ' = [| lift AT.peekChar' |]

parserT :: Type
parserT = ConT ''AT.Parser

parserT' :: Type -> Type -> Type
parserT' styp typ = AppT (AppT (AppT (ConT ''State.StateT) styp) (ConT ''AT.Parser)) typ

-- parserT' :: Type -> Type
-- parserT' typ = AppT parserT typ

-- AppT (AppT (AppT (ConT ''State.StateT) styp) (ConT ''AT.Parser)) typ

-- AppT (AppT (ConT ''State.StateT) styp) (ConT ''AT.Parser)

parserTQ :: Quote q => Type -> q Type
parserTQ theType = [t| State.StateT $(pure theType) AT.Parser |]

parserTQ' :: Quote q => Type -> Type -> q Type
parserTQ' stType outType = [t| State.StateT $(pure stType) AT.Parser $(pure outType) |]

eqJustCon :: Exp -> Name -> Exp
eqJustCon mbExp theVarb
  = InfixE (Just mbExp) (VarE '(==)) (Just (AppE (ConE 'Just) (ConE theVarb)))

eqJustChar :: Name -> Char -> Exp
eqJustChar mbVar theChar
  = InfixE (Just (VarE mbVar)) (VarE '(==)) (Just (AppE (ConE 'Just) (LitE (CharL theChar))))

eqChar :: Name -> Char -> Exp
eqChar xvar theChar
  = InfixE (Just (VarE xvar)) (VarE '(==)) (Just (LitE (CharL theChar)))

otherwiseG :: Exp -> (Guard, Exp)
otherwiseG exp1 = (NormalG (VarE 'otherwise), exp1)

anyE :: [Exp] -> Exp
anyE xs  = case (NE.nonEmpty xs) of
  Nothing   -> falseE -- since False is the identity of "or".
  (Just ys) -> intersperseInfixRE (VarE '(||)) ys

anyInfixE :: Exp -> [Exp] -> Exp -> Exp
anyInfixE ifxE xs expr = anyE $ map (eqE expr) xs
  where
    eqE e1 e2 = InfixE (Just e1) ifxE (Just e2)

anyEqE :: [Exp] -> Exp -> Exp
anyEqE = anyInfixE (VarE '(==))

allE :: [Exp] -> Exp
allE xs = case (NE.nonEmpty xs) of
  Nothing   -> trueE -- since True is the identity of "and".
  (Just ys) -> intersperseInfixRE (VarE '(&&)) ys

allInfixE :: Exp -> [Exp] -> Exp -> Exp
allInfixE ifxE xs expr = allE $ map (eqE expr) xs
  where
    eqE e1 e2 = InfixE (Just e1) ifxE (Just e2)

allNeqE :: [Exp] -> Exp -> Exp
allNeqE = allInfixE (VarE '(/=))

-- | Lift the name of a predicate to one
--   that works on @Maybe a@.
liftPred :: Name -> Exp
liftPred funcName = (AppE (VarE 'any) (VarE funcName))

-- | Like `liftPred`, but defaults to
--   `True` if Nothing.
liftPredT :: Name -> Exp
liftPredT funcName = (AppE (VarE 'all) (VarE funcName))

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
phoneNamePattern :: M.Map String Name -> M.Map String [M.Map String Name] -> PhoneName -> Either [String] Exp
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
charPatternGuard :: M.Map String (Name,[Char]) -> Name -> Name -> CharPattern -> Name -> Either String Exp
charPatternGuard _classMap _endWordFunc _notEndWordFunc (PlainChar _ c)   charVarName = Right (eqJustChar charVarName c)
charPatternGuard _classMap _endWordFunc _notEndWordFunc (CharOptCase _ c) charVarName = Right (anyE (map (eqJustChar charVarName) (getCases c)))
charPatternGuard  classMap _endWordFunc _notEndWordFunc (CharClass _ cnm) charVarName = do
  (funcName, _) <- eitherMaybe' (M.lookup cnm classMap) ("Couldn't find class name: \"" <> cnm <> "\".")
  return $ AppE (AppE (VarE 'any) (VarE funcName)) (VarE charVarName)
charPatternGuard _classMap _endWordFunc _notEndWordFunc WordStart _charVarName = Left $ "Can't have a 'WordStart' makrer in the middle of a Word."
charPatternGuard _classMap _endWordFunc _notEndWordFunc NotStart  _charVarName = Left $ "Can't have a 'NotStart' marker in the middle of a Word."
charPatternGuard _classMap  endWordFunc _notEndWordFunc WordEnd    charVarName = return $ AppE (liftPredT endWordFunc   ) (VarE charVarName)
charPatternGuard _classMap _endWordFunc  notEndWordFunc NotEnd     charVarName = return $ AppE (liftPred  notEndWordFunc) (VarE charVarName)

charPatternGuard' :: M.Map String (Name,[Char]) -> Name -> Name -> CharPattern -> Name -> Either String (Exp, Bool)
charPatternGuard' _classMap _endWordFunc _notEndWordFunc (PlainChar   _ c) charVarName = Right (eqJustChar charVarName c, isCasable c)
charPatternGuard' _classMap _endWordFunc _notEndWordFunc (CharOptCase _ c) charVarName = Right (anyE (map (eqJustChar charVarName) (getCases c)), isCasable c)
charPatternGuard'  classMap _endWordFunc _notEndWordFunc (CharClass _ cnm) charVarName = do
  (funcName, chrs) <- eitherMaybe' (M.lookup cnm classMap) ("Couldn't find class name: \"" <> cnm <> "\".")
  return (AppE (AppE (VarE 'any) (VarE funcName)) (VarE charVarName), any isCasable chrs)
charPatternGuard' _classMap _endWordFunc _notEndWordFunc WordStart _charVarName = Left $ "Can't have a 'WordStart' marker in the middle of a Word."
charPatternGuard' _classMap _endWordFunc _notEndWordFunc NotStart  _charVarName = Left $ "Can't have a 'NotStart' marker in the middle of a Word."
charPatternGuard' _classMap  endWordFunc _notEndWordFunc WordEnd    charVarName = return (AppE (liftPredT endWordFunc   ) (VarE charVarName), False)
charPatternGuard' _classMap _endWordFunc  notEndWordFunc NotEnd     charVarName = return (AppE (liftPred  notEndWordFunc) (VarE charVarName), False)

exampleInfo :: StaticParserInfo
exampleInfo
  = StaticParserInfo
      -- hmm...
      (M.fromList $ forMap [1..100]   $ \n -> (TrieAnn n, mkName ("trieAnn_" ++ show n)))
      (M.fromList $ forMap ((map (:[]) ['a'..'z']) ++ ["glt","asp","schwa","etc"]) $ \c -> (c, mkName $ dataName c))
      (M.fromList $ forMap ((map (:[]) ['a'..'z']) ++ ["glt","asp","schwa","etc"]) $ \c -> (c, []))
      (M.empty) -- class Map (empty for now)
      id
      id
      (mkName "notSomeChar")
      (mkName "isSomeChar")
      (mkName "isPunctChar")
      (mkName "Phoneme")
      (mkName "PhonemeState")
      (mkName "PhonemeState2")
      (mkName "defStateVal")
      (M.fromList $ [("position",(mkName "vowPosition",Just (mkName "Position", M.fromList [("front", mkName "Front"), ("back", mkName "Back")]))), ("hasw", (mkName "doesHaveW", Nothing))]) -- for now.
      (mkName "CasedWord", (mkName "WordPh", mkName "WordPunct"))

exampleInfo2 :: StaticParserInfo
exampleInfo2
  = StaticParserInfo
      -- hmm...
      (M.fromList $ forMap [1..100]   $ \n -> (TrieAnn n, mkName ("trieAnn_" ++ show n)))
      (M.fromList $ forMap ((map (:[]) ['a'..'z']) ++ ["gh","ts","ch","sh","sep"]) $ \c -> (c, mkName $ dataName c))
      consMap3
      (M.empty) -- class Map (empty for now)
      id
      id
      (mkName "notSomeChar")
      (mkName "isSomeChar")
      (mkName "isPunctChar")
      (mkName "Phoneme")
      (mkName "PhonemeState")
      (mkName "PhonemeState2")
      (mkName "defStateVal")
      (M.fromList $ [("position",(mkName "vowPosition",Just (mkName "Position", M.fromList [("front", mkName "Front"), ("back", mkName "Back")])))]) -- for now.
      (mkName "CasedWord", (mkName "WordPh", mkName "WordPunct"))
  where
    consMap1 = (M.fromList $ forMap ((map (:[]) ['a'..'z']) ++ ["gh","ts","ch","sh","sep"]) $ \c -> (c, []))
    subMap1  = [M.fromList $ [("front",mkName "Front"), ("back", mkName "Back")]]
    consMap2 = (M.fromList [("a", subMap1),("o", subMap1),("u", subMap1)])
    consMap3 = M.union consMap2 consMap1

exampleInfo3 :: StaticParserInfo
exampleInfo3
  = StaticParserInfo
      -- hmm...
      (M.fromList $ forMap [1..100]   $ \n -> (TrieAnn n, mkName ("trieAnn_" ++ show n)))
      (M.fromList $ forMap exampleOrth3 $ \c -> (c, mkName $ dataName c))
      (M.fromList $ forMap exampleOrth3 $ \c -> (c, []))
      (M.empty) -- filled in by `makeTheParser`
      id
      id
      (mkName "notSomeChar")
      (mkName "isSomeChar")
      (mkName "isPunctChar")
      (mkName "Phoneme")
      (mkName "PhonemeState")
      (mkName "PhonemeState2")
      (mkName "defStateVal")
      M.empty -- filled in by `makeTheParser`
      -- (M.fromList $ [("position",(mkName "vowPosition",Just (mkName "Position", M.fromList [("front", mkName "Front"), ("back", mkName "Back")]))), ("hasw", (mkName "doesHaveW", Nothing))]) -- for now.
      (mkName "CasedWord", (mkName "WordPh", mkName "WordPunct"))


exampleOrth3 :: [String]
exampleOrth3
  = [ "a", "e" , "i" , "o" , "u", "aw"
    , "l", "lg", "r" , "rg"
    , "s", "sh", "sc", "z" , "zh", "zc"
    , "g", "gu", "k" , "q"
    , "t", "ts", "d" , "dz"

    , "tsh", "tsc", "dzh", "dzc"

    , "t'", "ts'", "tsh'", "tsc'"

    , "w", "j"
    ]
