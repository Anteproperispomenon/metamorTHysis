{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Interpretation.Parser.TH.Lookahead
  -- * Lookahead Creating
  ( createLookahead
  , createMultiLookahead
  , createMultiLookahead2
  -- ** Pure Versions
  , createLookaheadPure
  , createMultiLookaheadPure
  , createMultiLookaheadPure2
  -- * Other Functions
  , unmapLookaheadTrie
  , groupMods
  , groupModsB
  , constructFollowPats
  , constructFollowPatsB
  -- * Phone Checks
  , makePhoneCheckSimple
  , makePhoneCheckAlt
  ) where

import Control.Applicative
import Control.Monad

import Data.Attoparsec.Text qualified as AT

import Data.Map.Strict qualified as M

import Data.List (groupBy, sort)
import Data.Maybe (isJust)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Metamorth.Helpers.Map
import Metamorth.Helpers.Parsing

import Metamorth.Interpretation.Parser.Parsing.Boolean
import Metamorth.Interpretation.Parser.Parsing.Types
import Metamorth.Interpretation.Parser.Types

import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty(..))

import Metamorth.Helpers.TH

import Control.Monad.Trans.State.Strict qualified as St

import Data.Trie.Map          qualified as TM
import Data.Trie.Map.Internal qualified as TMI

import Metamorth.Helpers.Trie

createLookahead :: (Quote q) => {-[ModifyStateX]-} Exp -> Name -> (Exp -> Exp) -> Exp -> q Exp
createLookahead stModLamb funcName rsltCheck rslt = do
  -- let x = 0
  zNom <- newName "z"
  okay <- 
    [| do
         -- hmm...
         { newRslt <- lookAheadSX $(pure stModLamb) $ do
           { $(pure $ VarP zNom) <- NE.head <$> $(pure $ VarE funcName) 
           ; return $ $(pure $ rsltCheck (VarE zNom))
           }
         ; if newRslt then (return $(pure $ rslt)) else (fail "Failed a lookahead")
         }
    |]
  return okay

-- [| do { newRslt <- lookAheadSX $(pure expZ) $ do { $(pure $ VarP zNom) <- NE.head <$> $(pure $ VarE funcName); return $ $(pure $ rsltCheck (VarE zNom)) } ; if newRslt then (return $(pure $ rslt)) else (fail "Failed a lookahead") } |]

-- do { newRslt <- lookAheadSX $(pure expZ) $ do { $(pure $ VarP zNom) <- NE.head <$> $(pure $ VarE funcName); return $ $(pure $ rsltCheck (VarE zNom)) } ; if newRslt then (return $(pure $ rslt)) else (fail "Failed a lookahead") }

createLookaheadPure :: Name -> Name -> Exp -> Name -> (Exp -> Exp) -> Exp -> Exp
createLookaheadPure zNom newRslt stModLamb funcName rsltCheck rslt
  = DoE Nothing 
    [ BindS (VarP newRslt) 
      (InfixE (Just (AppE (VarE 'lookAheadSX) stModLamb)) (VarE '($)) 
        (Just 
          (DoE Nothing 
            [ BindS (VarP zNom) (InfixE (Just (VarE 'NE.head)) (VarE '(<$>)) (Just (VarE funcName)))
            , NoBindS (InfixE (Just (VarE 'return)) (VarE '($)) (Just (rsltCheck (VarE zNom))   ))
            ])))
    , NoBindS (CondE (VarE newRslt) (AppE (VarE 'return) rslt) (AppE (VarE 'fail) (LitE (StringL "Failed a lookahead"))))
    ]

createMultiLookahead :: (Quote q) => {-[ModifyStateX]-} Exp -> Name -> [(Exp -> Exp, Exp)] -> Maybe Exp -> q Exp
createMultiLookahead stModLamb funcName rsltChecks otherRslt = do
  -- let x = 0
  zNom <- newName "z"
  okay <- 
    [| do
         -- hmm...
         -- Don't need the Bool for the casedness, since we're starting
         -- a new phoneme. The Bool for casedness only applies when
         -- running a parser midway through parsing a single phoneme.
         { $(pure $ VarP zNom) <- lookAheadSX $(pure stModLamb) $ NE.head <$> $(pure $ VarE funcName) 
         ; $(pure $ MultiIfE (ifBlocks zNom))
         }
    |]
  return ()
  return okay
  where
    otherRslt'
      | (Just oRslt) <- otherRslt
      = if (stModLamb == (VarE 'id))
        then AppE (VarE 'return) oRslt
        else InfixE (Just (AppE (VarE 'St.modify') stModLamb)) (VarE '(>>)) (Just (AppE (VarE 'return) oRslt))
      -- = [| return $(pure oRslt) |]
      | otherwise
      =  AppE (VarE 'fail) (LitE (StringL "Could not find a lookahead."))
      -- = [| fail "Could not find a lookahead." |]
    ifBlocks' :: Name -> [(Guard, Exp)]
    ifBlocks' nom = forMap rsltChecks $ \(expFunc, outp) ->
      ( NormalG $ expFunc (VarE nom)
      , AppE (VarE 'return) outp
      )
    ifBlocks :: Name -> [(Guard, Exp)]
    ifBlocks nom = ifBlocks' nom ++ [(NormalG $ VarE 'otherwise, otherRslt')]

createMultiLookaheadPure :: Name -> Exp -> Name -> [(Exp -> Exp, Exp)] -> Maybe Exp -> Exp
createMultiLookaheadPure zNom stModLamb funcName rsltChecks otherRslt = 
  DoE Nothing 
    [ BindS (VarP zNom) (InfixE (Just (AppE (VarE 'lookAheadSX) stModLamb)) (VarE '($)) (Just (InfixE (Just (VarE 'NE.head)) (VarE '(<$>)) (Just (VarE funcName)))))
    , NoBindS (MultiIfE (ifBlocks zNom))
    ]
  where
    otherRslt'
      | (Just oRslt) <- otherRslt
      = if (stModLamb == (VarE 'id))
        -- then AppE (VarE 'return) oRslt
        then oRslt
        -- else InfixE (Just (AppE (VarE 'St.modify') stModLamb)) (VarE '(>>)) (Just (AppE (VarE 'return) oRslt))
        else oRslt -- input already modifies code.
      -- = [| return $(pure oRslt) |]
      | otherwise
      =  AppE (VarE 'fail) (LitE (StringL "Could not find a lookahead."))
      -- = [| fail "Could not find a lookahead." |]
    ifBlocks' :: Name -> [(Guard, Exp)]
    ifBlocks' nom = forMap rsltChecks $ \(expFunc, outp) ->
      ( NormalG $ expFunc (VarE nom)
      -- , AppE (VarE 'return) outp
      -- , AppE (VarE 'return) outp
      , InfixE (Just (AppE (VarE 'St.modify') stModLamb)) (VarE '(>>)) (Just outp)
      )
    ifBlocks :: Name -> [(Guard, Exp)]
    ifBlocks nom = ifBlocks' nom ++ [(NormalG $ VarE 'otherwise, otherRslt')]



-- Warning: Needs to modify state!
-- Also: What if there are multiple lookaheads that
-- make different moddifications to the state(s)?

-- MultiIfE [(Guard, Exp)]
-- data Guard = NormalG Exp | PatG [Stmt]

-- | For use when you have multiple possible
--   lookaheads that have different state modifications.
createMultiLookahead2 :: (Quote q) => Name -> [(Exp, [(Exp -> Exp, Exp)])] -> Maybe (Exp, Exp) -> q Exp
createMultiLookahead2 funcName modCases otherRslt = do
  rsltList <- forM modCases $ \(stModLamb, rsltChecks) -> 
    createMultiLookahead stModLamb funcName rsltChecks Nothing
  return $ intersperseInfixRE (VarE '(<|>)) (snocNE rsltList otherRslt')
  where
    otherRslt'
      | Just (oRslt, stMod) <- otherRslt
      = if (stMod == (VarE 'id))
        then AppE (VarE 'return) oRslt
        else InfixE (Just (AppE (VarE 'St.modify') stMod)) (VarE '(>>)) (Just (AppE (VarE 'return) oRslt))
      | otherwise
      =  AppE (VarE 'fail) (LitE (StringL "Could not find a lookahead."))

createMultiLookaheadPure2 :: Name -> Name -> [(Exp, [(Exp -> Exp, Exp)])] -> Maybe (Exp, Exp) -> Exp
createMultiLookaheadPure2 zNom funcName modCases otherRslt =
  let rsltList = forMap modCases $ \(stModLamb, rsltChecks) -> 
        createMultiLookaheadPure zNom stModLamb funcName rsltChecks Nothing
  in  intersperseInfixRE (VarE '(<|>)) (snocNE rsltList otherRslt')
  where
    otherRslt'
      | Just (oRslt, stMod) <- otherRslt
      = if (stMod == (VarE 'id))
        -- then AppE (VarE 'return) oRslt
        then oRslt -- hmm...
        -- else InfixE (Just (AppE (VarE 'St.modify') stMod)) (VarE '(>>)) (Just (AppE (VarE 'return) oRslt))
        else InfixE (Just (AppE (VarE 'St.modify') stMod)) (VarE '(>>)) (Just oRslt)
      | otherwise
      =  AppE (VarE 'fail) (LitE (StringL "Could not find a lookahead."))


snocNE :: [a] -> a -> NonEmpty a
snocNE []     y = y :| []
snocNE (x:xs) y = x :| (xs ++ [y])

{-
lookAheadSX :: (s -> s) -> (State.StateT s AT.Parser a) -> (State.StateT s AT.Parser a)
lookAheadSX f prs = do
  st <- State.get
  lift $ AC.lookAhead (State.evalStateT prs (f $! st))
-}

-- intersperseInfixRE (VarE '(<|>))


----------------------------------------------------------------
-- Trie Map

-- | Turn the "lookahead" trie into a simple `M.Map`,
--   since lookeaheads must be the final modifier.
unmapLookaheadTrie :: (Ord c) => TM.TMap c (ann, Maybe a) -> M.Map c a
unmapLookaheadTrie (TMI.TMap (TMI.Node _ mp))
  = M.mapMaybe (snd <=< TM.lookup []) mp


groupMods 
  :: M.Map CharPattern     (PhoneResult  , Caseness) 
  -> M.Map [ModifyStateX] [(FollowPattern, (NonEmpty PhoneName, Caseness))]
groupMods mpIn = M.fromList finAssocs
  where
    mpIn' :: M.Map FollowPattern (PhoneResult, Caseness) 
    mpIn' = mapKeysMaybe getFollowPat mpIn
    mpAssocs :: [(FollowPattern, (PhoneResult, Caseness))]
    mpAssocs = M.assocs mpIn'

    mpAssocs' :: [([ModifyStateX], (FollowPattern, (NonEmpty PhoneName, Caseness)))]
    mpAssocs' = forMap mpAssocs $ \(folPat, (phoneRes, csn)) ->
      (sort (prStateMods phoneRes), (folPat, (prPhonemes phoneRes, csn)))
    
    finAssocs :: [([ModifyStateX], [(FollowPattern, (NonEmpty PhoneName, Caseness))])]
    finAssocs = firstUngroup $ groupBy (\(mdsts1,_) (mdsts2,_) -> mdsts1 == mdsts2) $ sort mpAssocs'

groupModsB 
  :: M.Map CharPattern     (PhoneResult  , Caseness) 
  -> M.Map [ModifyStateX] [(Boolean2 FollowPattern, (NonEmpty PhoneName, Caseness))]
groupModsB mpIn = M.fromList finAssocs
  where
    mpIn' :: M.Map (Boolean2 FollowPattern) (PhoneResult, Caseness) 
    mpIn' = mapKeysMaybe getFollowPatB mpIn
    mpAssocs :: [(Boolean2 FollowPattern, (PhoneResult, Caseness))]
    mpAssocs = M.assocs mpIn'

    mpAssocs' :: [([ModifyStateX], (Boolean2 FollowPattern, (NonEmpty PhoneName, Caseness)))]
    mpAssocs' = forMap mpAssocs $ \(folPat, (phoneRes, csn)) ->
      (sort (prStateMods phoneRes), (folPat, (prPhonemes phoneRes, csn)))
    
    finAssocs :: [([ModifyStateX], [(Boolean2 FollowPattern, (NonEmpty PhoneName, Caseness))])]
    finAssocs = firstUngroup $ groupBy (\(mdsts1,_) (mdsts2,_) -> mdsts1 == mdsts2) $ sort mpAssocs'



firstUngroup :: [[(a,b)]] -> [(a,[b])]
firstUngroup [] = []
firstUngroup ([]:xss) = firstUngroup xss
firstUngroup (xs@((x1,_):_):xss) = (x1, (map snd xs)) : (firstUngroup xss)

firstMaybe :: (a -> Maybe a') -> (a, b) -> Maybe (a', b)
firstMaybe f (x, y) = case f x of
  Nothing   -> Nothing
  (Just x') -> Just (x', y)

{-
data PhoneResult = PhoneResult
  { prPhonemes  :: NonEmpty PhoneName
  , prStateMods :: [ModifyStateX]
  } deriving (Show, Eq, Ord)
-}


-- pniGroups  :: M.Map String Name

----------------------------------------------------------------
-- Generating Lookahead Functions/Predicates

-- | Make an expression that checks whether
--   a variable is equal to a phoneme or not.
--   Doesn't work on phonemes with arguments.
makePhoneCheckSimple :: Name -> Exp
makePhoneCheckSimple phoneNom 
  = InfixE Nothing (VarE '(==)) (Just (ConE phoneNom))

-- | Make an expression that checks whether
--   a variable is equal to a phoneme or not.
--   This version *can* handle Constructors
--   that have arguments, but the generated
--   code is more complex.
--
--   In order to handle types with unknown numbers
--   of arguments, this function generates the
--   following expression:
--
--   @
--   \case
--      Phone1 {} -> True
--      _         -> False
--   @   
--
makePhoneCheckAlt :: Name -> Exp
makePhoneCheckAlt phoneNom = LamCaseE
  [ Match
      ( RecP phoneNom [] )
      (NormalB (ConE 'True))
      []
  , Match
      WildP
      (NormalB (ConE 'False))
      []
  ]


-- M.Map String (Name, (Name, M.Map String Name))

{-

  -> M.Map String (Name, (Name, M.Map String Name))       -- ^ Function names for checking aspects.
  -> M.Map String (Name, Maybe (Name, M.Map String Name)) -- ^ Functions for checking traits.
  -> M.Map String Name                -- ^ Functions for checking group membership.

aspectFuncs' = M.map (\(nom, mVal) -> (nom, snd <$> mVal)) aspectFuncs
-}

-- 
constructFollowPats 
  :: M.Map String (Name, [M.Map String Name]) -- converted from "PhonemeInformation"
  -> M.Map String Name -- pdbGroupMemberFuncs
  -> M.Map String (Name, (Name, M.Map String Name)) -- pniAspects
  -> M.Map String (Name, Maybe (M.Map String Name)) -- ondTraits
  -> FollowPattern
  -> (Exp -> Exp) -- Just a normal application, not an `fmap`ped application.
constructFollowPats _ _ asps _ (FollowAspect   aspName       ) = case (M.lookup aspName asps) of
  Nothing        -> \expr -> AppE (AppE (VarE 'const) (ConE 'False)) expr
  Just (aNom, _) -> \expr -> AppE (VarE 'isJust) (AppE (VarE aNom) expr)

constructFollowPats _ _ asps _ (FollowAspectAt aspName aspVal) = case (M.lookup aspName asps) of
  Nothing                -> \expr -> AppE (AppE (VarE 'const) (ConE 'False)) expr
  Just (aNom, (_, cMap)) -> case (M.lookup aspVal cMap) of
    Nothing     -> \expr -> AppE (AppE (VarE 'const) (ConE 'False)) expr
    (Just vNom) -> \expr -> AppE (AppE (VarE 'any) (InfixE Nothing (VarE '(==)) (Just (VarE vNom)))) (AppE (VarE aNom) expr)

constructFollowPats _ _ _ trts (FollowTrait   trtName)        = case (M.lookup trtName trts) of
  Nothing -> \expr -> AppE (AppE (VarE 'const) (ConE 'False)) expr
  Just (tNom, Nothing) -> \expr -> AppE (VarE tNom) expr
  Just (tNom, _)       -> \expr -> AppE (VarE 'isJust) (AppE (VarE tNom) expr)

constructFollowPats _ _ _ trts (FollowTraitAt trtName trtVal) = case (M.lookup trtName trts) of
  Nothing              -> \expr -> AppE (AppE (VarE 'const) (ConE 'False)) expr
  Just (tNom, Nothing) -> \expr -> AppE (AppE (VarE 'const) (ConE 'False)) expr
  Just (tNom, Just xm) -> case (M.lookup trtVal xm) of
    Nothing     -> \expr -> AppE (AppE (VarE 'const) (ConE 'False)) expr
    (Just vNom) -> \expr -> AppE (AppE (VarE 'any) (InfixE Nothing (VarE '(==)) (Just (VarE vNom)))) (AppE (VarE tNom) expr)
    -- [| any (== vNom) $ tNom expr  |]
    -- 
    -- Unsure if this is the name of 

constructFollowPats _ grps _ _ (FollowGroup grpName) = case (M.lookup grpName grps) of
  Nothing     -> \expr -> AppE (AppE (VarE 'const) (ConE 'False)) expr
  (Just fnom) -> \expr -> AppE (VarE fnom) expr

constructFollowPats phns _ _ _ (FollowPhone phnName) = case (M.lookup phnName phns) of
  Nothing -> \expr -> AppE (AppE (VarE 'const) (ConE 'False)) expr
  Just (nom,  []) -> \expr -> AppE (makePhoneCheckSimple nom) expr
  Just (nom, _cs) -> \expr -> AppE (makePhoneCheckAlt    nom) expr


constructFollowPatsB
  :: M.Map String (Name, [M.Map String Name]) -- converted from "PhonemeInformation"
  -> M.Map String Name -- pdbGroupMemberFuncs
  -> M.Map String (Name, (Name, M.Map String Name)) -- pniAspects
  -> M.Map String (Name, Maybe (M.Map String Name)) -- ondTraits
  -> Boolean2 FollowPattern
  -> (Exp -> Exp) -- Just a normal application, not an `fmap`ped application.
constructFollowPatsB phns grps asps trts folPats = case folPats of
  (PlainB2 folp) -> constructFollowPats phns grps asps trts folp
  (NotB2   folp) -> \expr -> AppE (VarE 'not) (constructFollowPatsB phns grps asps trts folp expr)
  (AndB2 fol1 fol2) -> \expr -> InfixE
    (Just (constructFollowPatsB phns grps asps trts fol1 expr))
    (VarE '(&&))
    (Just (constructFollowPatsB phns grps asps trts fol2 expr))
  (OrB2  fol1 fol2) -> \expr -> InfixE
    (Just (constructFollowPatsB phns grps asps trts fol1 expr))
    (VarE '(||))
    (Just (constructFollowPatsB phns grps asps trts fol2 expr))


{-
-- from Metamorth.Interpretation.Output.TH.Types
, ondAspects :: M.Map String (Name, (Name, M.Map String Name))

AppE (AppE (VarE 'any) (InfixE Nothing (VarE GHC.Classes.==) (Just (VarE vNom)))) (AppE (VarE tNom) expr)

data FollowPattern
  = FollowAspect   String
  | FollowAspectAt String String
  | FollowTrait    String
  | FollowTraitAt  String String
  | FollowGroup    String
  | FollowPhone    String
  deriving (Show, Eq, Ord)

data PhonemeNameInformation = PhonemeNameInformation
  { pniPhones  :: M.Map String (Name, [M.Map String Name])
  , pniAspects :: M.Map String (Name, (Name, M.Map String Name))
  , pniTraits  :: M.Map String (Name, Maybe (Name, M.Map String Name))
  , pniGroups  :: M.Map String Name -- :: Phoneme -> Bool
  , pniWordTypeNames  :: (Name, (Name, Name))
  , pniCaseExpr  :: Exp
  , pniPhoneType :: Name
  } deriving (Show, Eq)

data PhonemeDatabase = PhonemeDatabase
  { pdbPropertyData   :: PropertyData
  -- | A `M.Map` from Strings of Phonemes to the
  --   `Name`s of their constructors.
  , pdbPhonemeInfo    :: M.Map String PhonemeInformation
  -- | The top-type of the Phonemes.
  , pdbTopPhonemeType :: Name
  -- | The `Name` of the "Word" type, along
  --   with its two constructors.
  , pdbWordTypeNames  :: (Name, (Name, Name))
  -- | Functions for checking membership in a group.
  , pdbGroupMemberFuncs :: M.Map String Name
  -- | Functions for checking whether a function has
  --   a trait, and whether that trait is a value trait
  --   (@True@) or a boolean trait (@False@).
  , pdbTraitInformation :: M.Map String (Name, (Maybe (Name, M.Map String Name)))
  -- | Make an uncased expression an upper-case expression.
  , pdbMkMaj :: Exp -> Exp
  -- | Make an uncased expression a  lower-case expression.
  , pdbMkMin :: Exp -> Exp
  }


-- | A type to make understanding the output
--   of `producePropertyData` easier.
data PropertyData = PropertyData
  -- | Table of aspects. Return value is
  --   @Map of Aspect String -> (Type Name, (Record Name, Map of Option String -> Type Name ))@
  { aspectTable  :: M.Map String (Name, (Name, M.Map String Name))
  , traitTable   :: M.Map String (Name, Maybe (Name, M.Map String Name))
  , traitData    :: Maybe TraitData
  , propertyDecs :: [Dec]
  } deriving (Show, Eq)

data PhonemeInformation = PhonemeInformation
  -- | The name of the top-level Pattern Synonym.
  { phiPatternName :: Name
  -- | A list of the aspect options of the Phoneme.
  --   This is necessary to be able to build a 
  --   constructor.
  , phiArgumentOptions :: [M.Map String Name]
  } deriving (Show, Eq)

-}
