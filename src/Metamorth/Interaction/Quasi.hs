{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Interaction.Quasi
  ( metamorth ) where

import Data.Maybe

import Control.Monad (when)

import Metamorth.Helpers.Error
import Metamorth.Helpers.TH (forMap)
import Metamorth.Helpers.Q

import Metamorth.Interaction.Quasi.Parser
import Metamorth.Interaction.Quasi.Parser.Types
import Metamorth.Interaction.TH

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Attoparsec.Text qualified as AT

import Data.Text qualified as T

metamorth :: QuasiQuoter
metamorth = QuasiQuoter
  { quoteExp  = \_ -> quasiFail
  , quotePat  = \_ -> quasiFail
  , quoteType = \_ -> quasiFail
  , quoteDec  = makeTheDecs
  }

quasiFail :: Q a
quasiFail = fail "Can only use \"metamorth\" at the top-level."

makeTheDecs :: String -> Q [Dec]
makeTheDecs str = do
  let txt = T.pack str
      -- eRslt = AT.parseOnly (embedQQ parseOrthographyDetailsDebug) txt
      eRslt = AT.parseOnly (embedQQ parseOrthographyDetails) txt
  case eRslt of
    (Left err) -> do 
      reportError $ "Parse Error: " ++ err
      return []
    -- (Right ((pfp, ods, odsDebug, ondDebug), msgs)) -> do
    (Right ((pfp, ods), msgs)) -> do
      let (errs,wrns,_mmsgs) =  partitionMessages msgs
      mapM_ reportError   errs
      mapM_ reportWarning wrns
      let (irslts',orslts') = unzip $ forMap ods $ \od -> case (odInputFile od, odOutputFile od) of
            (Nothing, Nothing) -> (Nothing, Nothing)
            (Just inFile, Just outFile) -> 
              ( Just (inFile, ExtraParserDetails 
                 { epdParserName = fromMaybe "whoops" $ odInputName od
                 , epdOtherNames = odCLINames od
                 , epdUnifyBranches = fromMaybe True $ odUnifyBranches od
                 , epdGroupGuards   = fromMaybe True $ odGroupGuards   od
                 , epdCheckStates   = fromMaybe True $ odCheckStates   od
                 , epdNameSuffix    = fromMaybe "_x" $ odInSuffix      od
                 , epdMainFuncName  = "theActualParser"

                 })
              , Just (outFile, ExtraOutputDetails
                 { eodOutputName = fromMaybe "whoopsOut" $ odOutputName od
                 , eodSuffix     = fromMaybe "_xo"       $ odOutSuffix od
                 , eodOtherNames = odCLINames od
                 })
              )
            (Just inFile, Nothing) -> 
              ( Just (inFile, ExtraParserDetails 
                 { epdParserName = fromMaybe "whoops" $ odInputName od
                 , epdOtherNames = odCLINames od
                 , epdUnifyBranches = fromMaybe True $ odUnifyBranches od
                 , epdGroupGuards   = fromMaybe True $ odGroupGuards   od
                 , epdCheckStates   = fromMaybe True $ odCheckStates   od
                 , epdNameSuffix    = fromMaybe "_x" $ odInSuffix      od
                 , epdMainFuncName  = "theActualParser"
                 })
              , Nothing
              )
            (Nothing, Just outFile) ->
              ( Nothing
              , Just (outFile, ExtraOutputDetails
                 { eodOutputName = fromMaybe "whoopsOut" $ odOutputName od
                 , eodSuffix     = fromMaybe "_xo"       $ odOutSuffix od
                 , eodOtherNames = odCLINames od
                 })
              )
          irslts = catMaybes irslts'
          orslts = catMaybes orslts'
      when (null irslts) $ reportWarning  "No input specification files listed."
      when (null orslts) $ reportWarning "No output specification files listed."
      -- qDebugNotice $ "Phoneme path: \"" ++ pfp ++ "\"."
      -- qDebugNoticeUnflushed $ "Raw Orthography Details: " ++ show odsDebug
      -- qDebugNoticeUnflushed $ "Raw Orthography Set: " ++ ondDebug
      -- qDebugNotice $ "Orthography Details: " ++ show ods
      declareFullParsers pfp irslts orslts

