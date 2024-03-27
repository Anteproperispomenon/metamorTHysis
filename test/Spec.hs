
import Test.TH.Basic

import System.IO

import Data.Text qualified as T

import Data.Attoparsec.Text qualified as AT

main :: IO ()
main = do
  hSetEncoding stdout utf8
  putStrLn "Parsing \"ᓄᓇᑦᓯᐊᕗᑦ\":"
  print $ AT.parseOnly theActualParser "ᓄᓇᑦᓯᐊᕗᑦ"
