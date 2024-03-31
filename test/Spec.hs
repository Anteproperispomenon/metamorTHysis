
-- To see the resulting splices, run
-- stack test --ghc-options='-dth-dec-file'

import Test.TH.Basic
import Test.TH.TwoOrths as Two
import Test.TH.Grouped qualified as G

import System.IO

import Data.Text qualified as T

import Data.Attoparsec.Text qualified as AT

main :: IO ()
main = do
  hSetEncoding stdout utf8
  putStrLn "Parsing \"ᓄᓇᑦᓯᐊᕗᑦ\":"
  print $ AT.parseOnly theActualParser "ᓄᓇᑦᓯᐊᕗᑦ"
  putStrLn "Parsing longer text:"
  print $ AT.parseOnly theActualParser exampleText
  putStrLn "Parsing Latin text:"
  print $ AT.parseOnly Two.latinParser exampleText2
  putStrLn "Parsing \"ᓄᓇᑦᓯᐊᕗᑦ\" with other parser:"
  print $ AT.parseOnly Two.syllabicParser "ᓄᓇᑦᓯᐊᕗᑦ"
  putStrLn "Parsing \"ᓄᓇᑦᓯᐊᕗᑦ\" with grouped parser:"
  print $ AT.parseOnly G.theActualParser "ᓄᓇᑦᓯᐊᕗᑦ"


-- | From the Inuktitut Wikipedia page for Inuktitut.
exampleText :: T.Text
exampleText = "ᐃᓄᐃᑦ (ᓄᓇᖃᖅᑳᖅᓯᒪᔪᑦ) ᓄᓇᕗᒻᒥᐅᑦ ᐃᓄᒃᑎᑐᑦ ᐅᖃᐅᓯᕐᖓᐅᑎᖃᕐᒪᑕ. ᐃᓄᐃᑦ ᐅᖃᐅᓯᖏᑦ ᐊᔾᔨᒌᙱᑦᑑᑎᐅᒐᓗᐊᖅᑐᑎᒃ ᓄᓇᓖᑦ ᒪᓕᒃᖢᒋᑦ, ᐃᓄᐃᓐᓇᖅᑐᓐ ᐃᓚᐅᓪᓗᓂ, ᐅᖃᐅᓯᕆᔭᐅᔪᖅ ᐅᐊᓕᓂᖅᐸᓯᖓᓂᕐᒥᐅᑕᐅᔪᓂ ᓄᓇᕗᒥ. ᐃᓄᐃᓐᓇᖅᑐᓐ ᖃᓕᐅᔮᖅᐸᐃᑎᑐᑦ ᐃᓅᔨᖓᔪᖅᑎᑐᑦ ᑎᑎᕋᐅᓯᖃᖅᑐᑦ ᖃᓂᐅᔮᖅᐸᐃᑎᑑᖓᙱᑦᑐᑦ ᑎᑎᕋᐅᓯᖏᑦ."

exampleText2 :: T.Text 
exampleText2 = "inuit (nunaqaqqaaqŝimajut) inuktitut uqauŝirngautiqarmata. inuit uqauŝingit ajjigiinngittuutiugaluaqtutik nunaliit malikługit, inuinnaqtun ilaulluni, uqauŝirijaujuq ualiniqpaŝinganirmiutaujuni nunavumi."