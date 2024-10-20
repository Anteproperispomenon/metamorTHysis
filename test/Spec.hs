
-- To see the resulting splices, run
-- stack test --ghc-options='-dth-dec-file'

import Test.TH.Basic
import Test.TH.TwoOrths qualified as Two
import Test.TH.Grouped  qualified as G
import Test.TH.InAndOut  as InOut
import Test.TH.Kwakwala  as Kwak
import Test.TH.Mongolian as Mongolian
import Test.TH.Following qualified as Fol

import Test.TH.Backtrack qualified as Back

import Test.TH.KwakQuasi qualified as KwakQ

import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding qualified as TLE



import Data.Attoparsec.Text qualified as AT

import Test.Monad.Matcher
import Test.Monad.Matcher2 qualified as M2

import Metamorth.Helpers.IO 

import System.IO

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
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
  putStrLn "\n"

  print $ runTheMatcher exampleList1 matchThingy
  print $ runTheMatcher exampleList2 matchThingy
  print $ runTheMatcher exampleList3 matchThingy

  putStrLn "\n"

  print $ M2.runTheMatcher M2.exampleList1 M2.myMatch
  print $ M2.runTheMatcher M2.exampleList2 M2.myMatch
  print $ M2.runTheMatcher M2.exampleList3 M2.myMatch
  print $ M2.runTheMatcher M2.exampleList4 M2.myMatch

  putStrLn "Trying the output functions..."
  print $ (AT.parseOnly InOut.latinParser exampleText2) >>= (InOut.syllabicOutput id)

  putStrLn "Idempotency Checks..."
  let eRslts = do
        prs1 <- AT.parseOnly InOut.syllabicParser exampleText
        out1 <- InOut.syllabicOutput id prs1
        prs2 <- AT.parseOnly InOut.syllabicParser out1
        out2 <- InOut.syllabicOutput id prs2
        return (out1 == exampleText, out2 == out1)
  case eRslts of
    (Right (r1,r2)) -> do
      putStrLn $ "  f    x  ==   x : " ++ show r1
      putStrLn $ "  f (f x) == f x : " ++ show r2
    (Left err) -> do
      putStrLn $ "Couldn't parse input;"
      putStrLn err

  -- Direct conversion types...
  putStrLn "Trying direct conversions..."
  let rslt = InOut.convertOrthography InInuktitut_latin OutSyllabic exampleText2
  case rslt of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout tx
  
  putStrLn "Trying direct conversions with lazy text..."
  let rslt2 = InOut.convertOrthographyLazy InInuktitut_latin OutSyllabic exampleText2
  case rslt2 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)

  putStrLn "Trying direct conversions with ByteStrings..."
  let rslt3 = TLE.decodeUtf8 <$> InOut.convertOrthographyBS InInuktitut_latin OutSyllabic exampleText2
  case rslt3 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)
  
  putStrLn "Testing Kwak'wala..."
  let kwak1 = TLE.decodeUtf8 <$> Kwak.convertOrthographyBS InGrubb OutUmista kwakText1
  case kwak1 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)

  putStrLn "Testing Kwak'wala from QuasiQuoter..."
  let kwak2 = TLE.decodeUtf8 <$> KwakQ.convertOrthographyBS KwakQ.InGrubb KwakQ.OutUmista kwakText1
  case kwak2 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)

  putStrLn "Testing Mongolian..."
  let mongol1 = TLE.decodeUtf8 <$> Mongolian.convertOrthographyBS InTest_latin OutTest_latin mongolianText1
  case mongol1 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)

  let mongol2 = TLE.decodeUtf8 <$> Mongolian.convertOrthographyBS InTest_latin OutCyrillic mongolianText1
  case mongol2 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)

  putStrLn "Testing Follow patterns..."
  let follow1 = TLE.decodeUtf8 <$> Fol.convertOrthographyBS Fol.InFollowA Fol.OutFollowA followText1
  case follow1 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)
  putStrLn "Testing group-follow..."
  let follow2 = TLE.decodeUtf8 <$> Fol.convertOrthographyBS Fol.InFollowA Fol.OutFollowB followText1
  case follow2 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)
  putStrLn "Testing trait-follow..."
  let follow3 = TLE.decodeUtf8 <$> Fol.convertOrthographyBS Fol.InFollowA Fol.OutFollowC followText2
  case follow3 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)

  putStrLn "Testing auto-states..."
  let boas1 = TLE.decodeUtf8 <$> KwakQ.convertOrthographyBS KwakQ.InBoas KwakQ.OutGrubb boasText1
  case boas1 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)

  putStrLn "Testing auto-states for output..."
  let auto2 = TLE.decodeUtf8 <$> KwakQ.convertOrthographyBS KwakQ.InGrubb KwakQ.OutUmista2 autoTest2
  case auto2 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)
  
  putStrLn "Testing input backtracking..."
  let back1 = TLE.decodeUtf8 <$> Back.convertOrthographyBS Back.InBacktrack Back.OutBacktrack2 backTest1
  case back1 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)
  
  putStrLn "Testing input lookahead..."
  let look1 = TLE.decodeUtf8 <$> Fol.convertOrthographyBS Fol.InLookahead Fol.OutFollowA lookAheadTest1
  case look1 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)

  putStrLn "Testing input lookahead..."
  let look2 = TLE.decodeUtf8 <$> Fol.convertOrthographyBS Fol.InLookahead Fol.OutFollowA lookAheadTest2
  case look2 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)

  putStrLn "Testing input lookahead..."
  let look3 = TLE.decodeUtf8 <$> Fol.convertOrthographyBS Fol.InLookahead Fol.OutFollowA lookAheadTest3
  case look3 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)

  putStrLn "Testing output lookahead..."
  let look4 = TLE.decodeUtf8 <$> Fol.convertOrthographyBS Fol.InFollowA Fol.OutLookahead lookAheadTest4
  case look4 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)

  putStrLn "Testing Case Output..."
  let case1 = TLE.decodeUtf8 <$> KwakQ.convertOrthographyBS KwakQ.InGrubb KwakQ.OutUmista caseTest1
  case case1 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)

  putStrLn "Testing Case Output..."
  let case2 = TLE.decodeUtf8 <$> Fol.convertOrthographyBS Fol.InFollowA Fol.OutLookahead2 caseTest2
  case case2 of
    (Left err) -> putStrLn $ "Error: " ++ err
    (Right tx) -> hPutStrLnUtf8 stdout (toStrict tx)


-- | From the Inuktitut Wikipedia page for Inuktitut.
exampleText :: T.Text
exampleText = "ᐃᓄᐃᑦ (ᓄᓇᖃᖅᑳᖅᓯᒪᔪᑦ) ᓄᓇᕗᒻᒥᐅᑦ ᐃᓄᒃᑎᑐᑦ ᐅᖃᐅᓯᕐᖓᐅᑎᖃᕐᒪᑕ. ᐃᓄᐃᑦ ᐅᖃᐅᓯᖏᑦ ᐊᔾᔨᒌᙱᑦᑑᑎᐅᒐᓗᐊᖅᑐᑎᒃ ᓄᓇᓖᑦ ᒪᓕᒃᖢᒋᑦ, ᐃᓄᐃᓐᓇᖅᑐᓐ ᐃᓚᐅᓪᓗᓂ, ᐅᖃᐅᓯᕆᔭᐅᔪᖅ ᐅᐊᓕᓂᖅᐸᓯᖓᓂᕐᒥᐅᑕᐅᔪᓂ ᓄᓇᕗᒥ. ᐃᓄᐃᓐᓇᖅᑐᓐ ᖃᓕᐅᔮᖅᐸᐃᑎᑐᑦ ᐃᓅᔨᖓᔪᖅᑎᑐᑦ ᑎᑎᕋᐅᓯᖃᖅᑐᑦ ᖃᓂᐅᔮᖅᐸᐃᑎᑑᖓᙱᑦᑐᑦ ᑎᑎᕋᐅᓯᖏᑦ."

exampleText2 :: T.Text 
exampleText2 = "inuit (nunaqaqqaaqŝimajut) inuktitut uqauŝirngautiqarmata. inuit uqauŝingit ajjigiinngittuutiugaluaqtutik nunaliit malikługit, inuinnaqtun ilaulluni, uqauŝirijaujuq ualiniqpaŝinganirmiutaujuni nunavumi."

kwakText1 :: T.Text 
kwakText1 = "dadapa dadats'e'akw dagens dagens da'dagens didzu'yu digi'lats'i di'deganu di'xhstanu di'yu dukhwelaxhden dzadzuts'a dzekhwa dagens dzekhwa ghwilhghwa'ehla dzemba gawekh'anem ga'yas gembuts gensuxh gen'sa'os gigalilha'sidzi' gukwdzi ghadlekw ghaghe'o ghaghe'o ghedzekh"


mongolianText1 :: T.Text
mongolianText1 = "úlaanbaatar"

-- Just some random sounds thrown together...
followText1 :: T.Text
followText1 = "ɑgjuliʊ gæʃtɒlis θrəŋ aft"

followText2 :: T.Text
followText2 = "ɑgjuliʊ gæʃtɒlis θrəŋ aft iŋaninis init etes beedʒ ekiŋ"

boasText1 :: T.Text
boasText1 = "ăăë gŭ"

autoTest2 :: T.Text
autoTest2 = "Eh'eh'A gwA'uM'i" -- 

backTest1 :: T.Text
backTest1 = "Tough TouGra" -- 

lookAheadTest1 :: T.Text
lookAheadTest1 = "þa þnA þøn þøjþVþ ød"

lookAheadTest2 :: T.Text
lookAheadTest2 = "þaβ þбa þnaḅ"

lookAheadTest3 :: T.Text
lookAheadTest3 = "ɵin ɵyn ɵyd ɵyg"

lookAheadTest4 :: T.Text
lookAheadTest4 = "ApAbaranAdax AwshawchAwg"

caseTest1 :: T.Text
caseTest1 = "Ehtla eHtlA"

caseTest2 :: T.Text
caseTest2 = "Iga iga oga"

{-
   b : example=exam1
    p
    f : example=exam2
    w
    v
    m : nasal
  ** alveolar
    n : nasal
    d
    t : example=exam2
    s : example=exam1
    l
    r : rough
    sh
    ch
    zh
    dzh : example=exam2
    ts
    dz
    th : rough
    dh
  ** palatal_velar
    j
    k : example=exam1
    g
    ng : nasal
    h : rough
-}
