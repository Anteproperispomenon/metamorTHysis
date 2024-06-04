module Test.Monad.Matcher
  ( runTheMatcher
  , matchThingy
  , Example(..)
  , exampleList1
  , exampleList2
  , exampleList3

  ) where

import Metamorth.ForOutput.Monad.Matcher

-- evalMatcher :: (Monoid v) => (i -> v) -> [i] -> Matcher i v a -> Maybe a

runTheMatcher :: [Example] -> (Example -> MatchResult Maybe Example () String) -> Maybe [String]
runTheMatcher inp mtr = evalMatcher (\_ -> ()) inp (matches (\_ -> Nothing) mtr)

data Example = AB | BC | CD | DE | EF | FG | GH deriving (Show, Eq, Ord)

exampleList1, exampleList2, exampleList3 :: [Example]
exampleList1 = [AB,DE,EF,GH,DE]
exampleList2 = [AB,FG,DE,DE,BC,EF,FG,AB,EF]
exampleList3 = [CD,DE,FG,AB,FG,FG,FG,BC,DE,EF]

matchThingy :: Example -> MatchResult Maybe Example () String
matchThingy AB = myReturn "ab"
matchThingy BC = MatchContinue matchThingy2
matchThingy CD = MatchContinue matchThingy3
matchThingy DE = myReturn "de"
matchThingy EF = myReturn "ef"
matchThingy FG = MatchOptions [myReturn' "fg"] matchThingy4
matchThingy GH = myReturn "gh"

matchThingy2 :: Example -> MatchResult Maybe Example () String
matchThingy2 AB = myReturn "example1"
matchThingy2 BC = myReturn "example2"
matchThingy2 CD = myReturn "example3"
matchThingy2 DE = myReturn "example4"
matchThingy2 EF = myReturn "example5"
matchThingy2 FG = myReturn "example6"
matchThingy2 GH = myReturn "example7"

matchThingy3 :: Example -> MatchResult Maybe Example () String
matchThingy3 AB = myReturn "exampleA"
matchThingy3 BC = myReturn "exampleB"
matchThingy3 CD = myReturn "exampleC"
matchThingy3 DE = myReturn "exampleD"
matchThingy3 EF = myReturn "exampleE"
matchThingy3 FG = myReturn "exampleF"
matchThingy3 GH = myReturn "exampleG"

matchThingy4 :: Example -> MatchResult Maybe Example () String
matchThingy4 AB = MatchFail "nope"
matchThingy4 BC = MatchContinue matchThingy2
matchThingy4 CD = myReturn "fgcd"
matchThingy4 DE = MatchContinue matchThingy5
matchThingy4 EF = myReturn "efg"
matchThingy4 FG = myReturn "foggy"
matchThingy4 GH = MatchFail "nope"

matchThingy5 :: Example -> MatchResult Maybe Example () String
matchThingy5 AB = MatchFail "nope"
matchThingy5 BC = MatchFail "nope"
matchThingy5 CD = myReturn "cdefg"
matchThingy5 DE = myReturn "okay now what"
matchThingy5 EF = MatchFail "nope"
matchThingy5 FG = MatchFail "nope"
matchThingy5 GH = MatchFail "nope"

myReturn :: String -> MatchResult Maybe Example () String
myReturn str = MatchReturn [PlainReturn $ \_ -> Just str]

myReturn' :: String -> MatchReturn Maybe Example () String
myReturn' str = PlainReturn $ \_ -> Just str

{-
-- | How to match lists in a `MatcherT`.
data MatchResult m i v r
  -- | There is only one option, which
  --   is to consume nothing and produce
  --   a value.
  = MatchReturn (MatchReturn m i v r)
  | MatchContinue (i -> MatchResult m i v r)
  | MatchOptions (MatchReturn m i v r) (i -> MatchResult m i v r)
  | MatchFail (m String)

-- | Matching on return values.
data MatchReturn m i v r
  = PlainReturn (v -> m r)
  | ConditionalReturn (v -> Maybe i -> m r)
-}

