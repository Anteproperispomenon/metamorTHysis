module Test.Monad.Matcher2
  ( runTheMatcher
  , myMatch
  , Example(..)
  , exampleList1
  , exampleList2
  , exampleList3
  , exampleList4
  ) where

-- This is the example from Metamorth.ForOutput.Monad.Matcher.Result

import Metamorth.ForOutput.Monad.Matcher.Stateful

data Example = A | B | C | D deriving (Show, Eq, Ord)

runTheMatcher :: [Example] -> (Example -> MatchResult Maybe Example () () String) -> Maybe [String]
runTheMatcher inp mtr = evalMatcher (\_ -> ()) inp () (matches (\_ -> Nothing) mtr)

plainReturn :: Applicative m => r -> MatchReturn m i v s r
plainReturn x = PlainReturn $ \_ -> pure x

myMatch :: (MonadFail m) => Example -> MatchResult m Example () () String
myMatch A = MatchReturn $ plainReturn "(a)"
myMatch B = MatchContinue myMatch2
myMatch C = MatchOptions (plainReturn "(c)") myMatch3
myMatch D = MatchReturn $ plainReturn "(d)"

myMatch2 :: (MonadFail m) => Example -> MatchResult m Example () () String
myMatch2 A = MatchReturn $ plainReturn "(ba)"
myMatch2 B = MatchReturn $ plainReturn "(bb)"
myMatch2 C = MatchReturn $ plainReturn "(bc)"
myMatch2 D = MatchOptions (plainReturn "(bd)") myMatch4

myMatch3 :: (MonadFail m) => Example -> MatchResult m Example () () String
myMatch3 A = MatchFail $ fail "ca"
myMatch3 B = MatchReturn $ plainReturn "(cb)"
myMatch3 C = ('c':) <$> MatchContinue myMatch4
myMatch3 D = MatchReturn $ plainReturn "(cd)"

myMatch4 :: (MonadFail m) => Example -> MatchResult m Example () () String
myMatch4 A = MatchReturn $ plainReturn "[bda]"
myMatch4 B = MatchFail $ fail "bdb"
myMatch4 C = MatchReturn $ plainReturn "[bdc]"
myMatch4 D = MatchReturn $ plainReturn "[bdd]"

exampleList1, exampleList2, exampleList3, exampleList4 :: [Example]
exampleList1 = [A, D, A, D, A, D, A]
exampleList2 = [A, B, D, D, A, B, D, B, C, C, C, D, C, A]
exampleList3 = [C, C, B, D] -- double back-track
exampleList4 = [A, B, D, D, A, B, D, B, C, C, C, D, C, A, A, B, D, D, A, B, D, B, C, C, C, D, C, A, A, B, D, D, A, B, D, B, C, C, C, D, C, A, A, B, D, D, A, B, D, B, C, C, C, D, C, A, A, B, D, D, A, B, D, B, C, C, C, D, C, A, A, B, D, D, A, B, D, B, C, C, C, D, C, A]
