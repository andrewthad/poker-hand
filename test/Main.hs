{-# language LambdaCase #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}

import Control.Monad (when)
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit ((@=?))
import Poker.Types
import Poker.Evaluate (evaluate)

import qualified GHC.Exts as Exts
import qualified Poker.Card as Card
import qualified Poker.Evaluate as Evaluate
import qualified Poker.Sort as Sort
import qualified Test.Tasty.HUnit as THU

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "sort-by-rank-then-suit"
    [ THU.testCase "A" $
      (Sort.byRankThenSuit [spadeJack,spadeAce,spadeTen,spadeQueen,spadeKing])
      @=?
      [spadeAce,spadeKing,spadeQueen,spadeJack,spadeTen]
    , THU.testCase "B" $
      (Sort.byRankThenSuit [spadeJack,spadeTwo,spadeTen,spadeQueen,spadeFive])
      @=?
      [spadeQueen,spadeJack,spadeTen,spadeFive,spadeTwo]
    , THU.testCase "C" $
      (Sort.byRankThenSuit [clubSeven,heartFive,diamondSix,diamondEight,spadeNine])
      @=?
      [spadeNine,diamondEight,clubSeven,diamondSix,heartFive]
    ]
  , testGroup "encode"
    [ THU.testCase "A" (show spadeQueen @=? "Card Spade Queen")
    , THU.testCase "B" (show diamondTwo @=? "Card Diamond Two")
    ]
  , testGroup "evaluation"
    [ THU.testCase "A" $
        compare (Evaluate.Kind4 King Four) (Evaluate.Kind4 Queen Ace) @=? GT
    ]
  , testGroup "evaluate"
    [ testGroup "four-of-a-kind"
      [ THU.testCase "A" $ evaluate 
        [spadeAce,heartAce,diamondAce,spadeJack,clubAce]
        @=?
        Evaluate.Kind4 Ace Jack
      , THU.testCase "B" $ evaluate 
        [diamondTwo,heartAce,clubTwo,spadeTwo,spadeTwo]
        @=?
        Evaluate.Kind4 Two Ace
      ]
    , testGroup "five-of-a-kind"
      [ THU.testCase "A" $ evaluate 
        [spadeAce,heartAce,diamondAce,heartAce,clubAce]
        @=?
        Evaluate.Kind5 Ace
      , THU.testCase "B" $ evaluate 
        [diamondFive,heartFive,clubFive,spadeFive,diamondFive]
        @=?
        Evaluate.Kind5 Five
      ]
    , testGroup "straight"
      [ THU.testCase "A" $ evaluate 
        [clubSeven,heartFive,diamondSix,diamondEight,spadeNine]
        @=?
        Evaluate.Straight Nine
      , THU.testCase "B" $ evaluate 
        [clubFour,heartFive,diamondThree,spadeAce,heartTwo]
        @=?
        Evaluate.Straight Five
      , THU.testCase "C" $ evaluate 
        [diamondKing,spadeAce,clubQueen,spadeJack,spadeTen]
        @=?
        Evaluate.Straight Ace
      ]
    , testGroup "straight-flush"
      [ THU.testCase "A" $ evaluate 
        [clubSeven,clubFive,clubSix,clubEight,clubNine]
        @=?
        Evaluate.StraightFlush Nine
      , THU.testCase "B" $ evaluate 
        [heartFour,heartFive,heartThree,heartAce,heartTwo]
        @=?
        Evaluate.StraightFlush Five
      ]
    , testGroup "flush"
      [ THU.testCase "A" $ evaluate 
        [clubSeven,clubFive,clubSix,clubEight,clubAce]
        @=?
        Evaluate.Flush Ace Eight Seven Six Five
      , THU.testCase "B" $ evaluate 
        [heartFour,heartFive,heartSeven,heartKing,heartTwo]
        @=?
        Evaluate.Flush King Seven Five Four Two
      ]
    , testGroup "full-house"
      [ THU.testCase "A" $ evaluate 
        [clubSeven,heartFive,spadeSeven,diamondFive,clubSeven]
        @=?
        Evaluate.FullHouse Seven Five
      , THU.testCase "B" $ evaluate 
        [clubFive,heartSeven,spadeFive,diamondSeven,clubFive]
        @=?
        Evaluate.FullHouse Five Seven
      ]
    , testGroup "three-of-a-kind"
      [ THU.testCase "A" $ evaluate 
        [clubSeven,heartFour,spadeSeven,diamondFive,clubSeven]
        @=?
        Evaluate.Kind3 Seven Five Four
      , THU.testCase "B" $ evaluate 
        [clubFive,heartKing,spadeFive,diamondSeven,clubFive]
        @=?
        Evaluate.Kind3 Five King Seven
      ]
    , testGroup "two-pair"
      [ THU.testCase "A" $ evaluate 
        [clubSeven,heartFive,spadeSeven,diamondFive,clubKing]
        @=?
        Evaluate.Pair2 Seven Five King
      , THU.testCase "B" $ evaluate 
        [spadeTwo,heartSeven,spadeFive,diamondSeven,clubFive]
        @=?
        Evaluate.Pair2 Seven Five Two
      , THU.testCase "C" $ evaluate 
        [spadeAce,heartAce,clubTen,diamondNine,spadeNine]
        @=?
        Evaluate.Pair2 Ace Nine Ten
      ]
    , testGroup "one-pair"
      [ THU.testCase "A" $ evaluate 
        [clubSeven,heartSix,spadeSeven,diamondFive,clubKing]
        @=?
        Evaluate.Pair1 Seven King Six Five
      , THU.testCase "B" $ evaluate 
        [spadeTwo,heartEight,spadeFive,diamondSeven,clubFive]
        @=?
        Evaluate.Pair1 Five Eight Seven Two
      , THU.testCase "C" $ evaluate 
        [spadeAce,heartKing,clubTen,diamondNine,spadeNine]
        @=?
        Evaluate.Pair1 Nine Ace King Ten
      ]
    , testGroup "high-card"
      [ THU.testCase "A" $ evaluate 
        [clubSeven,heartSix,clubEight,spadeAce,clubKing]
        @=?
        Evaluate.High Ace King Eight Seven Six
      ]
    ]
  ]

spadeAce :: Card
spadeAce = Card.construct Spade Ace

spadeKing :: Card
spadeKing = Card.construct Spade King

spadeQueen :: Card
spadeQueen = Card.construct Spade Queen

spadeJack :: Card
spadeJack = Card.construct Spade Jack

spadeTen :: Card
spadeTen = Card.construct Spade Ten

spadeNine :: Card
spadeNine = Card.construct Spade Nine

spadeEight :: Card
spadeEight = Card.construct Spade Eight

spadeSeven :: Card
spadeSeven = Card.construct Spade Seven

spadeSix :: Card
spadeSix = Card.construct Spade Six

spadeFive :: Card
spadeFive = Card.construct Spade Five

spadeFour :: Card
spadeFour = Card.construct Spade Four

spadeThree :: Card
spadeThree = Card.construct Spade Three

spadeTwo :: Card
spadeTwo = Card.construct Spade Two

heartAce :: Card
heartAce = Card.construct Heart Ace

heartKing :: Card
heartKing = Card.construct Heart King

heartQueen :: Card
heartQueen = Card.construct Heart Queen

heartJack :: Card
heartJack = Card.construct Heart Jack

heartTen :: Card
heartTen = Card.construct Heart Ten

heartNine :: Card
heartNine = Card.construct Heart Nine

heartEight :: Card
heartEight = Card.construct Heart Eight

heartSeven :: Card
heartSeven = Card.construct Heart Seven

heartSix :: Card
heartSix = Card.construct Heart Six

heartFive :: Card
heartFive = Card.construct Heart Five

heartFour :: Card
heartFour = Card.construct Heart Four

heartThree :: Card
heartThree = Card.construct Heart Three

heartTwo :: Card
heartTwo = Card.construct Heart Two

clubAce :: Card
clubAce = Card.construct Club Ace

clubKing :: Card
clubKing = Card.construct Club King

clubQueen :: Card
clubQueen = Card.construct Club Queen

clubJack :: Card
clubJack = Card.construct Club Jack

clubTen :: Card
clubTen = Card.construct Club Ten

clubNine :: Card
clubNine = Card.construct Club Nine

clubEight :: Card
clubEight = Card.construct Club Eight

clubSeven :: Card
clubSeven = Card.construct Club Seven

clubSix :: Card
clubSix = Card.construct Club Six

clubFive :: Card
clubFive = Card.construct Club Five

clubFour :: Card
clubFour = Card.construct Club Four

clubThree :: Card
clubThree = Card.construct Club Three

clubTwo :: Card
clubTwo = Card.construct Club Two

diamondAce :: Card
diamondAce = Card.construct Diamond Ace

diamondKing :: Card
diamondKing = Card.construct Diamond King

diamondQueen :: Card
diamondQueen = Card.construct Diamond Queen

diamondJack :: Card
diamondJack = Card.construct Diamond Jack

diamondTen :: Card
diamondTen = Card.construct Diamond Ten

diamondNine :: Card
diamondNine = Card.construct Diamond Nine

diamondEight :: Card
diamondEight = Card.construct Diamond Eight

diamondSeven :: Card
diamondSeven = Card.construct Diamond Seven

diamondSix :: Card
diamondSix = Card.construct Diamond Six

diamondFive :: Card
diamondFive = Card.construct Diamond Five

diamondFour :: Card
diamondFour = Card.construct Diamond Four

diamondThree :: Card
diamondThree = Card.construct Diamond Three

diamondTwo :: Card
diamondTwo = Card.construct Diamond Two
