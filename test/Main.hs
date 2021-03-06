{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}

import Control.Applicative (liftA2)
import Control.Monad (when)
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit ((@=?))
import Poker.Types
import Poker.Evaluate (evaluate,evaluateMany)
import Data.Primitive.PrimArray (PrimArray)
import Test.Tasty.QuickCheck ((===))

import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Poker.Card as Card
import qualified Poker.Evaluate as Evaluate
import qualified Poker.Sort as Sort
import qualified Test.Tasty.HUnit as THU
import qualified Test.Tasty.QuickCheck as TQC

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
  , testGroup "evaluateMany"
    [ testGroup "flush"
      [ THU.testCase "A" $ evaluateMany
        [spadeTen,heartNine,spadeFour,heartKing,spadeSix
        ,heartFour,spadeFive,spadeQueen,spadeThree,heartFive
        ,heartTwo,diamondAce,heartThree
        ]
        @=?
        Evaluate.Flush King Nine Five Four Three
      , THU.testCase "B" $ evaluateMany
        [clubSeven,clubFive,clubSix,clubEight,clubAce]
        @=?
        Evaluate.Flush Ace Eight Seven Six Five
      , THU.testCase "C" $ evaluateMany
        [heartFour,heartFive,heartSeven,heartKing,heartTwo]
        @=?
        Evaluate.Flush King Seven Five Four Two
      ]
    , testGroup "five-of-a-kind"
      [ THU.testCase "A" $ evaluateMany
        [spadeAce,heartAce,diamondAce,heartAce,clubAce]
        @=?
        Evaluate.Kind5 Ace
      , THU.testCase "B" $ evaluateMany
        [diamondFive,diamondAce,heartFive,clubFive,heartTwo
        ,spadeFive,diamondFive
        ]
        @=?
        Evaluate.Kind5 Five
      ]
    , testGroup "four-of-a-kind"
      [ THU.testCase "A" $ evaluateMany
        [spadeAce,heartAce,diamondAce,spadeJack,clubAce]
        @=?
        Evaluate.Kind4 Ace Jack
      , THU.testCase "B" $ evaluateMany
        [diamondTwo,heartAce,clubTwo,spadeTwo,spadeTwo]
        @=?
        Evaluate.Kind4 Two Ace
      , THU.testCase "C" $ evaluateMany
        [diamondTwo,heartFour,clubTwo,spadeTwo,diamondQueen,spadeTwo]
        @=?
        Evaluate.Kind4 Two Queen
      , THU.testCase "D" $ evaluateMany
        [diamondKing,heartKing,heartTwo,clubFive,spadeKing,diamondKing]
        @=?
        Evaluate.Kind4 King Five
      ]
    , testGroup "three-of-a-kind"
      [ THU.testCase "A" $ evaluateMany
        [clubSeven,heartFour,spadeSeven,diamondFive,clubSeven]
        @=?
        Evaluate.Kind3 Seven Five Four
      , THU.testCase "B" $ evaluateMany
        [clubFive,heartKing,spadeFive,diamondSeven,clubFive]
        @=?
        Evaluate.Kind3 Five King Seven
      ]
    , testGroup "full-house"
      [ THU.testCase "A" $ evaluateMany
        [clubSeven,heartFive,spadeSeven,diamondFive,clubSeven]
        @=?
        Evaluate.FullHouse Seven Five
      , THU.testCase "B" $ evaluateMany
        [clubFive,heartSeven,spadeFive,diamondSeven,clubFive]
        @=?
        Evaluate.FullHouse Five Seven
      , THU.testCase "C" $ evaluateMany
        [spadeTwo,clubFive,heartSeven,spadeThree,spadeFive
        ,diamondSeven,clubFive,clubSeven
        ]
        @=?
        Evaluate.FullHouse Seven Five
      ]
    , testGroup "two-pair"
      [ THU.testCase "A" $ evaluateMany 
        [clubSeven,heartFive,spadeSeven,diamondFive,clubKing]
        @=?
        Evaluate.Pair2 Seven Five King
      , THU.testCase "B" $ evaluateMany 
        [spadeTwo,heartSeven,spadeFive,diamondSeven,clubFive]
        @=?
        Evaluate.Pair2 Seven Five Two
      , THU.testCase "C" $ evaluateMany 
        [spadeAce,heartAce,clubTen,diamondNine,spadeNine]
        @=?
        Evaluate.Pair2 Ace Nine Ten
      ]
    , testGroup "one-pair"
      [ THU.testCase "A" $ evaluateMany 
        [clubSeven,heartSix,spadeSeven,diamondFive,clubKing]
        @=?
        Evaluate.Pair1 Seven King Six Five
      , THU.testCase "B" $ evaluateMany 
        [spadeTwo,heartEight,spadeFive,diamondSeven,clubFive]
        @=?
        Evaluate.Pair1 Five Eight Seven Two
      , THU.testCase "C" $ evaluateMany 
        [spadeAce,heartKing,clubTen,diamondNine,spadeNine]
        @=?
        Evaluate.Pair1 Nine Ace King Ten
      ]
    , testGroup "straight-flush"
      [ THU.testCase "A" $ evaluateMany
        [clubSeven,clubFive,clubSix,clubEight,clubNine]
        @=?
        Evaluate.StraightFlush Nine
      , THU.testCase "B" $ evaluateMany
        [heartFour,heartFive,heartThree,heartAce,heartTwo]
        @=?
        Evaluate.StraightFlush Five
      , THU.testCase "C" $ evaluateMany
        [heartFour,heartFive,heartThree,heartAce,heartTwo
        ,clubThree,clubFour,clubFive,clubSix,clubSeven
        ]
        @=?
        Evaluate.StraightFlush Seven
      , THU.testCase "C" $ evaluateMany
        [heartFour,heartFive,heartThree,heartAce,heartTwo
        ,clubThree,clubFour,clubFive,clubSix,clubSeven
        ,spadeAce
        ,diamondTen,diamondJack,diamondNine,diamondEight,diamondQueen
        ]
        @=?
        Evaluate.StraightFlush Queen
      ]
    ]
  , testGroup "evaluation-agreement"
    [ TQC.testProperty "hand-5-all" $ TQC.forAll (genHandAll 5) $ \hand ->
        evaluateMany hand === evaluate hand
    , TQC.testProperty "hand-6-all" $ TQC.forAll (genHandAll 6) $ \hand ->
        evaluateMany hand === maximum (map evaluate (chooseFiveFromSix hand))
    , TQC.testProperty "hand-6-high" $ TQC.forAll (genHandHigh 6) $ \hand ->
        evaluateMany hand === maximum (map evaluate (chooseFiveFromSix hand))
    , TQC.testProperty "hand-6-low" $ TQC.forAll (genHandLow 6) $ \hand ->
        evaluateMany hand === maximum (map evaluate (chooseFiveFromSix hand))
    , TQC.testProperty "hand-6-high-spades" $ TQC.forAll (genHandSpadesHigh 6) $ \hand ->
        evaluateMany hand === maximum (map evaluate (chooseFiveFromSix hand))
    , TQC.testProperty "hand-6-low-diamonds" $ TQC.forAll (genHandDiamondsLow 6) $ \hand ->
        evaluateMany hand === maximum (map evaluate (chooseFiveFromSix hand))
    ]
  ]

chooseFiveFromSix :: PrimArray Card -> [PrimArray Card]
chooseFiveFromSix !cards
  | PM.sizeofPrimArray cards /= 6 =
      errorWithoutStackTrace "chooseFiveFromSix: invariant violated"
  | otherwise =
      let c0 = PM.indexPrimArray cards 0
          c1 = PM.indexPrimArray cards 1
          c2 = PM.indexPrimArray cards 2
          c3 = PM.indexPrimArray cards 3
          c4 = PM.indexPrimArray cards 4
          c5 = PM.indexPrimArray cards 5
       in [ Exts.fromList [c5,c1,c2,c3,c4]
          , Exts.fromList [c0,c5,c2,c3,c4]
          , Exts.fromList [c0,c1,c5,c3,c4]
          , Exts.fromList [c0,c1,c2,c5,c4]
          , Exts.fromList [c0,c1,c2,c3,c5]
          , Exts.fromList [c0,c1,c2,c3,c4]
          ]

genHandAll :: Int -> TQC.Gen (PrimArray Card)
genHandAll !n = fmap Exts.fromList (TQC.vectorOf n genCardAny)

genHandHigh :: Int -> TQC.Gen (PrimArray Card)
genHandHigh !n = fmap Exts.fromList (TQC.vectorOf n genCardHigh)

genHandSpadesHigh :: Int -> TQC.Gen (PrimArray Card)
genHandSpadesHigh !n = fmap Exts.fromList (TQC.vectorOf n genCardSpadeHigh)

genHandDiamondsLow :: Int -> TQC.Gen (PrimArray Card)
genHandDiamondsLow !n = fmap Exts.fromList (TQC.vectorOf n genCardDiamondLow)

genHandLow :: Int -> TQC.Gen (PrimArray Card)
genHandLow !n = fmap Exts.fromList (TQC.vectorOf n genCardLow)

genCardAny :: TQC.Gen Card
genCardAny = liftA2 Card.construct genSuit genRank

genCardHigh :: TQC.Gen Card
genCardHigh = liftA2 Card.construct genSuit genHighRank

genCardSpadeHigh :: TQC.Gen Card
genCardSpadeHigh = fmap (Card.construct Spade) genHighRank

genCardDiamondLow :: TQC.Gen Card
genCardDiamondLow = fmap (Card.construct Diamond) genLowRank

genCardLow :: TQC.Gen Card
genCardLow = liftA2 Card.construct genSuit genLowRank

genLowRank :: TQC.Gen Rank
genLowRank = do
  x <- TQC.chooseInt (1,8)
  if x == 1
    then pure Ace
    else pure (Rank (fromIntegral x))

genHighRank :: TQC.Gen Rank
genHighRank = fmap (Rank . fromIntegral) (TQC.chooseInt (7,14))

genRank :: TQC.Gen Rank
genRank = fmap (Rank . fromIntegral) (TQC.chooseInt (2,14))

genSuit :: TQC.Gen Suit
genSuit = fmap (Suit . fromIntegral) (TQC.chooseInt (0,3))

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
