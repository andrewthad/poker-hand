{-# language BangPatterns #-}
{-# language MultiWayIf #-}
{-# language PatternSynonyms #-}

module Poker.Evaluate
  ( Evaluation(..)
  , evaluate
  ) where

import Data.Primitive (PrimArray)
import Poker.Types (Card,Rank(Rank),Suit,pattern Ace,pattern King,pattern Five)
import Poker.Types (pattern Six, pattern Four, pattern Queen)

import qualified Poker.Card as Card
import qualified Poker.Sort as Sort
import qualified Data.Primitive as PM
import qualified Data.Primitive.Contiguous as C

-- | The value of a playable hand. Use its instance of 'Ord'
-- for comparing better hands. The ranks are sorted in each data
-- constructor so that 'Ord' can be derived.
data Evaluation
  = High      !Rank !Rank !Rank !Rank !Rank
  | Pair1     !Rank !Rank !Rank !Rank
  | Pair2     !Rank !Rank !Rank
  | Kind3     !Rank !Rank !Rank
  | Straight  !Rank
  | Flush     !Rank !Rank !Rank !Rank !Rank
  | FullHouse !Rank !Rank
  | Kind4     !Rank !Rank
  | StraightFlush !Rank
  | Kind5     !Rank
  deriving (Show, Eq, Ord)

data StraightResult
  = StraightYes !Rank
  | StraightNo 

data FlushResult
  = FlushYes
  | FlushNo

-- | Evaluate a hand of five cards.
evaluate :: PrimArray Card -> Evaluation
evaluate cards = case PM.sizeofPrimArray cards of
  5 | Just e <- checkKind5 cards -> e
    | StraightYes r <- straightRes, FlushYes <- flushRes -> StraightFlush r
    | Just e <- checkKind4 cardsByRank -> e
    | Just e <- checkFullHouse cardsByRank -> e
    | FlushYes <- flushRes -> convertToFlush cardsByRank
    | StraightYes r <- straightRes -> Straight r
    | Just e <- checkKind3 cardsByRank -> e
    | Just e <- checkPair cardsByRank -> e
    | otherwise -> convertToHighCard cardsByRank
  _ -> errorWithoutStackTrace "Poker.Evaluate.evaluate: hand must have exactly 5 cards"
  where
  !cardsByRank = Sort.byRankThenSuit cards
  !straightRes = checkStraight cardsByRank
  !flushRes = checkFlush cardsByRank

convertToHighCard :: PrimArray Card -> Evaluation
convertToHighCard !cards = High rank0 rank1 rank2 rank3 rank4
  where
  !rank0 = Card.rank (PM.indexPrimArray cards 0)
  !rank1 = Card.rank (PM.indexPrimArray cards 1)
  !rank2 = Card.rank (PM.indexPrimArray cards 2)
  !rank3 = Card.rank (PM.indexPrimArray cards 3)
  !rank4 = Card.rank (PM.indexPrimArray cards 4)

convertToFlush :: PrimArray Card -> Evaluation
convertToFlush !cards = Flush rank0 rank1 rank2 rank3 rank4
  where
  !rank0 = Card.rank (PM.indexPrimArray cards 0)
  !rank1 = Card.rank (PM.indexPrimArray cards 1)
  !rank2 = Card.rank (PM.indexPrimArray cards 2)
  !rank3 = Card.rank (PM.indexPrimArray cards 3)
  !rank4 = Card.rank (PM.indexPrimArray cards 4)

-- Undefined behavior when rank is 2. (No rank is below 2.)
predRank :: Rank -> Rank
predRank (Rank r) = Rank (r - 1)

-- Precondition: hand is five cards
checkStraight :: PrimArray Card -> StraightResult
checkStraight !cards
  | rank0 < Six = StraightNo
  | rank0 == Ace, rank1 <- Card.rank (PM.indexPrimArray cards 1) = case rank1 of
      Five -> go Five 2 3 Four
      King -> go Ace 2 3 Queen
      _ -> StraightNo
  | otherwise = go rank0 1 4 (predRank rank0)
  where
  !rank0 = Card.rank (PM.indexPrimArray cards 0)
  go :: Rank -> Int -> Int -> Rank -> StraightResult
  go !topRank !ix !remaining !expected = case remaining of
    0 -> StraightYes topRank
    _ -> if expected == Card.rank (PM.indexPrimArray cards ix)
      then go topRank (ix + 1) (remaining - 1) (predRank expected)
      else StraightNo

-- Precondition: hand has five cards.
checkFullHouse :: PrimArray Card -> Maybe Evaluation
checkFullHouse !cards
  | rank0 == rank2, rank3 == rank4 = Just (FullHouse rank0 rank3)
  | rank0 == rank1, rank2 == rank4 = Just (FullHouse rank2 rank0)
  | otherwise = Nothing
  where
  !rank0 = Card.rank (PM.indexPrimArray cards 0)
  !rank1 = Card.rank (PM.indexPrimArray cards 1)
  !rank2 = Card.rank (PM.indexPrimArray cards 2)
  !rank3 = Card.rank (PM.indexPrimArray cards 3)
  !rank4 = Card.rank (PM.indexPrimArray cards 4)

-- Precondition: hand has five cards.
-- Precondition: hand is not five of a kind
checkKind4 :: PrimArray Card -> Maybe Evaluation
checkKind4 !cards
  | rank0 == rank3 = Just (Kind4 rank0 rank4)
  | rank1 == rank4 = Just (Kind4 rank1 rank0)
  | otherwise = Nothing
  where
  !rank0 = Card.rank (PM.indexPrimArray cards 0)
  !rank1 = Card.rank (PM.indexPrimArray cards 1)
  !rank3 = Card.rank (PM.indexPrimArray cards 3)
  !rank4 = Card.rank (PM.indexPrimArray cards 4)

-- Precondition: hand has five cards.
-- Precondition: hand is not five of a kind or four of a kind
checkKind3 :: PrimArray Card -> Maybe Evaluation
checkKind3 !cards
  | rank0 == rank2 = Just (Kind3 rank0 rank3 rank4)
  | rank1 == rank3 = Just (Kind3 rank1 rank0 rank4)
  | rank2 == rank4 = Just (Kind3 rank2 rank0 rank1)
  | otherwise = Nothing
  where
  !rank0 = Card.rank (PM.indexPrimArray cards 0)
  !rank1 = Card.rank (PM.indexPrimArray cards 1)
  !rank2 = Card.rank (PM.indexPrimArray cards 2)
  !rank3 = Card.rank (PM.indexPrimArray cards 3)
  !rank4 = Card.rank (PM.indexPrimArray cards 4)

-- Precondition: hand has five cards.
-- Precondition: hand is not three/four/five of a kind
checkPair :: PrimArray Card -> Maybe Evaluation
checkPair !cards
  | rank0 == rank1 = checkLastThree
  | rank1 == rank2 = checkLastTwo
  | rank2 == rank3 = Just (Pair1 rank2 rank0 rank1 rank4)
  | rank3 == rank4 = Just (Pair1 rank3 rank0 rank1 rank2)
  | otherwise = Nothing
  where
  !rank0 = Card.rank (PM.indexPrimArray cards 0)
  !rank1 = Card.rank (PM.indexPrimArray cards 1)
  !rank2 = Card.rank (PM.indexPrimArray cards 2)
  !rank3 = Card.rank (PM.indexPrimArray cards 3)
  !rank4 = Card.rank (PM.indexPrimArray cards 4)
  checkLastThree
    | rank2 == rank3 = Just (Pair2 rank0 rank2 rank4)
    | rank3 == rank4 = Just (Pair2 rank0 rank3 rank2)
    | otherwise = Just (Pair1 rank0 rank2 rank3 rank4)
  checkLastTwo
    | rank3 == rank4 = Just (Pair2 rank1 rank3 rank0)
    | otherwise = Just (Pair1 rank1 rank0 rank3 rank4)

-- Precondition: hand has five cards.
checkKind5 :: PrimArray Card -> Maybe Evaluation
checkKind5 cards =
  let card0 = PM.indexPrimArray cards 0
      rank0 = Card.rank card0
   in if C.foldr (\card acc -> Card.rank card == rank0 && acc) True cards
        then Just (Kind5 rank0)
        else Nothing

-- Precondition: hand has five cards.
checkFlush :: PrimArray Card -> FlushResult
checkFlush cards =
  let card0 = PM.indexPrimArray cards 0
      suit0 = Card.suit card0
   in if C.foldr (\card acc -> Card.suit card == suit0 && acc) True cards
        then FlushYes
        else FlushNo

-- Currently unused
-- This should always return at least 1 if used correctly
-- countMatchesOf :: Rank -> PrimArray Card -> Int
-- countMatchesOf r cards = C.foldl' (\acc card -> if Card.rank card == r then acc + 1 else acc) 0 cards
