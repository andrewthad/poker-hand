{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language MultiWayIf #-}
{-# language NumericUnderscores #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}

module Poker.Evaluate
  ( Evaluation(..)
  , evaluate
  , evaluateMany
  ) where

import Data.Bits (unsafeShiftL,unsafeShiftR,(.|.),(.&.))
import Data.Word (Word8,Word16)
import Data.Primitive (PrimArray,MutablePrimArray)
import Data.Primitive.Unlifted.Array (UnliftedArray)
import Poker.Types (Card,Rank(Rank),Suit(Suit),pattern Ace,pattern King,pattern Five)
import Poker.Types (pattern Six, pattern Four, pattern Queen)
import Control.Monad.ST (ST)
import Control.Monad.ST.Run (runPrimArrayST)

import qualified Poker.Card as Card
import qualified Poker.Sort as Sort
import qualified Data.Primitive as PM
import qualified Data.Primitive.Unlifted.Array as PM
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

-- | Evaluate a hand of five or more cards
evaluateMany :: PrimArray Card -> Evaluation
{-# noinline evaluateMany #-}
evaluateMany !cards
  | PM.sizeofPrimArray cards < 5 =
      errorWithoutStackTrace "Poker.Evaluate.evaluateMany hand must have at least 5 cards"
  | otherwise = evaluateManyInternal cards

evaluateManyInternal :: PrimArray Card -> Evaluation
evaluateManyInternal !cards
  | CheckManyKindYes r _ _ <- checkManyKindN cardsByRank 5 0 cardsLen = Kind5 r
  | PM.sizeofUnliftedArray flushRanks > 0, StraightYes r <- bestStraightFlush flushRanks
    = StraightFlush r
  | CheckManyKindYes r _ _ <- checkManyKindN cardsByRank 4 0 cardsLen = Kind4 r (findHighestRankNotEqual1 cardsByRank r)
  | CheckManyKindYes r3 _ _ <- threeOfKindResult
  , CheckManyKindYes r2 _ _ <- checkManyPairExcluding cardsByRank r3 0 cardsLen
    = FullHouse r3 r2
  | PM.sizeofUnliftedArray flushRanks > 0 = case C.maximum flushRanks of
      Nothing -> errorWithoutStackTrace "Poker.Evaluate.evaluateMany: invariant violated"
      Just flushCards -> convertRanksToFlush flushCards
  | !mask <- generateStraightBitPattern cards
  , StraightYes r <- checkStraightBitPattern mask
    = Straight r
  | CheckManyKindYes r _ _ <- threeOfKindResult =
      let hiA = findHighestRankNotEqual1 cardsByRank r
          hiB = findHighestRankNotEqual2 cardsByRank r hiA
       in Kind3 r hiA hiB
  | CheckManyKindYes rhi _ _ <- pairResult = case checkManyPairExcluding cardsByRank rhi 0 cardsLen of
      CheckManyKindYes rlo _ _ -> Pair2 rhi rlo (findHighestRankNotEqual2 cardsByRank rhi rlo)
      CheckManyKindNo ->
        let hiA = findHighestRankNotEqual1 cardsByRank rhi
            hiB = findHighestRankNotEqual2 cardsByRank rhi hiA
            hiC = findHighestRankNotEqual3 cardsByRank rhi hiA hiB
         in Pair1 rhi hiA hiB hiC
  | otherwise = convertToHighCard cardsByRank
  where
  !cardsLen = PM.sizeofPrimArray cards
  !cardsByRank = Sort.byRankThenSuit cards
  !cardsBySuit = Sort.bySuitThenRank cards
  !flushSuits = checkFlushMany cards
  !threeOfKindResult = checkManyKindN cardsByRank 3 0 cardsLen
  !pairResult = checkManyKindN cardsByRank 2 0 cardsLen
  !(flushRanks :: UnliftedArray (PrimArray Rank)) = C.map' (sliceStartingAtSuit cardsBySuit) flushSuits 

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
  !straightRes = checkStraight cardsByRank 0
  !flushRes = checkFlush cardsByRank

convertToHighCard :: PrimArray Card -> Evaluation
convertToHighCard !cards = High rank0 rank1 rank2 rank3 rank4
  where
  !rank0 = Card.rank (PM.indexPrimArray cards 0)
  !rank1 = Card.rank (PM.indexPrimArray cards 1)
  !rank2 = Card.rank (PM.indexPrimArray cards 2)
  !rank3 = Card.rank (PM.indexPrimArray cards 3)
  !rank4 = Card.rank (PM.indexPrimArray cards 4)

-- Precondition: hand has at least five cards
convertToFlush :: PrimArray Card -> Evaluation
convertToFlush !cards = Flush rank0 rank1 rank2 rank3 rank4
  where
  !rank0 = Card.rank (PM.indexPrimArray cards 0)
  !rank1 = Card.rank (PM.indexPrimArray cards 1)
  !rank2 = Card.rank (PM.indexPrimArray cards 2)
  !rank3 = Card.rank (PM.indexPrimArray cards 3)
  !rank4 = Card.rank (PM.indexPrimArray cards 4)

-- Precondition: ranks are sorted high to low already
convertRanksToFlush :: PrimArray Rank -> Evaluation
convertRanksToFlush !cards = Flush rank0 rank1 rank2 rank3 rank4
  where
  !rank0 = PM.indexPrimArray cards 0
  !rank1 = PM.indexPrimArray cards 1
  !rank2 = PM.indexPrimArray cards 2
  !rank3 = PM.indexPrimArray cards 3
  !rank4 = PM.indexPrimArray cards 4

-- Undefined behavior when rank is 2. (No rank is below 2.)
predRank :: Rank -> Rank
predRank (Rank r) = Rank (r - 1)

-- Precondition: hand slice is five cards
-- Precondition: hand is ordered by rank
checkStraight :: PrimArray Card -> Int -> StraightResult
checkStraight !cards !ix0
  | rank0 < Six = StraightNo
  | rank0 == Ace, rank1 <- Card.rank (PM.indexPrimArray cards (ix0 + 1)) = case rank1 of
      Five -> go Five (ix0 + 2) 3 Four
      King -> go Ace (ix0 + 2) 3 Queen
      _ -> StraightNo
  | otherwise = go rank0 (ix0 + 1) 4 (predRank rank0)
  where
  !rank0 = Card.rank (PM.indexPrimArray cards ix0)
  go :: Rank -> Int -> Int -> Rank -> StraightResult
  go !topRank !ix !remaining !expected = case remaining of
    0 -> StraightYes topRank
    _ -> if expected == Card.rank (PM.indexPrimArray cards ix)
      then go topRank (ix + 1) (remaining - 1) (predRank expected)
      else StraightNo

generateStraightBitPattern :: PrimArray Card -> Word16
generateStraightBitPattern !cards = C.foldl'
  (\acc card ->
    let !rank@(Rank r) = Card.rank card
        !bits = if rank == Ace
          then 0b0100_0000_0000_0010
          else unsafeShiftL 1 (fromIntegral r)
     in bits .|. acc
  ) 0 cards

generateStraightBitPatternFromRanks :: PrimArray Rank -> Word16
generateStraightBitPatternFromRanks !cards = C.foldl'
  (\acc rank@(Rank r) ->
    let !bits = if rank == Ace
          then 0b0100_0000_0000_0010
          else unsafeShiftL 1 (fromIntegral r)
     in bits .|. acc
  ) 0 cards

-- Precondition: each Word8 is a valid rank. The additional rank 1 is allowed.
-- A rank-1 card is added to the end of hands that begin with an ace.
checkStraightBitPattern :: Word16 -> StraightResult
checkStraightBitPattern !mask = go 10
  where
  go :: Int -> StraightResult
  go !ix = if ix > 0
    then if (unsafeShiftR mask ix .&. 0b0001_1111) == 0b0001_1111
      then StraightYes (Rank (fromIntegral (4 + ix)))
      else go (ix - 1)
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

data CheckManyKindResult
  = CheckManyKindYes !Rank !Int !Int
  | CheckManyKindNo

-- Precondition: hand is sorted
-- Returns the highest rank n-of-a-kind
checkManyKindN ::
     PrimArray Card
  -> Int -- target length
  -> Int -- start index
  -> Int -- length
  -> CheckManyKindResult
checkManyKindN !cards !target !ix0 !len0 = goA ix0 len0
  where
  goA :: Int -> Int -> CheckManyKindResult
  goA !ix !len = case len of
    0 -> CheckManyKindNo
    _ -> goB (Card.rank (PM.indexPrimArray cards ix)) 1 (ix + 1) (len - 1)
  goB :: Rank -> Int -> Int -> Int -> CheckManyKindResult
  goB !rank !count !ix !len = if count == target
    then CheckManyKindYes rank ix len
    else case len of
      0 -> CheckManyKindNo
      _ ->
        let !newRank = Card.rank (PM.indexPrimArray cards ix)
         in if newRank == rank
              then goB rank (count + 1) (ix + 1) (len - 1)
              else goB newRank 1 (ix + 1) (len - 1)

-- Precondition: hand is sorted
-- Returns the highest rank pair. Matches not just pairs but
-- also three-of-a-kind, four-of-a-kind, etc.
checkManyPairExcluding ::
     PrimArray Card
  -> Rank -- rank to exclude
  -> Int -- start index
  -> Int -- length
  -> CheckManyKindResult
checkManyPairExcluding !cards !exclude !ix0 !len0 = goA ix0 len0
  where
  goA :: Int -> Int -> CheckManyKindResult
  goA !ix !len = case len of
    0 -> CheckManyKindNo
    _ -> goB (Card.rank (PM.indexPrimArray cards ix)) 1 (ix + 1) (len - 1)
  goB :: Rank -> Int -> Int -> Int -> CheckManyKindResult
  goB !rank !count !ix !len = if count == 2 && rank /= exclude
    then CheckManyKindYes rank ix len
    else case len of
      0 -> CheckManyKindNo
      _ ->
        let !newRank = Card.rank (PM.indexPrimArray cards ix)
         in if newRank == rank
              then goB rank (count + 1) (ix + 1) (len - 1)
              else goB newRank 1 (ix + 1) (len - 1)

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

-- Precondition: hand has five or more cards. Returns all suits in which
-- flush was found.
checkFlushMany :: PrimArray Card -> PrimArray Suit
checkFlushMany !cards = runPrimArrayST $ do
  dst <- PM.newPrimArray 4
  total <- go 0 0 dst
  PM.shrinkMutablePrimArray dst total
  PM.unsafeFreezePrimArray dst
  where
  go :: Word8 -> Int -> MutablePrimArray s Suit -> ST s Int
  go !suitW !dstIx !dst = if suitW < 4
    then if countSuit (Suit suitW) cards >= 5
      then do
        PM.writePrimArray dst dstIx (Suit suitW)
        go (suitW + 1) (dstIx + 1) dst
      else go (suitW + 1) dstIx dst
    else pure dstIx

countSuit :: Suit -> PrimArray Card -> Int
countSuit !suit !cards = C.foldl'
  (\acc card -> if Card.suit card == suit then acc + 1 else acc
  ) 0 cards

-- Precondition, hand is sorted first by suit and then by rank.
-- Precondition, the suit must exist somewhere in the hand, and there
-- must be at least five cards of the suit.
-- Returns all the ranks of that suit, sorted high to low.
sliceStartingAtSuit :: PrimArray Card -> Suit -> PrimArray Rank
sliceStartingAtSuit !cards !target =
  let ix = firstIndexMatchingSuit cards target
   in sliceRanksMatchingSuit cards target ix

-- Beginning at the start index, copy ranks of all the cards matching
-- the target suit. 
sliceRanksMatchingSuit :: PrimArray Card -> Suit -> Int -> PrimArray Rank
sliceRanksMatchingSuit !cards !targetSuit !srcIx0 = runPrimArrayST $ do
  let !n = PM.sizeofPrimArray cards
  dst <- PM.newPrimArray n
  let go !dstIx !srcIx = if srcIx < n
        then do
          let card = PM.indexPrimArray cards srcIx
          if Card.suit card == targetSuit
            then do
              PM.writePrimArray dst dstIx (Card.rank card)
              go (dstIx + 1) (srcIx + 1)
            else pure dstIx
        else pure dstIx
  sz <- go 0 srcIx0
  dst' <- PM.resizeMutablePrimArray dst sz
  PM.unsafeFreezePrimArray dst'

-- Returns the first index matching the suit.
-- Precondition, the suit must exist somewhere in the hand.
firstIndexMatchingSuit :: PrimArray Card -> Suit -> Int
firstIndexMatchingSuit !cards !target = go 0 where
  !n = PM.sizeofPrimArray cards
  go !ix = if ix < n
    then if target == Card.suit (PM.indexPrimArray cards ix)
      then ix
      else go (ix + 1)
    else errorWithoutStackTrace "Poker.Evaluate.firstIndexMatchingSuit: invariant violated"

-- Precondition: cards are sorted by rank
-- Precondition: there exists a card whose rank is not equal to the target rank
findHighestRankNotEqual1 ::
     PrimArray Card
  -> Rank
  -> Rank
findHighestRankNotEqual1 !cards !noMatch = go 0 where
  !n = PM.sizeofPrimArray cards
  go :: Int -> Rank
  go !ix = if ix < n
    then
      let r = Card.rank (PM.indexPrimArray cards ix)
       in if r == noMatch
            then go (ix + 1)
            else r
    else errorWithoutStackTrace "findHighestRankNotEqual1: invariant violated"

-- Precondition: cards are sorted by rank
-- Precondition: there exists least one cards whose rank is not
-- equal to the either of the targets.
findHighestRankNotEqual2 ::
     PrimArray Card
  -> Rank
  -> Rank
  -> Rank
findHighestRankNotEqual2 !cards !noMatchA !noMatchB = go 0 where
  !n = PM.sizeofPrimArray cards
  go :: Int -> Rank
  go !ix = if ix < n
    then
      let r = Card.rank (PM.indexPrimArray cards ix)
       in if r == noMatchA || r == noMatchB
            then go (ix + 1)
            else r
    else errorWithoutStackTrace "findHighestRankNotEqual2: invariant violated"

-- Precondition: cards are sorted by rank
-- Precondition: there exists least one cards whose rank is not
-- equal to any of the three targets.
findHighestRankNotEqual3 ::
     PrimArray Card
  -> Rank
  -> Rank
  -> Rank
  -> Rank
findHighestRankNotEqual3 !cards !noMatchA !noMatchB !noMatchC = go 0 where
  !n = PM.sizeofPrimArray cards
  go :: Int -> Rank
  go !ix = if ix < n
    then
      let r = Card.rank (PM.indexPrimArray cards ix)
       in if r == noMatchA || r == noMatchB || r == noMatchC
            then go (ix + 1)
            else r
    else errorWithoutStackTrace "findHighestRankNotEqual3: invariant violated"

bestStraightFlush :: UnliftedArray (PrimArray Rank) -> StraightResult
bestStraightFlush !flushRanks = go (PM.sizeofUnliftedArray flushRanks - 1) (0 :: Word8)
  where
  go :: Int -> Word8 -> StraightResult
  go !ix !acc = case ix of
    (-1) -> if acc == 0 then StraightNo else StraightYes (Rank acc)
    _ -> 
      let !mask = generateStraightBitPatternFromRanks (PM.indexUnliftedArray flushRanks ix)
       in case checkStraightBitPattern mask of
            StraightYes (Rank r) -> go (ix - 1) (max acc r)
            StraightNo -> go (ix - 1) acc
