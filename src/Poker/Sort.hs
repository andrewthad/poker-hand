{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language TypeApplications #-}

module Poker.Sort
  ( byRankThenSuit
  , bySuitThenRank
  ) where

import Control.Monad.ST.Run (runPrimArrayST)
import Data.Primitive (PrimArray)
import Data.Word (Word8)
import Poker.Types (Card,Rank(Rank),Suit(Suit))

import qualified Poker.Card as Card
import qualified Data.Primitive as PM
import qualified Data.Primitive.Contiguous as C

-- | Sort a hand so that high cards come first. Ace counts as the
-- highest card.
byRankThenSuit :: PrimArray Card -> PrimArray Card
{-# noinline byRankThenSuit #-}
byRankThenSuit !cards = byScore
  (\c ->
    let Rank r = Card.rank c
        Suit s = Card.suit c
     in (16 * (fromIntegral @Word8 @Int r)) + fromIntegral @Word8 @Int s
  )
  cards

-- | Sort a hand so that high cards come first. Ace counts as the
-- highest card.
bySuitThenRank :: PrimArray Card -> PrimArray Card
{-# noinline bySuitThenRank #-}
bySuitThenRank !cards = byScore
  (\c ->
    let Rank r = Card.rank c
        Suit s = Card.suit c
     in (16 * (fromIntegral @Word8 @Int s)) + fromIntegral @Word8 @Int r
  )
  cards

-- internal function
byScore ::
     (Card -> Int) -- scoring function, higher scores come first
  -> PrimArray Card
  -> PrimArray Card
{-# inline byScore #-}
byScore f !cards = runPrimArrayST $ do
  let n = PM.sizeofPrimArray cards
  -- In the "used" array, 0 means unused, 1 means used
  used <- PM.newPrimArray n
  PM.setPrimArray used 0 n (0 :: Word8)
  dst <- PM.newPrimArray n 
  let go !dstIx = if dstIx < n
        then do
          -- We invent a minimum score -1 that should get eliminated
          -- on each pass through.
          (_,hiIx) <- C.ifoldlM'
            (\ (!hi,!hiIx) ix card -> PM.readPrimArray used ix >>= \case
              1 -> pure (hi,hiIx)
              _ -> do
                let score = f card
                if score > hi
                  then pure (score,ix)
                  else pure (hi,hiIx)
            ) (-1 :: Int, -1 :: Int) cards
          PM.writePrimArray dst dstIx (PM.indexPrimArray cards hiIx)
          PM.writePrimArray used hiIx (1 :: Word8)
          go (dstIx + 1)
        else pure ()
  go 0
  PM.unsafeFreezePrimArray dst
