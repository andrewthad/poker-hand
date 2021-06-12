{-# language TypeApplications #-}

module Poker.Card
  ( -- * Constructor
    construct
    -- * Accessors
  , suit
  , rank
  ) where

import Data.Bits (unsafeShiftR,unsafeShiftL,(.|.))
import Data.Word (Word8, Word16)
import Poker.Types (Rank(..),Suit(..),Card(..))

-- | Construct a 'Card'.
construct :: Suit -> Rank -> Card
construct (Suit x) (Rank y) = Card
  (fromIntegral @Word8 @Word16 y .|. unsafeShiftL (fromIntegral @Word8 @Word16 x) 8)

rank :: Card -> Rank
rank (Card x) = Rank (fromIntegral @Word16 @Word8 x)

suit :: Card -> Suit
suit (Card x) = Suit (fromIntegral @Word16 @Word8 (unsafeShiftR x 8))
