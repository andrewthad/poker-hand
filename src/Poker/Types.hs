{-# language GeneralizedNewtypeDeriving #-}
{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language PatternSynonyms #-}
{-# language PolyKinds #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Poker.Types
  ( -- * Types
    Card(..)
  , Suit(..)
  , Rank(..)
    -- * Suits
  , pattern Spade
  , pattern Heart
  , pattern Diamond
  , pattern Club
    -- * Ranks
  , pattern Ace
  , pattern King
  , pattern Queen
  , pattern Jack
  , pattern Ten
  , pattern Nine
  , pattern Eight
  , pattern Seven
  , pattern Six
  , pattern Five
  , pattern Four
  , pattern Three
  , pattern Two
  ) where

import Data.Bits (unsafeShiftR)
import Data.Word (Word8,Word16)
import Data.Primitive (Prim)

newtype Card = Card Word16
  deriving (Eq,Prim)

newtype Suit = Suit Word8
  deriving (Eq,Prim)

pattern Spade :: Suit
pattern Spade = Suit 3

pattern Heart :: Suit
pattern Heart = Suit 2

pattern Club :: Suit
pattern Club = Suit 1

pattern Diamond :: Suit
pattern Diamond = Suit 0

newtype Rank = Rank Word8
  deriving (Eq,Ord,Prim)

pattern Two :: Rank
pattern Two = Rank 2

pattern Three :: Rank
pattern Three = Rank 3

pattern Four :: Rank
pattern Four = Rank 4

pattern Five :: Rank
pattern Five = Rank 5

pattern Six :: Rank
pattern Six = Rank 6

pattern Seven :: Rank
pattern Seven = Rank 7

pattern Eight :: Rank
pattern Eight = Rank 8

pattern Nine :: Rank
pattern Nine = Rank 9

pattern Ten :: Rank
pattern Ten = Rank 10

pattern Jack :: Rank
pattern Jack = Rank 11

pattern Queen :: Rank
pattern Queen = Rank 12

pattern King :: Rank
pattern King = Rank 13

pattern Ace :: Rank
pattern Ace = Rank 14

instance Show Rank where
  showsPrec p (Rank r) = case r of
    2 -> showString "Two"
    3 -> showString "Three"
    4 -> showString "Four"
    5 -> showString "Five"
    6 -> showString "Six"
    7 -> showString "Seven"
    8 -> showString "Eight"
    9 -> showString "Nine"
    10 -> showString "Ten"
    11 -> showString "Jack"
    12 -> showString "Queen"
    13 -> showString "King"
    14 -> showString "Ace"
    _ -> showParen (p > 10) (showString "Rank " . shows r)

instance Show Suit where
  showsPrec p (Suit r) = case r of
    3 -> showString "Spade"
    2 -> showString "Heart"
    1 -> showString "Club"
    0 -> showString "Diamond"
    _ -> showParen (p > 10) (showString "Suit " . shows r)

instance Show Card where
  showsPrec p (Card x) = showParen (p > 10)
    $ showString "Card "
    . shows suit
    . showChar ' '
    . shows rank
    where
    rank = Rank (fromIntegral @Word16 @Word8 x)
    suit = Suit (fromIntegral @Word16 @Word8 (unsafeShiftR x 8))
