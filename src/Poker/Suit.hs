{-# language OverloadedStrings #-}

module Poker.Suit
  ( encodeEnglishLower
  , encodeEnglishCharUpper
  , encodeSymbol
  ) where

import Poker.Types
import Data.Text (Text)

encodeEnglishLower :: Suit -> Text
encodeEnglishLower s = case s of
  Spade -> "spade"
  Diamond -> "diamond"
  Heart -> "heart"
  Club -> "club"
  _ -> "unknown"

encodeEnglishCharUpper :: Suit -> Char
encodeEnglishCharUpper s = case s of
  Spade -> 'S'
  Diamond -> 'D'
  Heart -> 'H'
  Club -> 'C'
  _ -> 'X'

encodeSymbol :: Suit -> Char
encodeSymbol s = case s of
  Spade -> '♠'
  Diamond -> '♦'
  Heart -> '♥'
  Club -> '♣'
  _ -> 'x'
