{-# language PatternSynonyms #-}
{-# language TypeApplications #-}

module Poker.Rank
  ( -- * Encoding
    encodeEnglishSymbolicUpper
  , encodeEnglishPluralTitle
  , encodeEnglishTitle
  ) where

import Data.Text (Text)
import Poker.Types

import qualified Data.Text as T

encodeEnglishSymbolicUpper :: Rank -> Text
{-# noinline encodeEnglishSymbolicUpper #-}
encodeEnglishSymbolicUpper s = case s of
  Two -> T.singleton '2'
  Three -> T.singleton '3'
  Four -> T.singleton '4'
  Five -> T.singleton '5'
  Six -> T.singleton '6'
  Seven -> T.singleton '7'
  Eight -> T.singleton '8'
  Nine -> T.singleton '9'
  Ten -> T.pack "10"
  Jack -> T.singleton 'J'
  Queen -> T.singleton 'Q'
  King -> T.singleton 'K'
  Ace -> T.singleton 'A'
  _ -> T.singleton 'X'

encodeEnglishPluralTitle :: Rank -> Text
{-# noinline encodeEnglishPluralTitle #-}
encodeEnglishPluralTitle s = case s of
  Two -> T.pack "Twos"
  Three -> T.pack "Threes"
  Four -> T.pack "Fours"
  Five -> T.pack "Fives"
  Six -> T.pack "Sixes"
  Seven -> T.pack "Sevens"
  Eight -> T.pack "Eights"
  Nine -> T.pack "Nines"
  Ten -> T.pack "Tens"
  Jack -> T.pack "Jacks"
  Queen -> T.pack "Queens"
  King -> T.pack "Kings"
  Ace -> T.pack "Aces"
  _ -> T.pack "Unknowns"

encodeEnglishTitle :: Rank -> Text
{-# noinline encodeEnglishTitle #-}
encodeEnglishTitle s = case s of
  Two -> T.pack "Two"
  Three -> T.pack "Three"
  Four -> T.pack "Four"
  Five -> T.pack "Five"
  Six -> T.pack "Six"
  Seven -> T.pack "Seven"
  Eight -> T.pack "Eight"
  Nine -> T.pack "Nine"
  Ten -> T.pack "Ten"
  Jack -> T.pack "Jack"
  Queen -> T.pack "Queen"
  King -> T.pack "King"
  Ace -> T.pack "Ace"
  _ -> T.pack "Unknown"
