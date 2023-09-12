{-# LANGUAGE OverloadedStrings #-}
module Domain.ContentItem where

import Data.Text (Text)

data ChordNote = A | B | C | D | E | F | G | U
                 deriving (Eq, Show)

data Alteration = Natural | Sharp | Flat
                  deriving (Eq, Show)

data ChordType = Maj | Min | Aug | Dim
                 deriving (Eq, Show)

data ContentItem
  = WhiteSpace Int
  | Chord ChordNote Alteration ChordType Int
  | Lyrics Text
  deriving (Eq, Show)

noteFromLetter :: Char -> ChordNote
noteFromLetter value =
  case value of
    'A' -> A
    'B' -> B
    'C' -> C
    'D' -> D
    'E' -> E
    'F' -> F
    'G' -> G
    _ -> U

alterationFromSign :: Char -> Alteration
alterationFromSign value =
  case value of
    '#' -> Sharp
    'b' -> Flat
    _   -> Natural

chordTypeFromWord :: Text -> ChordType
chordTypeFromWord value =
  case value of
    "m" -> Min
    "dim" -> Dim
    "aug" -> Aug
    _ -> Maj

inversionFromDigit :: Char -> Int
inversionFromDigit value =
  case value of
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '7' -> 7
    '9' -> 9
    _ -> 1