{-# LANGUAGE OverloadedStrings #-}
module Domain.Song where

import Data.Text
import Data.Default

import Domain.ContentItem

data Song = Song
  { title :: Text
  , subtitle :: Text
  , artist :: Text
  , composer :: Text
  , genre :: Text
  , key :: Text
  , tempo :: Int
  , creation_year :: Int
  , contents :: [ContentItem]
  } deriving (Eq, Show)

instance Default Song where
  def =
    Song
      { title = ""
      , subtitle = ""
      , artist = ""
      , composer = ""
      , genre = ""
      , key = ""
      , tempo = 0
      , creation_year = 0
      , contents = []
      }