{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Void
import Data.Default
import System.Environment (getArgs)
import Control.Applicative hiding (many, some)
import Data.Text as T
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
-- import Text.Megaparsec.Error
-- import qualified Text.Megaparsec.Char.Lexer as L

import Domain.Song
import Domain.ContentItem
import Domain.PropertyStmt

type Parser = Parsec Void Text

songFromProperties :: Song -> [PropertyStmt] -> Song
songFromProperties song properties =
  case properties of
    [] -> song
    (prop:tl) ->
      songFromProperties updated_song tl
      where updated_song = case prop of
                                Title value -> song { title=value }
                                Subtitle value -> song { subtitle=value }
                                Artist value -> song { artist=value }
                                Composer value -> song { composer=value }
                                Genre value -> song { genre=value }
                                Key value -> song { key=value }
                                Tempo value -> song { tempo=value }
                                CreationYear value -> song { creation_year=value }

skipSpaces :: Parser ()
skipSpaces = skipSome spaceChar

spaces :: Parser Text
spaces = T.pack <$> some spaceChar

propertyName :: Text -> Parser ()
propertyName name = do
  skipMany spaceChar
  _ <- string name
  skipSpaces

intValue :: Parser Int
intValue = read <$> someTill digitChar newline

textValue :: Parser Text
textValue = T.pack <$> someTill (letterChar <|> oneOf (" .'" :: String)) newline


titleStmt :: Parser PropertyStmt
titleStmt = propertyName "title" >> Title <$> textValue

subtitleStmt :: Parser PropertyStmt
subtitleStmt = propertyName "title" >> Title <$> textValue

artistStmt :: Parser PropertyStmt
artistStmt = propertyName "title" >> Title <$> textValue

composerStmt :: Parser PropertyStmt
composerStmt = propertyName "title" >> Title <$> textValue

genreStmt :: Parser PropertyStmt
genreStmt = propertyName "title" >> Title <$> textValue

keyStmt :: Parser PropertyStmt
keyStmt = propertyName "title" >> Title <$> textValue

tempoStmt :: Parser PropertyStmt
tempoStmt = propertyName "tempo" >> Tempo <$> intValue

creationYearStmt :: Parser PropertyStmt
creationYearStmt = propertyName "creation_year" >> CreationYear <$> intValue

propertyStmt :: Parser PropertyStmt
propertyStmt =
  choice [ titleStmt
         , subtitleStmt
         , artistStmt
         , composerStmt
         , genreStmt
         , keyStmt
         , tempoStmt
         , creationYearStmt
         ]

parseProperties :: Parser [PropertyStmt]
parseProperties = someTill propertyStmt (newline >> newline)

whitespaceItem :: Parser ContentItem
whitespaceItem =  WhiteSpace <$> (T.length <$> spaces)

alterationSign :: Parser Char
alterationSign = char '#' <|> char 'b'

chordType :: Parser Text
chordType = string "m" <|> string "dim" <|> string "aug"

chordInversion :: Parser Char
chordInversion = char '/' >> oneOf ("1234579" :: String)

chordItem :: Parser ContentItem
chordItem = do
  note <- noteFromLetter <$> oneOf ("ABCDEFG" :: String)
  alteration <- alterationFromSign <$> option '0' alterationSign
  chord_type <- chordTypeFromWord <$> option "" chordType
  inversion <- inversionFromDigit <$> option '0' chordInversion
  return $ Chord note alteration chord_type inversion

textItem :: Parser ContentItem
textItem = do
  line <- T.pack <$> many (satisfy (/= '\n'))
  return $ Lyrics line

contentItem :: Parser ContentItem
contentItem =
  choice [ whitespaceItem
         , chordItem
         , textItem
         ]

parseSong :: Parser Song
parseSong = do
  song <- songFromProperties def <$> parseProperties
  song_contents <- many contentItem
  return song{ contents=song_contents }

main :: IO ()
main = do
  args <- getArgs
  let file_name = case args of
                       (name:_) -> name
                       [] -> error "Por favor provee un nombre de archivo válido"
  file <- T.pack <$> readFile file_name
  let _ = runParser parseSong file_name file
  putStrLn ""