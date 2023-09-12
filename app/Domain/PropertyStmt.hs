module Domain.PropertyStmt where

import Data.Text (Text)

data PropertyStmt
  = Title Text
  | Subtitle Text
  | Artist Text
  | Composer Text
  | Genre Text
  | Key Text
  | Tempo Int
  | CreationYear Int
  deriving (Eq, Show)