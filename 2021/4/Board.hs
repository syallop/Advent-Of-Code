{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}

module Board
  ( Board ()
  , parseBoards

  , rows
  , columns
  )
  where

import AOC

import Bingo
import Row

import Data.List (intersperse)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Set as Set
import qualified Data.Text as Text

-- | A Board with 5 rows and 5 columns containing digits 0-99 that may be
-- marked and are not present more than once.
data Board = Board
  { _row1 :: Row
  , _row2 :: Row
  , _row3 :: Row
  , _row4 :: Row
  , _row5 :: Row
  }
  deriving (Eq, Ord)

instance Show Board where
  show (Board r1 r2 r3 r4 r5) = mconcat . intersperse "\n" . fmap show $ [r1,r2,r3,r4,r5]

rows
  :: Board
  -> [Row]
rows (Board r1 r2 r3 r4 r5) = [r1, r2, r3, r4, r5]

columns
  :: Board
  -> [Row]
columns = rows . transpose
  where
    transpose
      :: Board
      -> Board
    transpose (Board (Row r1c1 r1c2 r1c3 r1c4 r1c5)
                     (Row r2c1 r2c2 r2c3 r2c4 r2c5)
                     (Row r3c1 r3c2 r3c3 r3c4 r3c5)
                     (Row r4c1 r4c2 r4c3 r4c4 r4c5)
                     (Row r5c1 r5c2 r5c3 r5c4 r5c5)
              )
             = Board (Row r1c1 r2c1 r3c1 r4c1 r5c1)
                     (Row r1c2 r2c2 r3c2 r4c2 r5c2)
                     (Row r1c3 r2c3 r3c3 r4c3 r5c3)
                     (Row r1c4 r2c4 r3c4 r4c4 r5c4)
                     (Row r1c5 r2c5 r3c5 r4c5 r5c5)

instance BingoLike Board where

  -- If numbers are only allowed to appear once, we could short-circuit.
  mark i (Board r1 r2 r3 r4 r5)
    = Board (mark i r1) (mark i r2) (mark i r3) (mark i r4) (mark i r5)

  score (Board r1 r2 r3 r4 r5)
    = sum . fmap score $ [r1, r2, r3, r4, r5]

  won board
    = any won (rows board)
   || any won (columns board)



{- Parsing -}

parseBoards
  :: [Text]
  -> AOCM (Set Board)
parseBoards
  = parse boards . Text.unlines

boards
  :: Parser (Set Board)
boards
  = Set.fromList <$> sepBy1 board newline

board
  :: Parser Board
board
  = Board
 <$> (row <* newline)
 <*> (row <* newline)
 <*> (row <* newline)
 <*> (row <* newline)
 <*> (row <* newline)


