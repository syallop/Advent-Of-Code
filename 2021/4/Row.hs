module Row
  ( Row (..)
  , row

  , marks
  )
  where

import AOC
import Bingo
import Mark

import Data.List (intersperse)

data Row = Row
  { _column1 :: Mark Int
  , _column2 :: Mark Int
  , _column3 :: Mark Int
  , _column4 :: Mark Int
  , _column5 :: Mark Int
  }
  deriving (Eq, Ord)

marks
  :: Row
  -> [Mark Int]
marks (Row c1 c2 c3 c4 c5) = [c1, c2, c3, c4, c5]

instance Show Row where
  show (Row c1 c2 c3 c4 c5) = mconcat . intersperse " " . fmap show $ [c1,c2,c3,c4,c5]

-- A row is like a 1d board.
instance BingoLike Row where
  mark i (Row c1 c2 c3 c4 c5)
    = Row (mark i c1) (mark i c2) (mark i c3) (mark i c4) (mark i c5)

  score (Row c1 c2 c3 c4 c5)
    = sum . fmap score $ [c1, c2, c3, c4, c5]

  won = all won . marks

row
  :: Parser Row
row
  = Row
 <$> (intMark <* space)
 <*> (intMark <* space)
 <*> (intMark <* space)
 <*> (intMark <* space)
 <*> intMark

