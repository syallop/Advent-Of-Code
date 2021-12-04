{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
module Mark
  ( Mark (..)
  , intMark
  )
  where

import AOC
import Bingo

-- | A Mark is a single cell that may be marked or unmarked.
data Mark a
  = Unmarked a
  | Marked a
  deriving (Eq, Ord)

instance Show (Mark Int) where
  show m = case m of
    Marked _
      -> " X"

    Unmarked i
      -> padTens i

    where
      padTens :: Int -> String
      padTens i = (if i < 10 then " " else "") <> show i

-- A cell is like a 1x1 board.
instance BingoLike (Mark Int) where
  mark i m = case m of
    Unmarked j
      | j == i
       -> Marked j
      | otherwise
       -> m
    _
     -> m

  score m = case m of
    Marked _
      -> 0

    Unmarked i
      -> i

  won m = case m of
    Marked _
      -> True

    Unmarked _
      -> False

intMark
  :: Parser (Mark Int)
intMark
  = (\tens units -> Unmarked $ 10 * tens + units)
 <$> ( alternatives
        [ space *> pure 0
        , digit
        ]
    )
 <*> digit

