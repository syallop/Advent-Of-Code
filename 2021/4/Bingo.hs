module Bingo
  ( BingoLike (mark, score, won)
  )
  where

-- | Class of things that can be used like a bingo board.
class BingoLike b where
  -- Mark all occurances of a number in 'b'.
  mark :: Int -> b -> b

  -- The sum of unmarked numbers in 'b'.
  score :: b -> Int

  -- Has 'b' won?
  won :: b -> Bool

