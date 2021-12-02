{-# language TupleSections #-}
{-# language OverloadedStrings #-}

import AOC

import Data.List (tails)
import Data.Text (Text)

import Command
import Movement
import Position

solution
  :: Solution Int Int
solution = Solution
  { _parse   = parseCommands
  , _partOne = pure . value . foldl move (start :: Position)
  , _partTwo = pure . value . foldl move (start :: AimedPosition)
  }

main :: IO ()
main = do
  commands
    <- readLinesFromFile "2021/2/commands"

  execSolution commands solution

