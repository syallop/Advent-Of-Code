{-# language TupleSections #-}
{-# language OverloadedStrings #-}

import AOC

import BingoSubsystem

solution
  :: Solution Int Int
solution = Solution
  { _parse   = parseBingoSubsystem
  , _partOne = play toWin
  , _partTwo = play toLose
  }

main :: IO ()
main = do
  bingoSubsystem
    <- readLinesFromFile "2021/4/bingo-subsystem"

  execSolution bingoSubsystem solution

