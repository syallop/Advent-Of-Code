{-# language OverloadedStrings #-}

import AOC

import Vent
import Orientation

solution
  :: Solution Int Int
solution = Solution
  { _parse   = parseVents
  , _partOne = pure . countDangerousVents [isHorizontal, isVertical]
  , _partTwo = pure . countDangerousVents [isHorizontal, isVertical, isDiagonal]
  }

main :: IO ()
main = do
  vents
    <- readLinesFromFile "2021/5/vents"

  execSolution vents solution

