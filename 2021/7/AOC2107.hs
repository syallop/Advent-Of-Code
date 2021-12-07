{-# language OverloadedStrings #-}

import AOC

import Crab

solution
  :: Solution Int Int
solution = Solution
  { _parse   = parseCrabs
  , _partOne = pure . leastFuel . possibleAlignments constantFuelCost
  , _partTwo = pure . leastFuel . possibleAlignments increasingFuelCost
  }

main :: IO ()
main = do
  crabAlignments
    <- readLinesFromFile "2021/7/crab-alignments"

  execSolution crabAlignments solution

