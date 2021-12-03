{-# language TupleSections #-}
{-# language OverloadedStrings #-}

import AOC

import DiagnosticsReport

solution
  :: Solution Int Int
solution = Solution
  { _parse   = parseDiagnosticsReport
  , _partOne = pure . powerConsumption
  , _partTwo = pure . lifeSupportRating
  }

main :: IO ()
main = do
  diagnosticsReport
    <- readLinesFromFile "2021/3/diagnostics-report"

  execSolution diagnosticsReport solution

