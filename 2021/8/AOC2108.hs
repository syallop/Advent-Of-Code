{-# language OverloadedStrings #-}

import AOC

import Solve
import SevenSide

import Data.Foldable

solution
  :: Solution Int Int
solution = Solution
  { _parse   = parseInput
  , _partOne = \input -> do
      outputs <- decodeOutputs input
      pure . length
           . filter (\s -> or [(s == One)
                              ,(s == Four)
                              ,(s == Seven)
                              ,(s == Eight)
                              ]
                    )
           . mconcat
           $ outputs

  , _partTwo = \input -> do
      outputs <- decodeOutputs input
      pure . sum
           . fmap (\[th,h,t,u] -> sum [1000 * asDigit th
                                      , 100 * asDigit h
                                      ,  10 * asDigit t
                                      ,       asDigit u
                                      ])
           $ outputs
  }

main :: IO ()
main = do
  observedSevenSegments
    <- readLinesFromFile "2021/8/observed-seven-segments"

  execSolution observedSevenSegments solution

