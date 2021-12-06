{-# language OverloadedStrings #-}

import AOC

import Fish

solution
  :: Solution Int Int
solution = Solution
  { _parse   = parseFish
  , _partOne = pure . population . days 80
  , _partTwo = pure . population . days 256
  }

main :: IO ()
main = do
  fishReproductiveState
    <- readLinesFromFile "2021/6/fish-reproductive-states"

  execSolution fishReproductiveState solution

