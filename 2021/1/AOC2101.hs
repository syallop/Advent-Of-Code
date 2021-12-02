{-# language TupleSections #-}
{-# language OverloadedStrings #-}

import AOC

import Data.List (tails)

solution
  :: Solution Int Int
solution = Solution
  { _parse   = mapM parseDecimal
  , _partOne = pure . depthIncreases
  , _partTwo = pure . depthIncreases . fmap sum . window 3
  }

main :: IO ()
main = do
  depths
    <- readLinesFromFile "2021/1/depths"

  execSolution depths solution

-- | Count the number of increases between each element.
depthIncreases
  :: ( Num a
     , Ord a
     )
  => [a]
  -> a
depthIncreases
  = fst
  . foldl accumulateIncreases
          (0, Nothing)
  where
    accumulateIncreases
      :: ( Num a
         , Ord a
         )
      => (a, Maybe a)
      -> a
      -> (a, Maybe a)
    accumulateIncreases (increaseCount, mLastDepth) currentDepth = case mLastDepth of
      Just lastDepth
        | lastDepth < currentDepth
        -> (increaseCount + 1, Just currentDepth)

      _ -> (increaseCount, Just currentDepth)

-- | Break a list into a sliding window of sub-lists of exactly n length.
window
  :: Int
  -> [a]
  -> [[a]]
window n
  = filter ((== n) . length)
  . fmap (take 3)
  . tails

