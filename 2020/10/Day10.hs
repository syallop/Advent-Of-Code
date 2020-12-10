{-# LANGUAGE OverloadedStrings, DeriveAnyClass, MultiWayIf #-}
module Main where

import AOC

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (sort)
import Control.Applicative
import Control.Monad

import Data.Foldable

import Data.Maybe

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

solution = Solution
  { _parse   = parseAdapters
  , _partOne = partOne
  , _partTwo = partTwo
  }

-- | To attach an adapter to another it must be of 1-3 lower Joltage.
-- Connect each adapter, starting at an outlet 0, and ending at a built-in of MAX+3.
-- Count the number of adapters that were one different and the number that were
-- three different.
--
-- The answer is the product of these two counts.
partOne :: Adapters -> AOCM Int
partOne adapters = do
  let sortedAdapters :: Adapters
      sortedAdapters = sort adapters

      highestRated :: Adapter
      highestRated = last sortedAdapters

      builtIn :: Adapter
      builtIn = highestRated + 3

      outlet :: Adapter
      outlet = 0

      allSortedAdapters :: Adapters
      allSortedAdapters = sortedAdapters <> [builtIn]

  (_lastAdapter, countOneJoltDifferences, countThreeJoltDifferences)
    <- foldM (\(lastAdapter, countOneJoltDifferences, countThreeJoltDifferences)
               currentAdapter
               -> let joltDifference = currentAdapter - lastAdapter
                    in case joltDifference of
                         1 -> Right $ (currentAdapter, countOneJoltDifferences + 1, countThreeJoltDifferences    )
                         2 -> Right $ (currentAdapter, countOneJoltDifferences    , countThreeJoltDifferences    )
                         3 -> Right $ (currentAdapter, countOneJoltDifferences    , countThreeJoltDifferences + 1)
                         _ -> Left  $ err "adapter is more than 3 jolts difference from the last"
                                    `withAttributes`
                                      [("current", Text.pack . show $ currentAdapter)
                                      ,("last"   , Text.pack . show $ lastAdapter)
                                      ]
             )
             (outlet, 0,0)
             allSortedAdapters

  pure $ countOneJoltDifferences * countThreeJoltDifferences

-- | How many different ways are there to connect some adapters between the
-- outlet and the built-in?
partTwo :: Adapters -> AOCM Int
partTwo adapters = do
  let sortedAdapters :: Adapters
      sortedAdapters = sort adapters

      highestRated :: Adapter
      highestRated = last sortedAdapters

      builtIn :: Adapter
      builtIn = highestRated + 3

      outlet :: Adapter
      outlet = 0

      allSortedAdapters :: Adapters
      allSortedAdapters = sortedAdapters <> [builtIn]

      -- Group adapters into runs that end with a difference of 3
      -- so that we can divide-and-concur the permutations more easily.
      --
      -- I.E. by only having one way to bridge across groups, we can multiply
      -- each groups permutations together.
      groups = splitAtMaximumDiff outlet allSortedAdapters

  -- Make the problem easier by:
  -- - Only handling differences of 1 OR 3 Jolts (not 2).
  -- - Only handling runs of a limited size.
  when (not . groupDeltasAllOne $ groups)
    $ Left $ err "We only support adapters that differ by 1 or 3 Jolts - not 2"
           `withAttributes` [("reason", "lazyness")
                            ,("explanation", "the example input only uses differences of 1 or 3...")
                            ]
  when (not . groupSizesAllLessThanSix $ groups)
    $ Left $ err "We only support runs of adapters, before a difference of 3, up to size 4."
           `withAttributes` [("reason", "lazyness")
                            ,("explanation", "the example input only has runs up to 4...")]

  -- Calculate the total permutations, given the above restrictions.
  foldM
    (\accPermutations group
      -- TODO: Index into a memoised tribonacci sequence to calculate permutations instead of hardcoding some known answers.
      -> case length group - 1 of
           0 -> Right $ accPermutations
           1 -> Right $ accPermutations
           2 -> Right $ 2 * accPermutations
           3 -> Right $ 4 * accPermutations
           4 -> Right $ 7 * accPermutations
           _ -> Left $ err "Unsuccessfully cheated. Group size is larger than supported."
     )
     (let groupPermutations = 1 in groupPermutations)
     groups

  where
    -- Is the delta between each adapter in a group exactly 1?
    --
    -- If this is the case we can be lazy when computing permutations.
    groupDeltasAllOne :: [[Int]] -> Bool
    groupDeltasAllOne = all (\group -> all (== 1) . zipWith (-) (tail group) $ group)

    -- Is every group's length between 1 and 5 inclusively?
    --
    -- If so we can be lazy when computing permutations.
    groupSizesAllLessThanSix :: [[Int]] -> Bool
    groupSizesAllLessThanSix = all ((<= 6) . length)

    -- The maximum difference between adapters is 3.
    -- By grouping into runs that end with a difference of 3, we know we can
    -- simply combine permutations across the runs by multiplying.
    splitAtMaximumDiff :: Int -> [Int] -> [[Int]]
    splitAtMaximumDiff x xs =
      let (lastElem, lastSubList, accLists) = foldl (\(lastElem, accSublists, accLists) thisElem
                                                      -> if thisElem - lastElem < 3

                                                           -- Belongs in current
                                                           -- sublist
                                                           then (thisElem
                                                                ,thisElem:accSublists
                                                                ,accLists
                                                                )

                                                           -- Finishes sublist
                                                           else (thisElem
                                                                ,[thisElem]
                                                                ,(accSublists):accLists
                                                                )
                                                    )
                                                    (x
                                                    ,[x]
                                                    ,[])
                                                    xs
      in reverse $ fmap reverse (lastSubList:accLists)

type Adapters = [Adapter]

type Adapter = Int

parseAdapters :: [Text] -> AOCM Adapters
parseAdapters = parse adapters . Text.unlines

adapters :: Parser Adapters
adapters = sepBy1 digits newline <* newline

main :: IO ()
main = do
  let inputFile   = "2020/10/adapters"
  input <- readLinesFromFile inputFile
  execSolution input solution

