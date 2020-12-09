{-# LANGUAGE OverloadedStrings, DeriveAnyClass, MultiWayIf #-}
module Main where

import AOC

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (groupBy, init, tails, inits)
import Control.Applicative
import Control.Monad

import Data.Foldable

import Data.Maybe

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

solution = Solution
  { _parse   = parseCypher
  , _partOne = partOne
  , _partTwo = partTwo
  }

-- | After a preamble of 25 numbers, each subsequent number should equal the sum
-- of any two different prior different numbers in the sliding window.
-- The first number that doesnt is the 'invalid' number which is the solution.
partOne :: Cypher -> AOCM Int
partOne cypher =
  let (window, rest) = splitAt 25 cypher
      result = foldM (\window candidate
                       -> if (>= 1) . length                    -- If there is more than one possibility, we're good.
                                    . filter (== candidate)     -- Discard any which dont match the candidate
                                    . map (uncurry (+))         -- Calculate their sum
                                    . filter (\(x,y) -> x /= y) -- Exclude pairs where both numbers are the same
                                    . pairs                     -- Every possible pairing
                                    $ window
                          then Right $ tail window <> [candidate] -- TODO: These operations are inefficient on lists. Is it better to manipulate them in reverse/ change structures.
                          else Left candidate
                     )
                     window
                     rest

   in case result of
        Left outlier
          -> Right outlier

        Right _finalWindow
          -> Left $ err "Did not locate outlier in Cypher"

-- | Find the sum of the smallest and largest element in a continuous
-- subsequence that itself sums to the invalid number from part 1.
partTwo :: Cypher -> AOCM Int
partTwo cypher = do
  invalidNumber <- partOne cypher
  case filter ((== invalidNumber) . sum) . filter ((>= 2) . length) . sublists $ cypher of
    []
      -> Left $ err "No continuous sequences sum to the invalid number"

    [sequence]
      -> Right $ (minimum sequence) + (maximum sequence)

    _ -> Left $ err "More than one continous sequence sums to the invalid number"

type Cypher = [Int]

parseCypher :: [Text] -> AOCM Cypher
parseCypher = parse cypher . Text.unlines

cypher :: Parser Cypher
cypher = sepBy1 digits newline <* newline

pairs :: [a] -> [(a,a)]
pairs l = [(x, y) | (x:xs) <- tails l, y <- xs]

-- | Create all continuous sublists, E.G.
-- [1,2,3] => [[],[1],[2],[3],[1,2],[2,3],[1,2,3]]
sublists :: [a] -> [[a]]
sublists = ([]:) . (tail . inits) <=< tails

main :: IO ()
main = do
  let inputFile   = "2020/9/cypher"

  input <- readLinesFromFile inputFile
  execSolution input solution

