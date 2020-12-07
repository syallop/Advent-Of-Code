{-# LANGUAGE OverloadedStrings #-}
{-

TODO:
- Part one uses a fixed 2-tuple
- Part two uses a fixed 3-tuple

The code for computing the expense set could be generalised to a n-tuple, to remove the code duplication but either:
- Lose type safety by usings lists which we 'promise' are the correct length
- Use overkill type-level features (such as length-indexed types).

Is there a good middle-ground that allows re-use without too crazy types? Perhaps a smart constructor is fine.
 -}
module Main where

import AOC

import Data.Text (Text)
import qualified Data.Text      as Text
import qualified Data.Text.IO   as Text
import qualified Data.Text.Read as Text

import Data.List (tails)

solution
  :: Int
  -> Solution Int Int
solution targetTotal = Solution
  { _parse   = mapM parseDecimal
  , _partOne = \expenses -> fmap (uncurry (*))             $ oneExpensePairSumsTo   expenses targetTotal
  , _partTwo = \expenses -> fmap (\(x,y,z) -> (x * y * z)) $ oneExpenseTripleSumsTo expenses targetTotal
  }

main :: IO ()
main = do
  let targetTotal = 2020
      inputFile   = "2020/1/expenses"

  expenses <- readLinesFromFile inputFile
  execSolution expenses (solution targetTotal)


type Expense  = Int
type Expenses = [Expense]

-- | Find the single expense pair that sums to a given value.
oneExpensePairSumsTo :: Expenses -> Int -> AOCM (Expense, Expense)
oneExpensePairSumsTo expenses targetSum = case expenses `pairsThatSumTo` targetSum of
  []
    -> Left $ err "No two expenses sum to given target" `withAttributes`
              [("target", Text.pack . show $ targetSum)]

  [expensePair]
    -> Right expensePair

  matches
    -> Left $ err "More than two expenses sum to the target total" `withAttributes`
              [("target" , Text.pack . show $ targetSum)
              ,("matches", Text.pack . show $ matches)
              ]

-- | Find the single expense triple that sums to a given value.
oneExpenseTripleSumsTo :: Expenses -> Int -> AOCM (Expense, Expense, Expense)
oneExpenseTripleSumsTo expenses targetSum = case expenses `triplesThatSumTo` targetSum of
  []
    -> Left $ err "No three expenses sum to given target" `withAttributes`
              [("target", Text.pack . show $ targetSum)]

  [expenseTriple]
    -> Right expenseTriple

  matches
    -> Left $ err "More than two expenses sum to the target total" `withAttributes`
              [("target" , Text.pack . show $ targetSum)
              ,("matches", Text.pack . show $ matches)
              ]

-- | All pairs of numbers that sum to a target total.
pairsThatSumTo :: (Num a, Eq a) => [a] -> a -> [(a, a)]
pairsThatSumTo nums sum
  = filter ((== sum) . uncurry (+))
  . pairs
  $ nums

-- | All triples of numbers that sum to a target total.
triplesThatSumTo :: (Num a, Eq a) => [a] -> a -> [(a,a,a)]
triplesThatSumTo nums sum
  = filter (\(x,y,z) -> sum == (x + y + z))
  . triples
  $ nums

-- | Every two-pair combination of elements.
pairs :: [a] -> [(a,a)]
pairs l = [(x,y) | (x:xs) <- tails l
                 , y <- xs]

-- | Every three-pair combination of elements.
triples :: [a] -> [(a,a,a)]
triples l = [(x,y,z) | (x:xs) <- tails l
                     , (y:ys) <- tails xs
                     , z      <- ys]


