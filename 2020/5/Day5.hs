{-# LANGUAGE OverloadedStrings, DeriveAnyClass #-}
module Main where

import AOC

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Sort

import Data.List (groupBy)
import Control.Applicative
import Control.Monad

solution :: Solution Int Int
solution = Solution
  { _parse   = parseBoardingPasses
  , _partOne = partOne
  , _partTwo = partTwo
  }

main :: IO ()
main = do
  let inputFile   = "2020/5/boardingPasses"

  input <- readLinesFromFile inputFile
  execSolution input solution

-- | The maximum seat ID of all BoardingPass's
partOne :: [BoardingPass] -> AOCM Int
partOne boardingPasses = fmap maximum $ seatIDs boardingPasses (128, 8)

-- | Out of all the SeatIDs, find the gap which is our seat.
partTwo :: [BoardingPass] -> AOCM Int
partTwo boardingPasses = do
  -- To find our seat we need to look for a gap.
  -- - Sort the IDs
  sortedSeatIDs <- sort <$> seatIDs boardingPasses (128, 8)

  -- - There must be at least two seats for a gap to make sense.
  firstSeatID <- case sortedSeatIDs of
    []
      -> Left $ err "Cannot find a gap in the seat ID's without any seats."

    [_]
      -> Left $ err "Cannot find a gap in the seat ID's with only one seat."

    (s:_)
      -> Right s

  -- - When we find a gap counter-intuitively use Eithers 'Left' value to
  --   trigger a short-circuit.
  --   The right value therefore represents 'failure' and contains the last seat
  --   id we saw (that wasn't ours).
  let gap :: Either Int Int
      gap = foldM (\lastSeatID thisSeatID -> if thisSeatID /= lastSeatID+1
                                               then Left $ lastSeatID+1
                                               else Right thisSeatID
                  )
                  (firstSeatID-1)
                  sortedSeatIDs

  -- The Gap is our seat.
  case gap of
    Left gapID
      -> pure gapID

    Right lastSeatID
      -> Left $ err "Traversed all seat ids without finding a gap"
              `withAttributes` [("last-seat", Text.pack . show $ lastSeatID)]

-- | Calculate a collection of SeatIDs
seatIDs :: [BoardingPass] -> (Int,Int) -> AOCM [Int]
seatIDs boardingPasses (rowCount,colCount) = mapM (\boardingPass -> seatID boardingPass (rowCount,colCount)) boardingPasses

-- | A SeatID is calculated by partitioning the rows and columns and combining
-- the resulting index into a single unique number.
seatID :: BoardingPass -> (Int,Int) -> AOCM Int
seatID boardingPass (rowCount, colCount) = do
  let (lowerRowIx, upperRowIx) = partitionIndex rowCount (_rowPartition boardingPass)
      (lowerColIx, upperColIx) = partitionIndex colCount (_columnPartition boardingPass)
  when (lowerRowIx + 1 /= upperRowIx) $ Left $ err "Row not narrowed down to a single row"
                                        `withAttributes` [("lower", Text.pack . show $ lowerRowIx)
                                                         ,("upper", Text.pack . show $ upperRowIx)
                                                         ]
  when (lowerColIx + 1 /= upperColIx) $ Left $ err "Column not narrowed down to a single column"
                                        `withAttributes` [("lower", Text.pack . show $ lowerColIx)
                                                         ,("upper", Text.pack . show $ upperColIx)
                                                         ]

  pure $ lowerRowIx*colCount + lowerColIx

-- | A BoardingPass partitions the columns and rows of the aircraft, hopefully
-- narrowing down to a single seat.
data BoardingPass = BoardingPass
  { _rowPartition    :: Partition
  , _columnPartition :: Partition
  }
  deriving Show

-- | A Partition is a left-to-right series of partitions that divide the
-- available space into two.
newtype Partition = Partition {_unPartition :: [PartitionDirection]}
  deriving Show

-- | Either pick the left or right half of a partition.
data PartitionDirection
  = PartitionLeft
  | PartitionRight
  deriving Show

-- | Parse a collection of BoardingPass's.
parseBoardingPasses :: [Text] -> AOCM [BoardingPass]
parseBoardingPasses = mapM (parse boardingPass)

-- | A BoardingPass partitions it's rows, then it's columns.
boardingPass :: Parser BoardingPass
boardingPass = BoardingPass <$> rowPartition <*> columnPartition

-- | A Row is a sequence of F or B partitions.
rowPartition :: Parser Partition
rowPartition = Partition <$> many rowPartitionDirection
  where
    rowPartitionDirection :: Parser PartitionDirection
    rowPartitionDirection = (charIs 'F' *> pure PartitionLeft) <|> (charIs 'B' *> pure PartitionRight)

-- | A Column is a sequence of L or R partitions.
columnPartition :: Parser Partition
columnPartition = Partition <$> many columnPartitionDirection
  where
    columnPartitionDirection :: Parser PartitionDirection
    columnPartitionDirection = (charIs 'L' *> pure PartitionLeft) <|> (charIs 'R' *> pure PartitionRight)

-- | Follow the partition directions, for an initial size, returning the lower
-- and upper indexes.
partitionIndex :: Int -> Partition -> (Int,Int)
partitionIndex size (Partition partitionDirections) =
  foldl (\(lowerIx,upperIx) direction
          -> let size     = upperIx - lowerIx
                 midPoint = lowerIx + (size `div` 2)
              in case direction of
                   PartitionLeft
                     -> (lowerIx,midPoint)
                   PartitionRight
                     -> (midPoint,upperIx)
        )
        (0,size)
        partitionDirections

