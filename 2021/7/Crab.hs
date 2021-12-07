{-# language ScopedTypeVariables #-}
module Crab
  ( Crabs
  , Crab
  , Fuel

  , leastFuel
  , possibleAlignments
  , constantFuelCost
  , increasingFuelCost

  , parseCrabs
  )
  where

import AOC

import Data.Text (Text)
import Data.List (sortOn)

type Crabs = [Crab]

-- | A Crab is described by it's horizontal alignment.
type Crab = Int

-- | Fuel is required to move one step of alignment.
type Fuel = Int

-- | Find the minimum fuel across a set of alignments.
leastFuel
  :: [(Crab,Fuel)]
  -> Fuel
leastFuel
  = snd
  . head
  . sortOn snd

-- | Within the range of the left-most and right-most crab, what is the total
-- fuel cost to align to each of these positions?
possibleAlignments
  :: MovementCost
  -> [Crab]
  -> [(Crab,Fuel)]
possibleAlignments cost crabs
  = fmap (\candidatePosition -> ( candidatePosition
                                , sum . fmap (cost candidatePosition) $ crabs
                                )
         )
  . candidateAlignments
  $ crabs

-- | The candidate alignments are contained within the left-most and right-most
-- crab.
candidateAlignments
  :: [Crab]
  -> [Crab]
candidateAlignments crabs
  = [ minimum crabs .. maximum crabs ]


-- | The cost to move a Crab to a position.
type MovementCost
  = Int -> Crab -> Fuel

-- | Cost to move a crab to a position where each step costs 1 fuel.
constantFuelCost
  :: MovementCost
constantFuelCost position crab
  = abs (crab - position)

-- | Cost to move a crab to a position where each step takes one more than the
-- last.
increasingFuelCost
  :: MovementCost
increasingFuelCost position crab
  = sum
  . take (abs (crab - position))
  $ [1,2..]



{- Parsing -}

parseCrabs
  :: [Text]
  -> AOCM Crabs
parseCrabs
  = parse crabs
  . (!! 0)

crabs
  :: Parser Crabs
crabs
  = sepBy1 crab comma

crab
  :: Parser Crab
crab
  = digits

