{-# LANGUAGE OverloadedStrings, DeriveAnyClass, MultiWayIf, TypeFamilies, FlexibleInstances, TypeSynonymInstances #-}
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

import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V

import Data.Functor.Compose (Compose(..))
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment)

import Control.Comonad (Comonad(..))

solution = Solution
  { _parse   = parseSeatLayout
  , _partOne = partOne
  , _partTwo = partTwo
  }

-- | Iterate rules regarding occupied adjacent seats over the layout until the
-- seating arangement stabilizes. The number of occupied seats at that point is
-- the answer.
partOne :: SeatLayout -> AOCM Int
partOne seatLayout = do
  let grid           = mkGrid seatLayout
      gridIterations = iterate (step ruleA) grid

  case filter (uncurry (==)) . zip gridIterations . tail $ gridIterations of
    []
      -> Left $ err "Could not find a stabilized seating arangement"

    ((grid,_identicalGrid):_)
      -> pure . countOccupied $ grid

  where
    ruleA :: Rule
    ruleA grid = case extract grid of
      -- If a seat is empty and so are all of the adjacent seats, it becomes
      -- occupied.
      Unoccupied
        | adjacentOccupiedSeats == 0
        -> Occupied

        | otherwise
        -> Unoccupied

      -- If a seat is occupied and four or more of the adjacent seats are occupied,
      -- it is vacated.
      Occupied
        | 4 <= adjacentOccupiedSeats
        -> Unoccupied

        | otherwise
        -> Occupied

      -- Nobody sits on the floor
      Floor
        -> Floor

      where
        adjacentSeats = experiment (at adjacentCoords) grid
        adjacentOccupiedSeats = length . filter (== Occupied) $ adjacentSeats

-- | Iterate rules regarding visible seats over the layout until the seating
-- arangement stabilizes. The number of occupied seats at that point is the
-- answer.
partTwo :: SeatLayout -> AOCM Int
partTwo seatLayout = do
  let grid           = mkGrid seatLayout
      gridIterations = iterate (step ruleB) grid

  case filter (uncurry (==)) . zip gridIterations . tail $ gridIterations of
    []
      -> Left $ err "Could not find a stabilized seating arangement"

    ((grid,_identicalGrid):_)
      -> pure . countOccupied $ grid
  where
    ruleB :: Rule
    ruleB grid = case extract grid of
      -- If a seat is empty and either:
      -- - So are all of the adjacent seats
      -- - Or are so are all of the visible seats
      -- Then it becomes occupied.
      Unoccupied
        | visibleOccupiedSeats == 0
        -> Occupied

        | otherwise
        -> Unoccupied

      -- If a seat is occupied and either:
      -- - Four or more of the adjacent seats are occupied
      -- - Or five or more visible seats are occupied
      -- Then it is vacated.
      Occupied
        | 5 <= visibleOccupiedSeats
        -> Unoccupied

        | otherwise
        -> Occupied

      -- Nobody sits on the floor!
      Floor
        -> Floor

      where
        seatsInDirection :: (Int, Int) -> [Seat]
        seatsInDirection dir = experiment (coordsInDirection dir) grid

        visibleSeats :: [Seat]
        visibleSeats = catMaybes [find (/= Floor) (seatsInDirection d) | d <- adjacentCoords]

        visibleOccupiedSeats :: Int
        visibleOccupiedSeats = length . filter (== Occupied) $ visibleSeats



-- | A SeatLayout is a collection of SeatLines, that each represent a row of
-- Seats.
type SeatLayout = Vector SeatLine

-- | A SeatLine is a row of Seats.
type SeatLine = Vector Seat

-- | A possible Seat is either occupied/ unoccupied or floor space.
data Seat
  = Occupied
  | Unoccupied
  | Floor
  deriving Eq

-- | Parse the input SeatLayout.
parseSeatLayout :: [Text] -> AOCM SeatLayout
parseSeatLayout = parse seatLayout . Text.unlines

-- | Newline separated rows of Seats.
seatLayout :: Parser SeatLayout
seatLayout = V.fromList <$> many1 (seatLine <* newline)

-- | Many Seats.
seatLine :: Parser SeatLine
seatLine = V.fromList <$> many1 seat

-- | A Seat's state is identified by one of three characters.
seat :: Parser Seat
seat = alternatives
  [ charIs '.' *> pure Floor
  , charIs 'L' *> pure Unoccupied
  , charIs '#' *> pure Occupied
  ]

main :: IO ()
main = do
  let inputFile   = "2020/11/seatLayout"
  input <- readLinesFromFile inputFile
  execSolution input solution

{- Hacks/ hardcoding -}

-- Hardcode the width and height of our specific problem input and use it to
-- prevent functions from going out of bounds without having to thread the
-- values through everywhere.
--
-- TODO: It would be more flexible to thread these values around or use
-- datastructures that know their capacity.
gridWidth :: Int
gridWidth = 95

gridHeight :: Int
gridHeight = 97

-- | Are two indexes within our hardcoded bounds?
withinBounds :: (Int,Int) -> Bool
withinBounds (x,y) = and
  [ 0 <= x
  , x < gridWidth
  , 0 <= y
  , y < gridHeight
  ]

-- Size of backing vectors to generate. This is used for both rows and columns
-- and so should be at least the maximum of the two.
fixedMaximumVectorSize :: Int
fixedMaximumVectorSize = 100

-- Pretend Vectors are never bigger than the fixed size so that we can index and
-- tabulate them.
--
-- When indexes are used in the Grid, we must make sure they obey the actual
-- width and height limits.
instance Distributive Vector where
  distribute = distributeRep
instance Representable Vector where
  type Rep Vector = Int
  index v i = v ! i
  tabulate  = V.generate fixedMaximumVectorSize


{- Core Grid abstraction -}

-- | A coordinate index into the Grid.
type Coord = (Int, Int)

-- | A Grid is a 'Store' Co-Monad over the SeatLayout that makes it easy to
-- apply a rule across the seats as a whole.
type Grid = Store (Compose Vector Vector) Seat
instance Eq Grid where
  (StoreT (Identity (Compose g0)) _) == (StoreT (Identity (Compose g1)) _) = g0 == g1

-- | Initialise a 'Grid' that can be iterated against 'Rule's from an initial
-- 'SeatLayout'.
--
-- Attempting to travel out of bounds will generate 'Floor' cells.
-- In general, you wont want to do this but its okay for this problem?
mkGrid :: SeatLayout -> Grid
mkGrid layout = store
  (\(x,y) -> case layout !? y >>= (!? x) of
               -- TODO: Define the Store with an AOCM stack so we can error
               -- here.
               Nothing
                 -> Floor

               Just seat
                 -> seat
  )
  (0,0)

-- | A Rule produces an updated Seat given context of all of the other seats in
-- the Grid.
type Rule = Grid -> Seat

-- | Apply a single iteration of the rules to the Grid.
step :: Rule -> Grid -> Grid
step = extend

-- | How many occupied Seats are there in the grid?
countOccupied :: Grid -> Int
countOccupied (StoreT (Identity (Compose g)) _) = V.length . V.filter (== Occupied) . V.concat . V.toList $ g

-- | Relative co-ordinates which describe adjacent seats like:
--
-- X X X
-- X   X
-- X X X
adjacentCoords :: [Coord]
adjacentCoords =
  [(x, y) | x <- [-1, 0, 1]
          , y <- [-1, 0, 1]
          , (x, y) /= (0, 0)
  ]

-- | Attempt to generate every coordinate in a direction - that is within
-- bounds.
coordsInDirection :: (Int,Int) -> Coord ->  [Coord]
coordsInDirection (xDelta, yDelta) (initialX, initialY) = takeWhile withinBounds $
  [ (initialX + xDelta*i, initialY + yDelta*i)
  | i <- [1..]
  ]

-- | Compute multiple relative co-ordinates, restricted within some range.
at :: [Coord] -> Coord -> [Coord]
at coords (originX, originY) = filter withinBounds
                             . map (\(x,y) -> (x+originX,y+originY))
                             $ coords

