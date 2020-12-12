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

import Data.Function (on)

solution = Solution
  { _parse   = parseNavigationPlan
  , _partOne = partOne
  , _partTwo = partTwo
  }

main :: IO ()
main = do
  let inputFile   = "2020/12/navigationPlan"
  input <- readLinesFromFile inputFile
  execSolution input solution

-- | Interpret the navigation plan as moving the ship.
-- The answer is the manhattan distance the ship has moved.
partOne :: NavigationPlan -> AOCM Int
partOne plan = do
  let ship      = mkShip (0,0) East
      finalShip = foldl (\accShip (command,amount) -> followNavigationCommand accShip command amount) ship plan
   in pure $ on manhattanDistance position ship finalShip

-- | Interpret the navigation plan as moving the ship forward, relative to an
-- adjustable waypoint.
-- The answer is the manhattan distance the ship has moved.
partTwo :: NavigationPlan -> AOCM Int
partTwo plan = do
  let ship     = mkShip (0 ,0) East
      waypoint = mkWaypoint (10,1)
      (finalShip, _finalWaypoint) = foldl (\accObjects (command,amount) -> followNavigationCommandWithWaypoint accObjects command amount) (ship,waypoint) plan
   in pure $ on manhattanDistance position ship finalShip

-- | Cardinal directions.
data Direction
  = North
  | East
  | South
  | West
  deriving (Enum, Eq)

-- | Whether to turn left or right (anti-clockwise or clockwise).
data TurnDirection
  = TurnLeft
  | TurnRight

-- | A NavigationCommand specifies directions to move in, directions to turn in
-- and forward movement - without any values indicating by how much to do these
-- things.
data NavigationCommand
  = MoveDirection Direction
  | TurnDirection TurnDirection
  | MoveForward

-- | The amount to which a NavigationCommand should be performed.
type NavigationAmount = Int

-- | A Sequence of instructions for navigation.
type NavigationPlan = [(NavigationCommand, NavigationAmount)]

-- | Parse the navigation plan.
parseNavigationPlan :: [Text] -> AOCM NavigationPlan
parseNavigationPlan = parse navigationPlan . Text.unlines

-- | Newline separated navigation items
navigationPlan :: Parser NavigationPlan
navigationPlan = sepBy1 navigationItem newline <* newline

-- | A command and an amount in which to perform it.
navigationItem :: Parser (NavigationCommand, NavigationAmount)
navigationItem = (,) <$> navigationCommand <*> navigationAmount

-- | One of several alternative commands without values.
navigationCommand :: Parser NavigationCommand
navigationCommand = alternatives . map (\(char,cmd) -> charIs char *> pure cmd) $
  [ ('N', MoveDirection North)
  , ('S', MoveDirection South)
  , ('E', MoveDirection East)
  , ('W', MoveDirection West)

  , ('L', TurnDirection TurnLeft)
  , ('R', TurnDirection TurnRight)
  , ('F', MoveForward)
  ]

-- | An integer amount.
navigationAmount :: Parser NavigationAmount
navigationAmount = digits

-- | A Compass has a current facing direction and can be turned left or right.
newtype Compass = Compass {_directions :: [Direction]}

-- | Construct a Compass correctly.
mkCompass :: Direction -> Compass
mkCompass initialDirection = Compass . dropWhile (/= initialDirection) . cycle $ [North .. West]

-- | The direction the Compass is facing.
currentDirection :: Compass -> Direction
currentDirection (Compass (d:_)) = d
currentDirection _               = error "Compass' should only be constructed with mkCompass"

-- | Turn a compass 90 degrees right.
turnRight :: Compass -> Compass
turnRight (Compass ds) = Compass . drop 1 $ ds

-- | Turn a compass 90 degrees left.
turnLeft :: Compass -> Compass
turnLeft (Compass ds) = Compass . drop 3 $ ds

-- | Turn right by a 90 degree multiple.
turnRightBy :: Int -> Compass -> Compass
turnRightBy degrees = head . drop (quot degrees 90) . iterate turnRight

-- | Turn left by a 90 degree multiple.
turnLeftBy :: Int -> Compass -> Compass
turnLeftBy degrees = head . drop (quot degrees $ 90) . iterate turnLeft

-- | The manhattan distance between two points is the sum of the absolute
-- difference between.
manhattanDistance :: (Int,Int) -> (Int,Int) -> Int
manhattanDistance (x0, y0) (x1, y1) = sum . map abs $ [ x0 - x1, y0 - y1]

-- | A Ship has a position and a direction it is facing.
data Ship = Ship
  { _posX     :: Int
  , _posY     :: Int
  , _compass  :: Compass
  }

-- | Create a ship with an initial position and direction it is facing.
mkShip :: (Int, Int) -> Direction -> Ship
mkShip (x,y) dir = Ship x y $ mkCompass dir

-- | Move a Ship by an x, y delta.
moveBy :: Ship -> (Int, Int) -> Ship
moveBy (Ship x y compass) (xDelta, yDelta) = Ship (x+xDelta) (y+yDelta) compass

-- | Apply an operation over the ships compass, E.G. to turn left or right.
withCompass :: Ship -> (Compass -> Compass) -> Ship
withCompass (Ship x y compass) f = Ship x y (f compass)

-- | The direction the ship is facing.
facingDirection :: Ship -> Direction
facingDirection (Ship _ _ compass) = currentDirection compass

-- | The ships current position.
position :: Ship -> (Int, Int)
position (Ship x y _) = (x, y)

-- | A Ship after following a command.
followNavigationCommand
  :: Ship
  -> NavigationCommand
  -> NavigationAmount
  -> Ship
followNavigationCommand ship cmd amount = case cmd of
  MoveDirection dir
    -> moveBy ship $ case dir of
         North -> (0, amount)
         South -> (0, negate amount)
         East  -> (amount, 0)
         West  -> (negate amount, 0)

  TurnDirection turn
    -> withCompass ship $ case turn of
         TurnLeft  -> turnLeftBy  amount
         TurnRight -> turnRightBy amount

  MoveForward
    -> followNavigationCommand ship (MoveDirection . facingDirection $ ship) amount


-- | A Waypoint is a point relative to another frame of reference, such as a
-- Ship.
data Waypoint = Waypoint
  { _relativeX :: Int
  , _relativeY :: Int
  }

-- | Create a Waypoint, relative to some other object.
mkWaypoint :: (Int,Int) -> Waypoint
mkWaypoint (relativeX, relativeY) = Waypoint relativeX relativeY

-- | Move a Waypoint nearer and farther by some amount.
moveWaypointBy :: Waypoint -> (Int, Int) -> Waypoint
moveWaypointBy (Waypoint x y) (xDelta, yDelta) = Waypoint (x+xDelta) (y+yDelta)

-- | Rotate the position of the Waypoint left 90 degrees.
rotateLeft :: Waypoint -> Waypoint
rotateLeft (Waypoint x y) = Waypoint (negate y) x

-- | Rotate the position of the Waypoint right 90 degrees.
rotateRight :: Waypoint -> Waypoint
rotateRight (Waypoint x y) = Waypoint y (negate x)

-- | Rotate left by the nearest multiple of 90 degrees.
rotateLeftBy :: Waypoint -> Int -> Waypoint
rotateLeftBy waypoint degrees = foldr ($) waypoint $ replicate (degrees `quot` 90) rotateLeft

-- | Rotate right by the nestest multiple of 90 degrees.
rotateRightBy :: Waypoint -> Int -> Waypoint
rotateRightBy waypoint degrees = foldr ($) waypoint $ replicate (degrees `quot` 90) rotateRight

-- | Move a Ship towards the current position of the Waypoint (which by
-- definition remains the same relative position away).
moveTowards :: Waypoint -> Ship -> Ship
moveTowards (Waypoint deltaX deltaY) = flip moveBy (deltaX, deltaY)

-- | A Ship and Waypoint after following a command.
followNavigationCommandWithWaypoint
  :: (Ship, Waypoint)
  -> NavigationCommand
  -> NavigationAmount
  -> (Ship, Waypoint)
followNavigationCommandWithWaypoint (ship, waypoint) cmd amount = case cmd of
  -- Move the waypoint
  MoveDirection dir
    -> ( ship
       , moveWaypointBy waypoint $ case dir of
           North -> (0, amount)
           South -> (0, negate amount)
           East  -> (amount, 0)
           West  -> (negate amount, 0)
       )

  -- Rotate the waypoint
  TurnDirection turn
    -> ( ship
       , (case turn of
           TurnLeft  -> rotateLeftBy
           TurnRight -> rotateRightBy) waypoint amount
       )

  -- Move towards the waypoint (which remains the same relative distance away)
  -- a number of times.
  MoveForward
    -> ( foldr ($) ship $ replicate amount $ moveTowards waypoint
       , waypoint
       )

