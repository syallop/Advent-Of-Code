{-# LANGUAGE OverloadedStrings, DeriveAnyClass, MultiWayIf, TypeFamilies, FlexibleInstances, TypeSynonymInstances #-}
module Main where

import AOC

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (sort, sortBy)
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
  { _parse   = parseBusSchedule
  , _partOne = partOne
  , _partTwo = pure . partTwo
  }

main :: IO ()
main = do
  let inputFile   = "2020/13/busSchedule"
  input <- readLinesFromFile inputFile
  execSolution input solution

-- | Find the first time after the earliest departure time in which there is a
-- single bus.
--
-- The result is that bus id multiplied by the waiting time.
partOne :: BusSchedule -> AOCM Int
partOne (BusSchedule earliestDepartureTime schedule) = do
  let busIDs :: [BusID]
      busIDs = knownIDs schedule

      allArrivalTimes :: [(BusID, [Time])]
      allArrivalTimes = map (\busID -> (busID, arrivalTimes busID)) busIDs

      arrivals :: Arrivals
      arrivals = mkArrivals allArrivalTimes

      earliestPossibleArrivals :: Arrivals
      earliestPossibleArrivals = waitUntil earliestDepartureTime arrivals

  (earliestTime, earliestBusses) <- peekNext earliestPossibleArrivals
  case Set.toList earliestBusses of
    []
      -> Left $ err "No busses arrive after we get to the stop"

    [BusID busID]
      -> Right $ busID * (earliestTime - earliestDepartureTime)

    _
      -> Left $ err "More than one bus arrives at the earliest time"

-- | Find the earliest possible time when each bus in the schedule arrives,
-- sequentially.
--
-- Solve with mysterious maths that relies upon the input coincidentally having
-- some kind of prime relationships.
partTwo :: BusSchedule -> Int
partTwo (BusSchedule _ schedule) = chineseRemainderTheorem (indexes 0 schedule)
  where
    -- | Gather positional indexes of each known bus.
    indexes :: Int -> Schedule -> [(Int, Int)]
    indexes _ [] = []
    indexes n (UnknownBusID : bs)  = indexes (n+1) bs
    indexes n ((KnownBusID (BusID id)) : bs) = (n, id) : indexes (n+1) bs

    -- Takes pairs of indexes and repeating periods and computes the earliest
    -- index where the given indexes align sequentially, allowing for gaps when
    -- the index was not required.
    --
    -- How does it work? Maths magic.
    --
    -- Requires some prime-like quality on the numbers, which happens to be
    -- true.
    chineseRemainderTheorem :: [(Int, Int)] -> Int
    chineseRemainderTheorem buses =
      let productOfBusNumbers :: Int
          productOfBusNumbers = product . map snd $ buses

          magic :: Int -> Int -> Int
          magic timeOffset busNumber =
            let modulus = productOfBusNumbers `div` busNumber
             in (busNumber - timeOffset) * (modulus * (modulus `invMod` busNumber))

          magics :: [Int]
          magics = map (uncurry magic) buses

       in sum magics `mod` productOfBusNumbers

    invMod :: Int -> Int -> Int
    invMod m n = let (_, x, _) = extendedEuclid m n in x

    extendedEuclid :: Int -> Int -> (Int, Int, Int)
    extendedEuclid a 0 = (a, 1, 0)
    extendedEuclid a b = let (d', x', y') = extendedEuclid b (a `mod` b)
                          in (d', y', x' - (a `div` b) * y')

-- | The upcoming bus schedule is useful given:
-- - The earliest time we can depart.
-- - The schedule of the busses themselves.
data BusSchedule = BusSchedule
  { _earliestDepartureTime :: Int
  , _schedule              :: Schedule
  }
  deriving Show

-- | A Schedule is an ordered list of possible Bus IDs.
type Schedule = [PossibleBusID]

-- | Either a known bus or unknown/ no bus.
data PossibleBusID
  = KnownBusID BusID
  | UnknownBusID
  deriving Show

-- | The ID that uniquely identifies a bus. The ID _may_ also determine the
-- period in which it arrives at stops.
newtype BusID = BusID {_unBusID :: Int}
  deriving (Show, Eq, Ord)

-- | Parse a bus schedule.
parseBusSchedule :: [Text] -> AOCM BusSchedule
parseBusSchedule = parse busSchedule . Text.unlines

-- | The first line is the earliest time we can leave, the second line is the
-- schedule.
busSchedule :: Parser BusSchedule
busSchedule = BusSchedule <$> (digits <* newline) <*> (schedule <* newline)

-- | A schedule is comma separated.
schedule :: Parser Schedule
schedule = sepBy1 possibleBusID (charIs ',')

-- | Either a known number or unknown.
possibleBusID :: Parser PossibleBusID
possibleBusID = alternatives
  [ charIs 'x' *> pure UnknownBusID
  , KnownBusID <$> busID
  ]

-- | A known bus ID.
busID :: Parser BusID
busID = BusID <$> digits

-- | Extract the known bus IDs from a schedule.
knownIDs :: Schedule -> [BusID]
knownIDs [] = []
knownIDs (UnknownBusID : s) = knownIDs s
knownIDs (KnownBusID i : s) = i : knownIDs s

-- | An integer timestamp when busses may arrive.
type Time = Int

-- | Assume that:
-- - A Bus's ID is the period it arrives
-- - It first arrives at 0
--
-- Produce a stream of times the bus arrives (I.E. the result is infinite)
arrivalTimes :: BusID -> [Time]
arrivalTimes (BusID i) = [0, i ..]


-- | Sort the collection of busses and their (potentially infinite) arrival
-- times by their next arrival time only.
sortByNextDue :: [(BusID, [Time])] -> [(BusID, [Time])]
sortByNextDue = sortBy (\(_id0, times0) (_id1, times1) -> case (times0, times1) of
  ([],[])
    -> EQ

  (_,[])
    -> GT

  ([],_)
    -> LT

  ((t0:_), (t1:_))
    -> compare t0 t1)


-- | Arrivals can be consulted for:
-- - The Busses arriving in the next period
-- - The next time period
--
-- For an infinite schedule.
newtype Arrivals = Arrivals {_arrivals :: [(Time, Set BusID)]}

-- | Given a list of busses arrival times that:
-- - May be infinite
-- - Must be sorted
--
-- Produce an ordered list describing which busses will arrive at any time.
-- Time's where no busses arrive are not included.
-- If the input lists are infinite (as anticipated) so will this result.
mkArrivals :: [(BusID, [Time])] -> Arrivals
mkArrivals = Arrivals . arrivals
  where
    -- Algorithm:
    -- - Sort by the next arrival time
    -- - Fold over input:
    --   - Accumulate all busses that arrive at the next time (as there _could_ be many)
    --   - Accumulate the remaining busses, with any next arrivals removed
    -- - Yield the next arriving busses
    --   - Cons'd to the merged remainder
    arrivals :: [(BusID, [Time])] -> [(Time, Set BusID)]
    arrivals xs =
      let bussesByNextDue :: [(BusID, [Time])]
          bussesByNextDue = sortByNextDue xs

          nextTime :: Time
          nextTime = case bussesByNextDue of
            []
              -> error . show $ err "No busses remaining in schedule"

            ((_busID,times):_)
              -> case times of
                   []
                     -> error . show $ err $ "No times for bus we expected next"

                   (t:_)
                     -> t

          (nextDue, rest) = foldl (\(accNextBusses,accAllBusses) (busID, busArrivals)
                                    -> case busArrivals of
                                         (thisBussesNextArrival:thisBussesTrailingArrivals)
                                           -- This bus is arriving next. Record that,
                                           -- and remove from it's upcoming schedule.
                                           | thisBussesNextArrival == nextTime
                                           -> (Set.insert busID accNextBusses, (busID,thisBussesTrailingArrivals):accAllBusses)

                                           -- This bus is not arriving next. No
                                           -- change.
                                           | otherwise
                                           -> (accNextBusses, (busID, busArrivals):accAllBusses)

                                         -- This bus has no further arrivals
                                         -- scheduled (which isn't expected due to
                                         -- the inifinite schedule, but okay)
                                         []
                                           -> (accNextBusses, accAllBusses)
                                  )
                                  (Set.empty, [])
                                  bussesByNextDue

      in (nextTime, nextDue) : arrivals rest

-- | Forget all of the Arrivals that are earlier than the provided time.
-- The next busses will arrive at an equal or greater time.
waitUntil :: Time -> Arrivals -> Arrivals
waitUntil earliestDepartureTime (Arrivals arrivals) = Arrivals . dropWhile (\(time, arrivingBusses) -> time < earliestDepartureTime) $ arrivals

-- | Peek ahead at the next arriving busses.
peekNext :: Arrivals -> AOCM (Time, Set BusID)
peekNext (Arrivals as) = case as of
  []
    -> Left $ err "No more busses scheduled to arrive"

  (a:_)
    -> Right a

takeNext :: Arrivals -> AOCM ((Time, Set BusID), Arrivals)
takeNext (Arrivals as) = case as of
  []
    -> Left $ err "No more busses scheduled to arrive"

  (a:as)
    -> Right (a, Arrivals as)

takeNextN :: Int -> Arrivals -> AOCM ([(Time, Set BusID)], Arrivals)
takeNextN n (Arrivals as) = Right (take n as, Arrivals $ drop n as)

