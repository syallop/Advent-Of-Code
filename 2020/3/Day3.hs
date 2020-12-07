{-# LANGUAGE OverloadedStrings #-}
{-
TODO:
Could re-implement with full parser.
-}
module Main where

import AOC

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Set (Set)
import qualified Data.Set as Set

solution :: Solution Int Int
solution = Solution
  { _parse   = parseTobogganMap
  , _partOne = partOne
  , _partTwo = partTwo
  }

main :: IO ()
main = do
  let inputFile   = "2020/3/map"

  input <- readLinesFromFile inputFile
  execSolution input solution

-- | Calculate how many trees are hit in a particular trajectory.
partOne :: TobogganMap -> AOCM Int
partOne tobogganMap = calculateHits tobogganMap (3,1)

-- | Calculate the product of how many trees are hit in multiple trajectories.
partTwo :: TobogganMap -> AOCM Int
partTwo tobogganMap = do
  hits <- mapM (calculateHits tobogganMap)
               [ (1,1)
               , (3,1)
               , (5,1)
               , (7,1)
               , (1,2)]
  pure . product $ hits

-- | A Tree either exists or doesnt
type Tree = Bool

-- | A horizontal line in a map describes whether there are trees or not.
newtype TobogganMapLine = TobogganMapLine {_unTobogganMapLine :: [Tree]}

-- | Parse a single horizontal line of a toboggan map:
-- ....#...#.,
--
-- Where '.' indicates no tree, and '#' indicates a tree.
parseTobogganMapLine :: Text -> AOCM TobogganMapLine
parseTobogganMapLine txt = fmap (TobogganMapLine . reverse) . parseTobogganMapLine' [] $ txt
  where
    parseTobogganMapLine' :: [Bool] -> Text -> AOCM [Bool]
    parseTobogganMapLine' acc txt = case Text.uncons txt of
      Nothing
        -> Right acc

      Just (c,txt')
        | c == '.'
         -> parseTobogganMapLine' (False:acc) txt'
        | c == '#'
         -> parseTobogganMapLine' (True:acc)  txt'

        | otherwise
         -> Left $ err "Toboggan line contains an unrecognised character"
                 `withAttributes` [("unrecognised", Text.pack $ show c)]

-- | A 'TobogganMap' is a collection of TobogganMapLines where every line must
-- have the same length so that it can repeat horizontally.
parseTobogganMap :: [Text] -> AOCM TobogganMap
parseTobogganMap input = mapM parseTobogganMapLine input >>= mkTobogganMap

-- | A TobogganMap can be compiled from a list of TobogganMapLine's with
-- 'mkTobogganMap' to represent a horizontally repeating map.
data TobogganMap = TobogganMap
  { _repeatingWidth :: Int           -- ^ The width at which the map repeats horizontally.
  , _height         :: Int           -- ^ The total height at which the map ends vertically.
  , _treeLocations  :: Set (Int,Int) -- ^ Co-ordinates from (0,0) top-left where trees are present. A Set makes lookup faster than traversing a list of Bools and likely saves space.
  }

-- | A TobogganMap is a horizontally repeating list of MapLines.
mkTobogganMap :: [TobogganMapLine] -> AOCM TobogganMap
mkTobogganMap lines = do
  -- Each line must have the same width to repeat sensibly.
  width <- allSameLength . map _unTobogganMapLine $ lines

  -- Accumulate a Set of positions where trees are present. We can indicate
  -- spaces by their lack of presence in the set.
  let (_lastIx, treeLocations) = foldl (\(accY, accSet) (TobogganMapLine trees)
                                         -> let nextY = accY+1
                                                (_lastIx, nextSet) = foldl (\(accX, accSet) hasTree
                                                                             -> let nextX   = accX+1
                                                                                    nextSet = if hasTree
                                                                                                then Set.insert (accX,accY) accSet
                                                                                                else accSet
                                                                                 in (nextX, nextSet)
                                                                           )
                                                                           (0, accSet)
                                                                           trees

                                             in (nextY, nextSet)
                                        )
                                        (0, Set.empty)
                                        lines

  pure $ TobogganMap width (length lines) treeLocations

-- | In a list of lists, all must have the same length.
allSameLength :: [[a]] -> AOCM Int
allSameLength xs = case map length xs of
  []
    -> Right 0

  [length]
    -> Right length

  (length:lengths)
    | all (== length) lengths
     -> Right length

    | otherwise
     -> Left $ err "Not all sublists had the expected length"
             `withAttributes` [("expectedLength", Text.pack $ show length)]

-- | Calculate hits by:
-- - Starting at the top left (0,0)
-- - Checking whether the co-ordinate contains a tree
-- - Moving by the x and y delta that describes the trajectory
--
-- When the y limit is reached, the journey is over.
-- There is no x limit, as the map repeats horizontally.
calculateHits :: TobogganMap -> (Int, Int) -> AOCM Int
calculateHits (TobogganMap width height trees) (deltaX, deltaY)
  | deltaY < 1 = Left $ err "If we never move downwards the journey will never end!"
  | otherwise  = pure $ calculateHits' (0,0) 0
  where
    calculateHits' :: (Int,Int) -> Int -> Int
    calculateHits' (x,y) accHits
      | height <= y
       = accHits

      | otherwise
       = calculateHits' (x+deltaX, y+deltaY) $ if Set.member (x `mod` width, y) trees then accHits+1 else accHits


