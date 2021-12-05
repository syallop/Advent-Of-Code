module Histogram
  ( Histogram ()
  , mkHistogram
  , pointsGreaterThanTwo
  )
  where

import Point

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Histogram = Histogram
  { _points :: Map Point Int
  }
  deriving Show

mkHistogram
  :: [Point]
  -> Histogram
mkHistogram
  = Histogram
  . foldl (\acc point -> Map.insertWith (+) point 1 acc) Map.empty

pointsGreaterThanTwo
  :: Histogram
  -> Int
pointsGreaterThanTwo
  = length
  . filter (2 <=)
  . Map.elems
  . _points

