module Point
  ( Point (..)
  , point

  , X
  , Y

  , horizontalPoints
  , verticalPoints
  , upDiagonalPoints
  , downDiagonalPoints
  )
  where

import AOC

-- | A point in 2D space.
data Point = Point
  { _x :: X
  , _y :: Y
  }
  deriving (Eq, Ord)

instance Show Point where
  show (Point x y) = show x <> "," <> show y

type X = Int
type Y = Int

horizontalPoints
  :: Y
  -> X
  -> X
  -> [Point]
horizontalPoints y left right
  = fmap (`Point` y) [left..right]

verticalPoints
  :: X
  -> Y
  -> Y
  -> [Point]
verticalPoints x bottom top
  = fmap (x `Point`) [bottom..top]

upDiagonalPoints
  :: Point
  -> Point
  -> [Point]
upDiagonalPoints (Point x0 y0) (Point x1 y1)
  = fmap (uncurry Point)
  . zip [x0..x1 ]
  $ [y0..y1]

downDiagonalPoints
  :: Point
  -> Point
  -> [Point]
downDiagonalPoints  (Point x0 y0) (Point x1 y1)
  = fmap (uncurry Point)
  . zip [x0..x1]
  $ [y0,y0-1..y1]

point
  :: Parser Point
point
  = Point
 <$> digits
 <*> (charIs ',' *> digits)

