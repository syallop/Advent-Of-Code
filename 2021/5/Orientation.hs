module Orientation
  ( Orientation ()
  , orientation
  , points

  , isHorizontal
  , isVertical
  , isUpDiagonal
  , isDownDiagonal
  , isDiagonal
  , isComplex
  )
  where

import Point

-- | A line with specific orientations
data Orientation
  = Horizontal Y X X         -- ^ Horizontal left-to-right
  | Vertical   X Y Y         -- ^ Vertical bottom-to-top

  | UpDiagonal   Point Point -- ^ Diagonal bottom-left to top-right
  | DownDiagonal Point Point -- ^ Diagonal top-left to bottom-right

  | Complex                  -- ^ Another kind of diagonal we don't care about


{- Predicates on the orientation -}

isHorizontal
  :: Orientation
  -> Bool
isHorizontal o = case o of
  Horizontal _ _ _
    -> True
  _
    -> False

isVertical
  :: Orientation
  -> Bool
isVertical o = case o of
  Vertical _ _ _
    -> True
  _
    -> False

isUpDiagonal
  :: Orientation
  -> Bool
isUpDiagonal o = case o of
  UpDiagonal _ _
    -> True
  _
    -> False

isDownDiagonal
  :: Orientation
  -> Bool
isDownDiagonal o = case o of
  DownDiagonal _ _
    -> True
  _
    -> False

isDiagonal
  :: Orientation
  -> Bool
isDiagonal o = isUpDiagonal o || isDownDiagonal o

isComplex
  :: Orientation
  -> Bool
isComplex o = case o of
  Complex
    -> True
  _
    -> False


{- Create and consume -}

-- | Given two points that form a line, classify it's orientation.
orientation
  :: Point
  -> Point
  -> Orientation
orientation
  (Point x0 y0)
  (Point x1 y1)
  | horizontal   = Horizontal y0 (min x0 x1) (max x0 x1)
  | vertical     = Vertical   x0 (min y0 y1) (max y0 y1)

  | complex      = Complex

  | upDiagonal   = UpDiagonal   (Point (min x0 x1) (min y0 y1)) (Point (max x0 x1) (max y0 y1))
  | otherwise    = DownDiagonal (Point (min x0 x1) (max y0 y1)) (Point (max x0 x1) (min y0 y1))

  where
  horizontal = y0 == y1
  vertical   = x0 == x1

  complex    = abs    (x0 - x1) /= abs    (y0 - y1)
  upDiagonal = signum (x1 - x0) == signum (y1 - y0)

-- | Given a non-complex line orientation, return its points.
points
  :: Orientation
  -> Maybe [Point]
points o = case o of
  Horizontal y x0 x1
    -> Just $ horizontalPoints y x0 x1

  Vertical x y0 y1
    -> Just $ verticalPoints x y0 y1

  UpDiagonal p0 p1
    -> Just $ upDiagonalPoints p0 p1

  DownDiagonal p0 p1
    -> Just $ downDiagonalPoints p0 p1

  Complex
    -> Nothing

