{-# language PatternSynonyms #-}
{-# language FlexibleInstances #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveAnyClass #-}
{-# language LambdaCase #-}
module SevenSide
  ( SevenSide (..)
  , top
  , topLeft
  , topRight
  , middle
  , bottomLeft
  , bottomRight
  , bottom

  , Power (On, Off)

  , dead

  , poweredOn
  , bothPowered

  , asDigit

  , pattern Zero
  , pattern One
  , pattern Two
  , pattern Three
  , pattern Four
  , pattern Five
  , pattern Six
  , pattern Seven
  , pattern Eight
  , pattern Nine
  )
  where

import Data.Foldable
import Data.Function (on)


-- | A display with seven segments that may individually be on or off.
--
--   ----TS---
--  |        |
--  TL       TR
--  |        |
--  |----M---|
--  |        |
--  BL       BR
--  |        |
--   ---BS---
data SevenSide t = SevenSide
  { _topSide         :: t
  , _topLeftSide     :: t
  , _topRightSide    :: t
  , _middleSide      :: t
  , _bottomLeftSide  :: t
  , _bottomRightSide :: t
  , _bottomSide      :: t
  }
  deriving (Eq, Functor, Foldable)

instance Semigroup t => Semigroup (SevenSide t) where
  (SevenSide tL tlL trL mL blL brL bL)
    <>
    (SevenSide tR tlR trR mR blR brR bR)
      = SevenSide (tL <> tR) (tlL <> tlR) (trL <> trR) (mL <> mR) (blL <> blR) (brL <> brR) (bL <> bR)

instance Monoid t => Monoid (SevenSide t) where
  mempty = SevenSide mempty mempty mempty mempty mempty mempty mempty

instance Show (SevenSide Power) where
  show (SevenSide top topLeft topRight middle bottomLeft bottomRight bottom) = mconcat
    [ if top        == On then " ---- " else "      ", "\n"
    , if topLeft    == On then "."      else " ", "    ", if topRight    == On then "." else " ", "\n"
    , if topLeft    == On then "."      else " ", "    ", if topRight    == On then "." else " ", "\n"
    , if middle     == On then " ---- " else "      ", "\n"
    , if bottomLeft == On then "."      else " ", "    ", if bottomRight == On then "." else " ", "\n"
    , if bottomLeft == On then "."      else " ", "    ", if bottomRight == On then "." else " ", "\n"
    , if bottom     == On then " ---- " else "      "
    ]

top
  :: SevenSide t
  -> t
top
  = _topSide

topLeft
  :: SevenSide t
  -> t
topLeft
  = _topLeftSide

topRight
  :: SevenSide t
  -> t
topRight
  = _topRightSide

middle
  :: SevenSide t
  -> t
middle
  = _middleSide

bottomLeft
  :: SevenSide t
  -> t
bottomLeft
  = _bottomLeftSide

bottomRight
  :: SevenSide t
  -> t
bottomRight
  = _bottomRightSide

bottom
  :: SevenSide t
  -> t
bottom
  = _bottomSide


-- | A segment may be powered on or off.
data Power
  = On
  | Off
  deriving Eq

instance Semigroup Power where
  Off <> On  = On
  On  <> Off = On
  On  <> On  = On
  Off <> Off = Off

instance Monoid Power where
  mempty = Off

dead
  :: SevenSide Power
dead = SevenSide Off Off Off Off Off Off Off


-- | Count the number of segments powered on.
poweredOn
  :: SevenSide Power
  -> Int
poweredOn = foldl' (\count power -> if power == On then count + 1 else count) 0

-- | Keep segments powered which are powered in both segments.
bothPowered
  :: SevenSide Power
  -> SevenSide Power
  -> SevenSide Power
bothPowered l r = SevenSide
  { _topSide         = if on (on (&&) (== On)) _topSide l r then On else Off
  , _topLeftSide     = if on (on (&&) (== On)) _topLeftSide l r then On else Off
  , _topRightSide    = if on (on (&&) (== On)) _topRightSide l r then On else Off
  , _middleSide      = if on (on (&&) (== On)) _middleSide l r then On else Off
  , _bottomLeftSide  = if on (on (&&) (== On)) _bottomLeftSide l r then On else Off
  , _bottomRightSide = if on (on (&&) (== On)) _bottomRightSide l r then On else Off
  , _bottomSide      = if on (on (&&) (== On)) _bottomSide l r then On else Off
  }

asDigit
  :: SevenSide Power
  -> Int
asDigit = \case
  Zero
    -> 0
  One
    -> 1
  Two
    -> 2
  Three
    -> 3
  Four
    -> 4
  Five
    -> 5
  Six
    -> 6
  Seven
    -> 7
  Eight
    -> 8
  Nine
    -> 9
  _ -> error "Not a digit"

--   ----TS---
--  |        |
--  TL       TR
--  |        |
--  |--------|
--  |        |
--  BL       BR
--  |        |
--   ---BS---
pattern Zero :: SevenSide Power
pattern Zero = SevenSide
  { _topSide         = On
  , _topLeftSide     = On
  , _topRightSide    = On
  , _middleSide      = Off
  , _bottomLeftSide  = On
  , _bottomRightSide = On
  , _bottomSide      = On
  }

--   ---------
--  |        |
--  |        TR
--  |        |
--  |--------|
--  |        |
--  |       BR
--  |        |
--   --------
pattern One :: SevenSide Power
pattern One = SevenSide
  { _topSide         = Off
  , _topLeftSide     = Off
  , _topRightSide    = On
  , _middleSide      = Off
  , _bottomLeftSide  = Off
  , _bottomRightSide = On
  , _bottomSide      = Off
  }

--   ----TS---
--  |        |
--  |        TR
--  |        |
--  |---M----|
--  |        |
--  BL       |
--  |        |
--   ---BS---
pattern Two :: SevenSide Power
pattern Two = SevenSide
  { _topSide         = On
  , _topLeftSide     = Off
  , _topRightSide    = On
  , _middleSide      = On
  , _bottomLeftSide  = On
  , _bottomRightSide = Off
  , _bottomSide      = On
  }

--   ----TS---
--  |        |
--  |        TR
--  |        |
--  |---M----|
--  |        |
--  |       BR
--  |        |
--   ---BS---
pattern Three :: SevenSide Power
pattern Three = SevenSide
  { _topSide         = On
  , _topLeftSide     = Off
  , _topRightSide    = On
  , _middleSide      = On
  , _bottomLeftSide  = Off
  , _bottomRightSide = On
  , _bottomSide      = On
  }

--   ---------
--  |        |
--  TL       TR
--  |        |
--  |---M----|
--  |        |
--  |       BR
--  |        |
--   --------
pattern Four :: SevenSide Power
pattern Four = SevenSide
  { _topSide         = Off
  , _topLeftSide     = On
  , _topRightSide    = On
  , _middleSide      = On
  , _bottomLeftSide  = Off
  , _bottomRightSide = On
  , _bottomSide      = Off
  }

--   ----TS---
--  |        |
--  TL       |
--  |        |
--  |---M----|
--  |        |
--  |        BR
--  |        |
--   ---BS---
pattern Five :: SevenSide Power
pattern Five = SevenSide
  { _topSide         = On
  , _topLeftSide     = On
  , _topRightSide    = Off
  , _middleSide      = On
  , _bottomLeftSide  = Off
  , _bottomRightSide = On
  , _bottomSide      = On
  }

--   ----TS---
--  |        |
--  TL       |
--  |        |
--  |---M----|
--  |        |
--  BL       BR
--  |        |
--   ---BS---
pattern Six :: SevenSide Power
pattern Six = SevenSide
  { _topSide         = On
  , _topLeftSide     = On
  , _topRightSide    = Off
  , _middleSide      = On
  , _bottomLeftSide  = On
  , _bottomRightSide = On
  , _bottomSide      = On
  }

--   ----TS---
--  |        |
--  |       TR
--  |        |
--  |--------|
--  |        |
--  |       BR
--  |        |
--   --------
pattern Seven :: SevenSide Power
pattern Seven = SevenSide
  { _topSide         = On
  , _topLeftSide     = Off
  , _topRightSide    = On
  , _middleSide      = Off
  , _bottomLeftSide  = Off
  , _bottomRightSide = On
  , _bottomSide      = Off
  }

--   ----TS---
--  |        |
--  TL       TR
--  |        |
--  |---M----|
--  |        |
--  BL       BR
--  |        |
--   ---BS---
pattern Eight :: SevenSide Power
pattern Eight = SevenSide
  { _topSide         = On
  , _topLeftSide     = On
  , _topRightSide    = On
  , _middleSide      = On
  , _bottomLeftSide  = On
  , _bottomRightSide = On
  , _bottomSide      = On
  }

--   ----TS---
--  |        |
--  TL       TR
--  |        |
--  |---M----|
--  |        |
--  |       BR
--  |        |
--   ---BS---
pattern Nine :: SevenSide Power
pattern Nine = SevenSide
  { _topSide         = On
  , _topLeftSide     = On
  , _topRightSide    = On
  , _middleSide      = On
  , _bottomLeftSide  = Off
  , _bottomRightSide = On
  , _bottomSide      = On
  }

