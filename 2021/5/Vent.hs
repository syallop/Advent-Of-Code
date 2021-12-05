{-# language OverloadedStrings #-}
{-# language MultiWayIf #-}
{-# language ParallelListComp #-}
module Vent
  ( Vent ()
  , parseVents

  , Dangers
  , Danger
  , countDangerousVents
  )
  where

import AOC

import Data.Text (Text)
import Data.Maybe (catMaybes)

import Point
import Orientation
import Histogram

-- | A Vent is a line from one point to another.
data Vent = Vent
  { _start :: Point
  , _end   :: Point
  }

type Danger = Orientation -> Bool
type Dangers = [Danger]

countDangerousVents
  :: Dangers
  -> [Vent]
  -> Int
countDangerousVents dangers
  = pointsGreaterThanTwo
  . mkHistogram
  . mconcat
  . catMaybes
  . fmap points
  . filter (allDangerous dangers)
  . fmap (\(Vent start end) -> orientation start end)

  where
    allDangerous
      :: [Orientation -> Bool]
      -> (Orientation -> Bool)
    allDangerous dangerous = \o -> or . fmap ($ o) $ dangerous

{- Parsing -}

parseVents
  :: [Text]
  -> AOCM [Vent]
parseVents
  = mapM parseVent

parseVent
  :: Text
  -> AOCM Vent
parseVent
  = parse vent

vent
  :: Parser Vent
vent
  = Vent
 <$> point
 <*> (textIs " -> " *> point)

