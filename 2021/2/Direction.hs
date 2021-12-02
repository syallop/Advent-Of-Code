{-# language OverloadedStrings #-}
module Direction
  ( Direction (..)
  , direction
  )
  where

import AOC

data Direction
  = Forward
  | Down
  | Up

direction
  :: Parser Direction
direction
  = alternatives
      [ textIs "forward" *> pure Forward
      , textIs "down"    *> pure Down
      , textIs "up"      *> pure Up
      ]

