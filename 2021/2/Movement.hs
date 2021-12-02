module Movement
  ( Move (start, act, value)
  , move
  )
  where

import AOC

import Position
import Command
import Direction

class Move p where
  start :: p
  act   :: Direction -> Int -> p -> p
  value :: p -> Int

move
  :: Move p
  => p
  -> Command
  -> p
move position cmd = act (commandDirection cmd) (commandAmount cmd) position

instance Move Position where
  start = initialPosition

  act direction = case direction of
    Forward
      -> moveForward

    Up
      -> moveUp

    Down
      -> moveDown

  value position = horizontal position * depth position

instance Move AimedPosition where
  start = initialAimedPosition

  act direction = case direction of
    Forward
      -> aimForward

    Up
      -> aimUp

    Down
      -> aimDown

  value = value . position
