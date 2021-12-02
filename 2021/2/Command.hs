module Command
  ( Command ()
  , commandDirection
  , commandAmount

  , Commands
  , parseCommands
  )
  where

import AOC

import Direction

import Data.Text (Text)

data Command = Command
  { _direction :: Direction
  , _amount    :: Int
  }

commandDirection
  :: Command
  -> Direction
commandDirection = _direction

commandAmount
  :: Command
  -> Int
commandAmount = _amount

type Commands = [Command]

parseCommands
  :: [Text]
  -> AOCM Commands
parseCommands
  = mapM parseCommand

parseCommand
  :: Text
  -> AOCM Command
parseCommand
  = parse command

command
  :: Parser Command
command
  = Command
 <$> (direction <* space)
 <*> digits


