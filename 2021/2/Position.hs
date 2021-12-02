module Position
  ( Position ()
  , initialPosition
  , horizontal
  , depth

  , moveForward
  , moveUp
  , moveDown

  , AimedPosition ()
  , initialAimedPosition
  , position

  , aimForward
  , aimUp
  , aimDown
  )
  where

data Position = Position
  { _horizontal :: Int
  , _depth      :: Int
  }

initialPosition
  :: Position
initialPosition
  = Position
      { _horizontal = 0
      , _depth      = 0
      }

horizontal
  :: Position
  -> Int
horizontal = _horizontal

depth
  :: Position
  -> Int
depth = _depth

moveForward
  :: Int
  -> Position
  -> Position
moveForward amount (Position h d) = Position (h + amount) d

moveUp
  :: Int
  -> Position
  -> Position
moveUp amount (Position h d) = Position h (d - amount)

moveDown
  :: Int
  -> Position
  -> Position
moveDown amount (Position h d) = Position h (d + amount)


data AimedPosition = AimedPosition
  { _position :: Position
  , _aim      :: Int
  }

initialAimedPosition
  :: AimedPosition
initialAimedPosition
  = AimedPosition
      { _position = Position 0 0
      , _aim      = 0
      }

position
  :: AimedPosition
  -> Position
position = _position

aimUp
  :: Int
  -> AimedPosition
  -> AimedPosition
aimUp amount (AimedPosition p a) = AimedPosition p (a - amount)

aimDown
  :: Int
  -> AimedPosition
  -> AimedPosition
aimDown amount (AimedPosition p a) = AimedPosition p (a + amount)

aimForward
  :: Int
  -> AimedPosition
  -> AimedPosition
aimForward amount (AimedPosition p a) = AimedPosition (moveDown (a * amount) . moveForward amount $ p) a

