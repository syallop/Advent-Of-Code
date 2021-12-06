{-# language ScopedTypeVariables #-}
module Fish
  ( Fish ()
  , parseFish

  , days
  , population
  )
  where

import AOC

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Foldable (foldl')
import qualified Data.Text as Text

type Count = Int

-- | The state of a fishes reproductive cycle.
data ReproductiveState
  = Reproducing
  | One
  | Two
  | Three
  | Four
  | Five
  | AbleToReproduce
  | Infant
  | Newborn
  deriving (Eq, Ord, Enum)

-- | A collection of Fish.
data Fish = Fish
  { _fishReproductiveState :: Map ReproductiveState Count
  }

-- | Step to the next day of fish population.
nextDay
  :: Fish
  -> Fish
nextDay
  = Fish
 . foldl' (\accMap (st,count) -> case st of
              Reproducing
                -> increaseState Newborn count
                 . increaseState AbleToReproduce count
                 $ accMap

              _
                -> increaseState (pred st) count accMap
          )
          Map.empty
 .  Map.toList
 . _fishReproductiveState

  where
    increaseState :: ReproductiveState -> Count -> Map ReproductiveState Count -> Map ReproductiveState Count
    increaseState = Map.insertWith (+)

-- | Iterate until a number of additional days pass.
days
  :: Int
  -> Fish
  -> Fish
days i
  = head
  . drop i
  . iterate nextDay

-- | Count the current population.
population
  :: Fish
  -> Int
population
  = sum
  . Map.elems
  . _fishReproductiveState

{- Parsing -}

parseFish
  :: [Text]
  -> AOCM Fish
parseFish
  = parse fish
  . (!! 0)

fish
  :: Parser Fish
fish
  = (Fish . foldl' (\m st -> Map.insertWith (+) st 1 m) Map.empty)
 <$> sepBy1 reproductiveState comma

reproductiveState
  :: Parser ReproductiveState
reproductiveState
  = alternatives
    [ charIs '0' *> pure Reproducing
    , charIs '1' *> pure One
    , charIs '2' *> pure Two
    , charIs '3' *> pure Three
    , charIs '4' *> pure Four
    , charIs '5' *> pure Five
    , charIs '6' *> pure AbleToReproduce
    , charIs '7' *> pure Infant
    , charIs '8' *> pure Newborn
    ]

