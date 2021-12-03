module Bit
  ( Bit (Zero, One)
  , bit

  , invert
  , toDecimal
  )
  where

import AOC

data Bit
  = Zero
  | One
  deriving (Show, Eq)

bit
  :: Parser Bit
bit
  = alternatives
      [ charIs '0' *> pure Zero
      , charIs '1' *> pure One
      ]

invert
  :: Bit
  -> Bit
invert b = case b of
  Zero
    -> One

  One
    -> Zero

toDecimal :: [Bit] -> Int
toDecimal = sum
          . fmap (\(oneValue,bit) -> case bit of
                     Zero
                       -> 0

                     One
                       -> oneValue
                 )
          . zip (fmap (2 ^) [0..])
          . reverse

