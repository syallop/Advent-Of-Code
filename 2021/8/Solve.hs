{-# language MultiWayIf #-}
{-# language OverloadedStrings #-}
module Solve
  ( parseInput
  , decodeOutputs
  )
  where

import SevenSide
import AOC

import Data.Text (Text)
import qualified Data.Text (Text)
import Data.Foldable

decodeOutputs
  :: [([SevenSide Power], [SevenSide Power])]
  -> AOCM [[SevenSide Power]]
decodeOutputs = mapM (uncurry decodeOutput)

decodeOutput
  :: [SevenSide Power]
  -> [SevenSide Power]
  -> AOCM [SevenSide Power]
decodeOutput wires outputs = do
  zero  <- findScrambledZero  wires
  one   <- findScrambledOne   wires
  two   <- findScrambledTwo   wires
  three <- findScrambledThree wires
  four  <- findScrambledFour  wires
  five  <- findScrambledFive  wires
  six   <- findScrambledSix   wires
  seven <- findScrambledSeven wires
  eight <- findScrambledEight wires
  nine  <- findScrambledNine  wires

  mapM (\output -> if | output == zero  -> pure Zero
                      | output == one   -> pure One
                      | output == two   -> pure Two
                      | output == three -> pure Three
                      | output == four  -> pure Four
                      | output == five  -> pure Five
                      | output == six   -> pure Six
                      | output == seven -> pure Seven
                      | output == eight -> pure Eight
                      | output == nine  -> pure Nine
                      | otherwise -> Left . err $ "Scrambled output does not correspond to any understood digit."
       )
       $ outputs

-- Could be more explicit about the dependencies between deducing some of these
-- numbers.
--
-- It doesn't seem impossible that inlining and common-subexpression elimination
-- could optimise away the 'overhead'...

-- | A One is the only digit with two segments powered on.
findScrambledOne
  :: [SevenSide Power]
  -> AOCM (SevenSide Power)
findScrambledOne wires = case filter ((== 2) . poweredOn) wires of
  []
    -> Left . err $ "No size 2 means no ones. Unsolvable."

  (one:_)
    -> pure one

-- | A Seven is the only digit with three segments powered on.
findScrambledSeven
  :: [SevenSide Power]
  -> AOCM (SevenSide Power)
findScrambledSeven wires = case filter ((== 3) . poweredOn) wires of
  []
    -> Left . err $ "No size 3 means no sevens. Unsolvable."

  (seven:_)
    -> pure seven

-- | Four is the only digit with four segments powered on.
findScrambledFour
  :: [SevenSide Power]
  -> AOCM (SevenSide Power)
findScrambledFour wires = case filter ((== 4) . poweredOn) wires of
  []
    -> Left . err $ "No size 4 means no fours. Unsolvable."

  (four:_)
    -> pure four

-- | An eight is the only digit with seven segments powered on.
findScrambledEight
  :: [SevenSide Power]
  -> AOCM (SevenSide Power)
findScrambledEight wires = case filter ((== 7) . poweredOn) wires of
  []
    -> Left . err $ "No size 7 means no eights. Unsolvable."

  (eight:_)
    -> pure eight

-- | A three has five segments, where leaving all segments that coincide with a
-- one On, produces a one.
findScrambledThree
  :: [SevenSide Power]
  -> AOCM (SevenSide Power)
findScrambledThree wires = do
  one <- findScrambledOne wires
  case filter ((== 5) . poweredOn) wires of
    []
      -> Left . err $ "No size 5 means no threes. Unsolvable"

    fiveSegments
      -> case filter ((== one) . bothPowered one) fiveSegments of
           []
             -> Left . err $ "No size 5 can be overlayed with a 1 (&&) to identify a three. Unsolvable"

           (three:_)
             -> pure three

-- | A nine has 6 segments, where leaving all segments that coincide with a four
-- On, produces a four.
findScrambledNine
  :: [SevenSide Power]
  -> AOCM (SevenSide Power)
findScrambledNine wires = do
  four <- findScrambledFour wires
  case filter ((== 6) . poweredOn) wires of
    []
      -> Left . err $ "No size 6 means no nines. Unsolvable"

    sixSegments
      -> case filter ((== four) . bothPowered four) sixSegments of
           []
             -> Left . err $ "No size 6 can be overlayed with a 4 (&&) to identify a nine. Unsolvable"

           (nine:_)
             -> pure nine

-- | A Zero:
-- - Has 6 segments
--
-- Distinguishes between an 8 because:
-- - Does not produce a 4 when overlayed && with a 4 (unlike 8)
-- - Produces a 7 when overlayed && with a 7 (like an 8)
findScrambledZero
  :: [SevenSide Power]
  -> AOCM (SevenSide Power)
findScrambledZero wires = do
  four  <- findScrambledFour  wires
  seven <- findScrambledSeven wires
  case filter ((== 6) . poweredOn) wires of
    []
      -> Left . err $ "No size 6 means no zeros. Unsolvable"

    sixSegments
      -> case filter (\segments -> and [ (/= four)  . bothPowered four  $ segments
                                       , (== seven) . bothPowered seven $ segments
                                       ]) sixSegments of
           []
             -> Left . err $ "No zeros could be found"

           (zero:_)
             -> pure zero


findScrambledSix
  :: [SevenSide Power]
  -> AOCM (SevenSide Power)
findScrambledSix wires = do
  four  <- findScrambledFour  wires
  seven <- findScrambledSeven wires
  case filter ((== 6) . poweredOn) wires of
    []
      -> Left . err $ "No size 6 means no six. Unsolvable"

    sixSegments
      -> case filter (\segments -> and [ (/= four)  . bothPowered four  $ segments
                                       , (/= seven) . bothPowered seven $ segments
                                       ]) sixSegments of
           []
             -> Left . err $ "No six could be found"

           (six:_)
             -> pure six

findScrambledFive
  :: [SevenSide Power]
  -> AOCM (SevenSide Power)
findScrambledFive wires = do
  three <- findScrambledThree wires
  nine  <- findScrambledNine  wires
  case filter ((== 5) . poweredOn) wires of
    []
      -> Left . err $ "No size 5 means no five. Unsolvable"

    fiveSegments
      -> case filter (\segments -> and [ (/= three) segments
                                       , (== segments) . bothPowered nine $ segments
                                       ]) fiveSegments of
           []
             -> Left . err $ "No five could be found"

           (five:_)
             -> pure five

findScrambledTwo
  :: [SevenSide Power]
  -> AOCM (SevenSide Power)
findScrambledTwo wires = do
  three <- findScrambledThree wires
  nine  <- findScrambledNine  wires
  case filter ((== 5) . poweredOn) wires of
    []
      -> Left . err $ "No size 5 means no five. Unsolvable"

    fiveSegments
      -> case filter (\segments -> and [ (/= three) segments
                                       , (/= segments) . bothPowered nine $ segments
                                       ]) fiveSegments of
           []
             -> Left . err $ "No two could be found"

           (two:_)
             -> pure two

{- Parsing -}

parseInput
  :: [Text]
  -> AOCM [([SevenSide Power],[SevenSide Power])]
parseInput = mapM parseLine

parseLine
  :: Text
  -> AOCM ([SevenSide Power], [SevenSide Power])
parseLine = parse line

line
  :: Parser ([SevenSide Power], [SevenSide Power])
line
  = (,)
 <$> sepBy1 sevenSide space
 <*> (textIs " | " *> sepBy1 sevenSide space)

sevenSide
  :: Parser (SevenSide Power)
sevenSide = fmap mconcat . many1 . alternatives $
  [ charIs 'a' *> (pure $ dead{_topSide = On})
  , charIs 'b' *> (pure $ dead{_topLeftSide = On})
  , charIs 'c' *> (pure $ dead{_topRightSide = On})
  , charIs 'd' *> (pure $ dead{_middleSide = On})
  , charIs 'e' *> (pure $ dead{_bottomLeftSide = On})
  , charIs 'f' *> (pure $ dead{_bottomRightSide = On})
  , charIs 'g' *> (pure $ dead{_bottomSide = On})
  ]

