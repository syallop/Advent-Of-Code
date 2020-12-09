{-# LANGUAGE OverloadedStrings, DeriveAnyClass, MultiWayIf #-}
module Main where

import AOC

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (groupBy)
import Control.Applicative
import Control.Monad

import Data.Foldable

import Data.Maybe

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

solution = Solution
  { _parse   = parseBootCode
  , _partOne = partOne
  , _partTwo = partTwo
  }

-- | Halt as soon as an infinite loop is detected and return the current value
-- of the accumulator.
partOne :: BootCode -> AOCM Int
partOne bootCode = do
  (gameConsole, exitSuccess) <- run (mkGameConsole bootCode)
  if exitSuccess
    then Left $ err "Expected to detect infinite loop, but exited sucessfully"
    else Right $ currentAccumulator gameConsole

-- | Run every possible uncorruption (by making a single JMP<->NOP adjustment)
-- and return the accumulator for the only fix that doesnt get stuck in an
-- infinite loop, but makes it to the last instruction.
partTwo :: BootCode -> AOCM Int
partTwo bootCode = do
  let corruptions :: [BootCode]
      corruptions = possibleCorruptions bootCode

  allRuns <- mapM (run . mkGameConsole) $ corruptions
  case filter (\(_,exitSuccess) -> exitSuccess) allRuns of
    []
      -> Left $ err "After attempting to run each possible single-instruction corruption, no gameconsole made it to the end."

    [(g,_)]
      -> Right $ currentAccumulator g

    _ -> Left $ err "After attempting to run each possible single-instruction corruption, more than one gameconsole made it to the end."

main :: IO ()
main = do
  let inputFile   = "2020/8/bootCode"

  input <- readLinesFromFile inputFile
  execSolution input solution

-- | BootCode is an ordered list of Instructions wrapped in a newtype so we can
-- print it more nicely.
newtype BootCode = BootCode [Instruction]
instance Show BootCode where
  show (BootCode [])     = ""
  show (BootCode (i:is)) = show i <> "\n" <> show (BootCode is)

-- | An 'Instruction' is one of three real instructions (with arguments) or one
-- virtual instruction 'EndSection', which a GameConsole may insert to help
-- mark the end of a series of Instructions.
data Instruction
  = Accumulate
    { _addToAccumulator :: Int
    }

  | Jump
    { _offset :: Int
    }

  | NoOperation
    { _noOperationArgument :: Int
    }

  | EndSection
  deriving Eq

instance Show Instruction where
  show i = case i of
    Accumulate arg
      -> "acc " <> show arg

    Jump offset
      -> "jmp " <> show offset

    NoOperation arg
      -> "nop " <> show arg

    EndSection
      -> ""

-- | Parse the input BootCode.
parseBootCode :: [Text] -> AOCM BootCode
parseBootCode = parse bootCode . Text.unlines

-- | BootCode is a series of Instructions, separated by a newline.
bootCode :: Parser BootCode
bootCode = BootCode <$> sepBy1 instruction newline <* newline

-- | Instructions have a few alternatives, each with an 'argument'.
instruction :: Parser Instruction
instruction = alternatives
            . map (\(name,instruction)
                    -> instruction <$> (textIs name *> space *> argument)
                  )
            $ [ ("acc", Accumulate)
              , ("jmp", Jump)
              , ("nop", NoOperation)
              ]

-- | An argument is an integer with a leading sign character.
argument :: Parser Int
argument = sign <*> digits

-- | A Zipper is a data structure which has a 'focus'ed element and allows
-- moving left and right by a single position quickly.
data Zipper a = Zipper
  { _prev  :: [a] -- Ordered nearest to farthest, I.E. the head is one behind.
  , _next  :: [a] -- Ordered nearest to farthest, I.E. the head is the current focus, next is one ahead.
  }

-- | Construct a Zipper with the initial focus at the first element.
mkZipper :: [a] -> Zipper a
mkZipper = Zipper []

-- | Move the focus left a single element.
moveLeft :: Zipper a -> AOCM (Zipper a)
moveLeft (Zipper prev next) = case prev of
  []
    -> Left $ err "Cannot move left in Zipper, reached edge"

  (p:ps)
    -> Right $ Zipper ps (p:next)

-- | Move the focus right a single element.
moveRight :: Zipper a -> AOCM (Zipper a)
moveRight (Zipper prev next) = case next of
  []
    -> Left $ err "Cannot move right in Zipper, reached edge"

  (n:ns)
    -> Right $ Zipper (n:prev) ns

-- | Move the current focus by a positive or negative offset.
moveBy :: Int -> Zipper a -> AOCM (Zipper a)
moveBy i z
  | i == 0 = pure z
  | i <  0 = moveLeft  z >>= moveBy (i+1)
  | i >  0 = moveRight z >>= moveBy (i-1)
  | otherwise = Left $ err "Cannot move by an offset that is neither 0, <0 or >0 (because it shouldn't exist!)."

-- | The current focused element.
current :: Zipper a -> AOCM a
current (Zipper _ next) = case next of
  []
    -> Left $ err "no current focus in zipper - passed the end or empty"

  (n:_)
    -> Right n

-- | Modify the focused element.
modify :: (a -> a) -> Zipper a -> AOCM (Zipper a)
modify f (Zipper prev next) = case next of
  []
    -> Left $ err "cannot modify the current element in zipper - passed the end or empty"

  (n:ns)
    -> Right $ Zipper prev (f n : ns)

-- | Whether an instruction has been seen previously.
data Seen
  = NotSeen
  | Seen
  deriving Eq

-- | A GameConsole is an instant of state which tracks:
-- - The current 'Instruction' and whether it has been previously 'Seen'.
-- - The previous and next 'Instruction's and whether they have been seen.
-- - The current value of the accumulator.
data GameConsole = GameConsole
  -- TODO: A Zipper is a nice abstraction, but as implemented has O(n) random
  -- access, that is used when interpreting jump 'Instructions'.
  -- This is efficient enough for the example input but could be replaced by an
  -- O(1) indexable array if we needed to be faster and layering IO into the
  -- stack was worth it.
  { _instructionPosition :: Zipper (Seen,Instruction)
  , _accumulator         :: Int
  }

-- | A GameConsole with a 0 accumulator and boot code, terminated by a pseudo
-- EndSection instruction which makes it simpler to recognise when we're at the
-- end of the code, rather than attempting to move out of bounds.
mkGameConsole :: BootCode -> GameConsole
mkGameConsole (BootCode code) = GameConsole (mkZipper . zip (repeat NotSeen) $ code <> [EndSection]) 0

-- | The current focused instruction.
currentInstruction :: GameConsole -> AOCM (Seen, Instruction)
currentInstruction (GameConsole instructionPosition _) = current instructionPosition

-- | Move to the next instruction (without attempting to execute it).
nextInstruction :: GameConsole -> AOCM GameConsole
nextInstruction (GameConsole instructionPosition acc) =
  GameConsole
  <$> moveRight instructionPosition
  <*> pure acc

-- | Move to the previous instruction (without attempting to execute it).
previousInstruction :: GameConsole -> AOCM GameConsole
previousInstruction (GameConsole instructionPosition acc) =
  GameConsole
  <$> moveLeft instructionPosition
  <*> pure acc

-- | Move the instruction pointer by a positive or negative offset.
moveInstructionsBy :: Int -> GameConsole -> AOCM GameConsole
moveInstructionsBy i (GameConsole instructionPosition acc) =
  GameConsole
  <$> moveBy i instructionPosition
  <*> pure acc

-- | Mark the current instruction as 'Seen'.
markSeen :: GameConsole -> AOCM GameConsole
markSeen (GameConsole instructionPosition acc) =
  GameConsole
  <$> modify (\(_seen,inst) -> (Seen, inst)) instructionPosition
  <*> pure acc

-- | Access the value of the accumulator.
currentAccumulator :: GameConsole -> Int
currentAccumulator (GameConsole _ acc) = acc

-- | Modify the accumulator by a positive or negative delta.
addToAccumulator :: Int -> GameConsole -> GameConsole
addToAccumulator i (GameConsole instructions acc) = GameConsole instructions (acc+i)

-- | Evaluate the effect of an Instruction, including the step-effect.
-- This will usually be to move to the next instruction but in the case of a
-- jump may leave the console anywhere else within the code.
evaluate :: Instruction -> GameConsole -> AOCM GameConsole
evaluate instruction gameConsole = case instruction of
    NoOperation _a
      -> nextInstruction gameConsole

    Accumulate a
      -> nextInstruction $ addToAccumulator a gameConsole

    Jump offset
      -> moveInstructionsBy offset gameConsole

    EndSection
      -> pure gameConsole

-- | Execute the instructions step by step until we reach the end by either:
-- - Reaching an instruction that has been seen before (which indicates infinite recursion).
-- - Or hiting the last instruction.
--
-- The boolean indicates whether the run ended sucessfully or not.
run :: GameConsole -> AOCM (GameConsole, Bool)
run gameConsole = do
  (seenBefore, instruction) <- currentInstruction gameConsole

     -- Infinite loop
  if | seenBefore == Seen
     -> pure (gameConsole,False)

     -- End of program
     | instruction == EndSection
     -> pure (gameConsole,True)

     | otherwise
     -> do gameConsole'  <- markSeen             gameConsole
           gameConsole'' <- evaluate instruction gameConsole'
           run gameConsole''

-- | Adjust a corrupted instruction by exchanging jumps and noops.
adjustInstruction :: Instruction -> Instruction
adjustInstruction i = case i of
  Jump offset
    -> NoOperation offset

  NoOperation arg
    -> Jump arg

  _ -> i

-- | Every single instruction corruption of the input.
possibleCorruptions :: BootCode -> [BootCode]
possibleCorruptions (BootCode b) =
  let corruptedBootCode :: Seq Instruction
      corruptedBootCode = Seq.fromList b
   in fmap (\index -> BootCode . toList $ Seq.adjust adjustInstruction index corruptedBootCode)
           [0 .. (length corruptedBootCode)]

