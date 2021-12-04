{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
module BingoSubsystem
  ( BingoSubsystem ()
  , parseBingoSubsystem

  , play

  , Strategy
  , toWin
  , toLose
  )
  where

import AOC

import Bingo
import Board

import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Foldable (foldlM)
import Data.List (intersperse)
import qualified Data.Set as Set

data BingoSubsystem = BingoSubsystem
  { _draws  :: [Int]
  , _boards :: Set Board
  }
  deriving Show

-- | Play bingo, returning the winning score of the board picked by the
-- strategy.
play
  :: Strategy
  -> BingoSubsystem
  -> AOCM Int
play strategy bingo = do
  (lastDrawnNumber, winner)
    <- findWinningBoard bingo strategy

  pure $ score winner * lastDrawnNumber

-- | Strategy for searching for a winning bingo board.
type Strategy
  = Board     -- ^ Winner this round.
 -> Set Board -- ^ Remaining boards.
 -> Bool      -- ^ Continue with remaining? Otherwise return winner.

-- | Halt as soon as the first winner is found.
toWin
  :: Strategy
toWin _winner _remaining
  = False

-- | Continue until no other boards are remaining.
toLose
  :: Strategy
toLose _winner remaining
  = not . Set.null $ remaining

-- | Play rounds of bingo, returning the first winner to satisfy the strategy.
findWinningBoard
  :: BingoSubsystem
  -> Strategy
  -> AOCM (Int, Board)
findWinningBoard (BingoSubsystem draws boards) strategy
  = let result :: Either (Int, Board) (Set Board)
        result = foldlM (\boards draw
                          -> let boards' :: Set Board
                                 boards' = Set.map (mark draw) boards

                                 (winners,remaining) = Set.partition won boards'

                                 -- Assume only one board can win at a time.
                                 winner :: Maybe Board
                                 winner = if Set.null winners
                                            then Nothing
                                            else Just . Set.elemAt 0 $ winners

                              in case winner of
                                   -- No winner, continue
                                   Nothing
                                     -> Right remaining

                                   -- A winner this round.
                                   --
                                   -- Consult the strategy to determine whether we want this winner or not.
                                   Just winner
                                     | strategy winner remaining
                                     -> Right remaining

                                     | otherwise
                                     -> Left (draw, winner)
                        )
                        boards
                        draws
     in case result of
          Left (losingDraw, losingBoard)
            -> pure (losingDraw, losingBoard)

          Right finalBoards
            -> Left $ err "No board has won" `withAttributes`
                 [ ("final-boards", ("\n" <>) . Text.pack . mconcat . intersperse "\n\n" . fmap show . Set.toList $ finalBoards)
                 ]

{- Parsing -}

parseBingoSubsystem
  :: [Text]
  -> AOCM BingoSubsystem
parseBingoSubsystem input = case input of
  (draws:"":boards)
    -> BingoSubsystem
         <$> parseDraws draws
         <*> parseBoards boards
  _
    -> Left . err $ "Input does not have enough rows for draws and boards"

parseDraws
  :: Text
  -> AOCM [Int]
parseDraws = parse draws

draws
  :: Parser [Int]
draws
  = sepBy0 digits comma

