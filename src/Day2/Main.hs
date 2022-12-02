module Day2.Main where

import Data.Functor
import GHC.Generics

solution1 :: IO ()
solution1 = parsedInput1 >>= \games -> print $ sum $ calculateScore <$> games

solution2 :: IO ()
solution2 = parsedInput2 >>= \games -> print $ sum $ calculateScore <$> games


data Handshape = Rock | Paper | Scissor deriving (Show, Eq, Ord, Enum)

data Game = Game
  { me :: Handshape,
    opponent :: Handshape
  }
  deriving (Generic, Show, Eq)

newtype Score = Score Int deriving (Eq, Num, Show, Ord)

calculateScore :: Game -> Score
calculateScore (Game me opponent) = handshapeScore + winScore
  where
    winScore
      | me `beats` opponent = 6
      | opponent `beats` me = 0
      | otherwise = 3
    handshapeScore = case me of
      Rock -> 1
      Paper -> 2
      Scissor -> 3

beats :: Handshape -> Handshape -> Bool
beats Rock Scissor = True
beats Paper Rock = True
beats Scissor Paper = True
beats _ _ = False

input :: IO String
input = readFile "inputs/day2/input1"

parsedInput1 :: IO [Game]
parsedInput1 = fmap ((\[opponent, me] -> Game (meToHandshake me) (opponentToHandshake opponent)) . words) . lines <$> input
  where
    opponentToHandshake "A" = Rock
    opponentToHandshake "B" = Paper
    opponentToHandshake "C" = Scissor
    meToHandshake "X" = Rock
    meToHandshake "Y" = Paper
    meToHandshake "Z" = Scissor

parsedInput2 :: IO [Game]
parsedInput2 =
  fmap
    ( ( \[opponent, me] ->
          let opponentHandshake = opponentToHandshake opponent
           in Game (meToHandshake opponentHandshake me) opponentHandshake
      )
        . words
    )
    . lines
    <$> input
  where
    opponentToHandshake "A" = Rock
    opponentToHandshake "B" = Paper
    opponentToHandshake "C" = Scissor
    meToHandshake opponent "X" = head $ filter (\h -> opponent `beats` h) handshakes
    meToHandshake opponent "Y" = opponent
    meToHandshake opponent "Z" = head $ filter (\h -> h `beats` opponent) handshakes
    handshakes = [Rock .. Scissor]