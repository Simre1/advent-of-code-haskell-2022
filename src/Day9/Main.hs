module Day9.Main where

import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.Set qualified as S
import Linear.V2 (V2 (..))

data MoveDirection = R | L | U | D deriving (Show, Read)

data Move = Move MoveDirection Int deriving (Show)

data RopeState = RopeState (V2 Int) [V2 Int] (S.Set (V2 Int))

solution1 :: IO ()
solution1 = do
  moves <- input
  let (RopeState _ _ cache) = traceMoves 1 moves
  print $ S.size cache

solution2 :: IO ()
solution2 = do
  moves <- input
  let (RopeState _ _ cache) = traceMoves 9 moves
  print $ S.size cache

input :: IO [Move]
input = readFile "inputs/day9/input" <&> fmap parseLine . lines
  where
    parseLine (direction : number) = Move (read [direction]) (read number)

traceMoves :: Int -> [Move] -> RopeState
traceMoves ropeLength = foldl' traceMove (RopeState (V2 0 0) (replicate ropeLength (V2 0 0)) S.empty)

traceMove :: RopeState -> Move -> RopeState
traceMove moveCache@(RopeState head ropeTail ropeState) = \case
  Move _ 0 -> moveCache
  Move direction n ->
    let newHead = head + directionToV2 direction
        newTail = tail $ scanl moveCurrentKnot newHead ropeTail
        newRopeState = RopeState newHead newTail (S.insert (last newTail) ropeState)
     in traceMove newRopeState $ Move direction (pred n)
  where
    directionToV2 = \case
      R -> V2 1 0
      L -> V2 (-1) 0
      U -> V2 0 1
      D -> V2 0 (-1)
    distance a b = abs (a - b)
    moveCurrentKnot next current =
      if any (> 1) (distance next current)
        then current + (signum <$> (next - current))
        else current
