module Day4.Main where

import Data.Functor
import Data.List.Split (splitOneOf)

data Pair = Pair (Int, Int) (Int, Int) deriving (Eq, Show)

solution1 :: IO ()
solution1 = do
  i <- input
  print $ sum $ fromEnum . pairFullyOverlaps <$> i

solution2 :: IO ()
solution2 = do
  i <- input
  print $ sum $ fromEnum . pairOverlaps <$> i

input :: IO [Pair]
input = do
  file <- readFile "inputs/day4/input1"
  pure $
    lines file <&> \line ->
      let [elf1L, elf1R, elf2L, elf2R] = read <$> splitOneOf ",-" line
       in Pair (elf1L, elf1R) (elf2L, elf2R)

pairFullyOverlaps :: Pair -> Bool
pairFullyOverlaps (Pair (elf1L, elf1R) (elf2L, elf2R))
  | elf1L <= elf2L && elf1R >= elf2R = True
  | elf2L <= elf1L && elf2R >= elf1R = True
  | otherwise = False

pairOverlaps :: Pair -> Bool
pairOverlaps (Pair (elf1L, elf1R) (elf2L, elf2R))
  | elf1L > elf2R = False
  | elf2L > elf1R = False
  | otherwise = True