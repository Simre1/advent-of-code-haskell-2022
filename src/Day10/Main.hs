module Day10.Main where

import Data.List.Split (chunksOf)

data Instruction = Add Int | Noop deriving (Show)

solution1 :: IO ()
solution1 = do
  instructions <- input
  let states = executeInstructions 1 instructions
  print $ signalStrength states

solution2 :: IO ()
solution2 = do
  instructions <- input
  let states = executeInstructions 1 instructions
  putStrLn $ drawCRT states

drawCRT :: [Int] -> String
drawCRT states =
  let screenRows = chunksOf 40 $ take 240 states
      pixels = zipWith (\a b -> if abs (a - b) < 2 then '#' else '.') [0 ..] <$> screenRows
   in unlines pixels

signalStrength :: [Int] -> Int
signalStrength states = sum $ (\x -> (states !! (x - 1)) * x) <$> [20, 60, 100, 140, 180, 220]

input :: IO [Instruction]
input = do
  file <- readFile "inputs/day10/input"
  pure $ parseInstruction <$> lines file

parseInstruction :: String -> Instruction
parseInstruction str = case splitAt 4 str of
  ("addx", number) -> Add (read number)
  ("noop", _) -> Noop

executeInstructions :: Int -> [Instruction] -> [Int]
executeInstructions state [] = repeat state
executeInstructions state (instruction : instructions) = case instruction of
  Noop -> state : executeInstructions state instructions
  Add i -> state : addInstruction i state instructions
  where
    addInstruction i state instructions = state : executeInstructions (state + i) instructions