module Day1.Main where

import Data.List.Split
import Debug.Trace
import Data.List (sortBy)


solution1 :: IO ()
solution1 = readInput >>= print . maxFood . parseInput

solution2 :: IO ()
solution2 = readInput >>= print . totalFoodForTop3 . parseInput

readInput :: IO String
readInput = readFile "inputs/day1/input1"

parseInput :: String -> [[Int]]
parseInput = fmap (fmap read)  . split (dropDelims $ oneOf [""])  . lines

maxFood :: [[Int]] -> Int
maxFood = maximum . fmap sum

totalFoodForTop3 :: [[Int]] -> Int
totalFoodForTop3 = sum . take 3 . sortBy (flip compare) . fmap sum
