module Day6.Main where

import Data.List

solution1 :: IO ()
solution1 = input >>= print . findEndOfPacket

solution2 :: IO ()
solution2 = input >>= print . findEndOfMessage

input :: IO String
input = readFile "inputs/day6/input"

findEndOfPacket :: String -> Int
findEndOfPacket stream = findStartOfMarker 4 stream + 4

findEndOfMessage :: String -> Int
findEndOfMessage stream = findStartOfMarker 14 stream + 14

findStartOfMarker :: Int -> String -> Int
findStartOfMarker markerLength = go 0
  where
    go i message
      | length (take markerLength message) < markerLength = i + length message
      | allDifferent (take markerLength message) = i
      | otherwise = go (succ i) (tail message)

allDifferent :: Eq a => [a] -> Bool
allDifferent xs = length xs == length (nub xs)