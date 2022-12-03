module Day3.Main where

import Data.Functor
import Data.List.Split
import Data.Map qualified as M
import Data.Set qualified as S

data Rucksack = Rucksack (S.Set Char) (S.Set Char) deriving (Show, Eq)

input :: IO [Rucksack]
input = do
  file <- readFile "inputs/day3/test"
  pure $
    lines file <&> \line ->
      let splitIndex = (length line `quot` 2)
          firstHalf = take splitIndex line
          secondHalf = drop splitIndex line
       in Rucksack (S.fromList firstHalf) (S.fromList secondHalf)

solution1 :: IO ()
solution1 = do
  rucksacks <- input
  print $ sum $ getPriority . commonItem <$> rucksacks

solution2 :: IO ()
solution2 = do
  rucksacks <- input
  let groups = chunksOf 3 rucksacks
      badges = groups <&> \[r1, r2, r3] -> commonBadge r1 r2 r3
  print $ sum $ getPriority <$> badges

commonBadge :: Rucksack -> Rucksack -> Rucksack -> Char
commonBadge r1 r2 r3 = head $ S.toList $ getContent r1 `S.intersection` getContent r2 `S.intersection` getContent r3

commonItem :: Rucksack -> Char
commonItem (Rucksack first second) = head $ S.toList $ S.intersection first second

getPriority :: Char -> Int
getPriority c = priorityMap M.! c
  where
    priorityMap :: M.Map Char Int
    priorityMap = M.fromList $ (['a' .. 'z'] ++ ['A' .. 'Z']) `zip` [1 .. 52]

getContent :: Rucksack -> S.Set Char
getContent (Rucksack first second) = S.union first second