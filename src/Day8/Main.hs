module Day8.Main where

import Data.List (foldl')
import Data.Map qualified as M
import Data.Monoid
import Linear.V2

solution1 :: IO ()
solution1 = do
  gridMap <- input

  print $ countVisibleTrees gridMap

solution2 :: IO ()
solution2 = do
  gridMap <- input

  print $ findBestScenicScore gridMap

input :: IO (M.Map (V2 Int) Int)
input = do
  file <- readFile "inputs/day8/input"
  let grid = zip [0 ..] $ zip [0 ..] <$> lines file
  pure $
    foldl'
      (\gridMap (rowIndex, row) -> foldl' (\gridMap (columnIndex, val) -> M.insert (V2 rowIndex columnIndex) (read [val]) gridMap) gridMap row)
      M.empty
      grid

visibleTrees :: M.Map (V2 Int) Int -> M.Map (V2 Int) Bool
visibleTrees gridMap = M.mapWithKey (\position _ -> isVisible gridMap position) gridMap

countVisibleTrees :: M.Map (V2 Int) Int -> Int
countVisibleTrees grid =
  getSum $
    M.foldMapWithKey (\key _ -> Sum $ fromEnum $ isVisible grid key) grid

findBestScenicScore :: M.Map (V2 Int) Int -> Int
findBestScenicScore grid =
  M.foldlWithKey'
    ( \bestScore position _ ->
        let nextScenicScore = scenicScore grid position
         in max nextScenicScore bestScore
    )
    (-100)
    grid

scenicScore :: M.Map (V2 Int) Int -> V2 Int -> Int
scenicScore gridMap treePosition = product $ scenicScoreInDirection treePosition <$> [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]
  where
    treeHeight = gridMap M.! treePosition
    scenicScoreInDirection position direction =
      case M.lookup (direction + position) gridMap of
        Nothing -> 0
        Just neighboringTree ->
          if neighboringTree >= treeHeight
            then 1
            else 1 + scenicScoreInDirection (direction + position) direction

isVisible :: M.Map (V2 Int) Int -> V2 Int -> Bool
isVisible gridMap treePosition = any (canLookOutside treePosition) [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]
  where
    treeHeight = gridMap M.! treePosition
    canLookOutside position direction =
      case M.lookup (direction + position) gridMap of
        Nothing -> True
        Just neighboringTree ->
          neighboringTree < treeHeight && canLookOutside (direction + position) direction