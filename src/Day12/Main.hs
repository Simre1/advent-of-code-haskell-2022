module Day12.Main where

import Algorithm.Search
import Data.List
import Data.List (find)
import Data.Map qualified as M
import Data.Maybe
import Linear.V2 (V2 (..))

newtype HeightMap = HeightMap (M.Map (V2 Int) Char) deriving (Show)

solution1 :: IO ()
solution1 = do
  (heightMap, startPosition, endPosition, _) <- input
  print $ length <$> findWay heightMap startPosition endPosition

solution2 :: IO ()
solution2 = do
  (heightMap, _, endPosition, allLowestPositions) <- input
  print $ minimum $ mapMaybe (\startPosition -> length <$> findWay heightMap startPosition endPosition) allLowestPositions

findWay :: HeightMap -> V2 Int -> V2 Int -> Maybe [V2 Int]
findWay (HeightMap heightMap) start end =
  snd
    <$> aStar
      nextStates
      cost
      costEstimate
      isGoal
      start
  where
    nextStates :: V2 Int -> [V2 Int]
    nextStates pos =
      let neighbors =
            mapMaybe
              ( (\neighborPosition -> (neighborPosition,) <$> M.lookup neighborPosition heightMap)
                  . (+ pos)
              )
              [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]
          currentHeight = heightMap M.! pos
       in fst
            <$> filter
              (\(neighborPosition, neighborHeight) -> neighborHeight <= succ currentHeight)
              neighbors
    cost a b = 1
    costEstimate currentPosition = sum (end - currentPosition)
    isGoal current = end == current

input :: IO (HeightMap, V2 Int, V2 Int, [V2 Int])
input = do
  file <- readFile "inputs/day12/test"
  let rows = [0 ..] `zip` lines file
      grid =
        concatMap
          ( \(rowIndex, row) ->
              zipWith (\columnIndex value -> (V2 rowIndex columnIndex, value)) [0 ..] row
          )
          rows
      gridMap = M.fromList grid
      (startPosition, _) = fromJust $ find (\(_, char) -> char == 'S') grid
      (endPosition, _) = fromJust $ find (\(_, char) -> char == 'E') grid
      allLowestPositions = fst <$> filter (\(_, char) -> char == 'a') grid
      gridMapWithReplacedStartAndEnd =
        M.update (const $ pure 'a') startPosition $
          M.update (const $ pure 'z') endPosition gridMap

  pure (HeightMap gridMapWithReplacedStartAndEnd, startPosition, endPosition, allLowestPositions)