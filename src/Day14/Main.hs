module Day14.Main where

import Data.Foldable
import Data.Functor
import Data.List.Split
import Data.Map qualified as M
import Data.Set qualified as S
import Debug.Trace
import Linear.V2
import Optics.Core

data Cell = Rock | Sand deriving (Show, Eq)

newtype Cave = Cave (M.Map (V2 Int) Cell) deriving (Show)

data SimulationResult = DoneBecauseFixpoint Cave | NextSimulationState Cave deriving (Show)

solution1 :: IO ()
solution1 = do
  cave <- input
  let caveDepth = lowestCaveDepth cave
      (Cave stableCave) = fixPointSimulation (simulateOneSandUnitTask1 caveDepth) cave
  print $ length $ filter (== Sand) $ snd <$> M.toList stableCave

solution2 :: IO ()
solution2 = do
  cave <- input
  let caveDepth = lowestCaveDepth cave
      (Cave stableCave) = fixPointSimulation (simulateOneSandUnitTask2 caveDepth) cave
  print $ length $ filter (== Sand) $ snd <$> M.toList stableCave

fixPointSimulation :: (Cave -> V2 Int -> SimulationResult) -> Cave -> Cave
fixPointSimulation simulate cave =
  case simulate cave sandSource of
    NextSimulationState cave -> fixPointSimulation simulate cave
    DoneBecauseFixpoint cave -> cave

simulateOneSandUnitTask1 :: Int -> Cave -> V2 Int -> SimulationResult
simulateOneSandUnitTask1 caveDepth (Cave cave) = go
  where
    go :: V2 Int -> SimulationResult
    go sandPosition
      | sandPosition ^. lensVL _y > caveDepth = DoneBecauseFixpoint (Cave cave)
      | not $ M.member (sandPosition + V2 0 1) cave = go (sandPosition + V2 0 1)
      | not $ M.member (sandPosition + V2 (-1) 1) cave = go (sandPosition + V2 (-1) 1)
      | not $ M.member (sandPosition + V2 1 1) cave = go (sandPosition + V2 1 1)
      | otherwise = NextSimulationState $ Cave $ M.insert sandPosition Sand cave

simulateOneSandUnitTask2 :: Int -> Cave -> V2 Int -> SimulationResult
simulateOneSandUnitTask2 caveDepth (Cave cave) = go
  where
    go :: V2 Int -> SimulationResult
    go sandPosition
      | sandPosition ^. lensVL _y == caveDepth + 1 = NextSimulationState $ Cave $ M.insert sandPosition Sand cave
      | not $ M.member (sandPosition + V2 0 1) cave = go (sandPosition + V2 0 1)
      | not $ M.member (sandPosition + V2 (-1) 1) cave = go (sandPosition + V2 (-1) 1)
      | not $ M.member (sandPosition + V2 1 1) cave = go (sandPosition + V2 1 1)
      | sandPosition == sandSource = DoneBecauseFixpoint $ Cave $ M.insert sandPosition Sand cave
      | otherwise = NextSimulationState $ Cave $ M.insert sandPosition Sand cave

sandSource :: V2 Int
sandSource = V2 500 0

lowestCaveDepth :: Cave -> Int
lowestCaveDepth (Cave cave) = maximum $ view (lensVL _y) . fst <$> M.toList cave

input :: IO Cave
input = do
  file <- readFile "inputs/day14/input"
  let rockLines = readLine <$> lines file
      rockPoints = concatMap traceLine rockLines
  pure $ Cave $ M.fromList $ rockPoints `zip` repeat Rock
  where
    readLine :: String -> [V2 Int]
    readLine lineString =
      let points = splitOn " -> " lineString
       in points <&> \pointString ->
            let [x, y] = read <$> splitOn "," pointString in V2 x y

tracePoints :: V2 Int -> V2 Int -> [V2 Int]
tracePoints start end =
  let (V2 dX dY) = end - start
    in (start +) <$> zipWith V2 [0, signum dX .. dX] [0, signum dY .. dY]

traceLine :: [V2 Int] -> [V2 Int]
traceLine [] = []
traceLine [x] = [x]
traceLine (x : y : r) = tracePoints x y ++ traceLine (y : r)