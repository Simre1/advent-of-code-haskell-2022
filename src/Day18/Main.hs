module Day18.Main where

import Data.Foldable (Foldable (..))
import Data.List.Split (splitOn)
import Data.Map qualified as M
import Data.Monoid
import Data.Set qualified as S
import Debug.Trace
import Linear.V3

newtype LavaDroplet = LavaDroplet (S.Set (V3 Int)) deriving (Show, Eq, Ord)

newtype Water = Water (S.Set (V3 Int)) deriving (Show, Eq, Ord)

solution1 :: IO ()
solution1 = do
  cubes <- input
  let lavaDroplet = makeLavaDroplet cubes
  print $ countSurfaceArea lavaDroplet

solution2 :: IO ()
solution2 = do
  cubes <- input
  let lavaDroplet = makeLavaDroplet cubes
      water = fillInWater lavaDroplet
  print $ countExternalSurfaceArea lavaDroplet water

makeLavaDroplet :: [V3 Int] -> LavaDroplet
makeLavaDroplet = LavaDroplet . S.fromList

countSurfaceArea :: LavaDroplet -> Int
countSurfaceArea (LavaDroplet cubes) = getSum $ foldMap getArea cubes
  where
    getArea position = foldMap (Sum . fromEnum . not . flip S.member cubes) (neighbors position)

countExternalSurfaceArea :: LavaDroplet -> Water -> Int
countExternalSurfaceArea (LavaDroplet cubes) (Water water) = getSum $ foldMap getArea cubes
  where
    getArea position = foldMap (\pos -> Sum . fromEnum $ S.member pos water) (neighbors position)

neighbors :: V3 Int -> [V3 Int]
neighbors position = (position +) <$> [V3 (-1) 0 0, V3 1 0 0, V3 0 (-1) 0, V3 0 (-1) 0, V3 0 0 (-1), V3 0 0 1]

fillInWater :: LavaDroplet -> Water
fillInWater (LavaDroplet cubes) =
  Water $
    fixPoint (\water -> foldl' spread water water) (S.fromList [minimum, maximum])
  where
    spread water position =
      foldl' spreadToPosition water (neighbors position)
    spreadToPosition water position
      | S.notMember position cubes && S.notMember position water && inBound position = S.insert position water
      | otherwise = water
    inBound position = and ((>=) <$> position <*> minimum) && and ((<=) <$> position <*> maximum)
    (minimum, maximum) = boundingBox (LavaDroplet cubes)

boundingBox :: LavaDroplet -> (V3 Int, V3 Int)
boundingBox (LavaDroplet lava) = (\(min, max) -> (min - V3 1 1 1, max + V3 1 1 1)) $ foldl update (pure maxBound, pure minBound) lava
  where
    update (minimum, maximum) current = (min <$> current <*> minimum, max <$> current <*> maximum)

input :: IO [V3 Int]
input = do
  file <- readFile "inputs/day18/input"
  pure $ (\[x, y, z] -> V3 x y z) . fmap read . splitOn "," <$> lines file

fixPoint :: (Eq a, Show a) => (a -> a) -> a -> a
fixPoint f !a =
  let next = f a
   in if next == a
        then a
        else fixPoint f next