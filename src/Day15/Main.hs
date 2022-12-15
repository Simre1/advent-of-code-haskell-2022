module Day15.Main where

import Control.Applicative
import Data.Char
import Data.Foldable
import Data.List.Split
import Data.Maybe
import Data.SBV
import Data.SBV.Internals qualified as I
import Day5.Main (splitOnAnyOf)
import Debug.Trace
import GHC.Generics
import Linear.V2
import Optics.Core

data Sensor = Sensor
  { position :: V2 Int,
    closestBeacon :: V2 Int,
    distance :: Int
  }
  deriving (Eq, Show, Generic)

solution1 :: IO ()
solution1 = do
  sensors <- input
  let (xmin, xmax) = xBoundaries sensors
      y = 2000000
      positionsToCheck = V2 <$> [xmin .. xmax] <*> [y]
  print $
    sum $
      fromEnum
        . ( \position ->
              pointCoveredBySensors sensors position
                && not (pointIsBeacon sensors position)
          )
        <$> positionsToCheck

solution2 :: IO ()
solution2 = do
  sensors <- input
  position <- solveWithSMT sensors 4000000
  print $ tuningFrequency position

input :: IO [Sensor]
input = do
  file <- readFile "inputs/day15/input"
  let sensorStrings = lines file
  pure $ extractSensor <$> sensorStrings

extractSensor :: String -> Sensor
extractSensor str =
  let simplifiedString = concatMap (\c -> if isNumber c || c == '-' || c == ',' || c == ':' then [c] else []) str
      [sensorX, sensorY, beaconX, beaconY] = read <$> splitOnAnyOf [":", ","] simplifiedString
      sensorPosition = V2 sensorX sensorY
      beaconPosition = V2 beaconX beaconY
   in Sensor sensorPosition beaconPosition (manhattan sensorPosition beaconPosition)

manhattan :: Num a => V2 a -> V2 a -> a
manhattan v1 v2 = sum $ abs (v2 - v1)

pointCoveredBySensors :: [Sensor] -> V2 Int -> Bool
pointCoveredBySensors sensors position =
  any inSensor sensors
  where
    inSensor sensor = sensor ^. #distance >= manhattan position (sensor ^. #position)

pointIsBeacon :: [Sensor] -> V2 Int -> Bool
pointIsBeacon sensors position = any (\sensor -> sensor ^. #closestBeacon == position) sensors

xBoundaries :: [Sensor] -> (Int, Int)
xBoundaries sensors =
  let boundaries = xBoundary <$> sensors
   in (minimum $ fst <$> boundaries, maximum $ snd <$> boundaries)
  where
    xBoundary :: Sensor -> (Int, Int)
    xBoundary (Sensor (V2 x _) _ distance) = (x - distance, x + distance)

solveWithSMT :: [Sensor] -> Int  -> IO (V2 Int)
solveWithSMT  sensors size = do
  (SatResult (Satisfiable _ smtModel)) <- sat $ do
    x <- sbvExists "x"
    y <- sbvExists "y"
    constrain $ x .>= 0 .&& x .<= literal32 size
    constrain $ y .>= 0 .&& y .<= literal32 size
    traverse_ (constrain . constraintsForSensor (V2 x y)) sensors

  let variables = I.modelAssocs smtModel
      cvX = fromJust $ lookup "x" variables
      cvY = fromJust $ lookup "y" variables

  case (I.cvVal cvX, I.cvVal cvY) of
    (I.CInteger x, I.CInteger y) -> pure $ fromIntegral <$> V2 x y
  where
    constraintsForSensor :: V2 (SBV Int32) -> Sensor -> SBool
    constraintsForSensor solutionPosition (Sensor sensorPosition _ distance) =
      manhattan solutionPosition (literal32 <$> sensorPosition) .> literal32 distance
    literal32 :: Int -> SBV Int32
    literal32 = literal . fromIntegral

tuningFrequency :: V2 Int -> Int
tuningFrequency (V2 x y) = x * 4000000 + y