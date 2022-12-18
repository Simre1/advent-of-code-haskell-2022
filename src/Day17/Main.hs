module Day17.Main where

import Control.Arrow
import Data.Bit qualified as B
import Data.Foldable (foldl')
import Data.Map qualified as M
import Data.Set qualified as S
import Debug.Trace
import GHC.Generics
import Linear.V2
import Optics.Core

data Chamber = Chamber
  { rockPositions :: S.Set (V2 Int),
    heighestRock :: Int
  }
  deriving (Show, Eq, Generic)

data Shape = HorizontalLine | Plus | MirroredL | VerticalLine | Square deriving (Show, Eq, Ord, Enum)

data Jet = PushLeft {index :: Int} | PushRight {index :: Int} deriving (Show, Eq, Generic)

data FallStep = HasLanded [V2 Int] | StillFalling [V2 Int] deriving (Show, Eq)

data Cache = Cache
  { cache :: M.Map (Int, [Int]) (Int, Int)
  }
  deriving (Show)

solution1 :: IO ()
solution1 = do
  jets <- input
  let height = simulateNTimesWithCache startChamber (cycle jets) 2022
  print $ height

solution2 :: IO ()
solution2 = do
  jets <- input
  let height = simulateNTimesWithCache startChamber (cycle jets) 1000000000000
  print height

simulateNTimesWithCache :: Chamber -> [Jet] -> Int -> Int
simulateNTimesWithCache chamber jets iterations = go (Cache M.empty) chamber jets (cycle [HorizontalLine .. Square]) iterations
  where
    go cache chamber _ _ 0 = chamber ^. #heighestRock
    go (Cache cache) chamber jets (shape : nextShapes) n =
      if M.member (head jets ^. #index, topSignature chamber) cache
        then
          let (pastIteration, pastHeight) = cache M.! (head jets ^. #index, topSignature chamber)
              cycleLength = iterations - n - pastIteration
              followingCycleAmount = n `quot` cycleLength
              cycleHeight = followingCycleAmount * ((chamber ^. #heighestRock) - pastHeight)
              restSimulation = go (Cache M.empty) chamber jets (shape:nextShapes) (n - followingCycleAmount * cycleLength)
           in cycleHeight + restSimulation
        else
          let (nextChamber, nextJets) = simulate jets chamber shape
              nextCache = Cache $ M.insert (head jets ^. #index, topSignature chamber) (iterations - n, chamber ^. #heighestRock) cache
           in go nextCache (takeTop50Rows nextChamber) nextJets nextShapes (pred n)

chamberFromSignature :: [Int] -> Chamber
chamberFromSignature signature =
  let max = maximum signature
      inversedSignature = (max -) <$> signature
      rocks = foldl' (\s a -> S.insert (uncurry V2 a) s) S.empty ([0 .. 6] `zip` inversedSignature)
   in Chamber rocks max

simulateNTimes :: Chamber -> [Jet] -> Int -> Int
simulateNTimes chamber jets n = go chamber jets (cycle [HorizontalLine .. Square]) n
  where
    go chamber _ _ 0 = chamber ^. #heighestRock
    go chamber jets (shape : nextShapes) n =
      let (nextChamber, nextJets) = simulate jets chamber shape
       in go (takeTop50Rows nextChamber) nextJets nextShapes (pred n)

simulate :: [Jet] -> Chamber -> Shape -> (Chamber, [Jet])
simulate jets chamber shape =
  let (formation, nextJets) = go startFormation jets
      nextChamber =
        chamber
          & #rockPositions %~ flip (foldl' (flip S.insert)) formation
          & #heighestRock %~ max (maximum $ formation ^. mapping (lensVL _y))
   in (nextChamber, nextJets)
  where
    go formation (jet : jets) = case oneStep formation jet of
      StillFalling nextFormation -> go nextFormation jets
      HasLanded landedFormation -> (landedFormation, jets)

    oneStep :: [V2 Int] -> Jet -> FallStep
    oneStep formation jet =
      let movedFormation = (jetToV2 jet +) <$> formation
          formationAfterJet = if isPossibleMove chamber movedFormation then movedFormation else formation
          formationAfterFall = (V2 0 (-1) +) <$> formationAfterJet
       in if isPossibleMove chamber formationAfterFall
            then StillFalling formationAfterFall
            else HasLanded formationAfterJet
    startFormation = getStartFormation chamber shape
    rockPositions = chamber ^. #rockPositions

takeTop50Rows :: Chamber -> Chamber
takeTop50Rows (Chamber rockPositions height) = Chamber (S.filter (\(V2 x y) -> y + 50 > height) rockPositions) height

topSignature :: Chamber -> [Int]
topSignature (Chamber rockPositions height) =
  fmap snd $
    M.toList $
      foldl'
        (\m (V2 x y) -> M.alter (update y) x m)
        (M.fromList $ [0 .. 6] `zip` repeat height)
        (S.toList rockPositions)
  where
    update a Nothing = Just (height - a)
    update a (Just b) = Just (min (height - a) b)

chamberWidth :: Int
chamberWidth = 7

startChamber :: Chamber
startChamber = Chamber S.empty 0

getStartFormation :: Chamber -> Shape -> [V2 Int]
getStartFormation chamber = \case
  HorizontalLine -> (bottomLeft +) . flip V2 0 <$> [0 .. 3]
  VerticalLine -> (bottomLeft +) . V2 0 <$> [0 .. 3]
  Plus -> (bottomLeft +) <$> [V2 0 1, V2 1 0, V2 1 1, V2 1 2, V2 2 1]
  MirroredL -> (bottomLeft +) <$> [V2 0 0, V2 1 0, V2 2 0, V2 2 1, V2 2 2]
  Square -> (bottomLeft +) <$> [V2 0 0, V2 1 0, V2 0 1, V2 1 1]
  where
    bottomLeft = V2 2 $ chamber ^. #heighestRock + 4

jetToV2 :: Jet -> V2 Int
jetToV2 (PushRight _) = V2 1 0
jetToV2 (PushLeft _) = V2 (-1) 0

isPossibleMove :: Chamber -> [V2 Int] -> Bool
isPossibleMove chamber formation =
  let positionFree position = not $ S.member position (chamber ^. #rockPositions)
      positionFitsInChamber (V2 x y) = x >= 0 && x < chamberWidth && y > 0
   in all (\position -> positionFree position && positionFitsInChamber position) formation

input :: IO [Jet]
input = toJet . zip [0 ..] <$> readFile "inputs/day17/input"
  where
    toJet ((i, '<') : next) = PushLeft i : toJet next
    toJet ((i, '>') : next) = PushRight i : toJet next
    toJet _ = []
