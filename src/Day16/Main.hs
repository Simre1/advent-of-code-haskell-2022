{-# LANGUAGE Strict #-}

module Day16.Main where

import Algorithm.Search (aStar, dijkstra)
import Data.Char ( isNumber, isUpper )
import Data.List ( foldl' )
import Data.Map.Strict qualified as M
import Data.Maybe ( fromJust )
import Data.Set qualified as S
import Data.Text (Text, pack)
import Data.Void ( Void )
import Day13.Main (manyWithDelimiter)
import GHC.Generics ( Generic )
import Optics.Core
    ( view, (^.), Field1(_1), MappingOptic(mapping) )
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

data Valve = Valve
  { name :: Text,
    flow :: Int,
    next :: [Text]
  }
  deriving (Show, Generic)

solution1 :: IO ()
solution1 = do
  valves <- input
  let shortestPath = (calculateShortestPath valves M.!)
  let valveOrder = findBestValveOrder (removeZeroFlowValves valves) shortestPath "AA" 30
  print $ calculateFlow valves shortestPath 30 valveOrder

solution2 :: IO ()
solution2 = do
  valves <- input
  let shortestPath = (calculateShortestPath valves M.!)
  let valveOrder = findBestValveOrder (removeZeroFlowValves valves) shortestPath "AA" 26
  let valveOrderElephant = findBestValveOrder (filter (not . flip elem valveOrder . view #name) $ removeZeroFlowValves valves) shortestPath "AA" 26
  print $ calculateFlow valves shortestPath 26 valveOrder + calculateFlow valves shortestPath 26 valveOrderElephant
  
findBestValveOrder :: [Valve] -> ((Text, Text) -> Int) -> Text -> Int -> [Text]
findBestValveOrder valves shortestPath start time =
  let solution =
        dijkstra
          nextStates
          cost
          isGoal
          (start, 0, S.empty)
   in view _1 <$> snd (fromJust solution)
  where
    nextStates (current, turn, activated) =
      let nextValves = filter (\v -> v /= current && not (S.member v activated) && nextTurn v <= 30) allRelevantValveNames
          nextTurn v = turn + shortestPath (current, v) + 1
          !nextValveStates = (\v -> (v, nextTurn v, S.insert v activated)) <$> nextValves
          doNothing = (current, turn + 1, activated)
       in doNothing : nextValveStates
    cost (valve1, turn1, activated1) (valve2, turn2, activated2) =
      let pathLength = shortestPath (valve1, valve2)
       in (turn2 - turn1) * (maximumFlow - flowOfSet activated1)
    isGoal (_, turn, activated) = turn == time
    maximumFlow = sum $ valves ^. mapping #flow
    flowOfSet set = sum $ getFlow <$> S.toList set
    allRelevantValveNames = valves ^. mapping #name
    valveMap :: M.Map Text Valve
    valveMap = M.fromList $ fmap (\valve@(Valve name _ _) -> (name, valve)) valves
    getFlow :: Text -> Int
    getFlow valve = valveMap M.! valve ^. #flow

removeZeroFlowValves :: [Valve] -> [Valve]
removeZeroFlowValves = filter (\(Valve _ flow _) -> flow > 0)

calculateFlow :: [Valve] -> ((Text, Text) -> Int) -> Int -> [Text] -> Int
calculateFlow valves shortestPath time valveOrder = go S.empty "AA" valveOrder 0
  where
    go activated last (current : next) turn =
      let nextActivated = S.insert current activated
          neededTurns = shortestPath (last, current) + 1
          remainingTurns = time - turn
       in if neededTurns >= remainingTurns
            then remainingTurns * sum (getFlow <$> S.toList activated)
            else neededTurns * sum (getFlow <$> S.toList activated) + go nextActivated current next (turn + neededTurns)
    go activated _ _ turn = (time - turn) * sum (getFlow <$> S.toList activated)

    valveMap :: M.Map Text Valve
    valveMap = M.fromList $ fmap (\valve@(Valve name _ _) -> (name, valve)) valves
    getFlow :: Text -> Int
    getFlow valve = let (Valve _ flow _) = valveMap M.! valve in flow
    maximumFlow :: Int
    maximumFlow = sum $ valves ^. mapping #flow

calculateShortestPath :: [Valve] -> M.Map (Text, Text) Int
calculateShortestPath valves = foldl' update initialMap indices
  where
    initialMap =
      let allInfiniteMap = M.fromList $ ((,) <$> allValveNames <*> allValveNames) `zip` repeat quasiInfinite
       in foldl' (\m (Valve n _ edges) -> M.insert (n, n) 0 $ foldl' (\m e -> M.insert (n, e) 1 m) m edges) allInfiniteMap valves
    allValveNames = valves ^. mapping #name
    indices = (,,) <$> allValveNames <*> allValveNames <*> allValveNames
    update m (k, i, j)
      | m M.! (i, j) > m M.! (i, k) + m M.! (k, j) = M.insert (i, j) (m M.! (i, k) + m M.! (k, j)) m
      | otherwise = m
    quasiInfinite = 1000000

input :: IO [Valve]
input = do
  file <- readFile "inputs/day16/input"
  traverse parseLine (lines file)

parseLine :: String -> IO Valve
parseLine str = either (fail . P.errorBundlePretty) pure $ P.parse parser "valve" (pack str)
  where
    parser :: P.Parsec Void Text Valve
    parser = do
      P.string "Valve "
      valveName <- P.takeWhile1P Nothing (/= ' ')
      P.takeWhile1P Nothing (not . isNumber)
      flowRate <- P.decimal
      P.takeWhile1P Nothing (not . isUpper)
      tunnels <- manyWithDelimiter (P.takeWhile1P Nothing isUpper) (P.takeWhile1P Nothing $ \c -> c == ',' || c == ' ')
      pure $ Valve valveName flowRate tunnels
