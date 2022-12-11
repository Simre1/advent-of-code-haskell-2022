module Day11.Main where

import Data.Aeson.KeyMap qualified as K
import Data.IntMap qualified as IM
import Data.List (foldl', sort)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Data.Yaml (Value (Number, String), decodeThrow)
import GHC.Generics (Generic)
import Optics.Core
  ( Field2 (_2),
    MappingOptic (mapping),
    (%%),
    (%~),
    (&),
    (.~),
    (^.),
  )
import Optics.Operators.Unsafe ((^?!))

data Monkey = Monkey
  { items :: [Int],
    operation :: Operation,
    testDivisible :: Int,
    ifTrue :: Int,
    ifFalse :: Int
  }
  deriving (Generic, Show)

data Operation = Add Int | Multiply Int | Square deriving (Show)

solution1 :: IO ()
solution1 = do
  monkeys <- input
  print $ countMonkeyBusiness 20 (`quot` 3) monkeys

solution2 :: IO ()
solution2 = do
  monkeys <- input
  let moduloInt = product $ IM.toList monkeys ^. mapping (_2 %% #testDivisible)
  print $ countMonkeyBusiness 10000 (`mod` moduloInt) monkeys

countMonkeyBusiness :: Int -> (Int -> Int) -> IM.IntMap Monkey -> Int
countMonkeyBusiness rounds handleWorryLevel monkeys =
  let (_, monkeyActivity) = foldl (\a b -> runRound handleWorryLevel a) (monkeys, IM.empty) [1 .. rounds]
   in product $ take 2 $ reverse $ sort $ fmap snd $ IM.toList monkeyActivity

countInspectionsForNextRound :: IM.IntMap Monkey -> IM.IntMap Int
countInspectionsForNextRound = fmap (\monkey -> length $ monkey ^. #items)

runRound :: (Int -> Int) -> (IM.IntMap Monkey, IM.IntMap Int) -> (IM.IntMap Monkey, IM.IntMap Int)
runRound handleWorryLevel (monkeys, activity) = foldl' runMonkey (monkeys, activity) [0 .. IM.size monkeys - 1]
  where
    runMonkey (monkeys, activity) monkeyId =
      let monkey = monkeys IM.! monkeyId
       in ( foldl
              (runItem handleWorryLevel monkey)
              (IM.update (\monkey -> pure $ monkey & #items .~ []) monkeyId monkeys)
              $ monkey ^. #items,
            let activityForCurrentRound = (length $ monkey ^. #items)
             in IM.alter
                  ( maybe
                      (pure activityForCurrentRound)
                      (\previousActivity -> pure $ previousActivity + activityForCurrentRound)
                  )
                  monkeyId
                  activity
          )

runItem :: (Int -> Int) -> Monkey -> IM.IntMap Monkey -> Int -> IM.IntMap Monkey
runItem handleWorryLevel monkey monkeys worryLevel =
  let newLevel = handleWorryLevel $ runOperation (monkey ^. #operation) worryLevel
      target =
        if newLevel `mod` (monkey ^. #testDivisible) == 0
          then monkey ^. #ifTrue
          else monkey ^. #ifFalse
   in appendItem newLevel target monkeys
  where
    appendItem worryLevel = IM.update (\monkey -> pure $ monkey & #items %~ (++ [worryLevel]))

runOperation :: Operation -> Int -> Int
runOperation (Add b) a = a + b
runOperation (Multiply b) a = a * b
runOperation Square a = a * a

input :: IO (IM.IntMap Monkey)
input = do
  fileText <- T.readFile "inputs/day11/input"
  let yamlCompatibleText = T.replace "divisible by" "\n    divisible:" fileText
  file :: Value <- decodeThrow $ T.encodeUtf8 yamlCompatibleText

  let monkeys = fmap snd $ K.toList $ file ^?! #_Object

  pure $ IM.fromList $ [0 ..] `zip` fmap (\monkeyValue -> makeMonkey (monkeyValue ^?! #_Object)) monkeys
  where
    makeMonkey :: K.KeyMap Value -> Monkey
    makeMonkey monkeyObject =
      let startingItemsValue = fromJust (K.lookup "Starting items" monkeyObject)
          startingItems = case startingItemsValue of
            String str -> read . T.unpack <$> T.splitOn "," str
            Number s -> [truncate s]
          operationText = T.drop 10 $ fromJust (K.lookup "Operation" monkeyObject) ^?! #_String
          operation = case (T.take 1 operationText, T.drop 2 operationText) of
            ("+", number) -> Add $ read (T.unpack number)
            ("*", "old") -> Square
            ("*", number) -> Multiply $ read (T.unpack number)
          testObject = fromJust (K.lookup "Test" monkeyObject) ^?! #_Object
          testDivisible = truncate $ fromJust (K.lookup "divisible" testObject) ^?! #_Number
          ifTrue :: Int = read $ last $ words $ T.unpack $ fromJust (K.lookup "If true" testObject) ^?! #_String
          ifFalse :: Int = read $ last $ words $ T.unpack $ fromJust (K.lookup "If false" testObject) ^?! #_String
       in Monkey startingItems operation testDivisible ifTrue ifFalse
