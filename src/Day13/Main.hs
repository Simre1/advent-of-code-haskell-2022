module Day13.Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Traversable (for)
import Data.Void
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as PL

data Packet = Single Int | Many [Packet] deriving (Show, Eq)

data Pair = Pair Packet Packet deriving (Show, Eq)

solution1 :: IO ()
solution1 = do
  pairs <- input1
  print $ countIndexSum pairs

solution2 :: IO ()
solution2 = do
  packets <- input2
  dividers <- makeDividers
  let sortedPackets = sort packets
      dividersWithIndex = filter (\(_, packet) -> packet `elem` dividers) $ zip [1 ..] sortedPackets
  print $ product $ fst <$> dividersWithIndex

countIndexSum :: [Pair] -> Int
countIndexSum pairs =
  sum $ fst <$> filter (hasRightOrder . snd) pairsWithIndices
  where
    pairsWithIndices = zip [1 ..] pairs

hasRightOrder :: Pair -> Bool
hasRightOrder (Pair l1 l2) = l1 < l2

instance Ord Packet where
  (Single i1) `compare` (Single i2) = i1 `compare` i2
  (Many l1) `compare` (Many l2) =
    let maybeOrder = find (/= EQ) (zipWith compare l1 l2)
     in fromMaybe (length l1 `compare` length l2) maybeOrder
  many@(Many _) `compare` single@(Single _) = many `compare` Many [single]
  single@(Single _) `compare` many@(Many _) = Many [single] `compare` many

input1 :: IO [Pair]
input1 = do
  file <- readFile "inputs/day13/input"
  let rawPairs = break (== '\n') <$> splitOn "\n\n" file
  for rawPairs $ \(packet1, packet2) -> do
    Pair <$> parsePacket packet1 <*> parsePacket (tail packet2)

input2 :: IO [Packet]
input2 = do
  pairs <- input1
  let packets = (\(Pair p1 p2) -> [p1, p2]) `concatMap` pairs
  dividers <- makeDividers
  pure $ packets ++ dividers

makeDividers :: IO [Packet]
makeDividers = traverse parsePacket ["[[6]]", "[[2]]"]

parsePacket :: String -> IO Packet
parsePacket str =
  either (fail . P.errorBundlePretty) pure $
    P.parse pairParser "pair parser" (pack str)

pairParser :: P.Parsec Void Text Packet
pairParser = do
  P.choice
    [ do
        lexeme $ P.char '['
        innerPairs <- manyWithDelimiter pairParser (lexeme $ P.char ',')
        lexeme $ P.char ']'
        pure $ Many innerPairs,
      Single <$> lexeme PL.decimal,
      pure (Many [])
    ]

manyWithDelimiter :: (Alternative p, Monad p) => p a -> p b -> p [a]
manyWithDelimiter parser delimiter =
  do
    a <- parser
    P.choice
      [ delimiter >> (a :) <$> manyWithDelimiter parser delimiter,
        pure [a]
      ]

lexeme :: P.Parsec Void Text a -> P.Parsec Void Text a
lexeme = PL.lexeme $ PL.space (P.space1 <|> void P.newline) empty empty