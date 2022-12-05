module Day5.Main where

import Control.Applicative ((<|>))
import Data.Coerce
import Data.Foldable
import Data.IntMap qualified as IM
import Data.List.Split
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

newtype Cargo = Cargo (IM.IntMap [Crate]) deriving (Show)

newtype Crate = Crate Char deriving (Show)

data Move = Move Int Int Int deriving (Show)

type Parser a = P.Parsec Void Text a

solution1 :: IO ()
solution1 = do
  (cargo, moves) <- input
  let expectedCargo = foldl' moveCrateOneByOne cargo moves
  print $ getMessage expectedCargo

solution2 :: IO ()
solution2 = do
  (cargo, moves) <- input
  let expectedCargo = foldl' moveMultipleCratesAtOnce cargo moves
  print $ getMessage expectedCargo

input :: IO (Cargo, [Move])
input = do
  file <- readFile "inputs/day5/input"
  let [cargoInput, moveInput] = splitOn "\n\n" file
      eitherStacks = P.parse parser "cargo input" $ T.pack cargoInput

  stacks <- either (fail . P.errorBundlePretty) pure eitherStacks
  let moves = parseMove <$> lines moveInput

  pure (stacks, moves)
  where
    parser :: Parser Cargo
    parser = parseField (Cargo IM.empty) 1
    parseField :: Cargo -> Int -> Parser Cargo
    parseField cargo position =
      P.choice
        [ P.string "    " *> parseField cargo (succ position),
          do
            P.char '['
            c <- P.satisfy (const True)
            P.char ']'
            P.char ' ' <|> P.lookAhead P.newline
            parseField (insert cargo position c) (succ position),
          P.newline *> parseField cargo 1,
          P.satisfy (const True) *> parseField cargo position,
          pure cargo
        ]
    insert (Cargo im) position c = Cargo $ IM.alter (pure . maybe [Crate c] (Crate c :)) position im

    parseMove :: String -> Move
    parseMove = (\[_, quantity, from, to] -> Move quantity from to) . fmap read . splitOnAnyOf ["move", "from", "to"]

moveCrateOneByOne :: Cargo -> Move -> Cargo
moveCrateOneByOne (Cargo im) (Move 0 from to) = Cargo im
moveCrateOneByOne (Cargo im) (Move quantity from to) =
  let crate = last $ im IM.! from
      im' = IM.update (pure . init) from im
      im'' = IM.alter (pure . maybe [crate] (\as -> as ++ [crate])) to im'
   in moveCrateOneByOne (Cargo im'') (Move (pred quantity) from to)

moveMultipleCratesAtOnce :: Cargo -> Move -> Cargo
moveMultipleCratesAtOnce (Cargo im) (Move quantity from to) =
  let fromCrates = im IM.! from
      leftOnStack = length fromCrates - quantity
      movedCrates = drop leftOnStack fromCrates
      im' = IM.update (pure . take leftOnStack) from im
      im'' = IM.alter (pure . maybe movedCrates (++ movedCrates)) to im'
   in Cargo im''

getMessage :: Cargo -> String
getMessage (Cargo im) = coerce . last . snd <$> IM.toAscList im

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds
