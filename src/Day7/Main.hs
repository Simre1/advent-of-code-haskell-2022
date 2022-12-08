module Day7.Main where

import Data.List
import Data.List.Split (split, whenElt)
import Data.Text (Text, pack, strip)

data FileNode = File Text Int | Directory Text [FileNode] deriving (Show)

solution1 :: IO ()
solution1 = do
  files <- input
  let smallDirectories = flip findMatchingFileNodes files $ \case
        File _ _ -> False
        node@(Directory _ _) -> getSize node < 100000
  print $ sum $ getSize <$> smallDirectories

solution2 :: IO ()
solution2 = do
  files@(Directory _ nodes) <- input
  let usedSpace = getSize files
      freeSpace = availableSpace - usedSpace
      neededSpace = 30000000 - freeSpace
  let directoriesFreeingEnoughSpace = flip findMatchingFileNodes files $ \case
        File _ _ -> False
        node@(Directory _ _) -> getSize node >= neededSpace
  print $ getSize $ minimumBy (\f1 f2 -> getSize f1 `compare` getSize f2) directoriesFreeingEnoughSpace
  where
    availableSpace = 70000000

findMatchingFileNodes :: (FileNode -> Bool) -> FileNode -> [FileNode]
findMatchingFileNodes predicate node@(File _ _) = [node | predicate node]
findMatchingFileNodes predicate node@(Directory name files) =
  [node | predicate node] ++ findMatchingFileNodes predicate `concatMap` files

getSize :: FileNode -> Int
getSize (File _ size) = size
getSize (Directory _ fileNodes) = sum $ getSize <$> fileNodes

input :: IO FileNode
input = do
  file <- readFile "inputs/day7/input"
  pure $ parseCommandResponses $ splitCommands file

parseCommandResponses :: [(String, [String])] -> FileNode
parseCommandResponses commands = Directory "" $ fst $ go [] commands
  where
    go :: [FileNode] -> [(String, [String])] -> ([FileNode], [(String, [String])])
    go currentNodes [] = (currentNodes, [])
    go currentNodes ((command, response) : commands) =
      case (take 2 $ drop 2 command, strip $ pack $ drop 4 command) of
        ("cd", "/") -> go [] commands
        ("cd", "..") -> (currentNodes, commands)
        ("cd", dirName) ->
          let (fileNodes, nextCommands) = go [] commands
           in go (Directory dirName fileNodes : currentNodes) nextCommands
        ("ls", _) ->
          let files = (\[size, name] -> File (pack name) (read size)) . words <$> filter (\text -> take 3 text /= "dir") response
           in go (files ++ currentNodes) commands
        (command, _) -> error command

splitCommands :: String -> [(String, [String])]
splitCommands file =
  let lines' = lines file
      commands =
        split
          ( whenElt $ \case
              ('$' : _) -> True
              _ -> False
          )
          lines'
   in combineCommandsWithResponse commands
  where
    combineCommandsWithResponse [] = []
    combineCommandsWithResponse ([] : rest) = combineCommandsWithResponse rest
    combineCommandsWithResponse (command : response : rest) = (concat command, response) : combineCommandsWithResponse rest
