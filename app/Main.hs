module Main where

import qualified Day1.Main as D1
import qualified Day2.Main as D2
import qualified Day3.Main as D3
import qualified Day4.Main as D4
import qualified Day5.Main as D5
import qualified Day6.Main as D6
import qualified Day7.Main as D7
import qualified Day8.Main as D8
import qualified Day9.Main as D9
import qualified Day10.Main as D10
import qualified Day11.Main as D11
import qualified Day12.Main as D12
import qualified Day13.Main as D13
import qualified Day14.Main as D14
import qualified Day15.Main as D15
import qualified Day16.Main as D16
import qualified Day17.Main as D17
import qualified Day18.Main as D18
import qualified Day19.Main as D19
import qualified Day20.Main as D20
import qualified Day21.Main as D21
import qualified Day22.Main as D22
import qualified Day23.Main as D23
import qualified Day24.Main as D24
import qualified Day25.Main as D25

import System.Environment ( getArgs )

main :: IO ()
main = do
  [arg] <- getArgs
  case arg of 
    "1" -> D1.solution1 >> D1.solution2
    "2" -> D2.solution1 >> D2.solution2
    "3" -> D3.solution1 >> D3.solution2
    "4" -> D4.solution1 >> D4.solution2
    "5" -> D5.solution1 >> D5.solution2
    "6" -> D6.solution1 >> D6.solution2
    "7" -> D7.solution1 >> D7.solution2
    "8" -> D8.solution1 >> D8.solution2
    "9" -> D9.solution1 >> D9.solution2
    "10" -> D10.solution1 >> D10.solution2
    "11" -> D11.solution1 >> D11.solution2
    "12" -> D12.solution1 >> D12.solution2
    "13" -> D13.solution1 >> D13.solution2
    "14" -> D14.solution1 >> D14.solution2
    "15" -> D15.solution1 >> D15.solution2
    "16" -> D16.solution1 >> D16.solution2
    "17" -> D17.solution1 >> D17.solution2
    "18" -> D18.solution1 >> D18.solution2
    "19" -> D19.solution1 >> D19.solution2
    "20" -> D20.solution1 >> D20.solution2
    "21" -> D21.solution1 >> D21.solution2
    "22" -> D22.solution1 >> D22.solution2
    "23" -> D23.solution1 >> D23.solution2
    "24" -> D24.solution1 >> D24.solution2
    "25" -> D25.solution1 >> D25.solution2
    _ -> print "No puzzle matched"


