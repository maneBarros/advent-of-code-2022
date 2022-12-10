module Main where

import Utils
import Data.List
import Data.Function

getElvesCalories :: [String] -> [Int]
getElvesCalories = 
    map (sum . map read) .
    filter (not . null . head) . 
    groupBy ((==) `on` null)

main = print . (part1 `split` part2) . getElvesCalories =<< getLines
    where part1 = maximum
          part2 = sum . take 3 . sortOn ((-1)*)
