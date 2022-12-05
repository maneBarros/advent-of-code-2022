module Day1 where

import Data.List
import Data.Function

getElvesCalories :: [String] -> [Int]
getElvesCalories = 
    map (sum . map read) .
    filter (not . null . head) . 
    groupBy ((==) `on` null)

part1 :: [String] -> String
part1 = show . maximum . getElvesCalories

part2 :: [String] -> String
part2 = show . sum . take 3 . sortOn ((-1)*) . getElvesCalories 
