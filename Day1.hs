module Day1 where

import Data.List

getElvesCalories :: [String] -> [Int]
getElvesCalories = 
    map (sum . map read) .
    filter (not . null . head) . 
    groupBy (\l1 l2 -> null l1 == null l2) 

part1 :: [String] -> String
part1 = show . maximum . getElvesCalories

part2 :: [String] -> String
part2 = show . sum . take 3 . sortOn (\x -> (-x)) . getElvesCalories 
