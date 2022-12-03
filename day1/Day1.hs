module Day1 where

import Data.List
import System.Environment

getElvesCalories :: String -> [Int]
getElvesCalories = 
    map (sum . map read) .
    filter (not . null . head) . 
    groupBy (\l1 l2 -> null l1 == null l2) . 
    lines 

problem1 :: [Int] -> Int
problem1 = maximum

problem2 :: [Int] -> Int
problem2 = sum . take 3 . sortOn (\x -> (-x)) 

main :: IO ()
main = print . problem2 . getElvesCalories =<< readFile . head =<< getArgs
