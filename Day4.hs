module Main where

import Utils
import Data.Bifunctor

type Assignment = (Int,Int)

contains :: Assignment -> Assignment -> Bool
contains (x1,y1) (x2,y2) = x1 <= x2 && y1 >= y2 

fullOverlap :: Assignment -> Assignment -> Bool
fullOverlap a b = a `contains` b || b `contains` a

overlap :: Assignment -> Assignment -> Bool
overlap (x1,y1) (x2,y2) = any (`elem` [x2..y2]) [x1..y1]

readAssignment :: String -> Assignment
readAssignment = bimap read (read . tail) . break (== '-')

processLines :: [String] -> [(Assignment,Assignment)]
processLines = map $ bimap readAssignment (readAssignment . tail) . break (== ',')

part1 :: [(Assignment,Assignment)] -> Int
part1 = length . filter (uncurry fullOverlap)

part2 :: [(Assignment,Assignment)] -> Int
part2 = length . filter (uncurry overlap)

main = print . (part1 `split` part2) . processLines =<< getLines
