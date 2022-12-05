module Day4 where

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

part1 :: [String] -> String
part1 = show . length . filter (uncurry fullOverlap) . processLines

part2 :: [String] -> String
part2 = show . length . filter (uncurry overlap) . processLines
