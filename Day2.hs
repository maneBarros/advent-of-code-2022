module Day2 where

import System.Environment
import Data.Maybe

data Move = Rock | Paper | Scissors deriving Eq
data Result = Win | Loss | Draw deriving Eq

rules :: [(Move,Move)]
rules = [(Rock,Scissors), (Paper,Rock), (Scissors,Paper)]

losesTo :: Move -> Move
losesTo = fromJust . (`lookup` rules)

defeats :: Move -> Move
defeats = fromJust . (`lookup` invertedRules)
    where invertedRules = map (\(w,l) -> (l,w)) rules 

roundScore :: Move -> Move -> Int
roundScore opponent me = calculateScore me $ roundResult opponent me

roundResult :: Move -> Move -> Result
roundResult opponent me | opponent == me = Draw
                        | me == losesTo opponent = Loss
                        | otherwise = Win

shapeScore :: Move -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

resultScore :: Result -> Int
resultScore Win = 6
resultScore Loss = 0
resultScore Draw = 3

calculateScore :: Move -> Result -> Int
calculateScore move result = resultScore result + shapeScore move

processContent :: [String] -> [(Move,Move)]
processContent = map ((\[o,m] -> (o,m)) . map readMove . words)

readMove :: String -> Move
readMove s | s `elem` ["X", "A"] = Rock
           | s `elem` ["Y", "B"] = Paper
           | otherwise = Scissors

part1 :: [String] -> String
part1 = show . sum . map (uncurry roundScore) . processContent

-- Problem 2

readResult :: String -> Result
readResult "X" = Loss
readResult "Y" = Draw
readResult "Z" = Win

chooseMove :: Move -> Result -> Move
chooseMove m Draw = m
chooseMove m Loss = losesTo m
chooseMove m Win = defeats m

processContent2 :: [String] -> [(Move,Move)]
processContent2 = map $ (\(o,r) -> (o, chooseMove o r)) . (\[o,r] -> (readMove o, readResult r)) . words

part2 :: [String] -> String
part2 = show . sum . map (uncurry roundScore) . processContent2
