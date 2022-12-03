module Day2 where

import System.Environment

data Move = Rock | Paper | Scissors deriving Eq
data Result = Win | Loss | Draw deriving Eq

rules :: [(Move,Move)]
rules = [(Rock,Scissors), (Paper,Rock), (Scissors,Paper)]

losesTo :: Move -> Move
losesTo m = let Just r = lookup m rules
             in r

defeats :: Move -> Move
defeats m = let Just r = lookup m $ map (\(w,l) -> (l,w)) rules
             in r 

roundScore :: Move -> Move -> Int
roundScore opp me = calculateScore me $ roundResult opp me

roundResult :: Move -> Move -> Result
roundResult opponent me | opponent == me = Draw
                        | losesTo opponent == me = Loss
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

processContent :: String -> [(Move,Move)]
processContent = map ((\[o,m] -> (o,m)) . map strToMove . words) . lines

strToMove :: String -> Move
strToMove s | s `elem` ["X", "A"] = Rock
            | s `elem` ["Y", "B"] = Paper
            | otherwise = Scissors

problem1 :: IO ()
problem1 = print . sum . map (uncurry roundScore) . processContent =<< getContents

-- Problem 2

strToResult :: String -> Result
strToResult "X" = Loss
strToResult "Y" = Draw
strToResult "Z" = Win

chooseMove :: Move -> Result -> Move
chooseMove m Draw = m
chooseMove m Loss = losesTo m
chooseMove m Win = defeats m

processContent2 :: String -> [(Move,Move)]
processContent2 = map ((\(o,r) -> (o, chooseMove o r)) . (\[o,r] -> (strToMove o, strToResult r)) . words) . lines

problem2 :: IO ()
problem2 = print . sum . map (uncurry roundScore) . processContent2 =<< getContents

main = problem2

