module Main where

import System.Environment
import Day1
import Day2
import Day3
import Day4
import Day5

type Solution = [String] -> String
type Day = Int

solutions :: [(Day, (Solution, Solution))]
solutions = zip [1..] [
    (Day1.part1, Day1.part2),
    (Day2.part1, Day2.part2),
    (Day3.part1, Day3.part2),
    (Day4.part1, Day4.part2),
    (Day5.part1, Day5.part2)
    ]

main :: IO ()
main = do
    args <- getArgs
    let day = read $ head args
        part = read $ args !! 1
        hasInputFile = length args == 3
    content <- if hasInputFile then readFile (args !! 2) else getContents 
    let solution = (if part == 1 then fst else snd) <$> lookup day solutions
    putStrLn $ maybe "No solution for this day yet" ($ lines content) solution

    
