module Day6 where

import Data.List

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs

solve :: Int -> [String] -> String
solve m = maybe "No marker" (show . length) .
    find (not . hasDuplicates . take m . reverse) .
    dropWhile ((<m) . length) .
    inits .
    head

part1 = solve 4
part2 = solve 14

