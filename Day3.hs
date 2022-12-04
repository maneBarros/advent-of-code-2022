module Day3 where

import Data.List
import Data.Maybe 
import Data.Function

type Item = Char
type Priority = Int
type Rucksack = [Item]

priority :: Item -> Priority 
priority item = fromJust $ lookup item $ zip (['a'..'z'] ++ ['A'..'Z']) [1..]

repeatedItem :: Rucksack -> Item
repeatedItem rucksack = (\(c1,c2) -> sharedItem [c1,c2]) $ splitAt (length rucksack `div` 2) rucksack

sharedItem :: [[Item]] -> Item
sharedItem (r:rs) = fromJust $ find (\i -> all (i `elem`) rs) r 

part1 :: [String] -> String
part1 = show . sum . map (priority . repeatedItem)

-- Problema 2
groupInThrees :: [a] -> [[a]]
groupInThrees = map (map snd) . groupBy ((==) `on` (`div` 3) . fst) . zip [0..]

part2 :: [String] -> String
part2 = show . sum . map (priority . sharedItem) . groupInThrees

