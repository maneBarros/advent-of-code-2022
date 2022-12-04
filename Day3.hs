module Day3 where

import Data.List
import Control.Monad
import Data.Maybe 

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
groupInThrees = map (map snd) . groupBy (\(i1,_) (i2,_) -> i1 `div` 3 == i2 `div` 3) . zip [0..]

part2 :: [String] -> String
part2 = show . sum . map (priority . sharedItem) . groupInThrees

