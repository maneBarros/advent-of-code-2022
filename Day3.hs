module Main where

import Data.List
import Data.Maybe 
import Data.Function
import Data.Bifunctor
import Utils

type Item = Char
type Priority = Int
type Rucksack = [Item]

priority :: Item -> Priority 
priority item = fromJust $ lookup item $ zip (['a'..'z'] ++ ['A'..'Z']) [1..]

repeatedItem :: Rucksack -> Item
repeatedItem rucksack = (\(c1,c2) -> sharedItem [c1,c2]) $ splitAt (length rucksack `div` 2) rucksack

sharedItem :: [[Item]] -> Item
sharedItem (r:rs) = fromJust $ find (\i -> all (i `elem`) rs) r 

groupInThrees :: [a] -> [[a]]
groupInThrees = map (map snd) . groupBy ((==) `on` (`div` 3) . fst) . zip [0..]

part1 :: [String] -> Int
part1 = sum . map (priority . repeatedItem)

part2 :: [String] -> Int
part2 = sum . map (priority . sharedItem) . groupInThrees

main = print . (part1 `split` part2) =<< getLines
