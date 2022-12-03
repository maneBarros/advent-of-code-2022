module Day3 where

import Data.List
import Control.Monad
import Data.Maybe 

type Item = Char
type Priority = Int
type Rucksack = [Item]

priority :: Item -> Maybe Priority 
priority item = lookup item $ zip (['a'..'z'] ++ ['A'..'Z']) [1..]

repeatedItem :: Rucksack -> Maybe Item
repeatedItem rucksack = (\(c1,c2) -> sharedItem [c1,c2]) $ splitAt (length rucksack `div` 2) rucksack

sharedItem :: [[Item]] -> Maybe Item
sharedItem [] = Nothing
sharedItem (r:rs) = find (\i -> all (i `elem`) rs) r 

problem1 :: IO ()
problem1 = print . fmap sum . mapM (priority <=< repeatedItem) . lines =<< getContents

-- Problema 2
groupInThrees :: [a] -> [[a]]
groupInThrees = map (map snd) . groupBy (\x y -> fst x `div` 3 == fst y `div` 3) . zip [0..]

problem2 :: IO ()
problem2 = print . sum . mapMaybe (priority <=< sharedItem) . groupInThrees . lines =<< getContents

main = problem2
