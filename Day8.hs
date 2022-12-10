module Main where

import Utils
import Data.Bifunctor
import Data.Char

type Forest = [[Height]]
type Height = Int
type Tree = (Int,Int)

isVisible :: Forest -> Tree -> Bool
isVisible forest (r,c) = isVisibleDirection c row || isVisibleDirection r column
    where row = forest !! r
          column = map (!! c) forest

isVisibleDirection :: Int -> [Height] -> Bool
isVisibleDirection i l = i == 0 || i == length l - 1 || any (all (< height)) [left,right]
    where height = l !! i
          (left,right) = splitAndDiscardAt i l

scenicScore :: Forest -> Tree -> Int
scenicScore forest (r,c) = vl * vr * vu * vd
    where (vl,vr) = viewingDistancesDirection c row
          (vu,vd) = viewingDistancesDirection r column
          row = forest !! r
          column = map (!! c) forest

viewingDistancesDirection :: Int -> [Height] -> (Int,Int)
viewingDistancesDirection i l = bimap length length (viewing height (reverse left),viewing height right)
    where height = l !! i
          (left,right) = second tail (splitAt i l)
          viewing _ [] = []
          viewing h (x:xs) | h > x = x : viewing h xs
                           | otherwise = [x]

allTrees :: Forest -> [Tree]
allTrees forest = [ (r,c) | r <- [0..(length forest - 1)], c <- [0..(length (head forest) - 1)] ] 

part1 :: Forest -> Int
part1 forest = length $ filter (isVisible forest) (allTrees forest)

part2 :: Forest -> Int
part2 forest = maximum $ map (scenicScore forest) (allTrees forest)

main = print . (part1 `split` part2 ) . map (map digitToInt) =<< getLines

