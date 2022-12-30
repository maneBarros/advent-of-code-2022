module Main where

import Utils
import Data.List.Split
import Data.List
import Data.Function

type Coordinates = (Int,Int,Int)
type Droplet = Coordinates

readDroplets :: [String] -> [Droplet]
readDroplets = map readDroplet
    where readDroplet = (\[x,y,z] -> (x,y,z)) . map read . splitOn ","

test :: [Droplet]
test = readDroplets $ lines dropletsText
    where dropletsText = 
            "2,2,2\n\
            \1,2,2\n\
            \3,2,2\n\
            \2,1,2\n\
            \2,3,2\n\
            \2,2,1\n\
            \2,2,3\n\
            \2,2,4\n\
            \2,2,6\n\
            \1,2,5\n\
            \3,2,5\n\
            \2,1,5\n\
            \2,3,5"

faces :: Droplet -> [Coordinates]
faces (x,y,z) = [
    (x+1,y,z),
    (x-1,y,z),
    (x,y+1,z),
    (x,y-1,z),
    (x,y,z+1),
    (x,y,z-1)
    ]

exposedFaces :: [Coordinates] -> [Droplet] -> [Coordinates]
exposedFaces faces droplets = filter (`notElem` droplets) faces

airPockets :: [Droplet] -> [Droplet]
airPockets = 
    map (fst . head) .
    filter ((==6) . length) .
    groupBy ((==) `on` fst) .
    sort . 
    concatMap (\d -> zip (faces d) (repeat d))

part1 :: [Droplet] -> Int
part1 droplets = let allFaces = concatMap faces droplets
                  in length $ exposedFaces allFaces droplets

part2 :: [Droplet] -> Int
part2 droplets = let allFaces = concatMap faces droplets
                     pockets = filter (`notElem` droplets) $ airPockets droplets
                  in length $ filter (`notElem` pockets) $ exposedFaces allFaces droplets

main = print . (part1 `Utils.split` part2) . readDroplets =<< getLines
