module Day5 where

import Text.Read
import Data.Maybe 
import Data.List
import Data.Char
import Data.Bifunctor

type Stack = [Char]
type Move = (Int,Int,Int)
type MoveFun = Int -> Stack -> Stack -> (Stack,Stack)

move :: MoveFun
move 0 from to = (from,to)
move n (f:fs) to = move (n-1) fs (f : to)

move2 :: MoveFun
move2 n from to = (b, a ++ to)
    where (a,b) = splitAt n from

replace :: Int -> a -> [a] -> [a]
replace index item l = pre ++ (item : pos)
    where (pre,_:pos) = splitAt index l

applyMove :: MoveFun -> [Stack] -> Move -> [Stack]
applyMove f stacks (quantity,from,to) = replace (to-1) to2 $ replace (from-1) from2 stacks 
    where (from2,to2) = f quantity (stacks !! (from-1)) (stacks !! (to-1))

applyMoves :: MoveFun -> [Stack] -> [Move] -> [Stack]
applyMoves f = foldl $ applyMove f

readMove :: String -> Move
readMove = (\[quant,from,to] -> (quant,from,to)) . mapMaybe (readMaybe :: String -> Maybe Int) . words

readStacks :: [String] -> [Stack]
readStacks = map (dropWhile isSpace . init) . filter (isDigit . last) . transpose

solve :: MoveFun -> [String] -> String
solve f = map head . uncurry (applyMoves f) . bimap readStacks (map readMove . tail) . break null

part1 = solve move
part2 = solve move2

