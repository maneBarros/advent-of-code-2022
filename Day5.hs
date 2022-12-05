module Day5 where

import Text.Read
import Data.Maybe 
import Data.List
import Data.Char
import Data.Bifunctor

type Stack = [Crate]
type Crate = Char
type Move = (Int,Int,Int)
type MoveFun = Int -> Stack -> Stack -> (Stack,Stack)

teste = "                [M]     [W] [M]    \n\
\            [L] [Q] [S] [C] [R]    \n\
\            [Q] [F] [F] [T] [N] [S]\n\
\    [N]     [V] [V] [H] [L] [J] [D]\n\
\    [D] [D] [W] [P] [G] [R] [D] [F]\n\
\[T] [T] [M] [G] [G] [Q] [N] [W] [L]\n\
\[Z] [H] [F] [J] [D] [Z] [S] [H] [Q]\n\
\[B] [V] [B] [T] [W] [V] [Z] [Z] [M]\n\
\ 1   2   3   4   5   6   7   8   9 "

teste2 = "    [D]    \n\
\[N] [C]    \n\
\[Z] [M] [P]\n\
\ 1   2   3 \n\
\\n\
\move 1 from 2 to 1\n\
\move 3 from 1 to 3\n\
\move 2 from 2 to 1\n\
\move 1 from 1 to 2"

move :: Int -> Stack -> Stack -> (Stack,Stack)
move 0 from to = (from,to)
move n (f:fs) to = move (n-1) fs (f : to)

move2 :: Int -> Stack -> Stack -> (Stack,Stack)
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
