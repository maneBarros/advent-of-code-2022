import Utils
import Data.Bifunctor

data Direction = U | D | L | R deriving (Show,Read,Eq)
type Position = (Int,Int)

-- Parsing
readMoves :: [String] -> [Direction]
readMoves = concatMap readMove

readMove :: String -> [Direction]
readMove s = replicate times move
    where (move,times) = (read m, read n) 
          [m,n] = words s

-- Solving
updatePosition :: Position -> Position -> Position
updatePosition (xh,yh) (xt,yt) | xt `elem` [xh-1..xh+1] && yt `elem` [yh-1..yh+1] = (xt,yt)
                               | xt == xh = (xt, if yh > yt then yt+1 else yt-1)
                               | yt == yh = (if xh > xt then xt+1 else xt-1, yt)
                               | otherwise = (if xh > xt then xt+1 else xt-1,if yh > yt then yt+1 else yt-1)

move :: Direction -> Position -> Position
move U (x,y) = (x,y+1) 
move D (x,y) = (x,y-1)
move L (x,y) = (x-1,y)
move R (x,y) = (x+1,y)

moveHead :: ([Position],[Position]) -> Direction -> ([Position],[Position])
moveHead (rope,history) d = (newRope,newHistory)
    where newH = move d (last rope)
          newRope = foldr (\old (u:us) -> updatePosition u old:u:us) [newH] (init rope)
          newHistory = if head newRope `elem` history then history else head newRope : history

visitedByTail :: ([Position],[Position]) -> [Direction] -> Int
visitedByTail st = length . snd . foldl moveHead st

main = print . 
       (visitedByTail rope1 `split` visitedByTail rope2) .
       readMoves =<< getLines
       where 
       rope1 = (replicate 2 origin, [origin])
       rope2 = (replicate 10 origin, [origin])
       origin = (0,0)
