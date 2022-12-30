import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Char
import Utils
import Data.Bifunctor

type Position = (Int,Int)
type Path = [Position]
type Graph a = Map.Map a [a]

test = "Sabqponm\n\
\abcryxxl\n\
\accszExk\n\
\acctuvwj\n\
\abdefghi"

-- Parsing
turnToGraph :: Map.Map Position Int -> Graph Position
turnToGraph intMap = Map.mapWithKey calculateNeighbors intMap
    where calculateNeighbors position localHeight = filter (\p -> p `Map.member` intMap && fromJust (Map.lookup p intMap) <= localHeight + 1) (adjacents position)
          adjacents (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

flattenWithPositions :: [[Char]] -> [(Position,Char)]
flattenWithPositions input = zip positions (concat input) 
    where positions = [ (r,c) | r <- [0..length input - 1], c <- [0..length (head input) - 1] ]

turnToIntMap :: [(Position,Char)] -> Map.Map Position Int
turnToIntMap = Map.fromList . map (second charToInt) 
    where charToInt c | c == 'S' = ord 'a'
                      | c == 'E' = ord 'z'
                      | otherwise = ord c

startAndEnd :: [(Position,Char)] -> (Position,Position)
startAndEnd = bimap fst fst .
              ((fromJust . find ((== 'S') . snd)) `split` (fromJust . find ((== 'E') . snd)))

process :: [String] -> (Graph Position,Position,Position)
process lines = (turnToGraph $ turnToIntMap positions, start, end)
    where positions = flattenWithPositions lines
          (start,end) = startAndEnd positions

-- Solving
shortestPath :: Graph Position -> Position -> Position -> Maybe [Position]
shortestPath graph start end = buildPath end (breadthFirst graph start) []

breadthFirst :: Graph Position -> Position -> Map.Map Position (Maybe Position)
breadthFirst graph start = bfAux graph (Map.fromList [(start,Nothing)]) [start]

bfAux :: Graph Position -> Map.Map Position (Maybe Position) -> [Position] -> Map.Map Position (Maybe Position)
bfAux graph visited [] = visited
bfAux graph visited (next:q) = let news = filter (`Map.notMember` visited) $ Map.findWithDefault [] next graph
                                   newVisited = Map.union visited $ Map.fromList $ zip news (repeat (Just next))
                                in bfAux graph newVisited (q ++ news)

buildPath :: Position -> Map.Map Position (Maybe Position) -> [Position] -> Maybe [Position]
buildPath current visited built = do parent <- Map.lookup current visited 
                                     maybe (return $ current : built) (\p -> buildPath p visited (current:built)) parent

-- Testing
graph1 :: Graph Position
graph1 = Map.fromList
    [
        ((0,0), [(1,0), (2,0)]),
        ((1,0), [(3,0)]),
        ((2,0), [(4,0), (5,0)]),
        ((3,0), [(5,0)]),
        ((4,0), []),
        ((5,0), [(1,0)])
    ]

part2 :: [[Char]] -> Maybe Int 
part2 lines = let positions = flattenWithPositions lines
                  allStarts = map fst $ filter ((`elem` "aS") . snd) positions
                  (graph,_,end) = process lines
               in (\l -> if null l then Nothing else Just (minimum l)) $ mapMaybe (\start -> length . tail <$> shortestPath graph start end) allStarts 

part1 :: [[Char]] -> Maybe Int
part1 lines = let (graph,start,end) = process lines
               in length . tail <$> shortestPath graph start end

main = print . (part1 `split` part2) =<< getLines
