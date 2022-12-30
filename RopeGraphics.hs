module Main where

import Utils
import Day9
import Graphics.Gloss
import Data.Bifunctor (Bifunctor(bimap))

type Rope = [(Int,Int)]

data State = State {
    prev :: Rope,
    next :: Rope,
    passedTime :: Float,
    directions :: [Direction]
    }

radius :: Float
radius = 15

spacement :: Float
spacement = 20

period :: Float
period = 0.1

vector :: Position -> Position -> (Float,Float)
vector (xi,yi) (xf,yf) = (fromIntegral (xf-xi), fromIntegral (yf-yi))

vectorScale :: Float -> (Float,Float) -> (Float,Float)
vectorScale f = bimap (*f) (*f)

knotPicture :: Int -> Picture
knotPicture index = Color (iterate dim blue !! index) $ circleSolid radius

drawKnot :: Int -> (Float,Float) -> Picture
drawKnot index (x,y) = Translate (x*spacement) (y*spacement) $ knotPicture index

drawRope :: [(Float,Float)] -> Picture
drawRope = Pictures . zipWith drawKnot [0..]

applyVectors :: [(Float,Float)] -> [(Int,Int)] -> [(Float,Float)]
applyVectors vectors = zipWith (\(x1,y1) (x2,y2) -> (x1+x2,y1+y2)) vectors . map (bimap fromIntegral fromIntegral) 

animation :: Float -> Rope -> Rope -> Picture
animation percent initial final = drawRope $ applyVectors vectors initial
    where vectors = map (vectorScale percent) $ zipWith vector initial final

drawState :: State -> Picture
drawState s = animation offset (prev s) (next s)
    where offset = passedTime s / period

changeState :: State -> State
changeState s | null (directions s) = s
              | otherwise = let State prev next passed (d:ds) = s
                                nnext = fst $ moveHead (next,[]) d
                             in State next nnext 0 ds

update :: Float -> State -> State
update t s@(State prev next passed dirs) | passed + t >= period = changeState s
                                         | otherwise = s { passedTime = passedTime s + t }

mydirections :: [Direction]
mydirections = take 1000 $ cycle $ replicate 10 U ++ replicate 20 D ++ replicate 10 U

myrope :: Rope
myrope = zip [-5,-4..5] (repeat 0)

main = do
    lines <- getLines
    let disp = InWindow "Rope motions" (600,600) (1200,300)
        (d:ds) = readMoves lines
        rope1 = replicate 10 origin
        rope2 = fst $ moveHead (rope1,[]) d
        origin = (0,0)
    simulate disp white 60 (State rope1 rope2 0 ds) drawState (const Main.update)
