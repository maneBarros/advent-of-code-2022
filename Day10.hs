import Utils
import Data.List
import Data.Function

data Instruction = Add Int
                 | Nop

type Register = Int
type Cycle = Int
type Pixel = (Int,Int)

readInstruction :: String -> Instruction
readInstruction "noop" = Nop
readInstruction add = Add $ read $ last $ words add

processInstruction :: Instruction -> (Register,[Register]) -> (Register,[Register])
processInstruction Nop (register,history) = (register,register:history)
processInstruction (Add v) (register,history) = (register + v, replicate 2 register ++ history)

buildHistory :: [Instruction] -> [(Cycle,Register)]
buildHistory = zip [1..] . reverse . uncurry (:) . foldr processInstruction (1,[])

part1 :: [Instruction] -> Int
part1 = sum . map signalStrength . filter ((`elem` [20,60..220]) . fst) . buildHistory
    where signalStrength = uncurry (*)

paints :: Int -> Int -> Char
paints sprite pixel | pixel `elem` [sprite-1,sprite,sprite+1] = '#'
                    | otherwise = '.'

screen :: [(Register,Pixel)] -> [String]
screen = map (map (\(m,(_,p)) -> paints m p)) . groupBy ((==) `on` fst . snd)

part2 :: [Instruction] -> String
part2 = intercalate "\n" . screen . (`zip` pixels) . map snd . buildHistory
    where pixels = [ (r,c) | r <- [0..5], c <- [0..39] ]

main = putStrLn . part2 . map readInstruction . reverse =<< getLines
