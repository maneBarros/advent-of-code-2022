import Utils
import Data.List
import Data.Function
import Data.Bifunctor (Bifunctor(bimap))
import Numeric.Natural

type WorryLevel = Int

data Monkey = Monkey {
    items :: [WorryLevel],
    inspections :: Int,
    operation :: WorryLevel -> WorryLevel,
    divisor :: Int,
    next :: (Int,Int)
    }

instance Show Monkey where
    show m = intercalate ", " $ map show (items m)

showMonkeys :: [Monkey] -> String
showMonkeys = intercalate "\n" . zipWith (\i m -> "Monkey " ++ show i ++ ": " ++ show m) [0..]

showInspections :: [Monkey] -> String
showInspections = intercalate "\n" . zipWith (\i m -> "Monkey " ++ show i ++ " inspected items " ++ show (inspections m) ++ " times.") [0..]

teste :: [Monkey]
teste = [
    Monkey [79,98] 0 (*19) 23 (2, 3),
    Monkey [54,65,75,74] 0 (+6) 19 (2,0),
    Monkey [79,60,97] 0 (^2) 13 (1,3),
    Monkey [74] 0 (+3) 17 (0,1)
    ]

test :: Int -> (Int,Int) -> Int -> Int
test d (m1,m2) wl = if (wl `mod` d) == 0 then m1 else m2

-- Parsing
readMonkeys :: [String] -> [Monkey]
readMonkeys = map readMonkey . 
              filter ((>1) . length) .
              groupBy ((==) `on` null)

readMonkey :: [String] -> Monkey
readMonkey monkeyStr = 
    Monkey { 
        items = readItems itemsStr, 
        inspections = 0,
        operation = readOperation operationStr,
        divisor = divisor,
        next = next
    }
    where itemsStr = monkeyStr !! 1
          operationStr = monkeyStr !! 2
          testStrs = drop 3 monkeyStr
          (divisor,next) = readTest testStrs

readItems :: String -> [WorryLevel]
readItems = map (read . \x -> if last x == ',' then init x else x) . drop 2 . words

readOperation :: String -> (WorryLevel -> WorryLevel)
readOperation str w = w `op` arg
    where op = case opStr of { "*" -> (*); "+" -> (+) }
          arg = case argStr of { "old" -> w; n -> read n }
          [opStr,argStr] = drop 4 $ words str

readTest :: [String] -> (Int,(Int,Int))
readTest strs = let [n,t,f] = map (read . last . words) strs
                 in (n,(t,f))

-- Solving
round :: Int 
      -> Bool -- Determines if worry level is divided by 3 or not
      -> [Monkey] -> [Monkey]
round mmc b monkeys = foldl (turn mmc b) monkeys [0..length monkeys - 1]

addItem :: WorryLevel -> Monkey -> Monkey
addItem i m = m { items = items m ++ [i] }

updateThrower :: Monkey -> Monkey
updateThrower m = m { items = [], inspections = inspections m + length (items m) }

turn :: Int 
     -> Bool -- Determines if worry level is divided by 3 or not
     -> [Monkey] -> Int -> [Monkey]
turn mmc b monkeys i = let Monkey items _ operation divisorM next = monkeys !! i
                           throws = foldl (\ths w -> processItem b w operation (test divisorM next) : ths) [] items
                           updatedReceivers = foldr (\(w,m) -> update (addItem (w `mod` mmc)) m) monkeys throws
                        in update updateThrower i updatedReceivers

processItem :: Bool -- Determines if worry level is divided by 3 or not
            -> WorryLevel -> (WorryLevel -> WorryLevel) -> (WorryLevel -> Int) -> (WorryLevel,Int)
processItem b worry operation test = let worry2 = operation worry
                                         worry3 = if b then floor (fromIntegral worry2 / 3) else worry2
                                         nextMonkey = test worry3
                                      in (worry3,nextMonkey)

afterRound :: Bool -> Int -> [Monkey] -> [Monkey]
afterRound b n monkis = let mmc = mcm $ map divisor monkis 
                         in (!! n) $ iterate (Main.round mmc b) monkis

part1 :: [Monkey] -> Int
part1 = product . take 2 . reverse . sort . map inspections . afterRound True 20

part2 :: [Monkey] -> Int
part2 = product . take 2 . reverse . sort . map inspections . afterRound False 10000

main = print . part2 . readMonkeys =<< getLines
