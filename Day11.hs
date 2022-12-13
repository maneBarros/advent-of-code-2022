import Utils
import Data.List

data Monkey = Monkey {
    items :: [WorryLevel],
    inspections :: Int,
    operation :: WorryLevel -> WorryLevel,
    test :: WorryLevel -> Int
    }

type WorryLevel = Int

instance Show Monkey where
    show m = intercalate ", " $ map show (items m)

showMonkeys :: [Monkey] -> String
showMonkeys = intercalate "\n" . zipWith (\i m -> "Monkey " ++ show i ++ ": " ++ show m) [0..]

showInspections :: [Monkey] -> String
showInspections = intercalate "\n" . zipWith (\i m -> "Monkey " ++ show i ++ " inspected items " ++ show (inspections m) ++ " times.") [0..]

teste :: [Monkey]
teste = [
    Monkey [79,98] 0 (*19) (\x -> if x `mod` 23 == 0 then 2 else 3),
    Monkey [54,65,75,74] 0 (+6) (\x -> if x `mod` 19 == 0 then 2 else 0),
    Monkey [79,60,97] 0 (^2) (\x -> if x `mod` 13 == 0 then 1 else 3),
    Monkey [74] 0 (+3) (\x -> if x `mod` 17 == 0 then 0 else 1)
    ]

round :: [Monkey] -> [Monkey]
round monkeys = foldl turn monkeys [0..length monkeys - 1]

addItem :: WorryLevel -> Monkey -> Monkey
addItem i m = m { items = items m ++ [i] }

updateThrower :: Monkey -> Monkey
updateThrower m = m { items = [], inspections = inspections m + length (items m) }

turn :: [Monkey] -> Int -> [Monkey]
turn monkeys i = let Monkey items _ operation test = monkeys !! i
                     throws = foldl (\ths w -> processItem w operation test : ths) [] items
                     updatedReceivers = foldr (\(w,m) -> update (addItem w) m) monkeys throws
                  in update updateThrower i updatedReceivers

processItem :: WorryLevel -> (Int -> Int) -> (Int -> Int) -> (WorryLevel,Int)
processItem worry operation test = let worry2 = operation worry
                                       worry3 = floor (fromIntegral worry2 / 3)
                                       nextMonkey = test worry3
                                    in (worry3,nextMonkey)

afterRoundN :: Int -> [Monkey] -> [Monkey]
afterRoundN n = (!! n) . iterate Main.round

part1 :: [Monkey] -> Int
part1 = product . take 2 . reverse . sort . map inspections . afterRoundN 20
