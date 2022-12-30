import Data.Function
import Utils
import Data.Char
import Data.Bifunctor
import Data.List
import Data.List.Split

data Value = List [Value]
           | Scalar Int
           deriving Show

instance Eq Value where
    Scalar v1 == Scalar v2 = v1 == v2
    List v1 == List v2 = v1 == v2
    Scalar v1 == v2 = List [Scalar v1] == v2
    v1 == v2 = v1 == List [v2]

instance Ord Value where
    Scalar v1 `compare` Scalar v2 = v1 `compare` v2
    List v1 `compare` List v2 = v1 `compare` v2
    Scalar v1 `compare` v2 = List [Scalar v1] `compare` v2
    v1 `compare` v2 = v1 `compare` List [v2]

getTokens :: String -> [String]
getTokens = groupBy (\c1 c2 -> isDigit c1 && isDigit c2)

readValue :: String -> Value
readValue str = let (t:ts) = getTokens str
                 in case t of "[" -> List $ fst $ processList ts
                              v  -> Scalar (read v)

processList :: [String] -> ([Value],[String])
processList ("]":rest) = ([],rest)
processList (",":rest) = processList rest
processList ("[":rest) = let (items,rest2) = processList rest 
                          in first (List items :) $ processList rest2
processList (i:rest) = first (Scalar (read i) :) $ processList rest


listOfValues :: [String] -> [Value]
listOfValues = map readValue . filter (not . null)

-- Solving
part1 :: [String] -> Int
part1 = sum
      . map fst
      . filter snd
      . zip [1..]
      . map (\[s1,s2] -> readValue s1 <= readValue s2) 
      . filter (not . null . head)
      . groupBy ((==) `on` null)

part2 :: [String] -> Int
part2 = product 
      . map fst
      . filter ((`elem` [divider1,divider2]) . snd)
      . zip [1..]
      . sort
      . ([divider1,divider2] ++)
      . listOfValues
    where 
    divider1 = List [List [Scalar 2]]
    divider2 = List [List [Scalar 6]]

main = print . (part1 `Utils.split` part2) =<< getLines



