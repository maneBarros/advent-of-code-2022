module Day6 where

import Utils
import Data.List
import Data.Bifunctor

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs

solve :: Int -> String -> Maybe Int
solve m = fmap length .
          find (not . hasDuplicates . take m . reverse) .
          dropWhile ((<m) . length) .
          inits

main = print . bimap f f . (part1 `split` part2) =<< getInput
    where f = maybe "No marker" show
          part1 = solve 4
          part2 = solve 14

