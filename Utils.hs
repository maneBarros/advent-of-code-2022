module Utils where

import System.Environment
import Data.Bifunctor

replace :: Int -> a -> [a] -> [a]
replace index item l = pre ++ (item : pos)
    where (pre,_:pos) = splitAt index l

update :: (a -> a) -> Int -> [a] -> [a]
update f i l = prefix ++ (updatedItem : rest)
    where (prefix, item:rest) = splitAt i l
          updatedItem = f item

splitAndDiscardAt :: Int -> [a] -> ([a],[a])
splitAndDiscardAt i = second tail . splitAt i

getInput :: IO String
getInput = do 
    args <- getArgs
    if null args
        then getContents
        else readFile $ head args

getLines :: IO [String]
getLines = lines <$> getInput

split :: (a -> b) -> (a -> c) -> a -> (b,c)
split f g x = (f x, g x)
