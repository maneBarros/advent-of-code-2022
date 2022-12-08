module Utils where

replace :: Int -> a -> [a] -> [a]
replace index item l = pre ++ (item : pos)
    where (pre,_:pos) = splitAt index l
