module Day7 where

import Data.Maybe
import Data.List
import Utils
import Data.Bifunctor

data File = File { name :: String, filesize :: Int }
          | Directory { name :: String, contents :: [File] }

type Path = [String]
type InvertedPath = Path
type Filesystem = File

data Command = Cd String | Ls [File]

instance Show File where
    show (File n s) = "- " ++ n ++ " (file, size=" ++ show s ++ ")\n"
    show (Directory n cs) = "- " ++ n ++ " (dir)\n" ++ concatMap (unlines . map ("  "++) . lines . show) cs

isDir :: File -> Bool
isDir (Directory _ _) = True
isDir _ = False

-- Parsing
--
readFilesystem :: [String] -> Filesystem
readFilesystem = fst .
                 applyCommands (Directory "/" [], []) .
                 map readCommand .
                 groupBy (\_ l -> head l /= '$') 

readCommand :: [String] -> Command
readCommand (command:output) = let ws = words command
                                in case ws !! 1 of "cd" -> Cd $ ws !! 2 
                                                   "ls" -> Ls $ map readEntry output

readEntry :: String -> File
readEntry str = let [w1,w2] = words str
                in case w1 of "dir" -> Directory w2 []
                              _ -> File w2 (read w1)

-- Solving
--
size :: File -> Int
size (File _ s) = s
size (Directory _ c) = sum $ map size c

changeDirectory :: String -> Path -> Path
changeDirectory ".." = tail
changeDirectory "/" = const []
changeDirectory x = (x:)

updateDirectory :: Filesystem -> InvertedPath -> [File] -> Filesystem
updateDirectory (Directory n _) [] newFs = Directory n newFs
updateDirectory (Directory n fs) (d:ds) newFs = let index = fromJust $ findIndex ((==d) . name) fs 
                                                    next = fs !! index
                                                 in Directory n (replace index (updateDirectory next ds newFs) fs)

applyCommand :: (Filesystem,Path) -> Command -> (Filesystem,Path)
applyCommand x (Cd dir) = second (changeDirectory dir) x
applyCommand (filesystem,path) (Ls files) = (updateDirectory filesystem (reverse path) files, path)

applyCommands :: (Filesystem,Path) -> [Command] -> (Filesystem,Path)
applyCommands = foldl applyCommand

findFiles :: (File -> Bool) -> Filesystem -> [File]
findFiles f file@(File _ _) = if f file then [file] else []
findFiles f dir@(Directory _ cs) = if f dir then dir:rec else rec
    where rec = concatMap (findFiles f) cs

part1 :: Filesystem -> Int
part1 = sum . 
        map size .
        findFiles (\f -> isDir f && sizeAtMost 100000 f)
        where
        sizeAtMost x = (<=x) . size

part2 :: Filesystem -> Int
part2 = minimum .
        map size .
        (\fs -> findFiles (\f -> isDir f && size f >= (required - unused fs)) fs)
        where
        total = 70000000
        required = 30000000
        unused f = total - size f

main = print . (part1 `split` part2) . readFilesystem =<< getLines 
