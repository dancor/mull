module Subject.General where

import Ask

import System.FilePath

myAsk :: IO ()
myAsk = askFile "/home/danl/sheet-music/analyze/Chopin/ask"

parseQas :: [String] -> [Qa]
parseQas [] = []
parseQas ls = Qa q a : parseQas (dropWhile null afterA)
  where
    (q, afterQ) = break null ls
    (a, afterA) = break null (dropWhile null afterQ)

parseQaFile :: FilePath -> IO [Qa]
parseQaFile = fmap (parseQas . dropWhile null . lines) . readFile

askFile :: FilePath -> IO ()
askFile path = parseQaFile path >>= askQas (HowToAsk 3)
