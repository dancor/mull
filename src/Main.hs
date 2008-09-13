module Main where

import FUtil
import Math
import Mus
import System.Environment
import System.Random

askRand :: (RandomGen a) => [a -> b] -> a -> b
askRand fs g = let (g1, g2) = split g in choice g1 fs $ g2 

main :: IO ()
main = do
  args <- getArgs
  let
    askNF = case args of
      [] -> id
      [n] -> take $ read n
      _ -> error "usage"
  sequence_ . askNF . map (askRand [intvl]) . gens =<< getStdGen
