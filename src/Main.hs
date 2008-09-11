module Main where

-- want this to be general so that we can have many different tests

import Data.Function
import Data.List
import Data.Maybe
import System.Console.Readline
import System.Environment
import System.Random

shuffle :: StdGen -> [a] -> [a]
shuffle rnd l = map snd (sortBy (compare `on` fst) (zip rndInts l))
  where
    rndInts :: [Int]
    rndInts = randoms rnd

choice :: StdGen -> [a] -> a 
choice rnd l = head (shuffle rnd l)

readMb :: Read a => String -> Maybe a
readMb s = fmap fst . listToMaybe $ reads s

intMull :: Int -> Int -> Int
intMull = (*)

killBksps :: [Char] -> [Char]
killBksps "" = ""
killBksps (_:'\b':rest) = killBksps rest
killBksps ('\b':rest) = killBksps rest
killBksps (c:rest) = c:killBksps rest

--mull = simple (*) (\ l r -> show l ++ " * " ++ show r)

--simple :: (a -> b -> c) -> (a -> b -> String) -> StdGen -> IO ()
mull :: (System.Random.RandomGen g) => g -> IO ()
mull g = do
  let
    n1:n2:_ = randomRs (11, 99 :: Int) g
    ans = n1 * n2
  putStrLn $ show n1 ++ " * " ++ show n2
  l <- readline ""
  let 
    ansGivenMb = l >>= readMb
    failz = putStrLn $ show (n1 * n2) ++ "\n!\n"
  case ansGivenMb of
    Nothing -> failz
    Just ansGiven -> if ansGiven == ans then putStrLn "" else failz

ask :: [StdGen -> t t1] -> StdGen -> t t1
ask fs g = do
  let 
    (g1, g2) = split g
  (choice g1 fs) g2 

gens :: (RandomGen t) => t -> [t]
gens g = let (g1, g2) = split g in g1:gens g2

main :: IO ()
main = do
  args <- getArgs
  let
    askNF = case args of
      [] -> id
      [n] -> take $ read n
      _ -> error "usage"
  sequence_ . askNF . map (ask [mull]) . gens =<< getStdGen
