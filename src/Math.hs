module Math where

import Ask
import System.Random

mul :: (RandomGen g) => (String, (String, g -> IO ()))
mul = (,) "mul" . (,) "multiply 2-digit numbers" $ let
  gen = take 2 . randomRs (11, 99 :: Int)
  disp [n1, n2] = putStrLn $ show n1 ++ " * " ++ show n2
  ans [n1, n2] = show (n1 * n2)
  in ask gen disp ans
