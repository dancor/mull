module Math where

import Ask
import Control.Arrow
import Control.Monad.Random
import Data.List
import FUtil
import System.Random

mul :: (RandomGen g) => AskDesc g
mul = (,) "mul" . (,) "multiply 2-digit numbers" $ mulDigLen [2, 2]

mul23 :: (RandomGen g) => AskDesc g
mul23 = (,) "mul23" . (,) "multiply 2 and 3-digit numbers" $ mulDigLen [2, 3]

nOf n i = foldr (\ x a -> x + 10 * a) 0 $ replicate n i

mulDigLen :: (RandomGen g) => [Int] -> Rand g (IO ())
mulDigLen digLens = let
  gen :: (RandomGen g) => Rand g [Int]
  gen = sequence $ map (\ n -> getRandomR (nOf n 1, nOf n 9)) digLens
  disp = putStrLn . intercalate " * " . map show
  ansFor = show . product
  in askUniqAns gen disp ansFor


elop :: (RandomGen g) => [(String, Int -> Int -> Maybe Int)] -> Int -> Int -> 
  Rand g (IO ())
elop ops numNum sum = ask gen disp isRight ansFor where
  gen :: (RandomGen g) => Rand g [Int]
  gen = rndUntil (not . null . anssFor) $
    sequence . replicate numNum $ getRandomR (1, 9)
  disp = putStrLn . intercalate " " . map show
  isRight q a = False -- todo
  apply ([], [x]) = Just x
  apply (f:fs, x:xs) = case apply (fs, xs) of
    Just res -> x `f` res
    Nothing -> Nothing
  strify ([], [x]) = show x
  strify (f:fs, x:xs) = show x ++ " " ++ f ++ " (" ++ strify (fs, xs) ++ ")"
  applyAndStr (fAndStrs, xs) = 
    (strify (map fst fAndStrs, xs), apply (map snd fAndStrs, xs))
  anssFor q = map fst $ filter ((== Just sum) . snd) poss where
    poss = [applyAndStr (f, xs) | f <- fOrds, xs <- perms q]
    fOrds = sequence $ replicate (numNum - 1) ops
  ansFor = unlines . anssFor

justify f x y = Just $ f x y
divInt x y = if y == 0 then Nothing else case divMod x y of
  (n, 0) -> Just n
  _ -> Nothing
ops = map (second justify) [("+", (+)), ("-", (-)), ("*", (*))] ++
  [("/", divInt)]

elop24 :: (RandomGen g) => AskDesc g
elop24 = (,) "elop24" . 
  (,) "find elementary operation combination to make 24" $ elop ops 4 24

elop30 :: (RandomGen g) => AskDesc g
elop30 = (,) "elop30" . 
  (,) "find elementary operation combination to make 30" $ elop ops 5 30
