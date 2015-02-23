module Subject.Math where

import Ask
import Control.Arrow
import Control.Monad.Random
import Data.List

mul :: (RandomGen g) => AskDesc g
mul = Ask "mul" "multiply 2-digit numbers" $ mulDigLen [2, 2]

mul23 :: (RandomGen g) => AskDesc g
mul23 = Ask "mul23" "multiply 2 and 3-digit numbers" $ mulDigLen [2, 3]

nOf :: Int -> Int -> Int
nOf n i = foldr (\ x a -> x + 10 * a) 0 $ replicate n i

mulDigLen :: (RandomGen g) => [Int] -> RandT g IO ()
mulDigLen digLens = askUniqAns gen disp ansFor where
  gen = sequence $ map (\ n -> getRandomR (nOf n (1 :: Int), nOf n 9)) digLens
  disp = putStrLn . intercalate " * " . map show
  ansFor = show . product

elop :: (RandomGen g) => [(String, Int -> Int -> Maybe Int)] -> Int -> Int ->
  RandT g IO ()
elop ops numNum theSum = ask gen disp isRight ansFor where
  gen = rndUntil (not . null . anssFor) $
    sequence . replicate numNum $ getRandomR (1, 9)
  disp = putStrLn . intercalate " " . map show
  isRight _q _a = False -- todo
  apply ([], [x]) = Just x
  apply (f:fs, x:xs) = case apply (fs, xs) of
    Just res -> x `f` res
    Nothing -> Nothing
  apply _ = error "Rewrite elop and apply.."
  strify ([], [x]) = show x
  strify ([f], x:x2:[]) = show x ++ " " ++ f ++ " " ++ show x2
  strify (f:fs, x:xs) = show x ++ " " ++ f ++ " (" ++ strify (fs, xs) ++ ")"
  strify _ = error "Rewrite elop and strify.."
  applyAndStr (fAndStrs, xs) =
    (strify (map fst fAndStrs, xs), apply (map snd fAndStrs, xs))
  anssFor q = map fst $ filter ((== Just theSum) . snd) poss where
    poss = [applyAndStr (f, xs) | f <- fOrds, xs <- permutations q]
    fOrds = sequence $ replicate (numNum - 1) ops
  ansFor = unlines . anssFor

justify :: (a -> b -> c) -> a -> b -> Maybe c
justify f x y = Just $ f x y

divInt :: Int -> Int -> Maybe Int
divInt x y = if y == 0 then Nothing else case divMod x y of
  (n, 0) -> Just n
  _ -> Nothing

elops :: [(String, Int -> Int -> Maybe Int)]
elops = map (second justify) [("+", (+)), ("-", (-)), ("*", (*))] ++
  [("/", divInt)]

elop24 :: (RandomGen g) => AskDesc g
elop24 = Ask "elop24" "find elementary operation combination to make 24" $
  elop elops 4 24

elop30 :: (RandomGen g) => AskDesc g
elop30 = Ask "elop30" "find elementary operation combination to make 30" $
  elop elops 5 30
