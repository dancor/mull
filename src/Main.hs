module Main where

import Control.Monad
import Control.Monad.Random
import Data.Either
import Data.Maybe
import FUtil
import System.Console.GetOpt
import System.Environment
import System.Random

import Ask
import Math
import Mus
import Opt

evalRandTIO :: RandT StdGen IO a -> IO a
evalRandTIO f = do
  (a, g) <- getStdGen >>= runRandT f
  setStdGen g
  return a

main :: IO ()
main = do
  let
    subjects :: [AskDesc StdGen]
    subjects = [
      geet,
      geetR,
      arpeg,
      mul,
      mul23,
      elop24,
      elop30]
    usage = "usage: mull [options] <subjects>\n" ++ Opt.optInfo ++ "\n" ++
      unlines ("subjects are:":map (\ (Ask s d _) -> s ++ "\t" ++ d) subjects)
  (opts, args) <- Opt.getOpts "" usage
  let
    doErr e = error $ e ++ usage
    subjs :: [AskDesc StdGen]
    subjs = case args of
      [] -> doErr ""
      ss -> case unfound of
          [] -> found
          _ -> doErr $ "Unknown subjects: " ++ show unfound ++ "\n"
        where
        (unfound, found) = partitionEithers $
          map (\ s -> maybe (Left s) Right . listToMaybe $
            filter (\ (Ask s' _ _) -> s' == s) subjects) ss
    askNFunc = maybe id take $ Opt.askNum opts
  sequence_ . askNFunc . repeat . evalRandTIO . join . choice $
    map (\ (Ask _ _ r) -> r) subjs
