module Main where

import Ask
import Control.Monad
import Control.Monad.Random
import Data.Either
import Data.Maybe
import FUtil
import Math
import Mus
import System.Console.GetOpt
import System.Environment
import System.Random

data Options = Options {
  oNum :: Maybe Int}

options :: [OptDescr (Options -> Options)]
options = [
  Option "n" ["ask-num"]
    (ReqArg (\ n o -> o {oNum = Just $ read n}) "NUM") "only ask NUM questions"
  ]

defaultOptions :: Options
defaultOptions = Options {
  oNum = Nothing}

askRand :: (MonadRandom r) => [r a] -> r a
askRand fs = choice fs >>= id

evalRandTIO :: RandT StdGen IO a -> IO a
evalRandTIO f = do
  (a, g) <- getStdGen >>= runRandT f
  setStdGen g
  return a

main :: IO ()
main = do
  args <- getArgs
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
    usage = "usage: mull [options] <subjects>"
    subjMsg = unlines $
      "subjects are:":map (\ (Ask s d _) -> s ++ "\t" ++ d) subjects
    doErr e = error $ e ++ usageInfo usage options ++ subjMsg
    (opts, moreArgs) = case getOpt Permute options args of
      (o, n, []) -> (foldl (flip id) defaultOptions o, n)
      (_, _, errs) -> doErr $ concat errs
    subjs :: [AskDesc StdGen]
    subjs = case moreArgs of
      [] -> doErr ""
      ss -> case unfound of
          [] -> found
          _ -> doErr $ "Unknown subjects: " ++ show unfound ++ "\n"
        where
        (unfound, found) = partitionEithers $
          map (\ s -> maybe (Left s) Right . listToMaybe $
            filter (\ (Ask s' _ _) -> s' == s) subjects) ss
    askNF = maybe id take $ oNum opts
  sequence_ . askNF . repeat . evalRandTIO . askRand $ map (\ (Ask _ _ r) -> r)
    subjs
