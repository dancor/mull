module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Random
import Data.Maybe
import FUtil
import Math
import Mus
import System.Console.GetOpt
import System.Environment
import System.Random

data Options = Options {
  oNum :: Maybe Int
  }

options :: [OptDescr (Options -> Options)]
options = [
  Option "n" ["ask-num"]
    (ReqArg (\ n o -> o {oNum = Just $ read n}) "NUM") "only ask NUM questions"
  ]

defaultOptions :: Options
defaultOptions = Options {
  oNum = Nothing
}

askRand :: (RandomGen g) => [Rand g a] -> Rand g a
askRand fs = choice fs >>= id

waitForStop :: [MVar ()] -> IO ()
waitForStop = mapM_ (flip putMVar ())

main :: IO ()
main = do
  args <- getArgs
  playing <- newEmptyMVar
  let
    subjects :: [(String, (String, Rand StdGen (IO ())))]
    subjects = [
      geet, 
      geetR, 
      intvl playing, 
      mul,
      mul23,
      elop24,
      elop30
      ]
    usage = "usage: mull [options] <subjects>"
    subjMsg = unlines $
      "subjects are:":map (\ (s, (d, _)) -> s ++ "\t" ++ d) subjects
    doErr e = error $ e ++ usageInfo usage options ++ subjMsg
    (opts, moreArgs) = case getOpt Permute options args of
      (o, n, []) -> (foldl (flip id) defaultOptions o, n)
      (_, _, errs) -> doErr $ concat errs
    subj = case moreArgs of
      [] -> doErr ""
      ss -> let
        (found, unfound) = span (isJust . snd) . zip ss $
          map (flip lookup subjects) ss
        in case unfound of
          [] -> map (snd . fromJust . snd) found
          _ -> doErr $ "Unknown subjects: " ++ show (map fst unfound) ++ "\n"
    askNF = case oNum opts of
      Nothing -> id
      Just n -> take n
  sequence_ . askNF . repeat . (join . evalRandIO) $ askRand subj
  waitForStop [playing]
