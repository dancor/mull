module Main where

import Control.Concurrent
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

askRand :: (RandomGen a) => [a -> b] -> a -> b
askRand fs g = let (g1, g2) = split g in choice g1 fs $ g2 

main :: IO ()
main = do
  args <- getArgs
  playing <- newEmptyMVar
  let
    subjects = [
      geet, 
      geetR, 
      intvl playing, 
      mul
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
  sequence_ . askNF . map (askRand subj) . gens =<< getStdGen
