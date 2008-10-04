module Ask where

import Control.Concurrent
import Control.Monad.Random
import Data.Maybe
import System.Console.Readline

type AskDesc g = ([Char], ([Char], Rand g (IO ())))

rndUntil :: (RandomGen g) => (a -> Bool) -> Rand g a -> Rand g a
rndUntil t f = do
  y <- f
  if t y then return y else rndUntil t f

-- generic asker
-- questions are not re-asked on incorrect answers.
-- question re-asked on blank answer.  (useful for multimedia)
ask :: (RandomGen g) => Rand g a -> (a -> IO ()) -> (a -> String -> Bool) -> 
  (a -> String) -> Rand g (IO ())
ask gen dispM isRight ansFor = do
  qInstance <- gen
  let
    reAsk = do
      -- fork if it takes a while (playing a sound) so readline stays happy
      forkIO $ dispM qInstance
      l <- fmap (fromMaybe "") $ readline ""
      if isRight qInstance l then putStrLn "" else case l of
        "" -> reAsk
        _ -> putStrLn $ ansFor qInstance ++ "\n!\n"
  return reAsk

-- generic asker where answer is unique
askUniqAns :: (RandomGen g) => Rand g a -> (a -> IO ()) -> (a -> String) -> 
  Rand g (IO ())
askUniqAns gen dispM ansFor = ask gen dispM ((==) . ansFor) ansFor

-- generic asker where answer possibilities are listed
askAnsPoss :: (RandomGen g) => Rand g a -> (a -> IO ()) -> (a -> [String]) -> 
  Rand g (IO ())
askAnsPoss gen dispM ansPoss = 
  ask gen dispM (flip elem . ansPoss) (head . ansPoss)
