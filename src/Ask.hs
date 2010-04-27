module Ask where

import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.Random
import Data.Maybe
import System.Console.Readline

data AskDesc g = Ask String String (RandT g IO ())

rndUntil :: (MonadRandom r) => (a -> Bool) -> r a -> r a
rndUntil t f = do
  y <- f
  if t y then return y else rndUntil t f

-- generic asker
-- questions are not re-asked on incorrect answers.
-- question re-asked on blank answer.  (useful for multimedia)
ask :: (RandomGen g) => RandT g IO a -> (a -> IO ()) ->
  (a -> String -> Bool) -> (a -> String) -> RandT g IO ()
ask gen dispM isRight ansFor = gen >>= liftIO . reAsk where
  reAsk qInstance = do
    -- fork in case disp takes a bit so that readline stays happy..
    forkIO $ dispM qInstance
    l <- fmap (fromMaybe "") $ readline ""
    if isRight qInstance l then putStrLn "" else case l of
      "" -> reAsk qInstance
      _ -> putStrLn $ ansFor qInstance ++ "\n!\n"

-- generic asker where answer is unique
askUniqAns :: (RandomGen g) => RandT g IO a -> (a -> IO ()) -> (a -> String) ->
  RandT g IO ()
askUniqAns gen dispM ansFor = ask gen dispM ((==) . ansFor) ansFor

-- generic asker where answer possibilities are listed
askAnsPoss :: (RandomGen g) => RandT g IO a -> (a -> IO ()) ->
  (a -> [String]) -> RandT g IO ()
askAnsPoss gen dispM ansPoss =
  ask gen dispM (flip elem . ansPoss) (head . ansPoss)
