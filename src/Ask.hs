module Ask where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Random
import Data.Function
import Data.List
import Data.Maybe
import System.Console.ANSI
import System.Console.Readline

data Qa = Qa
    { qaQ :: [String]
    , qaA :: [String]
    }

data HowToAsk = HowToAsk
    { reqRightNTimes :: Int
    }

askQas :: HowToAsk -> [Qa] -> IO ()
askQas how = mapM_ (askQa how)

askQa :: HowToAsk -> Qa -> IO ()
askQa (HowToAsk needsNRight) qa = do
  gotRight <- askQaOnce qa
  let needsNRight' = if gotRight then needsNRight - 1 else needsNRight
  unless (needsNRight' <= 0) $ askQa (HowToAsk needsNRight') qa

getLines :: IO [String]
getLines = do
    l <- fmap (fromMaybe "") $ readline ""
    if null l then return [] else fmap (l :) getLines

askQaOnce :: Qa -> IO Bool
askQaOnce (Qa q a) = do
    clearScreen
    setCursorPosition 0 0
    putStrLn $ unlines q
    aGiven <- getLines
    if a == aGiven
      then do
        putStrLn "Correct! Press enter to continue."
        _ <- getLine
        return True
      else do
        putStrLn "No, the answer was: "
        putStrLn ""
        putStrLn $ unlines a
        putStrLn "Press enter to continue."
        _ <- getLine
        return False

data AskDesc g = Ask String String (RandT g IO ())

randShuffle :: (MonadRandom m) => [b] -> m [b]
randShuffle l = do
  rndInts <- getRandoms
  return . map snd . sortBy (compare `on` fst) $ zip (rndInts :: [Int]) l

randChoice :: (MonadRandom m) => [b] -> m b
randChoice l = randShuffle l >>= return . head

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
    _ <- forkIO $ dispM qInstance
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
