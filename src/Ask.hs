module Ask where

import Control.Concurrent
import Data.Maybe
import System.Console.Readline

-- generic asker where there is a unique answer.
-- questions are not re-asked on incorrect answers.
-- question re-asked on blank answer.  (useful for multimedia)
ask :: (t -> t1) -> (t1 -> IO ()) -> (t1 -> [Char]) -> t -> IO ()
ask gen dispM ans g = let
  inst = gen g
  a = ans inst
  reAsk = do
    -- fork if it takes a while (playing a sound) so readline stays happy
    forkIO $ dispM inst
    l <- fmap (fromMaybe "") $ readline ""
    if l == a then putStrLn "" else case l of
      "" -> reAsk
      _ -> putStrLn $ a ++ "\n!\n"
  in reAsk
