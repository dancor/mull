module Ask where

import Control.Concurrent
import Data.Maybe
import System.Console.Readline

-- generic asker where there is a unique answer.
-- questions are not re-asked on incorrect answers.
-- question re-asked on blank answer.  (useful for multimedia)
ask gen dispM ans g = let
  inst = gen g
  a = ans inst
  reAsk = do
    -- fork if it takes a while (playing a sound) so readline stays happy
    -- FIXME sound never plays: debugging printing hm, ok, and inst here
    forkIO (print "hm" >> dispM inst >> print "ok")
    print inst
    l <- fmap (fromMaybe "") $ readline ""
    if l == a then putStrLn "" else case l of
      "" -> reAsk
      _ -> putStrLn $ a ++ "\n!\n"
  in reAsk
