module Ask where

import Data.Maybe
import System.Console.Readline

-- generic asker where there is a unique answer.
-- questions are not re-asked on incorrect answers.
ask :: (Show a) => (t -> a) -> (a -> IO t1) -> (a -> [Char]) -> t -> IO ()
ask gen dispM ans g = do
  let
    inst = gen g
    a = ans inst
  dispM inst
  l <- fmap (fromMaybe "") $ readline ""
  putStrLn $ if l == a then "" else a ++ "\n!\n"
