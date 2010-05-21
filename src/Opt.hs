{-# LANGUAGE TemplateHaskell #-}

module Opt where

import Data.PolyOpt

$(polyOpt [
  reqArgGen ["ask-num"] "n"
    "NUM" [t|Maybe Int|] [|Nothing|] [|Just . read|]
    "exit after asking NUM number of questions"])
