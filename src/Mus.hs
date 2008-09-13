module Mus where

import Ask
import FUtil
import Haskore.Melody
import Haskore.Melody.Standard as Melody
import Haskore.Music.GeneralMIDI as MidiMusic
import qualified Haskore.Music as Mus
import Haskore.Interface.MIDI.Render
import Haskore.Basic.Pitch as Pitch
import HSH
import System.Random

-- haskore has its 0 octave at C3 (low C) for some reason
myPitch (o, n) = (o - 3, n)

rndUntil :: (RandomGen g) => (a -> Bool) -> (g -> a) -> g -> a
rndUntil t f g = let
  g1:g2:_ = gens g
  y = f g1
  in if t y then y else rndUntil t f g2

playQ m = do
  let
    fileName = "test.mid"
    cmd = "timidity"
    opts = ["-B8,9"]
  fileFromGeneralMIDIMusic fileName m
  runSL (cmd, opts ++ [fileName])
  return ()

playP :: Melody.T -> IO ()
playP = playQ . fromStdMelody AcousticGrandPiano

intvl :: StdGen -> IO ()
intvl = let
  lowPitch = myPitch (3, C)
  hiPitch = myPitch (5, C)
  maxIntvl = 15

  gen = rndUntil ((<= hiPitch) . (!! 1)) (\ g -> let
    g1:g2:_ = gens g
    low :: Int
    low = fst $ randomR (Pitch.toInt lowPitch, Pitch.toInt hiPitch - 1) g1
    hi :: Int
    hi = fst $ randomR (low + 1, low + maxIntvl) g2
    in [Pitch.fromInt low, Pitch.fromInt hi]
    )
  disp = playP . line . map (\ x -> note x qn na)
  ans [n1, n2] = show $ Pitch.toInt n2 - Pitch.toInt n1
  in ask gen disp ans
