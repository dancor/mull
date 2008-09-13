module Mus where

import Ask
import Control.Concurrent
import Data.List
import FUtil
import Haskore.Melody
import Haskore.Melody.Standard as Melody
import Haskore.Music.GeneralMIDI as MidiMusic
import Haskore.Interface.MIDI.Render
import Haskore.Basic.Pitch as Pitch
import HSH
import System.Directory
import System.IO
import System.Random

-- haskore has its 0 octave at C3 (low C) for some reason
myPitch :: (Num t) => (t, t1) -> (t, t1)
myPitch (o, n) = (o - 3, n)

rndUntil :: (RandomGen g) => (a -> Bool) -> (g -> a) -> g -> a
rndUntil t f g = let
  g1:g2:_ = gens g
  y = f g1
  in if t y then y else rndUntil t f g2

playQ :: MVar () -> MidiMusic.T -> IO ()
playQ playing m = do
  let
    cmd = "timidity"
    opts = ["-B8,9"]
  (fileName, h) <- openTempFile "." "test.mid"
  hClose h
  fileFromGeneralMIDIMusic fileName m
  putMVar playing ()
  runSL (cmd, opts ++ [fileName])
  takeMVar playing
  removeFile fileName
  return ()

playP :: MVar () -> Melody.T -> IO ()
playP playing = playQ playing . fromStdMelody AcousticGrandPiano

intvl :: (RandomGen g) => MVar () -> ([Char], ([Char], g -> IO ()))
intvl playing = (,) "intvl" . (,) "identify ascending intervals" $ let
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
  disp m = do
    putStrLn "(audio)"
    playP playing . line $ map (\ x -> note x qn na) m
  ans [n1, n2] = show $ Pitch.toInt n2 - Pitch.toInt n1
  in ask gen disp ans

geet :: (RandomGen g) => ([Char], ([Char], g -> IO ()))
geet = (,) "geet" . (,) "guitar frets -> note names" $ let
  strs = map myPitch [(2, E), (2, A), (3, D), (3, G), (3, B), (4, E)]
  showPitch (o, n) = show n ++ show (o + 3)

  gen g = let
    g1:g2:_ = gens g
    in (choice g1 strs, fst $ randomR (1, 11) g2)
  disp (s, f) = putStrLn $ showPitch s ++ " + " ++ show f
  ans (s, f) = showPitch . Pitch.fromInt $ Pitch.toInt s + f
  in ask gen disp ans

geetR :: (RandomGen g) => ([Char], ([Char], g -> IO ()))
geetR = (,) "geetR" . (,) "note names -> guitar frets" $ let
  strs = map myPitch [(2, E), (2, A), (3, D), (3, G), (3, B), (4, E)]
  showPitch (o, n) = show n ++ show (o + 3)

  gen g = let
    g1:g2:_ = gens g
    in (choice g1 strs, fst $ randomR (1, 11) g2)
  disp (s, f) = putStrLn $ 
    showPitch s ++ "'s " ++ showPitch (Pitch.fromInt (Pitch.toInt s + f))
  ans (s, f) = show f
  in ask gen disp ans
