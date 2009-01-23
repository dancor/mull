module Mus where

import Ask
import Control.Concurrent
import Control.Monad
import Control.Monad.Random
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

intvl :: (RandomGen g) => MVar () -> AskDesc g
intvl playing = (,) "intvl" . (,) "identify ascending intervals" $ let
  lowPitch = myPitch (3, C)
  hiPitch = myPitch (5, C)
  maxIntvl = 16

  gen = rndUntil ((<= hiPitch) . (!! 1)) $ do
    low <- getRandomR (Pitch.toInt lowPitch, Pitch.toInt hiPitch - 1)
    hi <- getRandomR (low + 1, low + maxIntvl)
    return [Pitch.fromInt low, Pitch.fromInt hi]
  disp m = do
    putStrLn "(audio)"
    playP playing . line $ map (\ x -> note x qn na) m
  ansFor [n1, n2] = show $ Pitch.toInt n2 - Pitch.toInt n1
  in askUniqAns gen disp ansFor

strs = map myPitch [(2, E), (2, A), (3, D), (3, G), (3, B), (4, E)]

showNote Pitch.C = ["C"]
showNote Pitch.Cs = ["Cs", "Db"]
showNote Pitch.D = ["D"]
showNote Pitch.Ds = ["Ds", "Eb"]
showNote Pitch.E = ["E"]
showNote Pitch.F = ["F"]
showNote Pitch.Fs = ["Fs", "Gb"]
showNote Pitch.G = ["G"]
showNote Pitch.Gs = ["Gs", "Ab"]
showNote Pitch.A = ["A"]
showNote Pitch.As = ["As", "Bb"]
showNote Pitch.B = ["B"]

showPitch (o, n) = map (++ show (o + 3)) $ showNote n

geet :: (RandomGen g) => AskDesc g
geet = (,) "geet" . (,) "guitar frets -> note names" $ let
  gen = liftM2 (,) (choice strs) $ getRandomR (1, 11)
  disp (s, f) = putStrLn $ head (showPitch s) ++ " + " ++ show f
  ansPoss (s, f) = showPitch . Pitch.fromInt $ Pitch.toInt s + f
  in askAnsPoss gen disp ansPoss

geetR :: (RandomGen g) => AskDesc g
geetR = (,) "geetR" . (,) "note names -> guitar frets" $ let
  gen = liftM2 (,) (choice strs) $ getRandomR (1, 11)
  disp (s, f) = putStrLn $ head (showPitch s) ++ "'s " ++
    head (showPitch . Pitch.fromInt $ Pitch.toInt s + f)
  ansFor (s, f) = show f
  in askUniqAns gen disp ansFor
