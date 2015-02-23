module Subject.Music where

import Ask

import Control.Arrow
import Control.Concurrent
import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans
import Data.Function
import Data.List
import Sound.ALUT
import System.Directory
import System.IO
import System.Random

data ArpegForm = Ascend | Descend | Chord | Arb

data Note = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
  deriving (Enum, Eq, Ord)

type Pitch = (Int, Note)

playMus :: [Pitch] -> IO ()
playMus ps = withProgNameAndArgs runALUT $ \_progName _args -> do
  let
    dur = 0.6
    playBuffer toneBuffer toneSrc = do
      buffer toneSrc $= Just toneBuffer
      play [toneSrc]
      sleep dur
  toneBuffers <- mapM (\ p -> createBuffer $ Sine (pToFreq p) 0 dur) ps
  toneSrcs <- genObjectNames $ length ps
  zipWithM_ playBuffer toneBuffers toneSrcs
  deleteObjectNames toneSrcs
  deleteObjectNames toneBuffers
  where
  pToFreq p =
    440 * 2 ** ((fromIntegral $ pitchToInt p - pitchToInt (4, A)) / 12)

pitchFromInt :: Int -> Pitch
pitchFromInt = second toEnum . (`divMod` 12)

pitchToInt :: Pitch -> Int
pitchToInt (o, n) = 12 * o + fromEnum n

arpeg :: (RandomGen g) => AskDesc g
arpeg = Ask "arpeg" "identify intervals/arpeggios" $
  askUniqAns gen disp ansFor
  where
  lowPitch = (3, C)
  hiPitch = (5, C)
  maxIntvl = 16

  gen = rndUntil ((<= hiPitch) . (!! 1)) $ do
    low <- getRandomR (pitchToInt lowPitch, pitchToInt hiPitch - 1)
    hi <- getRandomR (low + 1, low + maxIntvl)
    kind <- randChoice [Ascend, Descend]
    let r = [pitchFromInt low, pitchFromInt hi]
    return $ case kind of
      Ascend -> r
      Descend -> reverse r
    {-
    when (noteNum > pitchToInt hiPitch - pitchToInt lowPitch) $
      error "too many notes for note range"
    -- todo
    let
      noteOrdFunc = case Ascend of
    -}
    --notes <- map pitchFromInt . take noteNum . nub <$> getRandomRs

  disp ns = do
    putStrLn "(audio)"
    playMus ns
  ansFor [n1, n2] = show $ pitchToInt n2 - pitchToInt n1

geetStrs :: [Pitch]
geetStrs = [(2, E), (2, A), (3, D), (3, G), (3, B), (4, E)]

showNote :: Note -> [String]
showNote C = ["C"]
showNote Cs = ["Cs", "Db"]
showNote D = ["D"]
showNote Ds = ["Ds", "Eb"]
showNote E = ["E"]
showNote F = ["F"]
showNote Fs = ["Fs", "Gb"]
showNote G = ["G"]
showNote Gs = ["Gs", "Ab"]
showNote A = ["A"]
showNote As = ["As", "Bb"]
showNote B = ["B"]

showPitch :: Pitch -> [String]
showPitch (o, n) = map (++ show o) $ showNote n

geet :: (RandomGen g) => AskDesc g
geet = Ask "geet" "guitar frets -> note names" $ askAnsPoss gen disp ansPoss
  where
  gen = liftM2 (,) (randChoice geetStrs) $ getRandomR (1, 11)
  disp (s, f) = putStrLn $ head (showPitch s) ++ " + " ++ show f
  ansPoss (s, f) = showPitch . pitchFromInt $ pitchToInt s + f

geetR :: (RandomGen g) => AskDesc g
geetR = Ask "geetR" "note names -> guitar frets" $ askUniqAns gen disp ansFor
  where
  gen = liftM2 (,) (randChoice geetStrs) $ getRandomR (1, 11)
  disp (s, f) = putStrLn $ head (showPitch s) ++ "'s " ++
    head (showPitch . pitchFromInt $ pitchToInt s + f)
  ansFor (s, f) = show f
