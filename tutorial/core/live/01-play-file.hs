{-# Language OverloadedStrings #-}
-- | Plays single audio file
module Main where

import Data.String
import Csound.Core

-- change main function to listen to results
main :: IO ()
main = polyphony

file1, file2 :: IsString a => a
file1 = "/home/anton/over-minus.wav"
file2 = "/home/anton/space-lullaby-v1.mp3"

-- read files from disk or from memory

playWav    = dac $ diskin file1
playMp3    = dac $ mp3in file2
ramPlayWav = dac $ oscWav file1 1
ramPlayMp3 = dac $ oscMp3 file2 1

-- change speed with control rate signal linseg / expseg

-- | changing the speed of playback
changeSpeed = dac $ loopWav file1 speed
  where
    speed = linseg [1, 5, 1, 2, 0.5, 8, 0.5, 0, -1, 10, -1, 0, 1]

-- | using withInits
changeSpeedWithInits = dac $ diskin file1 `withInits` (speed, 0 :: D, 1 :: D)
  where
    speed = linseg [1, 5, 1, 2, 0.5, 8, 0.5, 0, -1, 10, -1, 0, 1]

-- change volume with multiplication

-- | changing the volume with relative logarithmic scale (0, 1)
changeVolume = dac $ mul vol $ diskin file1
  where
    vol = dbfs (linseg [0, 5, 1, 5, 1, 5, 0.75])

-- different ways to write polyphonic parts with additions

polyphony1 = dac $ mul 0.3 $ a1 + a2 + a3
  where
    a1 = loopWav file1 1
    a2 = loopWav file1 (-1)
    a3 = loopWav file1 (-0.5)

polyphony2 = dac $ mul 0.3 $ sum
  [ loopWav file1 1
  , loopWav file1 (-1)
  , loopWav file1 (-0.5)
  ]

polyphony3 = dac $ mean $ map (loopWav file1) [1, -1, -0.5]
