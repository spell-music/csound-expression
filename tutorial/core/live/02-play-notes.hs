{-# Language OverloadedStrings #-}
module Main where

import Control.Monad
import Csound.Core

main :: IO ()
main = midiPlay

file1, file2 :: IsString a => a
file1 = "/home/anton/over-minus.wav"
file2 = "/home/anton/space-lullaby-v1.mp3"


playFile :: (D, Str) -> SE ()
playFile (amp, file) = writeOuts $ mul (sig amp) $ diskin file

playProc1 :: IO ()
playProc1 = dac $ do
  playId <- newProc playFile
  schedule playId 0 3 (0.5, file1)
  schedule playId 4 5 (0.8, file1)
  schedule playId 10 5 (0.4, file1)

playProc2 :: IO ()
playProc2 = dac $ do
  playId <- newProc playFile

  play playId
    [ Note 0 3 (0.5, file1)
    , Note 4 5 (0.8, file1)
    , Note 10 5 (0.4, file1)
    ]

playPeriodic = dac $ do
  playId <- newProc playFile

  when1 (metro 0.2 `equals` 1) $ do
    schedule playId 0 2 (0.8, file1)
    schedule playId 4 1 (0.5, file1)

playFile2 :: (D, D, Str) -> SE ()
playFile2 (amp, speed, file) = writeOuts $ mul (sig amp * fadeOut) $ loopWav file (sig speed)
  where
    fadeOut = linsegr [1] 0.1 0

playPeriodic2 = dac $ do
  playId <- newProc playFile2

  when1 (metro 0.25 `equals` 1) $ do
    play playId
      [ Note 0 1 (1, 1, file1)
      , Note 2 2 (0.5, 0.5, file1)
      , Note 3 1 (0.7, 2, file1)
      , Note 4 2 (0.5, 0.5, file1)
      , Note 2 4 (0.2, -1, file1)
      , Note 8 8 (0.2, -0.5, file1)
      , Note 10 18 (0.5, -0.25, file1)
      ]

midiEcho = dacBy setTrace $ do
  receiveId <- newProc receive
  massign 0 receiveId
  where
    receive :: () -> SE ()
    receive _ = do
      n <- notnum
      v <- veloc
      prints "Midi note:\n    velocity : %d\n    pitch    : %d\n" (v, n)


midiPlay = writeCsdBy setTrace "tmp.csd" $ do
  playId <- newProc playFile2
  melId  <- newProc playPeriodicInstr2

  receiveId <- newProc (receive playId melId)
  massign 0 receiveId
  where
    receive :: InstrRef (D, D, Str) -> InstrRef ()  -> () -> SE ()
    receive playId melId _ = do
      n <- notnum
      let tagPlayId = setFraction 1000 n playId
          tagMelId = setFraction 1000 n melId
      v <- veloc
      prints "Midi note:\n    velocity : %d\n    pitch    : %d\n" (v, n)

      when1 (n `equals` 9)  $ schedule tagPlayId 0 (-1) (0.8, 1, file1)
      when1 (n `equals` 10) $ schedule tagPlayId 0 (-1) (0.8, -1, file1)
      when1 (n `equals` 11) $ schedule tagPlayId 0 (-1) (0.8, -0.5, file1)
      when1 (n `equals` 12) $ schedule tagPlayId 0 (-1) (0.8, 2, file1)
      when1 (n `equals` 28) $ schedule tagMelId 0 (-1) ()

      when1 (n `equals` 26) $ schedule tagMelId 0 (-1) ()
      when1 (n `equals` 27) $ turnoff2 (setFraction 1000 26 $ melId) 4 0.1

      isRelease <- release
      when1 (isRelease `equals` 1) $ do
        turnoff2 tagPlayId 4 0.1
        when1 (n `notEquals` 26) $ do
          turnoff2 tagMelId 4 0.1


playPeriodicInstr :: () -> SE ()
playPeriodicInstr _ = do
  playId <- newProc playFile2

  when1 (metro 0.25 `equals` 1) $ do
    play playId
      [ Note 0 1 (1, 1, file1)
      , Note 2 2 (0.5, 0.5, file1)
      , Note 3 1 (0.7, 2, file1)
      , Note 4 2 (0.5, 0.5, file1)
      , Note 2 4 (0.2, -1, file1)
      , Note 8 8 (0.2, -0.5, file1)
      , Note 10 18 (0.5, -0.25, file1)
      ]

playPeriodicInstr2 :: () -> SE ()
playPeriodicInstr2 _ = do
  (playId, res) <- newInstr PolyMix 0.1 playFile3
  writeOuts res

  when1 (metro 0.25 `equals` 1) $ do
    play playId
      [ Note 0 1 (1, 1, file1)
      , Note 2 2 (0.5, 0.5, file1)
      , Note 3 1 (0.7, 2, file1)
      , Note 4 2 (0.5, 0.5, file1)
      , Note 2 4 (0.2, -1, file1)
      , Note 8 8 (0.2, -0.5, file1)
      , Note 10 18 (0.5, -0.25, file1)
      ]


playFile3 :: (D, D, Str) -> SE Sig2
playFile3 (amp, speed, file) =
  pure $ mul (sig amp * fadeOut) $ loopWav file (sig speed)
  where
    fadeOut = linsegr [1] 0.1 0


midiPlay2 = dacBy setTrace $ do
  receiveId <- newProc receive
  massign 0 receiveId
  where
    receive :: () -> SE ()
    receive _ = do
      [pad1, pad2, pad3] <- mapM (newProc . padInstr) [1,2,3]

      n <- notnum
      v <- veloc
      prints "Midi note:\n    velocity : %d\n    pitch    : %d\n" (v, n)

      when1 (n `equals` 9)  $ padOn pad1
      when1 (n `equals` 10) $ padOn pad2
      when1 (n `equals` 11) $ padOn pad3

      isRelease <- release
      when1 (isRelease `equals` 1) $ do
        when1 (n `equals` 9)  $ padOff pad1
        when1 (n `equals` 10) $ padOff pad2
        when1 (n `equals` 11) $ padOff pad3

    padOn p = schedule p 0 (-1) ()
    padOff p = turnoff2 p 0 0.1

    padInstr :: Int -> () -> SE ()
    padInstr n _ = case n of
      1 -> playFile2 (0.8, 1, file1)
      2 -> playFile2 (0.8, -1, file1)
      3 -> playFile2 (0.8, -0.5, file1)
      _ -> pure ()


midiPlay3 = dacBy setTrace $ do
  receiveId <- newProc receive
  massign 0 receiveId
  where
    receive :: () -> SE ()
    receive _ = do
      pads <- mapM (newProc . padInstr) [1,2,3]

      n <- notnum
      v <- veloc
      prints "Midi note:\n    velocity : %d\n    pitch    : %d\n" (v, n)

      selectBy n pads padOn

      isRelease <- release
      when1 (isRelease `equals` 1) $ do
        selectBy n pads padOff

    padOn p = schedule p 0 (-1) ()
    padOff p = turnoff2 p 0 0.1

    selectBy n pads f =
      zipWithM_ (\padNum pad ->  when1 (n `equals` padNum)  $ f pad) [9, 10, 11] pads

    padInstr :: Int -> () -> SE ()
    padInstr n _ = case n of
      1 -> playFile2 (0.8, 1, file1)
      2 -> playFile2 (0.8, -1, file1)
      3 -> playFile2 (0.8, -0.5, file1)
      _ -> pure ()

midiToggle = undefined -- TODO

