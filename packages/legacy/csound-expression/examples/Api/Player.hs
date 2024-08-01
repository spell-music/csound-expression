module Main where

import Csound.Base

declick :: Sig2 -> Sig2
declick = mul (fades 0.01 0.1)

playWav :: Str -> SE Sig2
playWav file = return $ declick $ diskin2 file 1

playMp3 :: Str -> SE Sig2
playMp3 file = return $ declick $ mp3in file

stop :: Unit -> SE ()
stop _ = do
  turnoffByName "wav" 0 0.1
  turnoffByName "mp3" 0 0.1
  turnoff

main = writeCsd "player.csd" $ do
  wavs <- trigByName "wav" playWav
  mp3s <- trigByName "mp3" playMp3
  trigByName_ "stop" stop
  return $ wavs + mp3s
