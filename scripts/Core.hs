{-# Language OverloadedStrings #-}
module Main where

import Csound.Typed.Core.Types
import Csound.Typed.Core.Opcodes hiding (schedule)
import Data.Default

event_i :: (Arg a, IsInstrId instrId) => Str -> instrId a -> D -> D -> a -> SE ()
event_i _ instrId start dur args = play instrId [Note start dur args]

schedule :: (Arg a, IsInstrId instrId) => instrId a -> D -> D -> a -> SE ()
schedule instrId start dur args = play instrId [Note start dur args]

main = do
  file <- renderSE def playFileInstr
  putStrLn file
  writeFile "tmp.csd" file

res :: SE ()
res = do
  ref <- newRef (0 :: Sig, 0 :: K Sig)
  instr1 <- newProc (setInstr1 ref)
  instr2 <- newProc setInstr2
  instr3 <- newProc setInstr2
  event_i "i" instr1 0 1 (200, 100)
  event_i "i" instr2 0 2 100
  event_i "i" instr3 0 3 100
  where
    setInstr1 :: Ref (Sig, K Sig) -> (D, D) -> SE ()
    setInstr1 ref (a, b) = do
      lref <- newRef (0 :: Sig)
      lk <- readRef lref
      (ga, K gb) <- readRef ref
      outs (ares + ga + lk, ares + gb)
      where
        ares = linseg [a, 1, b]

    setInstr2 :: D -> SE ()
    setInstr2 a = do
      outs $ fromMono $ linseg [a, 1, a] * oscil 1 220 (sines [1])

pureSine :: SE ()
pureSine = do
  instr1 <- newProc sineInstr
  event_i "i" instr1 0 2 220
  where
    sineInstr cps = outs $ fromMono $ 0.5 * oscil 1 (sig cps) (sines [1])

pureSineConst :: SE ()
pureSineConst = do
  instr1 <- newProc sineInstr
  event_i "i" instr1 0 2 ()
  where
    sineInstr :: () -> SE ()
    sineInstr _ = outs $ fromMono $ 0.5 * oscil 1 220 (sines [1])

playFile :: SE ()
playFile = do
  instr1 <- newProc fileInstr
  schedule instr1 0 3 "/home/anton/over-minus.wav"
  schedule instr1 5 7 "/home/anton/over-minus.wav"
  where
    fileInstr :: Str -> SE ()
    fileInstr file = outs $ diskin2 file

playFileInstr :: SE ()
playFileInstr = do
  (instr1, (al, ar)) <- newInstr PolyMix 0.2 fileInstr
  schedule instr1 0 3 "/home/anton/over-minus.wav"
  schedule instr1 5 7 "/home/anton/over-minus.wav"
  outs (al, ar)
  where
    fileInstr :: Str -> SE Sig2
    fileInstr file = pure $ diskin2 file



