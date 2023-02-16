{-# Language OverloadedStrings #-}
module Main where

import Csound.Typed.Core.Types
import Csound.Typed.Core.Opcodes hiding (schedule)
import Data.Default

outs :: Sig2 -> SE ()
outs = writeOuts

event_i :: (Arg a) => Str -> InstrRef a -> D -> D -> a -> SE ()
event_i _ instrId start dur args = play instrId [Note start dur args]

schedule :: (Arg a) => InstrRef a -> D -> D -> a -> SE ()
schedule instrId start dur args = play instrId [Note start dur args]

main = do
  file <- renderSE def (repeatExampleK {-playFileInstr-})
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

minBugPlayFileInstr :: SE ()
minBugPlayFileInstr = do
  _ <- newProc instr
  pure ()
  where
    instr :: Port (K Sig)-> SE ()
    instr port = do
      let q = oscil 1 1 (sines [1])
      ksig <- unK <$> readRef port
      when1 (q `greater` 0) $ outs (ksig, q)

playFileInstr2 :: SE ()
playFileInstr2 = do
  (instr1, res) <- newInstr PolyMix 0.2 (\() -> playFileInstr)
  schedule instr1 0  10 ()
  schedule instr1 12 8 ()
  schedule instr1 24 10 ()
  outs res

playFileInstr :: SE (Sig, Sig)
playFileInstr = do
  (instr1, (al, ar)) <- newInstr PolyMix 0.2 fileInstr
  global $ massign 1 instr1
  schedule instr1 0 3 "/home/anton/over-minus.wav"
  schedule instr1 5 7 "/home/anton/over-minus.wav"
  pure (al, ar)
  where
    fileInstr :: Str -> SE Sig2
    fileInstr file = pure $ diskin2 file

oscEcho :: SE ()
oscEcho = do
  oscHandle <- oscInit 8020
  sendId <- newProc sendInstr
  listenId <- newProc (listenInstr oscHandle)
  schedule sendId 0 20 ()
  schedule listenId 0 20 ()
  where
    sendInstr :: () -> SE ()
    sendInstr _ = do
      krand <- randomh (-50) 50 4
      let ktrig = metro 1
      oscSend ktrig "localhost" 8020 "/foo" "sf" ("zoo" :: Str, krand)

    listenInstr :: OscHandle -> () -> SE ()
    listenInstr oscHandle _ = do
      printId <- newProc printInstr
      (kwhen, ref) <- oscListen oscHandle "/foo" "sf"
      when1 kwhen $ do
        val <- readRef ref
        schedule printId 0 0.1 val

    printInstr :: (Str, D) -> SE ()
    printInstr s = do
      prints "%s %f" s

repeatExample :: SE ()
repeatExample = do
  instrId <- newProc instr
  schedule instrId 0 1 4
  schedule instrId 1 1 2
  schedule instrId 2 1 1
  where
    instr :: D -> SE ()
    instr n = doRepeat n $ (\k -> prints "Repeat: %d out of %d.\n" (k, n))

repeatExampleK :: SE ()
repeatExampleK = do
  instrId <- newProc instr
  schedule instrId 0 5 4
  schedule instrId 5 5 2
  schedule instrId 10 5 1
  where
    instr :: D -> SE ()
    instr n = do
      printId <- newProc printInstr
      when1 (metro 1 `equals` 1) $
        doRepeat (sig n) $ \k -> schedule printId 0 1 (toD k, n)

    printInstr :: (D, D) -> SE ()
    printInstr arg = prints "Repeat: %d out of %d.\n" arg

