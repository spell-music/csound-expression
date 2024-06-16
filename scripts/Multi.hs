module Main where

import Csound.Core
import Data.String

main = writeCsdBy setDebugTrace "tmp.csd" $ do
  global $ massign 0 (instrRefFromNum 0 :: InstrRef ())
  instrRef <- newProc $ \() -> do
    (a,b,c,d) <- midiin
    printks (fromString "%d %d %d %d\n") 1 (a,b,c,d)
    outs (0 :: Sig2)
  play instrRef [Note 0 (-1) ()]

{-
main = writeCsdBy setDebugTrace "tmp.csd" $ do
  global $ massign 0 (instrRefFromNum 0 :: InstrRef ())
  instrRef <- newProc $ \() -> do
    a <- notnum
    printks (fromString "%d\n") 1 (toSig a)
  play instrRef [Note 0 (-1) ()]
-}
