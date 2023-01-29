module Core where

import Csound.Typed.Core.Types
import Csound.Typed.Core.Opcodes
import Data.Default

main =
  putStrLn =<< renderSE def res

res :: SE ()
res = do
  ref <- newCtrlRef (0 :: Sig, 0 :: Sig)
  instr1 <- newInstr (setInstr1 ref)
  instr2 <- newInstr setInstr2
  instr3 <- newInstr setInstr2
  event_i "i" instr1 0 1 []
  event_i "i" instr2 0 2 []
  event_i "i" instr3 0 3 []
  where
    setInstr1 :: Ref (Sig, Sig) -> (D, D) -> SE ()
    setInstr1 ref (a, b) = do
      lref <- newRef (0 :: Sig)
      lk <- readRef lref
      (ga, gb) <- readRef ref
      outs (ares + ga + lk) (ares + gb)
      where
        ares = linseg [a, 1, b]

    setInstr2 :: D -> SE ()
    setInstr2 a = do
      outs ares ares
      where
        ares = linseg [a, 1, a] * oscil 1 220 (preTab (SizePlain 2048) 10 (ArgsPlain $ const [1]))
