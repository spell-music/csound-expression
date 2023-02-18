module Main where

import Csound.Base

instr :: (D, D) -> SE Sig
instr (amp, cps) = return $ (sig amp) * fades 0.01 0.1 * osc (sig cps)

main = writeCsd "message.csd" $ trigByName "osc" instr
