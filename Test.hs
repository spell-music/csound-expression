module Main where

import Csound.Base

osc :: Sig -> Sig
osc phs = oscil 1 phs (genHigh 10 [1])

instr :: D -> Out
instr pch = out $ 0.5 * (osc $ kr pch)

res = score instr [(0, 1, 440)]

main :: IO ()
main = writeFile "tmp.csd" $ renderCsd [res]
