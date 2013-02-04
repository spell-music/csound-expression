module Main where

import Data.List(transpose)
import Csound.Base

sinWave = gen 16384 10 [1]
osc :: Sig -> Sig
osc phs = oscil 1 phs sinWave

instr :: D -> Out
instr pch = out $ 0.5 * (osc $ sig pch)


q = renderCsd $ return $ score instr [(0, 1, 440)]

main :: IO ()
main = writeFile "tmp.csd" q
