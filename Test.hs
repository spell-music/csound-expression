module Main where

import Data.List(transpose)
import Csound

sinWave = gen 16384 10 [1]
osc :: Tab -> Sig -> Sig
osc tab phs = oscil 1 phs tab
    
rec :: Sig 
rec = osc sinWave rec

instr1 :: (D, D) -> SE [Sig]
instr1 (amp, phs) = do
    r1 <- rand 440
    r2 <- rand 440    
    out $ k r1 r2 * osc sinWave (sig phs)
    where k r1 r2 = sig amp * ifB (notB $ ((1 :: Sig) <* 2) ||* ((1 :: Sig) >* 2)) r1 r2
          
          
instr2 :: (D, D, S) -> SE [Sig]
instr2 (amp, phs, fileName) = out $ a * b
    where (a, b) = soundin2 fileName


sco1 = line $ map temp [(1, 440), (0.5, 220), (1, 440)]            
sco2 = chord [
    delay 0.5 $ stretch 10 $ temp (0.25, 1000, str "moo.wav"),
    temp (0.1, 220, str "boo.wav"),
    temp (1, 440, str "boo.wav")]
    
res :: Msg -> SE [Sig]
res msg = do
    b <- delayr 1
    tap <- deltap 0.5    
    delayw (q + 0.2 * tap)    

    b2 <- delayr 1
    tap2 <- deltap 0.5    
    delayw (q + 0.2 * tap2)        

    return [q + tap, q + tap2]
    where q = osc sinWave $ sig $ cpsmidi msg    


q = csd def mixing [pgmassign Nothing 4 res, sco instr1 sco1, sco instr2 sco2]

main :: IO ()
main = putStrLn q
