module Test where

import Csound

sinWave = gen 16384 10 [1]
osc :: Tab -> Sig -> Sig
osc tab phs = oscil 1 phs tab
    
rec :: Sig 
rec = osc sinWave rec

instr1 :: (D, D) -> Sig
instr1 (amp, phs) = k * osc sinWave (sig phs)
    where k = sig amp * ifB (notB $ ((1 :: Sig) <* 2) ||* ((1 :: Sig) >* 2)) r1 r2
          (r1, r2, _) = se2 $ rand 440  
          
instr2 :: (D, D, S) -> Sig
instr2 (amp, phs, fileName) = q * sig amp * osc sinWave (sig phs)
    where q = soundin fileName


sco1 = line $ map temp [(1, 440), (0.5, 220), (1, 440)]            
sco2 = delay 0.5 $ stretch 10 $ temp (0.25, 1000, str "moo.wav")
    
res :: Msg -> Sig
res msg = (\(a, b) -> a + b) $ buf $ do
    b <- delayr 1
    tap <- deltap 0.5    
    delayw (q + 0.2 * tap)    

    b2 <- delayr 1
    tap2 <- deltap 0.5    
    delayw (q + 0.2 * tap2)        

    return (q + tap, q + tap2)
    where q = osc sinWave $ sig $ cpsmidi msg    


q = csd def id [midi 4 res, sco instr1 sco1, sco instr2 sco2]
