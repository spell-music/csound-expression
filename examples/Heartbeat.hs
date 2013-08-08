-- | The Heartbeat by Julie Friedman (without crackle)
--
-- requires temporal-csound
--
-- > cabal install temporal-csound
module Main where

import Csound

linenIdur :: Sig -> D -> D -> Sig
linenIdur a rise dec = linen a (idur * rise) idur (idur * dec) 

--------------------------------------

crackle amp cps att dec = (a3 * kpan, a3 * (1 - kpan))
    where a1 = k1 * osc (sig cps)
          k1 = linen (sig amp) att idur dec
          kpan = 0.5 * (1 + osc (5 * sig idur))
          a2 = fof a1 (a1 + sig cps) (a1 * sig amp / 50) k1 200 0.003 0.017 0.005 20 f1 f2 cps
          arev = reverb2 a2 5 1
          a3 = 0.2 * (a2 + arev) 
          f1 = sines [1] 								            -- SINE WAVE
          f2 = sines [1, 0.5, 0.3, 0.25, 0.2, 0.167, 0.14, 0.111] 	-- SAWTOOTH


heartbeat :: D -> Sig
heartbeat amp = phi amp 0.0024 f12 + phi amp 0.0078 f13
    where phi amp dec tab = oscili (sig amp * linseg [1, idur, dec]) 4 tab
          f12 = sines2 [(10, 1), (16, 1.5), (22, 2), (23, 1.5)]
          f13 = sines2 [(25, 1), (29, 0.5), (32, 0.2)] 


monoPluck :: D -> D -> D -> D -> Sig
monoPluck amp cps pick plk = a3  
    where repluck' freq a = repluck plk (sig amp) freq (sig pick) 0.5 a           
          a1 = 0.5 * (
                repluck' (cps - 1) (osc $ sig $ cps - 2) 
              + repluck' (cps + 1) (osc $ sig $ cps + 2))
          a2 = linenIdur (a1/2) 0.2 0.8
          arev = reverb2 a2 1.5 1
          a3 = (a2 + 0.6 * arev) / 1.6
          

chorusel amp cps rise dec = (ar1, ar2)
    where k1 = linen (sig amp) rise idur dec
          k2 = linseg [1, idur, 0]
          k3 = kr $ osc 2
          k4 = kr $ 0.5 * osc 2
          inote = cpspch cps
          as = fmap (\(d, a, f) -> k1 * f (sig (inote + d) + a)) [
            (-1, k3, saw),
            (1,  k4, f9),
            (-0.5, 0, f9),
            (0.5, 0, saw),
            (-2, k4, saw),
            (2, k3, f9),
            (-1.5, k3, saw),
            (1.5, k3, f9),
            (-0.25, 0, f9),
            (0.25, 0, saw),
            (-0.8, k4, saw),
            (0.8, k4, f9)]
            
          ars = zipWith3 (\a k d -> withInits (areson a k d) (1::D)) as (k2:(k2*k3):repeat k2) (fmap (sig . double) $ [10, 20 .. 80] ++ [50, 60 .. 80])
          meanArs = (/ 5.5) . sum . fmap (ars !!) 
          asig1 = meanArs [0, 3, 5, 7, 8]
          asig2 = meanArs [1, 2, 4, 6, 9]
          asig3 = 0.5 * (ars !! 10 + ars !! 11)          
          
          ar1 = 0.5 * (asig1 + asig3)
          ar2 = 0.5 * (asig2 + asig3)          
                    
          f9 phs = oscil 1 phs $ sines 
                        [ 0.28, 1, 0.74, 0.66, 0.78, 0.48, 0.05, 0.33, 0.12
                        , 0.08, 0.01, 0.54, 0.19, 0.08, 0.05, 0.16, 0.01
                        , 0.11, 0.3, 0.02, 0.2] 
          
instr1 :: D -> Sig2
instr1 amp = (a, a)
    where a = 0.8 * heartbeat amp

instrPluck :: (D, D) -> Sig2
instrPluck (amp, pan) = (sig pan * a,  (sig $ 1 - pan) * a)
    where a = monoPluck 0.5 (cpspch 8) 0.8 0.3

instrChorusel :: (D, D, D, D) -> Sig2
instrChorusel (cps, pan, a, b) = chorusel cps pan a b

instrCrackle :: D -> Sig2
instrCrackle cps = crackle (0.5::D) cps 12 20

scoBeat = sco instr1 $ delay 2 $ loop 32 $ line [0.25 *| lineTemp [0.5, 0.3], rest 1.5]

scoPluck = sco instrPluck $ delay 8 $ line $ take n $ zipWith (\amp pan -> 0.5 *| temp (amp, pan)) 
    ([0, (v/40) .. v] ++ repeat v) (cycle [0.2, 0.8])
    where v = 0.6
          dur = 65
          n = floor $ (dur - 8)/0.5
 
scoChorusel = sco instrChorusel $ chord $ unroll =<< [
    (0, 15, (0.4, [7, 7.07, 6, 8], 10, 5)),
    (18, 17, (0.27, [6, 7, 7.07, 8.02, 8.03, 5], 9, 6)),
    (34, 21, (0.35, [6], 8, 8)),
    (40, 15, (0.35, [6.07], 7, 7)),
    (48, 7,  (0.35, [7.05], 3.5, 3.5)),
    (55, 10, (0.35, [7, 8, 7.07, 6], 5, 8))]
    where unroll (t, dur, (amp, cps, rise, dec)) = [delay t $ stretch dur $ temp (amp, c, rise, dec) | c <- cps]
    
scoCrackle = sco instrCrackle $ chord [
    event 8 100,
    delay 13 $ event 5 50]   
   
main = dac $ runMix $ chord [
    scoBeat, 
    scoChorusel, 
    scoPluck, 
    scoCrackle]

