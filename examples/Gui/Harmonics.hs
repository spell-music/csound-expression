-- Experiments with harmonics.
module Harmonics where

import Csound.Base 

harms :: [Sig] -> Sig -> Sig
harms ks cps = sum $ zipWith f ks (fmap (sig . double) [1 .. ])
    where f k n = k * osc (n * cps)

main = vdac $ do
    (g, ks) <- sliderBank "Harmonics"  (1 : replicate 13 0)
    (gv, v) <- slider "volume" uspan 0.5
    let instr = onMsg $ mul v . harms ks
    panel $ ver [g, sca 0.1 gv]
    return $ midi instr

