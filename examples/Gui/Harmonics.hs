-- Experiments with harmonics.
module Harmonics where

import Csound.Base

-- a sum of pure tones that form a hormanic series.
harms :: [Sig] -> Sig -> Sig
harms ks cps = sum $ zipWith f ks (fmap (sig . double) [1 .. ])
    where f k n = k * osc (n * cps)

main = dac $ do
    -- Let's create a list of coefficients.
    (g, ks) <- unSource $ sliderBank "Harmonics"  (1 : replicate 13 0)
    (gv, v) <- unSource $ masterVolume
    (gcps, cps)  <- unSource $ slider "frequency"  (expSpan 50 1000) 220

    -- A simple instrument that plays a harmonic series
    -- with the given coefficients.
    let instr = onMsg $ harms ks
    -- Places elements on window with vertical alignment.
    panel $ ver [g, sca 0.1 gv, sca 0.1 gcps]
    -- Let's trigger the instrument with midi controller.
    let instr = harms ks cps
    return $ mul v $ sched (const $ return instr) $ withDur 1 $ metro 0.5
