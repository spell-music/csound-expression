module EnvelopeAndHarmonics where

import Csound.Base

import Harmonics(harms)

main = vdac $ do
    (gharms, ks) <- sliderBank "harmonics" (1 : replicate 14 0)
    (gadsr, env) <- linAdsr "amplitude envelope" (AdsrBound 1 1 3) (AdsrInit 0.1 0.1 0.5 0.1)
    (gvol, vol)  <- masterVolume
    panel $ ver [sca 0.1 gvol, sca 0.25 gadsr, gharms]

    let instr = onMsg $ \cps -> vol * env * harms ks cps
    return $ midi instr

