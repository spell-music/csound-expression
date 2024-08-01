-- | Experiments with amplitude envelope and harmonics.
module EnvelopeAndHarmonics where

import Csound.Base

import Harmonics (harms)

main = dac $ do
  -- Creates a bank of 15 sliders. We specify the label
  -- and initial values for slider (0 is minimum and 1 is maximum
  -- for each slider.
  (gharms, ks) <- sliderBank "harmonics" (1 : replicate 14 0)
  -- We create an Adsr envelope (see example Envelope.hs)
  (gadsr, env) <- linAdsr "amplitude envelope" (AdsrBound 1 1 3) (AdsrInit 0.1 0.1 0.5 0.1)
  -- Creates a master volume slider
  (gvol, vol) <- masterVolume
  (gcps, cps) <- slider "frequency" (expSpan 50 1000) 220

  -- Places everything on window. Elements are aligned vertically.
  -- The volume slider is smaller than the ADSR-envelope. The element
  -- for harmonics is the biggest one. We alter the sizes with the function `sca`
  -- (it's short for scale)
  panel $ ver [sca 0.1 gvol, sca 0.1 gcps, sca 0.25 gadsr, gharms]

  -- Let's create a simple instrument with the custom harmonics
  -- and amplitude envelope.
  let
    instr = env * harms ks cps
  return $ mul vol $ sched (const $ return instr) $ withDur 1 $ metro 0.5
