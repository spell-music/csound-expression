-- | An ADSR-envelope.
module Envelope where

import Csound.Base

-- We use `vdac` to activate the virtual midi keyboard.
-- We can use just `dac` if we have a hardware midi controller.
main = vdac $ do
    -- Creates an Attack-decay-sustain-release widget.
    -- We specify a label, time bounds for attack, decay and release
    -- and init values. It returns a gui-widget and the signal of
    -- the envelope.
    (genv, env) <- linAdsr "" (AdsrBound 1 1 3) (AdsrInit 0.1 0.5 0.5 0.1)
    
    -- Creates a master volume widget.
    (gvol, vol) <- masterVolume 

    -- A simple midi-instrument that plays a triangle wave with
    -- given amplitude envelope.
    let instr = onMsg $ \cps -> env * tri cps

    -- Let's place everything on window. Two elements
    -- are aligned vertically and the master volume slider is
    -- smaller than the envelope widget (we scale it by 0.25).
    panel $ ver [genv, sca 0.25 gvol]

    -- Triggers the midi instrument and scales
    -- the output by given master volume.
    return $ vol * midi instr

