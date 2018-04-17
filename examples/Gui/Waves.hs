-- | Plays standard waveforms: pure tone, triangle, square and sawtooth.
module Waves where

import Csound.Base

main = dac $ do
    -- Creates a master volume slider.
    (gvol, vol) <- unSource $ masterVolume

    -- We use a standard element to experiment with four classic waveforms.
    -- The return value of the widget is a function that takes a frequency
    -- and returns the signal.
    (gw, f) <- unSource $ classicWaves "waves" 0

    -- Let's create a simple instrument that uses the waveform.
    let instr x = fades 0.1 0.1 * f x

    -- Places elements on window aligned horizontally.
    panel $ hor [sca 0.1 gvol, gw]

    -- Triggrs the instrument with midi keyboard.
    return $ vol * instr 220

