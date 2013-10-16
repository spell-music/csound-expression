module Main where
 
-- imports everything
import Csound.Base

-- Let's define a simple sound unit that 
-- reads in cycles the table that contains a single sine partial.
-- oscil1 is the standard oscillator with linear interpolation.
-- 1 - means the amplitude, cps - is cycles per second and the last argument
-- is the table that we want to read. 
myOsc :: Sig -> Sig
myOsc cps = oscili 1 cps (sines [1])

-- Let's define a simple instrument that plays a sound on the specified frequency.
-- We use @sig@ to convert a constant value to signal and then plug it in the osc unit. 
-- We make it a bit quieter by multiplying with 0.5.
pureTone :: (D, D) -> Sig
pureTone (amp, cps) = 0.5 * sig amp * env * (myOsc $ sig cps)
    where env = linsegr [0, 0.2, 1, 1, 0.5] 1.5 0

-- Let's create a midi instrument. It takes a special value of type Msg
-- that contains midi parameters.
midiInstr :: Msg -> Sig
midiInstr msg = pureTone $ ampCps msg

main :: IO ()
main = dac $ midi midiInstr 

