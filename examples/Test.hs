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
pureTone :: D -> Sig
pureTone cps = 0.4 * (myOsc $ sig cps)

-- Let's trigger the instrument from the score section. It plays three notes.
res = sco pureTone $ CsdEventList 5 [(0, 1, 440), (1, 1, 330), (2, 2, 220)]

-- Renders generated csd-file to the "tmp.csd".
main :: IO ()
main = dac $ runMix res
