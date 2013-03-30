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
-- We use kr to convert a constant value to signal and then plug it in the osc unit. 
-- We make it a bit quieter by multiplying with 0.5.
pureTone :: D -> Sig
pureTone cps = 0.4 * (myOsc $ kr cps)

-- Let's trigger the instrument from the score section.
-- It plays a single note that starts at 0 and lasts for 1 second and 
-- triggers the instrument 'instr' with frequency of 440 (Hz).
res = mix dist $ mix echo $ mix env $ sco pureTone $ stretch 2 $ chord [x, delay 0.5 x]
    where x = line [temp 350, temp 220, temp 440]

env a = expseg [0.01, 0.2*idur, 1, 0.6*idur, 1, 0.2*idur, 0.01] * a
echo a = do
    delayr 1
    a1 <- deltap3 0.05
    a2 <- deltap3 0.12
    a3 <- deltap3 0.22
    delayw a
    return $ 0.7 * a + 0.2 * a1 + 0.1 * a2 + 0.05 * a3

dist x = 1.2 * osc 7 * x

-- Renders generated csd-file to the "tmp.csd".
main :: IO ()
main = writeCsd "tmp.csd" res
