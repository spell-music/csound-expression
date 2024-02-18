module Main where
 
-- imports everything
import Csound.Base

-- Renders generated csd-file to the "tmp.csd".
-- press Ctrl-C to stop
main :: IO ()
main = dac $ osc 440
