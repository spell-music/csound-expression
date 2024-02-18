module Main where

import Csound.Base

pulseWidth :: Sig -> Sig -> Sig
pulseWidth amp cps = asignal
    where
        ilforate  = 		2.3					-- LFO SPEED IN Hz
        isawlvl   = 		0.5	 				-- LEVEL OF SAWTOOTH WAVEFORM
        ipwmlvl   = 		0.5	 				-- LEVEL OF PULSE WAVEFORM
        ipwm	  = 		0.2	 				-- DC OFFSET OF PULSE width
        ipwmlfo   = 		0.1	 				-- DEPTH OF PULSE WIDTH MODULATION
        ivcffrq   = 		800	 				-- CUTOFF OF GLOBAL LOW PASS FILTER
        ienvflt   = 		200	 				-- MAX CHANGE IN LPF CUTOFF BY ENVELOPE
        ikbdflt   = 		0.1	 				-- RELATIVE CHANGE IN LPF CUTOFF TO PITCH
        -- the oscillators
        klfo        = kr $ osc ilforate
        asaw        = oscBy (elins [-1, 1]) cps 
        apwm        = table (0.5 + asaw / 2 + (klfo * ipwmlfo + ipwm)) (lins [-1, 50, -1, 0, 1, 50, 1]) `withD` 1
        awaves      = isawlvl * asaw + ipwmlvl * apwm
        -- the envelope
        -- the filters
        asignal     = amp * butlp awaves (ivcffrq + cps * ikbdflt + ienvflt * amp)

instr :: Msg -> Sig
instr msg = mean $ fmap (f . (+cps)) [0.25, 0.2, 0.1, 0, -0.1, -0.2, -0.25]
    where amp = ampmidi msg  
          cps = cpsmidi msg
          env1 = linsegr [0, 0.5, 1, 3, 0.8, 1, 0.8] 2 0
          env2 = expsegr [1, 10 + amp * 5, 0.0001] (2 + 5 * amp) 0.0001

          f x = 0.5 * sig amp * pulseWidth env2 (sig x)

main = dac $ midi 1 instr


