-- | Originally coded in Csound by Jacob Joaquin
--
-- http://codehop.com/2011/07/
module Main where

import Csound.Base

wobbly :: Sig -> Sig -> Sig -> Sig
wobbly spb coeff cps = a2
    where
        a1 = mean [saw (cps * 1.005), sqr (cps * 0.495)]
        idivision = 1 / (coeff * spb)
        klfo = kr $ triSeq [1] idivision

        -- filter
        ibase = cps
        imod  = ibase * 9
        a2    = moogladder a1 (ibase + imod * klfo) 0.6

spb = dspb
dspb = 0.45

instr (coeff, cps) = return $ wobbly (sig spb) (sig coeff) (sig $ cpspch cps)


main = dac $ mix $ str (dspb * 2) $ loopBy 2 $ sco instr $ melMap temp $ 
    [ (2, 6.04)
    , (1/3, 7.04)
    , (2, 6.04)
    , (1/1.5, 7.07)
    
    , (2, 5.09)
    , (1, 6.09)
    , (1/1.5, 5.09)
    , (1/3, 6.11)

    , (1, 6.04)
    , (1/3, 7.04)
    , (2, 6.04)
    , (1/1.5, 7.07) 
    
    , (2, 6.09)
    , (1, 7.09)
    , (1/1.5, 6.11)
    , (1/3, 6.07) 
    
    , (2, 6.04)
    , (1/3, 7.04)
    , (2, 6.04)
    , (1/1.5, 7.07) ]

