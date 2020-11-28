module Csound.Catalog.Wave.Deserted(
    simpleMarimba, marimbaWave, phasingSynth, noiz, wind        
) where

import Csound.Base

-- | Simple marimba (by John Fitch) with percussive envelope.
--
--  > simpleMarimba noteDur cps
simpleMarimba :: D -> Sig -> Sig
simpleMarimba xdur = marimbaWave xdur kenv
    where kenv = expseg [0.0001, 0.03, 1, xdur - 0.03, 0.001]
               * linseg [1, 0.03, 1, xdur - 0.03, 3]

-- | Simple marimba (by John Fitch) without fixed envelope.
--
--  > marimba noteDur amp cps
marimbaWave :: D -> Sig -> Sig -> Sig
marimbaWave xdur amp cps = a6
    where
        k10 = linseg [2.25, 0.03, 3, xdur - 0.03, 2] 
        a1  = gbuzz amp cps k10 0 35 (sines3 [(1, 1, 90)])
        a2  = reson' a1 500 50 
        a3  = reson' a2 150 100 
        a4  = reson' a3 3500 150 
        a5  = reson' a4 3500 150 
        a6  = balance a5 a1
        reson' a b c = reson a b c `withD` 1

-- | Sound of the wind.
--
-- > wind noteDur (bandRise, bandDecay) (freqRise, freqDecay) attackDecayRatio
--
-- -* bandRise, banDecay, freqRise, freqDecay -- (50, 1000)
--
-- * attackDecayRatio -- (0, 1)
wind :: D -> (D, D) -> (D, D) -> D -> SE Sig
wind xdur (bandRise, bandDec) (freqRise, freqDec) winds = 
    fmap fromRnd $ rand 1
    where
        valu1 = 100
        valu2 = 50
        winde = 1 - winds
        ramp a b = linseg [a, xdur, b]
        fromRnd a = aout
            where
                a2 = butbp a  (ramp freqRise freqDec) (ramp bandRise bandDec) 
                a3 = butbp a2 (ramp (freqRise - valu1) (freqDec + valu2))
                              (ramp (bandRise + valu1) (bandDec - valu2))
                
                aout = (a2 + a3) * linseg [0, xdur * winds, 1, xdur * winde, 0]

-- | 
-- > noiz cps
noiz :: Sig -> SE Sig
noiz cps = fmap a2 k2
    where 
        k1 = linseg [1, 0.05, 100, 0.2, 100, 2, 1, idur, 1]
        k2 = fmap ( `withD` 1) $ rand 500   

        buzz' kamp kcps = buzz kamp (sig kcps * cps)  k1 sine
        
        a1 = mean $ zipWith buzz' [0.3, 1, 1] [1, 0.5, 0.501]       
        a2 k = a1 * osc k

-- | 
-- > phasingSynth amp cps
phasingSynth :: Sig -> Sig -> Sig
phasingSynth amp cps = aout
    where
        osc' ftab k ph = oscBy ftab (cps * k) `withD` ph
        osc1 = osc' sine
        osc2 = osc' $ sines [1, 0, 0.9, 0, 0.8, 0, 0.7, 0, 0.6, 0, 0.5, 0, 0.4, 0, 0.3, 0, 0.2, 0, 0.1] 
        asum = amp * mean 
                [ osc1 1 0
                , osc2 1.008 0.02
                , osc1 0.992 0.04
                , osc2 2     0.06
                , osc2 1     0.08
                , osc1 1     0.01 ]
        kosc1 = 0.5 * once sine 
        kosc2 = 0.5 * once sine `withD` 0.4
        kosc3 = 1.0 * once sine `withD` 0.8

        afilt  = sum 
                [ butbp asum kosc1 1000
                , butbp asum kosc2 300
                , butbp asum kosc3 20 ]

        aout  = mean [afilt, asum]
