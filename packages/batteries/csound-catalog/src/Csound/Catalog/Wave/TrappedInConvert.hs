module Csound.Catalog.Wave.TrappedInConvert (
    ivory, blue, black, blackMarimba        
) where

import Csound.Base

-- | 
-- > ivory xdur glisDur vibRate cpsCoeff cps
ivory :: D -> D -> Sig -> D -> Sig -> Sig
ivory xdur glisDur vibRate cpsCoeff cps = mean 
    --    vibrato env                amplitude env               freq bias   phase   vibrato coeff   wave
    [ alg (linseg [0, xdur, 5])      (lincone 0 0.7 1 0.3 0)     0           0       1               sine
    , alg (lincone 0 0.6 6 0.4 0)    (lincone 0 0.9 1 0.1 0)     0.009       0.2     0.9             (sines [10, 9 .. 1])
    , alg (lincone 9 0.7 1 0.3 1)    (linenXdur 0.5 0.333)       0.007       0.3     1.2             (sines [10, 0, 9, 0, 8, 0, 7, 0, 6, 0, 5])
    , alg (expcone 1 0.4 3 0.6 0.02) 
          (expcone 0.0001 0.8 1 0.2 0.0001)                      0.005       0.5     0.97            (sines [10, 10, 9, 0, 0, 0, 3, 2, 0, 0, 1])  
    , alg (expcone 1 0.4 3 0.6 0.02) 
          (expdur [0.001, 0.5, 1, 0.1, 0.6, 0.2, 0.97, 0.2, 0.001])
                                                                 0.003       0.8     0.99            (sines [10, 0, 0, 0, 5, 0, 0, 0, 0, 0, 3])
    , alg (expcone 4 0.91 1 0.09 1)  
          (expdur [0.001, 0.6, 1, 0.2, 0.8, 0.1, 0.98, 0.1, 0.001])
                                                                 0.001       1.3     1.4             (sines [10, 0, 0, 0, 0, 3, 1])
    ]
    where
        alg :: Sig -> Sig -> D -> D -> D -> Tab -> Sig    
        alg vibrEnv ampEnv cpsBias phsBias vibrCoeff wave = 
            ampEnv * (oscBy wave ((cps + sig cpsBias + kvibr) * glis) `withD` phsBias)
            where glis = expseg [1, glisDur, 1, xdur - glisDur, cpsCoeff]
                  kvibr = vibrEnv * osc (vibRate * sig vibrCoeff)
                
        cone a x1 b x2 c = [a, x1 * xdur, b, x2 * xdur, c]
        lincone a x1 b x2 c = linseg $ cone a x1 b x2 c
        expcone a x1 b x2 c = expseg $ cone a x1 b x2 c
        linenXdur a b = linen 1 (a * xdur) xdur (b * xdur)


-- snow flakes

-- | 
-- > blue noteDuration numberOfHarmonics sweepRate lfoCps cps
--
-- * numberOfHarmonics ~ (6, 10)
-- 
-- * sweepRate ~ (0, 1)
--
-- * lfoCps ~ 20
blue :: D -> D -> D -> Sig -> Sig -> SE Sig
blue xdur harmNum sweepRate lfoCps cps = fmap aout k1
    where 
        k1 = randi 1 50
        k2 = lindurBy xdur [0, 0.5, 1, 0.5, 0]
        k3 = lindurBy xdur [0.005, 0.71, 0.015, 0.29, 0.01]
        k4 = k2 * (kr $ osc lfoCps `withD` 0.2)
        k5 = k4 + 2

        ksweep = lindurBy xdur [harmNum, sweepRate, 1, 1 - sweepRate, 1]
        kenv = expdurBy xdur [0.001, 0.01, 1, 0.99, 0.001]
        aout k = gbuzz kenv (cps + k3) k5 ksweep k (sines3 [(1, 1, 90)])

-- | Noise filtered with sweep filter.
--
-- > black noteDuration filterSweepStart filterSweepEnd bandWidth cps
--
-- * @filterSweepStart@, @filterSweepEnd@ - hearing range
--
-- * @bandWidth@ - (10, 50)
black :: D -> D -> D -> Sig -> Sig -> SE Sig
black xdur filterSweepStart filterSweepEnd bandWidth cps = 
    fmap aout $ rand 1
    where 
        k1 = expdurBy xdur [filterSweepStart, 1, filterSweepEnd]
        a1 anoise = reson anoise k1 (k1 / bandWidth) `withD` 1
        k3 = expdurBy xdur [0.001, 0.001, 1, 0.999, 0.001]
        a2 = k3 * osc (cps + 0.6 * (osc 11.3 `withD` 0.1))
        aout anoise = mean [a1 anoise, a2]


-- | Black with fixed parameters.
--
-- > blackMarimba cps
blackMarimba :: Sig -> SE Sig
blackMarimba = black 3 100 500 50

