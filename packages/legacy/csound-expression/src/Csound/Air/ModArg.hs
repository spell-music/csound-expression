{-# Language TypeFamilies, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
-- | Argument modifiers. Functions to transform arguments of the function with flexibility.
module Csound.Air.ModArg(
    -- * Basic class
    ModArg1(..), ModArg2(..), ModArg3(..), ModArg4(..),
    -- ** Delayed
    delModArg1, delModArg2, delModArg3, delModArg4,

    -- * Oscillators
    oscArg1, oscArg2, oscArg3, oscArg4,
    triArg1, triArg2, triArg3, triArg4,
    sqrArg1, sqrArg2, sqrArg3, sqrArg4,
    sawArg1, sawArg2, sawArg3, sawArg4,

    -- ** Random phase
    rndOscArg1, rndOscArg2, rndOscArg3, rndOscArg4,
    rndTriArg1, rndTriArg2, rndTriArg3, rndTriArg4,
    rndSqrArg1, rndSqrArg2, rndSqrArg3, rndSqrArg4,
    rndSawArg1, rndSawArg2, rndSawArg3, rndSawArg4,

    -- ** Delayed
    delOscArg1, delOscArg2, delOscArg3, delOscArg4,
    delTriArg1, delTriArg2, delTriArg3, delTriArg4,
    delSqrArg1, delSqrArg2, delSqrArg3, delSqrArg4,
    delSawArg1, delSawArg2, delSawArg3, delSawArg4,

    -- ** Delayed with Random phase
    delRndOscArg1, delRndOscArg2, delRndOscArg3, delRndOscArg4,
    delRndTriArg1, delRndTriArg2, delRndTriArg3, delRndTriArg4,
    delRndSqrArg1, delRndSqrArg2, delRndSqrArg3, delRndSqrArg4,
    delRndSawArg1, delRndSawArg2, delRndSawArg3, delRndSawArg4,

    -- * Noise
    noiseArg1, noiseArg2, noiseArg3, noiseArg4,
    pinkArg1, pinkArg2, pinkArg3, pinkArg4,
    jitArg1, jitArg2, jitArg3, jitArg4,
    gaussArg1, gaussArg2, gaussArg3, gaussArg4,
    gaussiArg1, gaussiArg2, gaussiArg3, gaussiArg4,

    -- ** Delayed
    delNoiseArg1, delNoiseArg2, delNoiseArg3, delNoiseArg4,
    delPinkArg1, delPinkArg2, delPinkArg3, delPinkArg4,
    delJitArg1, delJitArg2, delJitArg3, delJitArg4,
    delGaussArg1, delGaussArg2, delGaussArg3, delGaussArg4,
    delGaussiArg1, delGaussiArg2, delGaussiArg3, delGaussiArg4,

    -- * Envelopes
    adsrArg1, adsrArg2, adsrArg3, adsrArg4,
    xadsrArg1, xadsrArg2, xadsrArg3, xadsrArg4,

    -- ** Delayed
    delAdsrArg1, delAdsrArg2, delAdsrArg3, delAdsrArg4,
    delXadsrArg1, delXadsrArg2, delXadsrArg3, delXadsrArg4

) where

import Data.Kind (Type)

import Csound.Typed
import Csound.Typed.Opcode(gauss, gaussi, jitter, linseg, linsegr, expsegr)
import Csound.Air.Wave
import Csound.Air.Envelope

-- trumpet:
-- dac $ mul 1.3 $ mixAt 0.15 largeHall2 $ midi $ onMsg (\cps -> (mul (linsegr [0,0.01, 1, 3, 0.2] 0.2 0) . at (jitterArg1 (0.15 + 0.05 * uosc 0.2) 3 20  alp1 (mul (fades 0.2 0.2) $ 2700 + 0.6 * cps) 0.2) . gaussArg1 0.03 (\x -> return (saw x) + mul (0.12 * expseg [1, 2, 0.1]) (bat (alp1 cps 0.4) white))) cps)

delEnv :: SigSpace a => D -> D -> a -> a
delEnv delTime riseTime asig = mul (linseg [0, delTime, 0, riseTime, 1]) asig

delModArg1 :: (SigSpace a, ModArg1 a b) => D -> D -> Sig -> a -> b -> ModArgOut1 a b
delModArg1 delTime riseTime depth modSig f = modArg1 (delEnv delTime riseTime depth) modSig f

delModArg2 :: (SigSpace a, ModArg2 a b) => D -> D -> Sig -> a -> b -> ModArgOut2 a b
delModArg2 delTime riseTime depth modSig f = modArg2 (delEnv delTime riseTime depth) modSig f

delModArg3 :: (SigSpace a, ModArg3 a b) => D -> D -> Sig -> a -> b -> ModArgOut3 a b
delModArg3 delTime riseTime depth modSig f = modArg3 (delEnv delTime riseTime depth) modSig f

delModArg4 :: (SigSpace a, ModArg4 a b) => D -> D -> Sig -> a -> b -> ModArgOut4 a b
delModArg4 delTime riseTime depth modSig f = modArg4 (delEnv delTime riseTime depth) modSig f

-- adsr mod

adsrArg1 :: (ModArg1 Sig b) => Sig -> D -> D -> D -> D -> b -> ModArgOut1 Sig b
adsrArg1 depth a d s r f = modArg1 depth (leg a d s r) f

adsrArg2 :: (ModArg2 Sig b) => Sig -> D -> D -> D -> D -> b -> ModArgOut2 Sig b
adsrArg2 depth a d s r f = modArg2 depth (leg a d s r) f

adsrArg3 :: (ModArg3 Sig b) => Sig -> D -> D -> D -> D -> b -> ModArgOut3 Sig b
adsrArg3 depth a d s r f = modArg3 depth (leg a d s r) f

adsrArg4 :: (ModArg4 Sig b) => Sig -> D -> D -> D -> D -> b -> ModArgOut4 Sig b
adsrArg4 depth a d s r f = modArg4 depth (leg a d s r) f

-- delayed adsr mod

delLeg :: D -> D -> D -> D -> D -> Sig
delLeg delTime a d s r = linsegr [0, delTime, 0, a, 1, d, s] r 0

delAdsrArg1 :: (ModArg1 Sig b) => D -> Sig -> D -> D -> D -> D -> b -> ModArgOut1 Sig b
delAdsrArg1 delTime depth a d s r f = modArg1 depth (delLeg delTime a d s r) f

delAdsrArg2 :: (ModArg2 Sig b) => D -> Sig -> D -> D -> D -> D -> b -> ModArgOut2 Sig b
delAdsrArg2 delTime depth a d s r f = modArg2 depth (delLeg delTime a d s r) f

delAdsrArg3 :: (ModArg3 Sig b) => D -> Sig -> D -> D -> D -> D -> b -> ModArgOut3 Sig b
delAdsrArg3 delTime depth a d s r f = modArg3 depth (delLeg delTime a d s r) f

delAdsrArg4 :: (ModArg4 Sig b) => D -> Sig -> D -> D -> D -> D -> b -> ModArgOut4 Sig b
delAdsrArg4 delTime depth a d s r f = modArg4 depth (delLeg delTime a d s r) f

-- expon adsr mod

xadsrArg1 :: (ModArg1 Sig b) => Sig -> D -> D -> D -> D -> b -> ModArgOut1 Sig b
xadsrArg1 depth a d s r f = modArg1 depth (xeg a d s r) f

xadsrArg2 :: (ModArg2 Sig b) => Sig -> D -> D -> D -> D -> b -> ModArgOut2 Sig b
xadsrArg2 depth a d s r f = modArg2 depth (xeg a d s r) f

xadsrArg3 :: (ModArg3 Sig b) => Sig -> D -> D -> D -> D -> b -> ModArgOut3 Sig b
xadsrArg3 depth a d s r f = modArg3 depth (xeg a d s r) f

xadsrArg4 :: (ModArg4 Sig b) => Sig -> D -> D -> D -> D -> b -> ModArgOut4 Sig b
xadsrArg4 depth a d s r f = modArg4 depth (xeg a d s r) f

-- delayed expon adsr mod

delXeg :: D -> D -> D -> D -> D -> Sig
delXeg delTime a d s r = expsegr [0.001, delTime, 0.001, a, 1, d, s] r 0.001

delXadsrArg1 :: (ModArg1 Sig b) => D -> Sig -> D -> D -> D -> D -> b -> ModArgOut1 Sig b
delXadsrArg1 delTime depth a d s r f = modArg1 depth (delXeg delTime a d s r) f

delXadsrArg2 :: (ModArg2 Sig b) => D -> Sig -> D -> D -> D -> D -> b -> ModArgOut2 Sig b
delXadsrArg2 delTime depth a d s r f = modArg2 depth (delXeg delTime a d s r) f

delXadsrArg3 :: (ModArg3 Sig b) => D -> Sig -> D -> D -> D -> D -> b -> ModArgOut3 Sig b
delXadsrArg3 delTime depth a d s r f = modArg3 depth (delXeg delTime a d s r) f

delXadsrArg4 :: (ModArg4 Sig b) => D -> Sig -> D -> D -> D -> D -> b -> ModArgOut4 Sig b
delXadsrArg4 delTime depth a d s r f = modArg4 depth (delXeg delTime a d s r) f

-- oscil lfo

oscArg1 :: (ModArg1 Sig b) => Sig -> Sig -> b -> ModArgOut1 Sig b
oscArg1 depth rate f = modArg1 depth (osc rate) f

oscArg2 :: (ModArg2 Sig b) => Sig -> Sig -> b -> ModArgOut2 Sig b
oscArg2 depth rate f = modArg2 depth (osc rate) f

oscArg3 :: (ModArg3 Sig b) => Sig -> Sig -> b -> ModArgOut3 Sig b
oscArg3 depth rate f = modArg3 depth (osc rate) f

oscArg4 :: (ModArg4 Sig b) => Sig -> Sig -> b -> ModArgOut4 Sig b
oscArg4 depth rate f = modArg4 depth (osc rate) f

-- delayed oscil lfo

delOscArg1 :: (ModArg1 Sig b) => D -> D -> Sig -> Sig -> b -> ModArgOut1 Sig b
delOscArg1 delTime riseTime depth rate f = delModArg1 delTime riseTime depth (osc rate) f

delOscArg2 :: (ModArg2 Sig b) => D -> D -> Sig -> Sig -> b -> ModArgOut2 Sig b
delOscArg2 delTime riseTime depth rate f = delModArg2 delTime riseTime depth (osc rate) f

delOscArg3 :: (ModArg3 Sig b) => D -> D -> Sig -> Sig -> b -> ModArgOut3 Sig b
delOscArg3 delTime riseTime depth rate f = delModArg3 delTime riseTime depth (osc rate) f

delOscArg4 :: (ModArg4 Sig b) => D -> D -> Sig -> Sig -> b -> ModArgOut4 Sig b
delOscArg4 delTime riseTime depth rate f = delModArg4 delTime riseTime depth (osc rate) f

-- tri lfo

triArg1 :: (ModArg1 Sig b) => Sig -> Sig -> b -> ModArgOut1 Sig b
triArg1 depth rate f = modArg1 depth (tri rate) f

triArg2 :: (ModArg2 Sig b) => Sig -> Sig -> b -> ModArgOut2 Sig b
triArg2 depth rate f = modArg2 depth (tri rate) f

triArg3 :: (ModArg3 Sig b) => Sig -> Sig -> b -> ModArgOut3 Sig b
triArg3 depth rate f = modArg3 depth (tri rate) f

triArg4 :: (ModArg4 Sig b) => Sig -> Sig -> b -> ModArgOut4 Sig b
triArg4 depth rate f = modArg4 depth (tri rate) f

-- delayed tri lfo

delTriArg1 :: (ModArg1 Sig b) => D -> D -> Sig -> Sig -> b -> ModArgOut1 Sig b
delTriArg1 delTime riseTime depth rate f = delModArg1 delTime riseTime depth (tri rate) f

delTriArg2 :: (ModArg2 Sig b) => D -> D -> Sig -> Sig -> b -> ModArgOut2 Sig b
delTriArg2 delTime riseTime depth rate f = delModArg2 delTime riseTime depth (tri rate) f

delTriArg3 :: (ModArg3 Sig b) => D -> D -> Sig -> Sig -> b -> ModArgOut3 Sig b
delTriArg3 delTime riseTime depth rate f = delModArg3 delTime riseTime depth (tri rate) f

delTriArg4 :: (ModArg4 Sig b) => D -> D -> Sig -> Sig -> b -> ModArgOut4 Sig b
delTriArg4 delTime riseTime depth rate f = delModArg4 delTime riseTime depth (tri rate) f

-- sqr lfo

sqrArg1 :: (ModArg1 Sig b) => Sig -> Sig -> b -> ModArgOut1 Sig b
sqrArg1 depth rate f = modArg1 depth (sqr rate) f

sqrArg2 :: (ModArg2 Sig b) => Sig -> Sig -> b -> ModArgOut2 Sig b
sqrArg2 depth rate f = modArg2 depth (sqr rate) f

sqrArg3 :: (ModArg3 Sig b) => Sig -> Sig -> b -> ModArgOut3 Sig b
sqrArg3 depth rate f = modArg3 depth (sqr rate) f

sqrArg4 :: (ModArg4 Sig b) => Sig -> Sig -> b -> ModArgOut4 Sig b
sqrArg4 depth rate f = modArg4 depth (sqr rate) f

-- sqr lfo

delSqrArg1 :: (ModArg1 Sig b) => D -> D -> Sig -> Sig -> b -> ModArgOut1 Sig b
delSqrArg1 delTime riseTime depth rate f = delModArg1 delTime riseTime depth (sqr rate) f

delSqrArg2 :: (ModArg2 Sig b) => D -> D -> Sig -> Sig -> b -> ModArgOut2 Sig b
delSqrArg2 delTime riseTime depth rate f = delModArg2 delTime riseTime depth (sqr rate) f

delSqrArg3 :: (ModArg3 Sig b) => D -> D -> Sig -> Sig -> b -> ModArgOut3 Sig b
delSqrArg3 delTime riseTime depth rate f = delModArg3 delTime riseTime depth (sqr rate) f

delSqrArg4 :: (ModArg4 Sig b) => D -> D -> Sig -> Sig -> b -> ModArgOut4 Sig b
delSqrArg4 delTime riseTime depth rate f = delModArg4 delTime riseTime depth (sqr rate) f

-- saw lfo

sawArg1 :: (ModArg1 Sig b) => Sig -> Sig -> b -> ModArgOut1 Sig b
sawArg1 depth rate f = modArg1 depth (saw rate) f

sawArg2 :: (ModArg2 Sig b) => Sig -> Sig -> b -> ModArgOut2 Sig b
sawArg2 depth rate f = modArg2 depth (saw rate) f

sawArg3 :: (ModArg3 Sig b) => Sig -> Sig -> b -> ModArgOut3 Sig b
sawArg3 depth rate f = modArg3 depth (saw rate) f

sawArg4 :: (ModArg4 Sig b) => Sig -> Sig -> b -> ModArgOut4 Sig b
sawArg4 depth rate f = modArg4 depth (saw rate) f

-- delayed saw lfo

delSawArg1 :: (ModArg1 Sig b) => D -> D -> Sig -> Sig -> b -> ModArgOut1 Sig b
delSawArg1 delTime riseTime depth rate f = delModArg1 delTime riseTime depth (saw rate) f

delSawArg2 :: (ModArg2 Sig b) => D -> D -> Sig -> Sig -> b -> ModArgOut2 Sig b
delSawArg2 delTime riseTime depth rate f = delModArg2 delTime riseTime depth (saw rate) f

delSawArg3 :: (ModArg3 Sig b) => D -> D -> Sig -> Sig -> b -> ModArgOut3 Sig b
delSawArg3 delTime riseTime depth rate f = delModArg3 delTime riseTime depth (saw rate) f

delSawArg4 :: (ModArg4 Sig b) => D -> D -> Sig -> Sig -> b -> ModArgOut4 Sig b
delSawArg4 delTime riseTime depth rate f = delModArg4 delTime riseTime depth (saw rate) f

-- oscil lfo rnd phase

rndOscArg1 :: (ModArg1 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut1 (SE Sig) b
rndOscArg1 depth rate f = modArg1 depth (rndOsc rate) f

rndOscArg2 :: (ModArg2 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut2 (SE Sig) b
rndOscArg2 depth rate f = modArg2 depth (rndOsc rate) f

rndOscArg3 :: (ModArg3 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut3 (SE Sig) b
rndOscArg3 depth rate f = modArg3 depth (rndOsc rate) f

rndOscArg4 :: (ModArg4 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut4 (SE Sig) b
rndOscArg4 depth rate f = modArg4 depth (rndOsc rate) f

-- delayed oscil lfo rnd phase

delRndOscArg1 :: (ModArg1 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut1 (SE Sig) b
delRndOscArg1 delTime riseTime depth rate f = delModArg1 delTime riseTime depth (rndOsc rate) f

delRndOscArg2 :: (ModArg2 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut2 (SE Sig) b
delRndOscArg2 delTime riseTime depth rate f = delModArg2 delTime riseTime depth (rndOsc rate) f

delRndOscArg3 :: (ModArg3 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut3 (SE Sig) b
delRndOscArg3 delTime riseTime depth rate f = delModArg3 delTime riseTime depth (rndOsc rate) f

delRndOscArg4 :: (ModArg4 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut4 (SE Sig) b
delRndOscArg4 delTime riseTime depth rate f = delModArg4 delTime riseTime depth (rndOsc rate) f

-- tri lfo rnd phase

rndTriArg1 :: (ModArg1 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut1 (SE Sig) b
rndTriArg1 depth rate f = modArg1 depth (rndTri rate) f

rndTriArg2 :: (ModArg2 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut2 (SE Sig) b
rndTriArg2 depth rate f = modArg2 depth (rndTri rate) f

rndTriArg3 :: (ModArg3 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut3 (SE Sig) b
rndTriArg3 depth rate f = modArg3 depth (rndTri rate) f

rndTriArg4 :: (ModArg4 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut4 (SE Sig) b
rndTriArg4 depth rate f = modArg4 depth (rndTri rate) f

-- delayed tri lfo rnd phase

delRndTriArg1 :: (ModArg1 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut1 (SE Sig) b
delRndTriArg1 delTime riseTime depth rate f = delModArg1 delTime riseTime depth (rndTri rate) f

delRndTriArg2 :: (ModArg2 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut2 (SE Sig) b
delRndTriArg2 delTime riseTime depth rate f = delModArg2 delTime riseTime depth (rndTri rate) f

delRndTriArg3 :: (ModArg3 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut3 (SE Sig) b
delRndTriArg3 delTime riseTime depth rate f = delModArg3 delTime riseTime depth (rndTri rate) f

delRndTriArg4 :: (ModArg4 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut4 (SE Sig) b
delRndTriArg4 delTime riseTime depth rate f = delModArg4 delTime riseTime depth (rndTri rate) f

-- sqr lfo rnd phase

rndSqrArg1 :: (ModArg1 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut1 (SE Sig) b
rndSqrArg1 depth rate f = modArg1 depth (rndSqr rate) f

rndSqrArg2 :: (ModArg2 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut2 (SE Sig) b
rndSqrArg2 depth rate f = modArg2 depth (rndSqr rate) f

rndSqrArg3 :: (ModArg3 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut3 (SE Sig) b
rndSqrArg3 depth rate f = modArg3 depth (rndSqr rate) f

rndSqrArg4 :: (ModArg4 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut4 (SE Sig) b
rndSqrArg4 depth rate f = modArg4 depth (rndSqr rate) f

-- sqr lfo rnd phase

delRndSqrArg1 :: (ModArg1 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut1 (SE Sig) b
delRndSqrArg1 delTime riseTime depth rate f = delModArg1 delTime riseTime depth (rndSqr rate) f

delRndSqrArg2 :: (ModArg2 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut2 (SE Sig) b
delRndSqrArg2 delTime riseTime depth rate f = delModArg2 delTime riseTime depth (rndSqr rate) f

delRndSqrArg3 :: (ModArg3 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut3 (SE Sig) b
delRndSqrArg3 delTime riseTime depth rate f = delModArg3 delTime riseTime depth (rndSqr rate) f

delRndSqrArg4 :: (ModArg4 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut4 (SE Sig) b
delRndSqrArg4 delTime riseTime depth rate f = delModArg4 delTime riseTime depth (rndSqr rate) f

-- sqr lfo rnd phase

rndSawArg1 :: (ModArg1 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut1 (SE Sig) b
rndSawArg1 depth rate f = modArg1 depth (rndSaw rate) f

rndSawArg2 :: (ModArg2 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut2 (SE Sig) b
rndSawArg2 depth rate f = modArg2 depth (rndSaw rate) f

rndSawArg3 :: (ModArg3 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut3 (SE Sig) b
rndSawArg3 depth rate f = modArg3 depth (rndSaw rate) f

rndSawArg4 :: (ModArg4 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut4 (SE Sig) b
rndSawArg4 depth rate f = modArg4 depth (rndSaw rate) f

-- delayed sqr lfo rnd phase

delRndSawArg1 :: (ModArg1 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut1 (SE Sig) b
delRndSawArg1 delTime riseTime depth rate f = delModArg1 delTime riseTime depth (rndSaw rate) f

delRndSawArg2 :: (ModArg2 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut2 (SE Sig) b
delRndSawArg2 delTime riseTime depth rate f = delModArg2 delTime riseTime depth (rndSaw rate) f

delRndSawArg3 :: (ModArg3 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut3 (SE Sig) b
delRndSawArg3 delTime riseTime depth rate f = delModArg3 delTime riseTime depth (rndSaw rate) f

delRndSawArg4 :: (ModArg4 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut4 (SE Sig) b
delRndSawArg4 delTime riseTime depth rate f = delModArg4 delTime riseTime depth (rndSaw rate) f

-- white noise

noiseArg1 :: (ModArg1 (SE Sig) b) => Sig -> b -> ModArgOut1 (SE Sig) b
noiseArg1 depth f = modArg1 depth white f

noiseArg2 :: (ModArg2 (SE Sig) b) => Sig -> b -> ModArgOut2 (SE Sig) b
noiseArg2 depth f = modArg2 depth white f

noiseArg3 :: (ModArg3 (SE Sig) b) => Sig -> b -> ModArgOut3 (SE Sig) b
noiseArg3 depth f = modArg3 depth white f

noiseArg4 :: (ModArg4 (SE Sig) b) => Sig -> b -> ModArgOut4 (SE Sig) b
noiseArg4 depth f = modArg4 depth white f

-- delayed white noise

delNoiseArg1 :: (ModArg1 (SE Sig) b) => D -> D -> Sig -> b -> ModArgOut1 (SE Sig) b
delNoiseArg1 delTime riseTime depth f = delModArg1 delTime riseTime depth white f

delNoiseArg2 :: (ModArg2 (SE Sig) b) => D -> D -> Sig -> b -> ModArgOut2 (SE Sig) b
delNoiseArg2 delTime riseTime depth f = delModArg2 delTime riseTime depth white f

delNoiseArg3 :: (ModArg3 (SE Sig) b) => D -> D -> Sig -> b -> ModArgOut3 (SE Sig) b
delNoiseArg3 delTime riseTime depth f = delModArg3 delTime riseTime depth white f

delNoiseArg4 :: (ModArg4 (SE Sig) b) => D -> D -> Sig -> b -> ModArgOut4 (SE Sig) b
delNoiseArg4 delTime riseTime depth f = delModArg4 delTime riseTime depth white f

-- pink noise

pinkArg1 :: (ModArg1 (SE Sig) b) => Sig -> b -> ModArgOut1 (SE Sig) b
pinkArg1 depth f = modArg1 depth pink f

pinkArg2 :: (ModArg2 (SE Sig) b) => Sig -> b -> ModArgOut2 (SE Sig) b
pinkArg2 depth f = modArg2 depth pink f

pinkArg3 :: (ModArg3 (SE Sig) b) => Sig -> b -> ModArgOut3 (SE Sig) b
pinkArg3 depth f = modArg3 depth pink f

pinkArg4 :: (ModArg4 (SE Sig) b) => Sig -> b -> ModArgOut4 (SE Sig) b
pinkArg4 depth f = modArg4 depth pink f

-- pink noise

delPinkArg1 :: (ModArg1 (SE Sig) b) => D -> D -> Sig -> b -> ModArgOut1 (SE Sig) b
delPinkArg1 delTime riseTime depth f = delModArg1 delTime riseTime depth pink f

delPinkArg2 :: (ModArg2 (SE Sig) b) => D -> D -> Sig -> b -> ModArgOut2 (SE Sig) b
delPinkArg2 delTime riseTime depth f = delModArg2 delTime riseTime depth pink f

delPinkArg3 :: (ModArg3 (SE Sig) b) => D -> D -> Sig -> b -> ModArgOut3 (SE Sig) b
delPinkArg3 delTime riseTime depth f = delModArg3 delTime riseTime depth pink f

delPinkArg4 :: (ModArg4 (SE Sig) b) => D -> D -> Sig -> b -> ModArgOut4 (SE Sig) b
delPinkArg4 delTime riseTime depth f = delModArg4 delTime riseTime depth pink f


-- jitter noise

jitArg1 :: (ModArg1 (SE Sig) b) => Sig -> Sig -> Sig -> b -> ModArgOut1 (SE Sig) b
jitArg1 depth cpsMin cpsMax f = modArg1 depth (jitter 1 cpsMin cpsMax) f

jitArg2 :: (ModArg2 (SE Sig) b) => Sig -> Sig -> Sig -> b -> ModArgOut2 (SE Sig) b
jitArg2 depth cpsMin cpsMax f = modArg2 depth (jitter 1 cpsMin cpsMax) f

jitArg3 :: (ModArg3 (SE Sig) b) => Sig -> Sig -> Sig -> b -> ModArgOut3 (SE Sig) b
jitArg3 depth cpsMin cpsMax f = modArg3 depth (jitter 1 cpsMin cpsMax) f

jitArg4 :: (ModArg4 (SE Sig) b) => Sig -> Sig -> Sig -> b -> ModArgOut4 (SE Sig) b
jitArg4 depth cpsMin cpsMax f = modArg4 depth (jitter 1 cpsMin cpsMax) f

-- jitter noise

delJitArg1 :: (ModArg1 (SE Sig) b) => D -> D -> Sig -> Sig -> Sig -> b -> ModArgOut1 (SE Sig) b
delJitArg1 delTime riseTime depth cpsMin cpsMax f = delModArg1 delTime riseTime depth (jitter 1 cpsMin cpsMax) f

delJitArg2 :: (ModArg2 (SE Sig) b) => D -> D -> Sig -> Sig -> Sig -> b -> ModArgOut2 (SE Sig) b
delJitArg2 delTime riseTime depth cpsMin cpsMax f = delModArg2 delTime riseTime depth (jitter 1 cpsMin cpsMax) f

delJitArg3 :: (ModArg3 (SE Sig) b) => D -> D -> Sig -> Sig -> Sig -> b -> ModArgOut3 (SE Sig) b
delJitArg3 delTime riseTime depth cpsMin cpsMax f = delModArg3 delTime riseTime depth (jitter 1 cpsMin cpsMax) f

delJitArg4 :: (ModArg4 (SE Sig) b) => D -> D -> Sig -> Sig -> Sig -> b -> ModArgOut4 (SE Sig) b
delJitArg4 delTime riseTime depth cpsMin cpsMax f = delModArg4 delTime riseTime depth (jitter 1 cpsMin cpsMax) f

-- gauss noise

gaussArg1 :: (ModArg1 (SE Sig) b) => Sig -> b -> ModArgOut1 (SE Sig) b
gaussArg1 depth f = modArg1 depth (gauss 1) f

gaussArg2 :: (ModArg2 (SE Sig) b) => Sig -> b -> ModArgOut2 (SE Sig) b
gaussArg2 depth f = modArg2 depth (gauss 1) f

gaussArg3 :: (ModArg3 (SE Sig) b) => Sig -> b -> ModArgOut3 (SE Sig) b
gaussArg3 depth f = modArg3 depth (gauss 1) f

gaussArg4 :: (ModArg4 (SE Sig) b) => Sig -> b -> ModArgOut4 (SE Sig) b
gaussArg4 depth f = modArg4 depth (gauss 1) f

-- delayed gauss noise

delGaussArg1 :: (ModArg1 (SE Sig) b) => D -> D -> Sig -> b -> ModArgOut1 (SE Sig) b
delGaussArg1 delTime riseTime depth f = delModArg1 delTime riseTime depth (gauss 1) f

delGaussArg2 :: (ModArg2 (SE Sig) b) => D -> D -> Sig -> b -> ModArgOut2 (SE Sig) b
delGaussArg2 delTime riseTime depth f = delModArg2 delTime riseTime depth (gauss 1) f

delGaussArg3 :: (ModArg3 (SE Sig) b) => D -> D -> Sig -> b -> ModArgOut3 (SE Sig) b
delGaussArg3 delTime riseTime depth f = delModArg3 delTime riseTime depth (gauss 1) f

delGaussArg4 :: (ModArg4 (SE Sig) b) => D -> D -> Sig -> b -> ModArgOut4 (SE Sig) b
delGaussArg4 delTime riseTime depth f = delModArg4 delTime riseTime depth (gauss 1) f

-- gauss noise with frequency

gaussiArg1 :: (ModArg1 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut1 (SE Sig) b
gaussiArg1 depth rate f = modArg1 depth (gaussi 1 1 rate) f

gaussiArg2 :: (ModArg2 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut2 (SE Sig) b
gaussiArg2 depth rate f = modArg2 depth (gaussi 1 1 rate) f

gaussiArg3 :: (ModArg3 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut3 (SE Sig) b
gaussiArg3 depth rate f = modArg3 depth (gaussi 1 1 rate) f

gaussiArg4 :: (ModArg4 (SE Sig) b) => Sig -> Sig -> b -> ModArgOut4 (SE Sig) b
gaussiArg4 depth rate f = modArg4 depth (gaussi 1 1 rate) f

-- delayed gauss noise with frequency

delGaussiArg1 :: (ModArg1 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut1 (SE Sig) b
delGaussiArg1 delTime riseTime depth rate f = delModArg1 delTime riseTime depth (gaussi 1 1 rate) f

delGaussiArg2 :: (ModArg2 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut2 (SE Sig) b
delGaussiArg2 delTime riseTime depth rate f = delModArg2 delTime riseTime depth (gaussi 1 1 rate) f

delGaussiArg3 :: (ModArg3 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut3 (SE Sig) b
delGaussiArg3 delTime riseTime depth rate f = delModArg3 delTime riseTime depth (gaussi 1 1 rate) f

delGaussiArg4 :: (ModArg4 (SE Sig) b) => D -> D -> Sig -> Sig -> b -> ModArgOut4 (SE Sig) b
delGaussiArg4 delTime riseTime depth rate f = delModArg4 delTime riseTime depth (gaussi 1 1 rate) f

--------------------------------------------
--------------------------------------------
-- modArg1

class ModArg1 a b where
    type ModArgOut1 a b :: Type
    modArg1 :: Sig -> a -> b -> ModArgOut1 a b

--------------------------------------------
-- pure in, pure mono out

instance ModArg1 Sig (Sig -> Sig) where
    type ModArgOut1 Sig (Sig -> Sig) = Sig -> Sig
    modArg1 depth a f = \x -> f (x * (1 + depth * a))

instance ModArg1 Sig (Sig -> a -> Sig) where
    type ModArgOut1 Sig (Sig -> a -> Sig) = Sig -> a -> Sig
    modArg1 depth a f = \x1 x2 -> f (x1 * (1 + depth * a)) x2

instance ModArg1 Sig (Sig -> a -> b -> Sig) where
    type ModArgOut1 Sig (Sig -> a -> b -> Sig) = Sig -> a -> b -> Sig
    modArg1 depth a f = \x1 x2 x3 -> f (x1 * (1 + depth * a)) x2 x3

instance ModArg1 Sig (Sig -> a -> b -> c -> Sig) where
    type ModArgOut1 Sig (Sig -> a -> b -> c -> Sig) = Sig -> a -> b -> c -> Sig
    modArg1 depth a f = \x1 x2 x3 x4 -> f (x1 * (1 + depth * a)) x2 x3 x4

--------------------------------------------
-- pure in, pure stereo out

instance ModArg1 Sig (Sig -> Sig2) where
    type ModArgOut1 Sig (Sig -> Sig2) = Sig -> Sig2
    modArg1 depth a f = \x -> f (x * (1 + depth * a))

instance ModArg1 Sig (Sig -> a -> Sig2) where
    type ModArgOut1 Sig (Sig -> a -> Sig2) = Sig -> a -> Sig2
    modArg1 depth a f = \x1 x2 -> f (x1 * (1 + depth * a)) x2

instance ModArg1 Sig (Sig -> a -> b -> Sig2) where
    type ModArgOut1 Sig (Sig -> a -> b -> Sig2) = Sig -> a -> b -> Sig2
    modArg1 depth a f = \x1 x2 x3 -> f (x1 * (1 + depth * a)) x2 x3

instance ModArg1 Sig (Sig -> a -> b -> c -> Sig2) where
    type ModArgOut1 Sig (Sig -> a -> b -> c -> Sig2) = Sig -> a -> b -> c -> Sig2
    modArg1 depth a f = \x1 x2 x3 x4 -> f (x1 * (1 + depth * a)) x2 x3 x4

--------------------------------------------
-- pure in, dirty mono out

instance ModArg1 Sig (Sig -> SE Sig) where
    type ModArgOut1 Sig (Sig -> SE Sig) = Sig -> SE Sig
    modArg1 depth a f = \x -> f (x * (1 + depth * a))

instance ModArg1 Sig (Sig -> a -> SE Sig) where
    type ModArgOut1 Sig (Sig -> a -> SE Sig) = Sig -> a -> SE Sig
    modArg1 depth a f = \x1 x2 -> f (x1 * (1 + depth * a)) x2

instance ModArg1 Sig (Sig -> a -> b -> SE Sig) where
    type ModArgOut1 Sig (Sig -> a -> b -> SE Sig) = Sig -> a -> b -> SE Sig
    modArg1 depth a f = \x1 x2 x3 -> f (x1 * (1 + depth * a)) x2 x3

instance ModArg1 Sig (Sig -> a -> b -> c -> SE Sig) where
    type ModArgOut1 Sig (Sig -> a -> b -> c -> SE Sig) = Sig -> a -> b -> c -> SE Sig
    modArg1 depth a f = \x1 x2 x3 x4 -> f (x1 * (1 + depth * a)) x2 x3 x4

--------------------------------------------
-- pure in, dirty stereo out

instance ModArg1 Sig (Sig -> SE Sig2) where
    type ModArgOut1 Sig (Sig -> SE Sig2) = Sig -> SE Sig2
    modArg1 depth a f = \x -> f (x * (1 + depth * a))

instance ModArg1 Sig (Sig -> a -> SE Sig2) where
    type ModArgOut1 Sig (Sig -> a -> SE Sig2) = Sig -> a -> SE Sig2
    modArg1 depth a f = \x1 x2 -> f (x1 * (1 + depth * a)) x2

instance ModArg1 Sig (Sig -> a -> b -> SE Sig2) where
    type ModArgOut1 Sig (Sig -> a -> b -> SE Sig2) = Sig -> a -> b -> SE Sig2
    modArg1 depth a f = \x1 x2 x3 -> f (x1 * (1 + depth * a)) x2 x3

instance ModArg1 Sig (Sig -> a -> b -> c -> SE Sig2) where
    type ModArgOut1 Sig (Sig -> a -> b -> c -> SE Sig2) = Sig -> a -> b -> c -> SE Sig2
    modArg1 depth a f = \x1 x2 x3 x4 -> f (x1 * (1 + depth * a)) x2 x3 x4

--------------------------------------------
-- dirty in, pure mono out

instance ModArg1 (SE Sig) (Sig -> Sig) where
    type ModArgOut1 (SE Sig) (Sig -> Sig) = Sig -> SE Sig
    modArg1 depth ma f = \x -> fmap (\a -> f (x * (1 + depth * a))) ma

instance ModArg1 (SE Sig) (Sig -> a -> Sig) where
    type ModArgOut1 (SE Sig) (Sig -> a -> Sig) = Sig -> a -> SE Sig
    modArg1 depth ma f = \x1 x2 -> fmap (\a -> f (x1 * (1 + depth * a)) x2) ma

instance ModArg1 (SE Sig) (Sig -> a -> b -> Sig) where
    type ModArgOut1 (SE Sig) (Sig -> a -> b -> Sig) = Sig -> a -> b -> SE Sig
    modArg1 depth ma f = \x1 x2 x3 -> fmap (\a -> f (x1 * (1 + depth * a)) x2 x3) ma

instance ModArg1 (SE Sig) (Sig -> a -> b -> c -> Sig) where
    type ModArgOut1 (SE Sig) (Sig -> a -> b -> c -> Sig) = Sig -> a -> b -> c -> SE Sig
    modArg1 depth ma f = \x1 x2 x3 x4 -> fmap (\a -> f (x1 * (1 + depth * a)) x2 x3 x4) ma

--------------------------------------------
-- dirty in, pure stereo out

instance ModArg1 (SE Sig) (Sig -> Sig2) where
    type ModArgOut1 (SE Sig) (Sig -> Sig2) = Sig -> SE Sig2
    modArg1 depth ma f = \x -> fmap (\a -> f (x * (1 + depth * a))) ma

instance ModArg1 (SE Sig) (Sig -> a -> Sig2) where
    type ModArgOut1 (SE Sig) (Sig -> a -> Sig2) = Sig -> a -> SE Sig2
    modArg1 depth ma f = \x1 x2 -> fmap (\a -> f (x1 * (1 + depth * a)) x2) ma

instance ModArg1 (SE Sig) (Sig -> a -> b -> Sig2) where
    type ModArgOut1 (SE Sig) (Sig -> a -> b -> Sig2) = Sig -> a -> b -> SE Sig2
    modArg1 depth ma f = \x1 x2 x3 -> fmap (\a -> f (x1 * (1 + depth * a)) x2 x3) ma

instance ModArg1 (SE Sig) (Sig -> a -> b -> c -> Sig2) where
    type ModArgOut1 (SE Sig) (Sig -> a -> b -> c -> Sig2) = Sig -> a -> b -> c -> SE Sig2
    modArg1 depth ma f = \x1 x2 x3 x4 -> fmap (\a -> f (x1 * (1 + depth * a)) x2 x3 x4) ma

--------------------------------------------
-- dirty in, dirty mono out

instance ModArg1 (SE Sig) (Sig -> SE Sig) where
    type ModArgOut1 (SE Sig) (Sig -> SE Sig) = Sig -> SE Sig
    modArg1 depth ma f = \x -> ma >>= (\a -> f (x * (1 + depth * a)))

instance ModArg1 (SE Sig) (Sig -> a -> SE Sig) where
    type ModArgOut1 (SE Sig) (Sig -> a -> SE Sig) = Sig -> a -> SE Sig
    modArg1 depth ma f = \x1 x2 -> ma >>= (\a -> f (x1 * (1 + depth * a)) x2)

instance ModArg1 (SE Sig) (Sig -> a -> b -> SE Sig) where
    type ModArgOut1 (SE Sig) (Sig -> a -> b -> SE Sig) = Sig -> a -> b -> SE Sig
    modArg1 depth ma f = \x1 x2 x3 -> ma >>= (\a -> f (x1 * (1 + depth * a)) x2 x3)

instance ModArg1 (SE Sig) (Sig -> a -> b -> c -> SE Sig) where
    type ModArgOut1 (SE Sig) (Sig -> a -> b -> c -> SE Sig) = Sig -> a -> b -> c -> SE Sig
    modArg1 depth ma f = \x1 x2 x3 x4 -> ma >>= (\a -> f (x1 * (1 + depth * a)) x2 x3 x4)

--------------------------------------------
-- dirty in, dirty stereo out

instance ModArg1 (SE Sig) (Sig -> SE Sig2) where
    type ModArgOut1 (SE Sig) (Sig -> SE Sig2) = Sig -> SE Sig2
    modArg1 depth ma f = \x -> ma >>= (\a -> f (x * (1 + depth * a)))

instance ModArg1 (SE Sig) (Sig -> a -> SE Sig2) where
    type ModArgOut1 (SE Sig) (Sig -> a -> SE Sig2) = Sig -> a -> SE Sig2
    modArg1 depth ma f = \x1 x2 -> ma >>= (\a -> f (x1 * (1 + depth * a)) x2)

instance ModArg1 (SE Sig) (Sig -> a -> b -> SE Sig2) where
    type ModArgOut1 (SE Sig) (Sig -> a -> b -> SE Sig2) = Sig -> a -> b -> SE Sig2
    modArg1 depth ma f = \x1 x2 x3 -> ma >>= (\a -> f (x1 * (1 + depth * a)) x2 x3)

instance ModArg1 (SE Sig) (Sig -> a -> b -> c -> SE Sig2) where
    type ModArgOut1 (SE Sig) (Sig -> a -> b -> c -> SE Sig2) = Sig -> a -> b -> c -> SE Sig2
    modArg1 depth ma f = \x1 x2 x3 x4 -> ma >>= (\a -> f (x1 * (1 + depth * a)) x2 x3 x4)

--------------------------------------------
--------------------------------------------
-- modArg2

class ModArg2 a b where
    type ModArgOut2 a b :: Type
    modArg2 :: Sig -> a -> b -> ModArgOut2 a b

--------------------------------------------
-- pure in, pure mono out

instance ModArg2 Sig (a -> Sig -> Sig) where
    type ModArgOut2 Sig (a -> Sig -> Sig) = a -> Sig -> Sig
    modArg2 depth a f = \x1 x2 -> f x1 (x2 * (1 + depth * a))

instance ModArg2 Sig (a -> Sig -> b -> Sig) where
    type ModArgOut2 Sig (a -> Sig -> b -> Sig) = a -> Sig -> b -> Sig
    modArg2 depth a f = \x1 x2 x3 -> f x1 (x2 * (1 + depth * a)) x3

instance ModArg2 Sig (a -> Sig -> b -> c -> Sig) where
    type ModArgOut2 Sig (a -> Sig -> b -> c -> Sig) = a -> Sig -> b -> c -> Sig
    modArg2 depth a f = \x1 x2 x3 x4 -> f x1 (x2 * (1 + depth * a)) x3 x4

--------------------------------------------
-- pure in, pure stereo out

instance ModArg2 Sig (a -> Sig -> Sig2) where
    type ModArgOut2 Sig (a -> Sig -> Sig2) = a -> Sig -> Sig2
    modArg2 depth a f = \x1 x2 -> f x1 (x2 * (1 + depth * a))

instance ModArg2 Sig (a -> Sig -> b -> Sig2) where
    type ModArgOut2 Sig (a -> Sig -> b -> Sig2) = a -> Sig -> b -> Sig2
    modArg2 depth a f = \x1 x2 x3 -> f x1 (x2 * (1 + depth * a)) x3

instance ModArg2 Sig (a -> Sig -> b -> c -> Sig2) where
    type ModArgOut2 Sig (a -> Sig -> b -> c -> Sig2) = a -> Sig -> b -> c -> Sig2
    modArg2 depth a f = \x1 x2 x3 x4 -> f x1 (x2 * (1 + depth * a)) x3 x4

--------------------------------------------
-- pure in, dirty mono out

instance ModArg2 Sig (a -> Sig -> SE Sig) where
    type ModArgOut2 Sig (a -> Sig -> SE Sig) = a -> Sig -> SE Sig
    modArg2 depth a f = \x1 x2 -> f x1 (x2 * (1 + depth * a))

instance ModArg2 Sig (a -> Sig -> b -> SE Sig) where
    type ModArgOut2 Sig (a -> Sig -> b -> SE Sig) = a -> Sig -> b -> SE Sig
    modArg2 depth a f = \x1 x2 x3 -> f x1 (x2 * (1 + depth * a)) x3

instance ModArg2 Sig (a -> Sig -> b -> c -> SE Sig) where
    type ModArgOut2 Sig (a -> Sig -> b -> c -> SE Sig) = a -> Sig -> b -> c -> SE Sig
    modArg2 depth a f = \x1 x2 x3 x4 -> f x1 (x2 * (1 + depth * a)) x3 x4

--------------------------------------------
-- pure in, dirty stereo out

instance ModArg2 Sig (a -> Sig -> SE Sig2) where
    type ModArgOut2 Sig (a -> Sig -> SE Sig2) = a -> Sig -> SE Sig2
    modArg2 depth a f = \x1 x2 -> f x1 (x2 * (1 + depth * a))

instance ModArg2 Sig (a -> Sig -> b -> SE Sig2) where
    type ModArgOut2 Sig (a -> Sig -> b -> SE Sig2) = a -> Sig -> b -> SE Sig2
    modArg2 depth a f = \x1 x2 x3 -> f x1 (x2 * (1 + depth * a)) x3

instance ModArg2 Sig (a -> Sig -> b -> c -> SE Sig2) where
    type ModArgOut2 Sig (a -> Sig -> b -> c -> SE Sig2) = a -> Sig -> b -> c -> SE Sig2
    modArg2 depth a f = \x1 x2 x3 x4 -> f x1 (x2 * (1 + depth * a)) x3 x4

--------------------------------------------
-- dirty in, pure mono out

instance ModArg2 (SE Sig) (a -> Sig -> Sig) where
    type ModArgOut2 (SE Sig) (a -> Sig -> Sig) = a -> Sig -> SE Sig
    modArg2 depth ma f = \x1 x2 -> fmap (\a -> f x1 (x2 * (1 + depth * a))) ma

instance ModArg2 (SE Sig) (a -> Sig -> b -> Sig) where
    type ModArgOut2 (SE Sig) (a -> Sig -> b -> Sig) = a -> Sig -> b -> SE Sig
    modArg2 depth ma f = \x1 x2 x3 -> fmap (\a -> f x1 (x2 * (1 + depth * a)) x3) ma

instance ModArg2 (SE Sig) (a -> Sig -> b -> c -> Sig) where
    type ModArgOut2 (SE Sig) (a -> Sig -> b -> c -> Sig) = a -> Sig -> b -> c -> SE Sig
    modArg2 depth ma f = \x1 x2 x3 x4 -> fmap (\a -> f x1 (x2 * (1 + depth * a)) x3 x4) ma

--------------------------------------------
-- dirty in, pure stereo out

instance ModArg2 (SE Sig) (a -> Sig -> Sig2) where
    type ModArgOut2 (SE Sig) (a -> Sig -> Sig2) = a -> Sig -> SE Sig2
    modArg2 depth ma f = \x1 x2 -> fmap (\a -> f x1 (x2 * (1 + depth * a))) ma

instance ModArg2 (SE Sig) (a -> Sig -> b -> Sig2) where
    type ModArgOut2 (SE Sig) (a -> Sig -> b -> Sig2) = a -> Sig -> b -> SE Sig2
    modArg2 depth ma f = \x1 x2 x3 -> fmap (\a -> f x1 (x2 * (1 + depth * a)) x3) ma

instance ModArg2 (SE Sig) (a -> Sig -> b -> c -> Sig2) where
    type ModArgOut2 (SE Sig) (a -> Sig -> b -> c -> Sig2) = a -> Sig -> b -> c -> SE Sig2
    modArg2 depth ma f = \x1 x2 x3 x4 -> fmap (\a -> f x1 (x2 * (1 + depth * a)) x3 x4) ma

--------------------------------------------
-- dirty in, dirty mono out

instance ModArg2 (SE Sig) (a -> Sig -> SE Sig) where
    type ModArgOut2 (SE Sig) (a -> Sig -> SE Sig) = a -> Sig -> SE Sig
    modArg2 depth ma f = \x1 x2 -> ma >>= (\a -> f x1 (x2 * (1 + depth * a)))

instance ModArg2 (SE Sig) (a -> Sig -> b -> SE Sig) where
    type ModArgOut2 (SE Sig) (a -> Sig -> b -> SE Sig) = a -> Sig -> b -> SE Sig
    modArg2 depth ma f = \x1 x2 x3 -> ma >>= (\a -> f x1 (x2 * (1 + depth * a)) x3)

instance ModArg2 (SE Sig) (a -> Sig -> b -> c -> SE Sig) where
    type ModArgOut2 (SE Sig) (a -> Sig -> b -> c -> SE Sig) = a -> Sig -> b -> c -> SE Sig
    modArg2 depth ma f = \x1 x2 x3 x4 -> ma >>= (\a -> f x1 (x2 * (1 + depth * a)) x3 x4)

--------------------------------------------
-- dirty in, dirty stereo out

instance ModArg2 (SE Sig) (a -> Sig -> SE Sig2) where
    type ModArgOut2 (SE Sig) (a -> Sig -> SE Sig2) = a -> Sig -> SE Sig2
    modArg2 depth ma f = \x1 x2 -> ma >>= (\a -> f x1 (x2 * (1 + depth * a)))

instance ModArg2 (SE Sig) (a -> Sig -> b -> SE Sig2) where
    type ModArgOut2 (SE Sig) (a -> Sig -> b -> SE Sig2) = a -> Sig -> b -> SE Sig2
    modArg2 depth ma f = \x1 x2 x3 -> ma >>= (\a -> f x1 (x2 * (1 + depth * a)) x3)

instance ModArg2 (SE Sig) (a -> Sig -> b -> c -> SE Sig2) where
    type ModArgOut2 (SE Sig) (a -> Sig -> b -> c -> SE Sig2) = a -> Sig -> b -> c -> SE Sig2
    modArg2 depth ma f = \x1 x2 x3 x4 -> ma >>= (\a -> f x1 (x2 * (1 + depth * a)) x3 x4)

--------------------------------------------
--------------------------------------------
-- modArg3

class ModArg3 a b where
    type ModArgOut3 a b :: Type
    modArg3 :: Sig -> a -> b -> ModArgOut3 a b

--------------------------------------------
-- pure in, pure mono out

instance ModArg3 Sig (a -> b -> Sig -> Sig) where
    type ModArgOut3 Sig (a -> b -> Sig -> Sig) = a -> b -> Sig -> Sig
    modArg3 depth a f = \x1 x2 x3 -> f x1 x2 (x3 * (1 + depth * a))

instance ModArg3 Sig (a -> b -> Sig -> c -> Sig) where
    type ModArgOut3 Sig (a -> b -> Sig -> c -> Sig) = a -> b -> Sig -> c -> Sig
    modArg3 depth a f = \x1 x2 x3 x4 -> f x1 x2 (x3 * (1 + depth * a)) x4

--------------------------------------------
-- pure in, pure stereo out

instance ModArg3 Sig (a -> b -> Sig -> Sig2) where
    type ModArgOut3 Sig (a -> b -> Sig -> Sig2) = a -> b -> Sig -> Sig2
    modArg3 depth a f = \x1 x2 x3 -> f x1 x2 (x3 * (1 + depth * a))

instance ModArg3 Sig (a -> b -> Sig -> c -> Sig2) where
    type ModArgOut3 Sig (a -> b -> Sig -> c -> Sig2) = a -> b -> Sig -> c -> Sig2
    modArg3 depth a f = \x1 x2 x3 x4 -> f x1 x2 (x3 * (1 + depth * a)) x4

--------------------------------------------
-- pure in, dirty mono out

instance ModArg3 Sig (a -> b -> Sig -> SE Sig) where
    type ModArgOut3 Sig (a -> b -> Sig -> SE Sig) = a -> b -> Sig -> SE Sig
    modArg3 depth a f = \x1 x2 x3 -> f x1 x2 (x3 * (1 + depth * a))

instance ModArg3 Sig (a -> b -> Sig -> c -> SE Sig) where
    type ModArgOut3 Sig (a -> b -> Sig -> c -> SE Sig) = a -> b -> Sig -> c -> SE Sig
    modArg3 depth a f = \x1 x2 x3 x4 -> f x1 x2 (x3 * (1 + depth * a)) x4

--------------------------------------------
-- pure in, dirty stereo out

instance ModArg3 Sig (a -> b -> Sig -> SE Sig2) where
    type ModArgOut3 Sig (a -> b -> Sig -> SE Sig2) = a -> b -> Sig -> SE Sig2
    modArg3 depth a f = \x1 x2 x3 -> f x1 x2 (x3 * (1 + depth * a))

instance ModArg3 Sig (a -> b -> Sig -> c -> SE Sig2) where
    type ModArgOut3 Sig (a -> b -> Sig -> c -> SE Sig2) = a -> b -> Sig -> c -> SE Sig2
    modArg3 depth a f = \x1 x2 x3 x4 -> f x1 x2 (x3 * (1 + depth * a)) x4

--------------------------------------------
-- dirty in, pure mono out

instance ModArg3 (SE Sig) (a -> b -> Sig -> Sig) where
    type ModArgOut3 (SE Sig) (a -> b -> Sig -> Sig) = a -> b -> Sig -> SE Sig
    modArg3 depth ma f = \x1 x2 x3 -> fmap (\a -> f x1 x2 (x3 * (1 + depth * a))) ma

instance ModArg3 (SE Sig) (a -> b -> Sig -> c -> Sig) where
    type ModArgOut3 (SE Sig) (a -> b -> Sig -> c -> Sig) = a -> b -> Sig -> c -> SE Sig
    modArg3 depth ma f = \x1 x2 x3 x4 -> fmap (\a -> f x1 x2 (x3 * (1 + depth * a)) x4) ma

--------------------------------------------
-- dirty in, pure stereo out

instance ModArg3 (SE Sig) (a -> b -> Sig -> Sig2) where
    type ModArgOut3 (SE Sig) (a -> b -> Sig -> Sig2) = a -> b -> Sig -> SE Sig2
    modArg3 depth ma f = \x1 x2 x3 -> fmap (\a -> f x1 x2 (x3 * (1 + depth * a))) ma

instance ModArg3 (SE Sig) (a -> b -> Sig -> c -> Sig2) where
    type ModArgOut3 (SE Sig) (a -> b -> Sig -> c -> Sig2) = a -> b -> Sig -> c -> SE Sig2
    modArg3 depth ma f = \x1 x2 x3 x4 -> fmap (\a -> f x1 x2 (x3 * (1 + depth * a)) x4) ma

--------------------------------------------
-- dirty in, dirty mono out

instance ModArg3 (SE Sig) (a -> b -> Sig -> SE Sig) where
    type ModArgOut3 (SE Sig) (a -> b -> Sig -> SE Sig) = a -> b -> Sig -> SE Sig
    modArg3 depth ma f = \x1 x2 x3 -> ma >>= (\a -> f x1 x2 (x3 * (1 + depth * a)))

instance ModArg3 (SE Sig) (a -> b -> Sig -> c -> SE Sig) where
    type ModArgOut3 (SE Sig) (a -> b -> Sig -> c -> SE Sig) = a -> b -> Sig -> c -> SE Sig
    modArg3 depth ma f = \x1 x2 x3 x4 -> ma >>= (\a -> f x1 x2 (x3 * (1 + depth * a)) x4)

--------------------------------------------
-- dirty in, dirty stereo out

instance ModArg3 (SE Sig) (a -> b -> Sig -> SE Sig2) where
    type ModArgOut3 (SE Sig) (a -> b -> Sig -> SE Sig2) = a -> b -> Sig -> SE Sig2
    modArg3 depth ma f = \x1 x2 x3 -> ma >>= (\a -> f x1 x2 (x3 * (1 + depth * a)))

instance ModArg3 (SE Sig) (a -> b -> Sig -> c -> SE Sig2) where
    type ModArgOut3 (SE Sig) (a -> b -> Sig -> c -> SE Sig2) = a -> b -> Sig -> c -> SE Sig2
    modArg3 depth ma f = \x1 x2 x3 x4 -> ma >>= (\a -> f x1 x2 (x3 * (1 + depth * a)) x4)

--------------------------------------------
--------------------------------------------
-- modArg4

class ModArg4 a b where
    type ModArgOut4 a b :: Type
    modArg4 :: Sig -> a -> b -> ModArgOut4 a b

--------------------------------------------
-- pure in, pure mono out

instance ModArg4 Sig (a -> b -> c -> Sig -> Sig) where
    type ModArgOut4 Sig (a -> b -> c -> Sig -> Sig) = a -> b -> c -> Sig -> Sig
    modArg4 depth a f = \x1 x2 x3 x4 -> f x1 x2 x3 (x4 * (1 + depth * a))

--------------------------------------------
-- pure in, pure stereo out

instance ModArg4 Sig (a -> b -> c -> Sig -> Sig2) where
    type ModArgOut4 Sig (a -> b -> c -> Sig -> Sig2) = a -> b -> c -> Sig -> Sig2
    modArg4 depth a f = \x1 x2 x3 x4 -> f x1 x2 x3 (x4 * (1 + depth * a))

--------------------------------------------
-- pure in, dirty mono out

instance ModArg4 Sig (a -> b -> c -> Sig -> SE Sig) where
    type ModArgOut4 Sig (a -> b -> c -> Sig -> SE Sig) = a -> b -> c -> Sig -> SE Sig
    modArg4 depth a f = \x1 x2 x3 x4 -> f x1 x2 x3 (x4 * (1 + depth * a))

--------------------------------------------
-- pure in, dirty stereo out

instance ModArg4 Sig (a -> b -> c -> Sig -> SE Sig2) where
    type ModArgOut4 Sig (a -> b -> c -> Sig -> SE Sig2) = a -> b -> c -> Sig -> SE Sig2
    modArg4 depth a f = \x1 x2 x3 x4 -> f x1 x2 x3 (x4 * (1 + depth * a))

--------------------------------------------
-- dirty in, pure mono out

instance ModArg4 (SE Sig) (a -> b -> c -> Sig -> Sig) where
    type ModArgOut4 (SE Sig) (a -> b -> c -> Sig -> Sig) = a -> b -> c -> Sig -> SE Sig
    modArg4 depth ma f = \x1 x2 x3 x4 -> fmap (\a -> f x1 x2 x3 (x4 * (1 + depth * a))) ma

--------------------------------------------
-- dirty in, pure stereo out

instance ModArg4 (SE Sig) (a -> b -> c -> Sig -> Sig2) where
    type ModArgOut4 (SE Sig) (a -> b -> c -> Sig -> Sig2) = a -> b -> c -> Sig -> SE Sig2
    modArg4 depth ma f = \x1 x2 x3 x4 -> fmap (\a -> f x1 x2 x3 (x4 * (1 + depth * a))) ma

--------------------------------------------
-- dirty in, dirty mono out

instance ModArg4 (SE Sig) (a -> b -> c -> Sig -> SE Sig) where
    type ModArgOut4 (SE Sig) (a -> b -> c -> Sig -> SE Sig) = a -> b -> c -> Sig -> SE Sig
    modArg4 depth ma f = \x1 x2 x3 x4 -> ma >>= (\a -> f x1 x2 x3 (x4 * (1 + depth * a)))

--------------------------------------------
-- dirty in, dirty stereo out

instance ModArg4 (SE Sig) (a -> b -> c -> Sig -> SE Sig2) where
    type ModArgOut4 (SE Sig) (a -> b -> c -> Sig -> SE Sig2) = a -> b -> c -> Sig -> SE Sig2
    modArg4 depth ma f = \x1 x2 x3 x4 -> ma >>= (\a -> f x1 x2 x3 (x4 * (1 + depth * a)))
