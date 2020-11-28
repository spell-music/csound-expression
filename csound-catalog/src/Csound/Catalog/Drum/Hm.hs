-- | Drums defined by Hans Mikelson.
module Csound.Catalog.Drum.Hm(
    -- * Subtractive
    dumb, dumbBass, pluckSnare, sortaKnockSweep, metalBoink,

    -- * Fullkit

    -- ** Non-pitched drums
    snare, openHihat, closedHihat,

    -- ** Pitched drums
    --
    -- The instrument turns a pitch (in Hz) to the signal.
    bassDrum, crash, handClap,
    bassDrum', crash', handClap',

    -- * Sampler
    bd1, bd2, sn1, sweep, boink, sn2, ohh, chh, bd3, cr, clap
) where

import Csound.Base
import Csound.Sam

-- subtractive

bpb :: Sig -> Sig -> Sig -> Sig
bpb cfq q asig = balance (bp cfq q asig) asig

rndAmp :: Sig -> SE Sig
rndAmp a = do
    k <- birnd 0.09
    return $ a * (1 + sig k)

addDur' dt x = xtratim dt >> return x
addDur = addDur' 0.1

toDrum :: Sig -> SE Sig
toDrum a = rndAmp =<< addDur a

-- | A \"dumb\" drum.
dumb :: SE Sig
dumb = (toDrum =<<) $ fmap (bpb 1000 100) $ rand $ expseg [0.0001, 0.01, 1, 0.04, 0.01]

dumbBass :: SE Sig
dumbBass = (toDrum =<<) $ fmap (bpb kfreqenv (kfreqenv / 8)) $ rand $ expseg [0.0001, 0.01, 1, 0.08, 0.01]
    where kfreqenv = expseg [50, 0.01, 200, 0.08, 50]

pluckSnare :: SE Sig
pluckSnare = toDrum $ pluck kampenv4 kptchenv 50 (elins [1, 1]) 4 `withDs` [0.8, 3]
    where
        kampenv4 = linseg [0, 0.001, 1, xdur - 0.21, 1, 0.02, 0]
        kptchenv = linseg [100, 0.01, 300, 0.2, 200, 0.01, 200]
        xdur     = 1.25


sortaKnockSweep :: SE Sig
sortaKnockSweep = (toDrum =<<) $
    fmap (resonsBy bpb
            [ (kfreqenv41, kfreqenv41 / 8)
            , (kfreqenv42, kfreqenv42 / 4)
            ]
         ) $ rand kampenv4
    where
        kfreqenv41  = expseg [ 50, 0.01, 200, 0.08, 50]
        kfreqenv42  = linseg [ 150, 0.01, 1000, 0.08, 250]
        kampenv4	= linseg [ 0, 0.01, 1, 0.08, 0, 0.01, 0]

metalBoink :: SE Sig
metalBoink = toDrum $ foscil kampenv61 30 1 6.726 kampenv62 sine
    where
        kampenv61   = expseg [ 0.01, 0.01, 1, 0.2, 0.1, 0.1, 0.001 ]
        kampenv62   = linseg [ 1, 0.1, 10, 0.1, 0.5, 0.01, 1 ]

-- fullkit

bassDrum = bassDrum' 64

bassDrum' :: D -> SE Sig
bassDrum' cps = toDrum $ env * osc (sig cps * kgliss * 0.5)
    where env = linseg [0, 0.00245, 1, 0.1225, 0]
          kgliss = expseg [10, 0.625, 1, 0.25, 1]


openHihat :: SE Sig
openHihat = hihat $ linseg [0, 0.004, 1, 0.121, 0]

closedHihat :: SE Sig
closedHihat = hihat $ linseg [0, 0.00245, 1, 0.1225, 0]

hihat :: Sig -> SE Sig
hihat kenv = toDrum =<< do
    asig <- rand kenv
    let a1 = moogvcf (butbp asig cf (cf / 2)) cf 0.7
        a2 = comb a1 0.5 (1/cpspch(8))
    return $ a2 * 0.05 + a1 * 0.93
    where cf = cpspch (5.08 + 8)

snare :: SE Sig
snare = toDrum =<< do
    asound <- rand 1
    let as1 = butbp asound 210 55
    return $ kenv2 * (as1 * 0.9 + asound * 0.8 * kenv1)
    where
        kenv1 = linseg [0, 0.00176, 1, 0.1232, 0]
        kenv2 = expseg [0.01, 0.0002, 1, 0.0297, 0.01, 0.09, 0.01]

crash = crash' $ cpspch 13.05

-- | Recommended values cpspch(13.03) - cpspch(13.10)
crash' :: D -> SE Sig
crash' cps = toDrum =<< do
    asig <- rand 1
    return $ 0.5 * (0.5 * aall + resonator (aall + asig * kenv2))
    where
        kenv2 = expseg [0.001, 0.0466, 1, 3.95, 0.001]

        resonator asig = 0.7 * a1 + 0.6 * kenv18 * a2
            where
                flt a = butbp asig (sig $ a * cps) (sig $ 0.5 * cps)
                a1 = sum $ fmap flt [1, 2.1, 2.8]
                a2 = comb a1 0.5 (0.5 / cps)
                kenv18 = expseg [0.01, 0.02, 1, 3.98, 0.01]

        harm k amp hs = k * amp * mean (fmap (osc . sig . ( * cps)) hs)

        aall = sum $ zipWith3 harm weights envelopes harmonics

        harmonics =
            [ [1, 0.89, 1.12341]
            , [2.24, 2.4 * 0.98, 2.14 * 1.02]
            , [3.312, 3.513312*0.89, 3.123312*1.11]
            , [4.89, 5.89*0.89, 6.89 * 1.03]
            , [5.12, 5.5612*1.02, 7.7312 * 0.998]
            , [6.97, 6.97 * 1.02, 6.97 * 0.98]
            , [7.89, 7.89 * 1.02, 7.89 * 0.98]
            , [1.25, 1.125 * 1.004, 1.134 * 0.996]
            ]

        weights = replicate 7 0.2 ++ [0.1]

        envelopes = fmap (\(a, b) -> expseg [0.01, a, 1, b, 0.01])
            [ (0.0274, 3.972)
            , (0.0264, 3.973)
            , (0.0209, 3.979)
            , (0.0248, 3.975)
            , (0.0283, 3.971)
            , (0.0330, 3.966)
            , (0.0396, 3.960)
            , (0.0209, 3.979)
            ]

handClap = handClap' 400

handClap' :: D -> SE Sig
handClap' cps = (toDrum =<< ) $ fmap onNoise $ rand 1
    where
        kenv1 = expseg [1.25, 0.03, 0.0001]
        kenv2 = expseg [0.001, 0.005, 1, 0.35, 0.001]

        onNoise asig = aout
            where
                anoise1 = kenv1 * asig
                anoise2 = kenv2 * asig

                adel1   = anoise1
                adel2   = delaySig 0.01 anoise1
                adel3   = delaySig 0.02 anoise1
                adel4   = delaySig 0.03 anoise2

                aout    = mean
                    [ rz adel1 2
                    , rz adel2 3
                    , rz adel3 4
                    , rz adel4 5.5
                    ]

                rz x y = resonz x (sig $ cps * y) (sig $ cps * 5.5)

--------------------------------------------------
-- sampler

mkSam = limSam 4

bd1 = mkSam dumb
bd2 = mkSam dumbBass

sn1 = mkSam pluckSnare
sweep = mkSam sortaKnockSweep
boink = mkSam metalBoink

sn2 = mkSam snare
ohh = mkSam openHihat
chh = mkSam closedHihat

bd3 = mkSam bassDrum
cr  = mkSam crash
clap = mkSam handClap
