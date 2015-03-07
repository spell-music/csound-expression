-- | Filters
module Csound.Air.Filter(
    -- | Arguemnts are inversed to get most out of curruing. First come parameters and the last one is the signal.

    -- * First order filters
    lp1, hp1,

    -- * Simple filters
    lp, hp, bp, br, alp,
    bp2, br2,
    
    -- * Butterworth filters
    blp, bhp, bbp, bbr,

    -- * Filter order
    ResonFilter, FlatFilter,
    filt, flatFilt,

    -- * Specific filters

    -- ** Moog filters
    mlp, mlp2, mlp3, lp18,

    -- ** Formant filters
    formant, singA, singO, singE, singU, singO2,

    -- * Making the smooth lines
    smooth, slide

) where

import Csound.Typed
import Csound.SigSpace(bat)
import Csound.Typed.Opcode

-- | Low-pass filter.
--
-- > lp cutoff resonance sig
lp :: Sig -> Sig -> Sig -> Sig
lp cf q a = bqrez a cf q

-- | High-pass filter.
--
-- > hp cutoff resonance sig
hp :: Sig -> Sig -> Sig -> Sig
hp cf q a = bqrez a cf q `withD` 1

-- | Band-pass filter.
--
-- > bp cutoff resonance sig
bp :: Sig -> Sig -> Sig -> Sig
bp cf q a = bqrez a cf q `withD` 2

-- | Band-reject filter.
--
-- > br cutoff resonance sig
br :: Sig -> Sig -> Sig -> Sig
br cf q a = bqrez a cf q `withD` 3

-- | All-pass filter.
--
-- > alp cutoff resonance sig
alp :: Sig -> Sig -> Sig -> Sig
alp cf q a = bqrez a cf q `withD` 4

-- Butterworth filters

-- | High-pass filter.
--
-- > bhp cutoff sig
bhp :: Sig -> Sig -> Sig
bhp = flip buthp

-- | Low-pass filter.
--
-- > blp cutoff sig
blp :: Sig -> Sig -> Sig
blp = flip butlp

-- | Band-pass filter.
--
-- > bbp cutoff bandwidth sig
bbp :: Sig -> Sig -> Sig -> Sig
bbp freq band a = butbp a freq band

-- | Band-regect filter.
--
-- > bbr cutoff bandwidth sig
bbr :: Sig -> Sig -> Sig -> Sig 
bbr freq band a = butbr a freq band


-- | Moog's low-pass filter.
--
-- > mlp centerFrequency qResonance signal
mlp :: Sig -> Sig -> Sig -> Sig
mlp cf q asig = moogladder asig cf q

-- | Makes slides between values in the signals.
-- The first value defines a duration in seconds for a transition from one
-- value to another in piecewise constant signals.
slide :: Sig -> Sig -> Sig
slide = flip lineto

-- | Produces smooth transitions between values in the signals.
-- The first value defines a duration in seconds for a transition from one
-- value to another in piecewise constant signals.
smooth :: Sig -> Sig -> Sig
smooth = flip portk

-- | Resonant filter.
-- 
-- > f centerFreq q asig
type ResonFilter = Sig -> Sig -> Sig -> Sig

-- | Filter without a resonance.
-- 
-- > f centerFreq q asig
type FlatFilter  = Sig -> Sig -> Sig

-- | Applies a filter n-times. The n is given in the first rgument.
filt :: Int -> ResonFilter -> ResonFilter
filt n f cfq q asig = (foldl (.) id $ replicate n (f cfq q)) asig

-- | Applies a flat filter (without resonance) n-times. The n is given in the first rgument.
flatFilt :: Int -> FlatFilter -> FlatFilter
flatFilt n f cfq asig = (foldl (.) id $ replicate n (f cfq)) asig

-- spec filt

-- | Low pass filter 18 dB  with built in distortion module.
--
-- > lp18 distortion centerFreq resonance asig
--
-- * distortion's range is 0 to 1
--
-- * resonance's range is 0 to 1
lp18 :: Sig -> Sig -> Sig -> Sig -> Sig
lp18 dist cfq q asig = lpf18 asig cfq q dist

-- | Another implementation of moog low pass filter (it's moogvcf in Csound).
-- The arguments have are just like in the @mlp@ filter.
mlp2 :: Sig -> Sig -> Sig -> Sig
mlp2 cfq q asig = moogvcf asig cfq q

-- | Mooglowpass filter with 18 dB.
mlp3 :: Sig -> Sig -> Sig -> Sig
mlp3 = lp18 0

-- | First order low pass filter (tone in Csound, 6 dB)
--
-- > lp1 centerFreq asig
lp1 :: Sig -> Sig -> Sig
lp1 cfq asig = tone asig cfq

-- | First order high pass filter (atone in Csound, 6 dB)
--
-- > hp1 centerFreq asig
hp1 :: Sig -> Sig -> Sig
hp1 cfq asig = atone asig cfq

-- | Resonance band pass filter (yet another implementation, it's reson in Csound) 
--
-- > bp2 centerFreq q asig
bp2 :: Sig -> Sig -> Sig -> Sig
bp2 cfq q asig = reson asig cfq q

-- | Resonance band reject filter (yet another implementation, it's areson in Csound) 
--
-- > br2 centerFreq q asig
br2 :: Sig -> Sig -> Sig -> Sig
br2 cfq q asig = areson asig cfq q

-- | Formant filter.
--
-- > formant bandPassFilter formants asig
--
-- It expects a band pass filter, a list of formants and processed signal.
-- The signal is processed with each filter the result is a sum of all proceessed signals.
-- Formant filters are used to mimic the vocalization of the sound.
formant :: ResonFilter -> [(Sig, Sig)] -> Sig -> Sig
formant f qs asig = sum (fmap (( $ asig) . uncurry f) qs)

-- | Formant filter that sings an A.
singA :: Sig -> Sig
singA = bat (formant bp2 anA)

-- | Formant filter that sings an O.
singO :: Sig -> Sig
singO = bat (formant bp2 anO)

-- | Formant filter that sings an E.
singE :: Sig -> Sig
singE = bat (formant bp2 anE)

-- | Formant filter that sings an U.
singU :: Sig -> Sig
singU = bat (formant bp2 anIY)

-- | Formant filter that sings an O.
singO2 :: Sig -> Sig
singO2 = bat (formant bp2 anO2)

anO  = [(280, 20), (650, 25), (2200, 30), (3450, 40), (4500, 50)]
anA  = [(650, 50), (1100, 50), (2860, 50), (3300, 50), (4500, 50)] 
anE  = [(500, 50), (1750, 50), (2450, 50), (3350, 50), (5000, 50)]
anIY = [(330, 50), (2000, 50), (2800, 50), (3650, 50), (5000, 50)]
anO2 = [(400, 50), (840, 50), (2800, 50), (3250, 50), (4500, 50)]
