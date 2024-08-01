-- | Filters
module Csound.Air.Filter (
  -- | Arguemnts are inversed to get most out of curruing. First come parameters and the last one is the signal.

  -- * First order filters
  lp1,
  hp1,

  -- * Simple filters
  lp,
  hp,
  bp,
  br,
  alp,
  bp2,
  br2,

  -- * Butterworth filters
  blp,
  bhp,
  bbp,
  bbr,

  -- * Filter order
  ResonFilter,
  FlatFilter,
  filt,
  flatFilt,
  toReson,

  -- * Specific filters

  -- ** Moog filters
  mlp,
  mlp2,
  mlp3,
  lp18,
  ladder,

  -- ** Formant filters
  formant,
  singA,
  singO,
  singE,
  singU,
  singO2,

  -- * Making the smooth lines
  smooth,
  slide,

  -- * Analog filters

  -- | Requires Csound 6.07 or higher
  alp1,
  alp2,
  alp3,
  alp4,
  ahp,

  -- * Zero delay filters

  -- ** One pole filters
  zdf1,
  zlp1,
  zhp1,
  zap1,

  -- ** Two pole filters
  zdf2,
  zlp,
  zhp,
  zbp,
  zubp,
  zbr,
  zap,
  zpeak,

  -- ** Ladder filter
  zladder,

  -- ** Four poles filters

  -- zdf4, zlp4, zbp4, zhp4,

  -- ** Eq-filters

  -- peakEq, highShelf, lowShelf,

  -- * Classic analog-like filters

  -- ** low pass
  lpCheb1,
  lpCheb1',
  lpCheb2,
  lpCheb2',
  clp,
  clp',

  -- ** band pass
  bpCheb1,
  bpCheb1',
  bpCheb2,
  bpCheb2',
  cbp,
  cbp',

  -- ** high pass
  hpCheb1,
  hpCheb1',
  hpCheb2,
  hpCheb2',
  chp,
  chp',
  -- resonant filters
  cheb1,
  cheb2,
  vcf,
  cheb1',
  cheb2',
  vcf',
  mode,

  -- * Named resonant low pass filters
  plastic,
  wobble,
  trumpy,
  harsh,

  -- * TB303 filter
  tbf,
  diode,
  fdiode,
  linDiode,
  -- Korg 35 filters
  linKorg_lp,
  linKorg_hp,
  linKorg_bp,
  korg_lp,
  korg_hp,
  korg_bp,
  klp,
  khp,
  kbp,

  -- * Statevariable filters
  slp,
  shp,
  sbp,
  sbr,

  -- * Multimode filters
  multiStatevar,
  multiSvfilter,
) where

import Csound.Typed

import Csound.Dynamic
import Csound.SigSpace (bat)
import Csound.Typed.Opcode

{- | Low-pass filter.

> lp cutoff resonance sig
-}
lp :: Sig -> Sig -> Sig -> Sig
lp cf q a = bqrez a cf q

{- | High-pass filter.

> hp cutoff resonance sig
-}
hp :: Sig -> Sig -> Sig -> Sig
hp cf q a = bqrez a cf q `withD` 1

{- | Band-pass filter.

> bp cutoff resonance sig
-}
bp :: Sig -> Sig -> Sig -> Sig
bp cf q a = bqrez a cf q `withD` 2

{- | Band-reject filter.

> br cutoff resonance sig
-}
br :: Sig -> Sig -> Sig -> Sig
br cf q a = bqrez a cf q `withD` 3

{- | All-pass filter.

> alp cutoff resonance sig
-}
alp :: Sig -> Sig -> Sig -> Sig
alp cf q a = bqrez a cf q `withD` 4

-- Butterworth filters

{- | High-pass filter.

> bhp cutoff sig
-}
bhp :: Sig -> Sig -> Sig
bhp = flip buthp

{- | Low-pass filter.

> blp cutoff sig
-}
blp :: Sig -> Sig -> Sig
blp = flip butlp

{- | Band-pass filter.

> bbp cutoff bandwidth sig
-}
bbp :: Sig -> Sig -> Sig -> Sig
bbp freq band a = butbp a freq band

{- | Band-regect filter.

> bbr cutoff bandwidth sig
-}
bbr :: Sig -> Sig -> Sig -> Sig
bbr freq band a = butbr a freq band

{- | Moog's low-pass filter.

> mlp centerFrequency qResonance signal
-}
mlp :: Sig -> Sig -> Sig -> Sig
mlp cf q asig = moogvcf asig cf q

{- | Makes slides between values in the signals.
The first value defines a duration in seconds for a transition from one
value to another in piecewise constant signals.
-}
slide :: Sig -> Sig -> Sig
slide = flip lineto

{- | Produces smooth transitions between values in the signals.
The first value defines a duration in seconds for a transition from one
value to another in piecewise constant signals.

> smooth transTime asig
-}
smooth :: Sig -> Sig -> Sig
smooth = flip portk

{- | Resonant filter.

> f centerFreq q asig
-}
type ResonFilter = Sig -> Sig -> Sig -> Sig

{- | Filter without a resonance.

> f centerFreq q asig
-}
type FlatFilter = Sig -> Sig -> Sig

-- | Makes fake resonant filter from flat filter. The resulting filter just ignores the resonance.
toReson :: FlatFilter -> ResonFilter
toReson f = \cfq _res -> f cfq

-- | Applies a filter n-times. The n is given in the first rgument.
filt :: Int -> ResonFilter -> ResonFilter
filt n f cfq q asig = (foldl (.) id $ replicate n (f cfq q)) asig

-- | Applies a flat filter (without resonance) n-times. The n is given in the first rgument.
flatFilt :: Int -> FlatFilter -> FlatFilter
flatFilt n f cfq asig = (foldl (.) id $ replicate n (f cfq)) asig

-- spec filt

{- | Low pass filter 18 dB  with built in distortion module.

> lp18 distortion centerFreq resonance asig

* distortion's range is 0 to 1

* resonance's range is 0 to 1
-}
lp18 :: Sig -> Sig -> Sig -> Sig -> Sig
lp18 dist cfq q asig = lpf18 asig cfq q dist

{- | Another implementation of moog low pass filter (it's moogladder in Csound).
The arguments have are just like in the @mlp@ filter.

> mlp2 centerFreq q asig
-}
mlp2 :: Sig -> Sig -> Sig -> Sig
mlp2 cfq q asig = moogladder asig cfq q

{- | Mooglowpass filter with 18 dB.

> mlp3 centerFreq q asig
-}
mlp3 :: Sig -> Sig -> Sig -> Sig
mlp3 = lp18 0

{- | First order low pass filter (tone in Csound, 6 dB)

> lp1 centerFreq asig
-}
lp1 :: Sig -> Sig -> Sig
lp1 cfq asig = tone asig cfq

{- | First order high pass filter (atone in Csound, 6 dB)

> hp1 centerFreq asig
-}
hp1 :: Sig -> Sig -> Sig
hp1 cfq asig = atone asig cfq

{- | Resonance band pass filter (yet another implementation, it's reson in Csound)

> bp2 centerFreq q asig
-}
bp2 :: Sig -> Sig -> Sig -> Sig
bp2 cfq q asig = reson asig cfq q

{- | Resonance band reject filter (yet another implementation, it's areson in Csound)

> br2 centerFreq q asig
-}
br2 :: Sig -> Sig -> Sig -> Sig
br2 cfq q asig = areson asig cfq q

{- | Formant filter.

> formant bandPassFilter formants asig

It expects a band pass filter, a list of formants and processed signal.
The signal is processed with each filter the result is a sum of all proceessed signals.
Formant filters are used to mimic the vocalization of the sound.
-}
formant :: ResonFilter -> [(Sig, Sig)] -> Sig -> Sig
formant f qs asig = sum (fmap (($ asig) . uncurry f) qs)

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

anO, anA, anE, anIY, anO2 :: [(Sig, Sig)]
anO = [(280, 20), (650, 25), (2200, 30), (3450, 40), (4500, 50)]
anA = [(650, 50), (1100, 50), (2860, 50), (3300, 50), (4500, 50)]
anE = [(500, 50), (1750, 50), (2450, 50), (3350, 50), (5000, 50)]
anIY = [(330, 50), (2000, 50), (2800, 50), (3650, 50), (5000, 50)]
anO2 = [(400, 50), (840, 50), (2800, 50), (3250, 50), (4500, 50)]

-------------------------------------------------------
-- new filters

{- | Analog-like low-pass filter

> alpf1 centerFrequency resonance asig
-}
alp1 :: Sig -> Sig -> Sig -> Sig
alp1 freq resonance asig = mvclpf1 asig freq resonance

{- | Analog-like low-pass filter

> alpf2 centerFrequency resonance asig
-}
alp2 :: Sig -> Sig -> Sig -> Sig
alp2 freq resonance asig = mvclpf2 asig freq resonance

{- | Analog-like low-pass filter

> alpf3 centerFrequency resonanceance asig
-}
alp3 :: Sig -> Sig -> Sig -> Sig
alp3 freq resonance asig = mvclpf3 asig freq resonance

{- | Analog-like low-pass filter

> alpf4 centerFrequency resonance asig

Analog outputs

* asig1 -- 6dB/oct low-pass response output.

* asig2 -- 12dB/oct low-pass response output.

* asig3 -- 18dB/oct low-pass response output..

* asig4 -- 24dB/oct low-pass response output.
-}
alp4 :: Sig -> Sig -> Sig -> (Sig, Sig, Sig, Sig)
alp4 freq resonance asig = mvclpf4 asig freq resonance

{- | Analog-like high-pass filter

> ahp centerFrequency asig
-}
ahp :: Sig -> Sig -> Sig
ahp freq asig = mvchpf asig freq

-----------------------------------------------
-- named filters

-- classic filters

-- low pass

{- | Chebyshev  type I low pass filter (with 2 poles).

lpCheb1 centerFreq asig
-}
lpCheb1 :: Sig -> Sig -> Sig
lpCheb1 = lpCheb1' 2

{- | Chebyshev  type I low pass filter (with given number of poles, first argument).

lpCheb1' npols centerFreq asig
-}
lpCheb1' :: D -> Sig -> Sig -> Sig
lpCheb1' npoles kcf asig = clfilt asig kcf 0 npoles `withD` 1

{- | Chebyshev  type II low pass filter (with 2 poles).

lpCheb2 centerFreq asig
-}
lpCheb2 :: Sig -> Sig -> Sig
lpCheb2 = lpCheb2' 2

{- | Chebyshev  type II low pass filter (with given number of poles, first argument).

lpCheb2' npols centerFreq asig
-}
lpCheb2' :: D -> Sig -> Sig -> Sig
lpCheb2' npoles kcf asig = clfilt asig kcf 0 npoles `withD` 2

{- | Butterworth lowpass filter based on clfilt opcode (with 2 poles).

clp centerFreq asig
-}
clp :: Sig -> Sig -> Sig
clp = clp' 2

{- | Butterworth lowpass filter based on clfilt opcode (with given number of poles, first argument).

clp' npols centerFreq asig
-}
clp' :: D -> Sig -> Sig -> Sig
clp' npoles kcf asig = clfilt asig kcf 0 npoles `withD` 0

-- high pass

{- | Chebyshev  type I high pass filter (with 2 poles).

hpCheb1 centerFreq asig
-}
hpCheb1 :: Sig -> Sig -> Sig
hpCheb1 = hpCheb1' 2

{- | Chebyshev  type I high pass filter (with given number of poles, first argument).

hpCheb1' npols centerFreq asig
-}
hpCheb1' :: D -> Sig -> Sig -> Sig
hpCheb1' npoles kcf asig = clfilt asig kcf 1 npoles `withD` 1

{- | Chebyshev  type II high pass filter (with 2 poles).

hpCheb2 centerFreq asig
-}
hpCheb2 :: Sig -> Sig -> Sig
hpCheb2 = hpCheb2' 2

{- | Chebyshev  type II high pass filter (with given number of poles, first argument).

hpCheb2' npols centerFreq asig
-}
hpCheb2' :: D -> Sig -> Sig -> Sig
hpCheb2' npoles kcf asig = clfilt asig kcf 1 npoles `withD` 2

{- | Butterworth high pass filter based on clfilt opcode (with 2 poles).

chp centerFreq asig
-}
chp :: Sig -> Sig -> Sig
chp = clp' 2

{- | Butterworth high pass filter based on clfilt opcode (with given number of poles, first argument).

chp' npols centerFreq asig
-}
chp' :: D -> Sig -> Sig -> Sig
chp' npoles kcf asig = clfilt asig kcf 1 npoles `withD` 0

------------------------------------------
-- band-pass

mkBp :: FlatFilter -> FlatFilter -> Sig -> Sig -> Sig -> Sig
mkBp lowPass highPass cfq bw asig = highPass (cfq - rad) $ lowPass (cfq + rad) asig
  where
    rad = bw / 2

bpCheb1 :: Sig -> Sig -> Sig -> Sig
bpCheb1 = bpCheb1' 2

bpCheb1' :: D -> Sig -> Sig -> Sig -> Sig
bpCheb1' npoles = mkBp (lpCheb1' npoles) (hpCheb1' npoles)

bpCheb2 :: Sig -> Sig -> Sig -> Sig
bpCheb2 = bpCheb2' 2

bpCheb2' :: D -> Sig -> Sig -> Sig -> Sig
bpCheb2' npoles = mkBp (lpCheb2' npoles) (hpCheb2' npoles)

cbp :: Sig -> Sig -> Sig -> Sig
cbp = cbp' 2

cbp' :: D -> Sig -> Sig -> Sig -> Sig
cbp' npoles = mkBp (clp' npoles) (chp' npoles)

---------------------------------------------
-- resonant filters

mkReson :: FlatFilter -> FlatFilter -> ResonFilter
mkReson lowPass highPass kcf res asig = 0.5 * (lowPass (kcf * 2) asig + bandPass bw kcf asig)
  where
    bw = kcf / (0.001 + abs res)
    bandPass = mkBp lowPass highPass

cheb1 :: Sig -> Sig -> Sig -> Sig
cheb1 = cheb1' 2

cheb1' :: D -> Sig -> Sig -> Sig -> Sig
cheb1' npoles = mkReson (lpCheb1' npoles) (hpCheb1' npoles)

cheb2 :: Sig -> Sig -> Sig -> Sig
cheb2 = cheb2' 2

cheb2' :: D -> Sig -> Sig -> Sig -> Sig
cheb2' npoles = mkReson (lpCheb2' npoles) (hpCheb2' npoles)

vcf :: Sig -> Sig -> Sig -> Sig
vcf = cbp' 2

vcf' :: D -> Sig -> Sig -> Sig -> Sig
vcf' npoles = mkReson (clp' npoles) (chp' npoles)

{- |
A filter that simulates a mass-spring-damper system

Filters the incoming signal with the specified resonance frequency and
      quality factor. It can also be seen as a signal generator for high quality
      factor, with an impulse for the excitation. You can combine several modes
      to built complex instruments such as bells or guitar tables.

> aout  mode  ain, xfreq, xQ [, iskip]

csound doc: <http://csound.com/docs/manual/mode.html>
-}
mode :: Sig -> Sig -> Sig -> Sig
mode b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "mode" [(Ar, [Ar, Xr, Xr, Ir])] [a1, a2, a3]

-- moog ladder

{- | Moog ladder filter

> ladder centerFreq q asig
-}
ladder :: Sig -> Sig -> Sig -> Sig
ladder kcf res asig = moogladder asig kcf res

-----------------------------------------
-- named filters

{- | plastic sound

> plastic centerFreq q asig
-}
plastic :: Sig -> Sig -> Sig -> Sig
plastic kcf res asig = rezzy asig kcf (1 + 99 * res)

{- | wobble sound

> wobble centerFreq q asig
-}
wobble :: Sig -> Sig -> Sig -> Sig
wobble kcf res asig = lowres asig kcf res

{- | trumpy sound

> trumpy centerFreq q asig
-}
trumpy :: Sig -> Sig -> Sig -> Sig
trumpy kcf res asig = vlowres asig kcf (res * 0.15) 6 (4 + res * 20)

{- | harsh sound

> harsh centerFreq q asig
-}
harsh :: Sig -> Sig -> Sig -> Sig
harsh kcf res asig = bat (\x -> bqrez x kcf (1 + 90 * res)) asig

-----------------------------

{- | Fixed version of tbfcv filter
the first argument is distortion (range [0, 1])
-}
tbf :: Sig -> Sig -> Sig -> Sig -> Sig
tbf dist kcf res asig = tbvcf asig (1010 + kcf) res (0.5 + 3.5 * dist) 0.5

-----------------------------
-- state variable filter

-- | State variable low-pass filter
slp :: Sig -> Sig -> Sig -> Sig
slp kcf res asig = lows
  where
    (_, lows, _, _) = statevar asig kcf res

-- | State variable high-pass filter
shp :: Sig -> Sig -> Sig -> Sig
shp kcf res asig = highs
  where
    (highs, _, _, _) = statevar asig kcf res

-- | State variable band-pass filter
sbp :: Sig -> Sig -> Sig -> Sig
sbp kcf res asig = mids
  where
    (_, _, mids, _) = statevar asig kcf res

-- | State variable band-reject filter
sbr :: Sig -> Sig -> Sig -> Sig
sbr kcf res asig = sides
  where
    (_, _, _, sides) = statevar asig kcf res

multiStatevar :: (Sig, Sig, Sig) -> Sig -> Sig -> Sig -> Sig
multiStatevar (weightLows, wieghtHighs, weightMids) kcf res asig = weightLows * lows + wieghtHighs * highs + weightMids * mids
  where
    (highs, lows, mids, _) = statevar asig kcf res

multiSvfilter :: (Sig, Sig, Sig) -> Sig -> Sig -> Sig -> Sig
multiSvfilter (weightLows, wieghtHighs, weightMids) kcf res asig = weightLows * lows + wieghtHighs * highs + weightMids * mids
  where
    (highs, lows, mids) = svfilter asig kcf res

--------------------------------

{- | Zero-delay feedback implementation of 1 pole filter.

ouputs low-pass and high-pass signals.

> zdf1 centerFreq asig = (alp, ahp)
-}
zdf1 :: Sig -> Sig -> (Sig, Sig)
zdf1 cfq asig = zdf_1pole_mode asig cfq

{- | Zero-delay feedback implementation of 1 pole low-pass filter.

> zlp1 centerFreq asig
-}
zlp1 :: Sig -> Sig -> Sig
zlp1 cfq asig = zdf_1pole asig cfq `withSig` 0

{- | Zero-delay feedback implementation of 1 pole high-pass filter.

> zhp1 centerFreq asig
-}
zhp1 :: Sig -> Sig -> Sig
zhp1 cfq asig = zdf_1pole asig cfq `withSig` 1

{- | Zero-delay feedback implementation of 1 pole allpass filter.

> zap1 centerFreq asig
-}
zap1 :: Sig -> Sig -> Sig
zap1 cfq asig = zdf_1pole asig cfq `withSig` 2

{- | zero delay feedback 2 pole filter

> zdf2 centerFreq q asig = (alp, abp, ahp)
-}
zdf2 :: Sig -> Sig -> Sig -> (Sig, Sig, Sig)
zdf2 cfq q asig = zdf_2pole_mode asig cfq (uon 0.5 25 q)

zpole2 :: Sig -> Sig -> Sig -> Sig -> Sig
zpole2 n cfq q asig = zdf_2pole asig cfq (uon 0.5 25 q) `withSig` n

{- | zero delay feedback 2 pole Low pass filter. Q is unipolar [0, 1]

> zlp centerFreq q asig
-}
zlp :: Sig -> Sig -> Sig -> Sig
zlp = zpole2 0

{- | zero delay feedback 2 pole High pass filter. Q is unipolar [0, 1]

> zhp centerFreq q asig
-}
zhp :: Sig -> Sig -> Sig -> Sig
zhp = zpole2 1

{- | zero delay feedback 2 pole Band pass. Q is unipolar [0, 1]

> zbp centerFreq q asig
-}
zbp :: Sig -> Sig -> Sig -> Sig
zbp = zpole2 2

{- | Unity-gain bandpass (zero delay feedback 2 pole). Q is unipolar [0, 1]

> zubp centerFreq q asig
-}
zubp :: Sig -> Sig -> Sig -> Sig
zubp = zpole2 3

{- | zero delay feedback 2 pole Notch (band reject). Q is unipolar [0, 1]

> zbr centerFreq q asig
-}
zbr :: Sig -> Sig -> Sig -> Sig
zbr = zpole2 4

{- | zero delay feedback 2 pole Allpass filter. Q is unipolar [0, 1]

> zap centerFreq q asig
-}
zap :: Sig -> Sig -> Sig -> Sig
zap = zpole2 5

{- | zero delay feedback 2 pole Peak filter. Q is unipolar [0, 1]

> zpeak centerFreq q asig
-}
zpeak :: Sig -> Sig -> Sig -> Sig
zpeak = zpole2 6

{- |  Zero-delay feedback implementation of 4 pole ladder filter. Q is unipolar [0, 1]

> zladder centerFreq q asig
-}
zladder :: Sig -> Sig -> Sig -> Sig
zladder cfq q asig = zdf_ladder asig cfq (uon 0.5 25 q)

{- |  Zero-delay feedback implementation of 4 pole diode ladder filter  (24 dB/oct) .
This filter design was originally used in the EMS VCS3 and was the resonant filter in the Roland TB-303.

* Q is unipolar [0, 1]

* saturation - amount to use for non-linear processing. Values > 1 increase the steepness of the NLP curve.

> diode saturation centerFreq q asig
-}
diode :: D -> Sig -> Sig -> Sig -> Sig
diode isaturation cfq fbk asig = diode_ladder asig cfq (17 * fbk) `withDs` [1, isaturation]

{- | Faster diode, but lesser quality

> fdiode saturation centerFreq q asig
-}
fdiode :: D -> Sig -> Sig -> Sig -> Sig
fdiode isaturation cfq fbk asig = diode_ladder asig cfq (17 * fbk) `withDs` [2, isaturation]

{- | Linear diode, no saturation involved

> linDiode centerFreq q asig
-}
linDiode :: Sig -> Sig -> Sig -> Sig
linDiode cfq fbk asig = diode_ladder asig cfq (17 * fbk) `withDs` [0]

{- | Korg35 resonant low-pass filter. Q is unipolar [0, 1]

> korg_lp saturation centerFreq q asig
-}
korg_lp :: D -> Sig -> Sig -> Sig -> Sig
korg_lp isaturation cfq q asig = k35_lpf asig cfq (uon 1 10 q) `withDs` [1, isaturation]

{- | Korg35 resonant high-pass filter. Q is unipolar [0, 1]

> korg_hp saturation centerFreq q asig
-}
korg_hp :: D -> Sig -> Sig -> Sig -> Sig
korg_hp isaturation cfq q asig = k35_hpf asig cfq (uon 1 10 q) `withDs` [1, isaturation]

{- | Korg35 resonant band-pass filter. Q is unipolar [0, 1]

> korg_bp saturation centerFreq q asig
-}
korg_bp :: D -> Sig -> Sig -> Sig -> Sig
korg_bp isaturation cfq q asig = korg_hp isaturation cfq q $ korg_lp isaturation cfq q asig

{- | Linear Korg35 resonant low-pass filter

> linKorg_lp centerFreq q asig
-}
linKorg_lp :: Sig -> Sig -> Sig -> Sig
linKorg_lp cfq q asig = k35_lpf asig cfq (uon 1 10 q) `withDs` [0]

{- | Linear Korg35 resonant high-pass filter

> linKorg_hp centerFreq q asig
-}
linKorg_hp :: Sig -> Sig -> Sig -> Sig
linKorg_hp cfq q asig = k35_hpf asig cfq (uon 1 10 q) `withDs` [0]

-- Linear Korg35 resonant band-pass filter
linKorg_bp :: Sig -> Sig -> Sig -> Sig
linKorg_bp cfq q asig = linKorg_hp cfq q $ linKorg_lp cfq q asig

-- | Alias for korg_lp
klp :: D -> Sig -> Sig -> Sig -> Sig
klp = korg_lp

-- | Alias for korg_hp
khp :: D -> Sig -> Sig -> Sig -> Sig
khp = korg_hp

-- | Alias for korg_bp
kbp :: D -> Sig -> Sig -> Sig -> Sig
kbp = korg_bp
