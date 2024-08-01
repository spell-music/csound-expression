module Csound.Typed.Opcode.SignalGenerators (
  -- * Additive Synthesis/Resynthesis.
  adsyn,
  adsynt,
  adsynt2,
  hsboscil,

  -- * Basic Oscillators.
  lfo,
  oscbnk,
  oscil,
  oscil3,
  oscili,
  oscilikt,
  osciliktp,
  oscilikts,
  osciln,
  oscils,
  poscil,
  poscil3,
  vibr,
  vibrato,

  -- * Dynamic Spectrum Oscillators.
  buzz,
  gbuzz,
  mpulse,
  squinewave,
  vco,
  vco2,
  vco2ft,
  vco2ift,
  vco2init,

  -- * FM Synthesis.
  crossfm,
  crossfmi,
  crosspm,
  crosspmi,
  crossfmpm,
  crossfmpmi,
  fmb3,
  fmbell,
  fmmetal,
  fmpercfl,
  fmrhode,
  fmvoice,
  fmwurlie,
  foscil,
  foscili,

  -- * Granular Synthesis.
  diskgrain,
  fof,
  fof2,
  fog,
  grain,
  grain2,
  grain3,
  granule,
  partikkel,
  partikkelget,
  partikkelset,
  partikkelsync,
  sndwarp,
  sndwarpst,
  syncgrain,
  syncloop,
  vosim,

  -- * Hyper Vectorial Synthesis.
  hvs1,
  hvs2,
  hvs3,

  -- * Linear and Exponential Generators.
  bpf,
  bpfcos,
  cosseg,
  cossegb,
  cossegr,
  expcurve,
  expon,
  expseg,
  expsega,
  expsegb,
  expsegba,
  expsegr,
  gainslider,
  lincos,
  line,
  linlin,
  linseg,
  linsegb,
  linsegr,
  logcurve,
  loopseg,
  loopsegp,
  looptseg,
  loopxseg,
  lpshold,
  lpsholdp,
  scale,
  scale2,
  transeg,
  transegb,
  transegr,
  trigexpseg,
  triglinseg,
  xyscale,

  -- * Envelope Generators.
  adsr,
  envlpx,
  envlpxr,
  gtadsr,
  linen,
  linenr,
  madsr,
  mxadsr,
  xadsr,

  -- * Models and Emulations.
  bamboo,
  barmodel,
  cabasa,
  chuap,
  crunch,
  dripwater,
  gendy,
  gendyc,
  gendyx,
  gogobel,
  guiro,
  lorenz,
  mandel,
  mandol,
  marimba,
  moog,
  planet,
  prepiano,
  sandpaper,
  sekere,
  shaker,
  sleighbells,
  stix,
  tambourine,
  vibes,
  voice,

  -- * Phasors.
  ephasor,
  phasor,
  phasorbnk,
  sc_phasor,
  syncphasor,
  trigphasor,

  -- * Random (Noise) Generators.
  betarand,
  bexprnd,
  cauchy,
  cauchyi,
  cuserrnd,
  duserrnd,
  dust,
  dust2,
  exprand,
  exprandi,
  fractalnoise,
  gauss,
  gaussi,
  gausstrig,
  getseed,
  jitter,
  jitter2,
  jspline,
  lfsr,
  linrand,
  noise,
  pcauchy,
  pinker,
  pinkish,
  poisson,
  rand,
  randh,
  randi,
  random,
  randomh,
  randomi,
  rnd31,
  rndseed,
  rspline,
  seed,
  trandom,
  trirand,
  unirand,
  urandom,
  urd,
  weibull,

  -- * Sample Playback.
  bbcutm,
  bbcuts,
  flooper,
  flooper2,
  fluidAllOut,
  fluidCCi,
  fluidCCk,
  fluidControl,
  fluidEngine,
  fluidInfo,
  fluidLoad,
  fluidNote,
  fluidOut,
  fluidProgramSelect,
  fluidSetInterpMethod,
  loscil,
  loscilphs,
  loscil3,
  loscil3phs,
  loscilx,
  lphasor,
  lposcil,
  lposcil3,
  lposcila,
  lposcilsa,
  lposcilsa2,
  sfilist,
  sfinstr,
  sfinstr3,
  sfinstr3m,
  sfinstrm,
  sfload,
  sflooper,
  sfpassign,
  sfplay,
  sfplay3,
  sfplay3m,
  sfplaym,
  sfplist,
  sfpreset,
  sndloop,
  waveset,

  -- * Scanned Synthesis.
  scanhammer,
  scanmap,
  scans,
  scansmap,
  scantable,
  scanu,
  scanu2,
  xscanmap,
  xscans,
  xscansmap,
  xscanu,

  -- * STK Opcodes.
  stkBandedWG,
  stkBeeThree,
  stkBlowBotl,
  stkBlowHole,
  stkBowed,
  stkBrass,
  stkClarinet,
  stkDrummer,
  stkFMVoices,
  stkFlute,
  stkHevyMetl,
  stkMandolin,
  stkModalBar,
  stkMoog,
  stkPercFlut,
  stkPlucked,
  stkResonate,
  stkRhodey,
  stkSaxofony,
  stkShakers,
  stkSimple,
  stkSitar,
  stkStifKarp,
  stkTubeBell,
  stkVoicForm,
  stkWhistle,
  stkWurley,

  -- * Table Access.
  oscil1,
  oscil1i,
  ptable,
  ptable3,
  ptablei,
  tab,
  tab_i,
  table,
  table3,
  tablei,
  tabw_i,

  -- * Wave Terrain Synthesis.
  sterrain,
  wterrain,
  wterrain2,

  -- * Waveguide Physical Modeling.
  pluck,
  repluck,
  streson,
  wgbow,
  wgbowedbar,
  wgbrass,
  wgclar,
  wgflute,
  wgpluck,
  wgpluck2,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Csound.Dynamic
import Csound.Typed

-- Additive Synthesis/Resynthesis.

{- |
Output is an additive set of individually controlled sinusoids, using an oscillator bank.

> ares  adsyn  kamod, kfmod, ksmod, ifilcod

csound doc: <https://csound.com/docs/manual/adsyn.html>
-}
adsyn :: Sig -> Sig -> Sig -> Str -> Sig
adsyn b1 b2 b3 b4 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unStr b4
  where
    f a1 a2 a3 a4 = opcs "adsyn" [(Ar, [Kr, Kr, Kr, Sr])] [a1, a2, a3, a4]

{- |
Performs additive synthesis with an arbitrary number of partials, not necessarily harmonic.

> ares  adsynt  kamp, kcps, iwfn, ifreqfn, iampfn, icnt [, iphs]

csound doc: <https://csound.com/docs/manual/adsynt.html>
-}
adsynt :: Sig -> Sig -> Tab -> Tab -> Tab -> D -> Sig
adsynt b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unTab b3 <*> unTab b4 <*> unTab b5 <*> unD b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "adsynt" [(Ar, [Kr, Kr, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
Performs additive synthesis with an arbitrary number of partials -not necessarily harmonic- with interpolation.

Performs additive synthesis with an arbitrary number of partials, not necessarily harmonic. (see adsynt for detailed manual)

> ar  adsynt2  kamp, kcps, iwfn, ifreqfn, iampfn, icnt [, iphs]

csound doc: <https://csound.com/docs/manual/adsynt2.html>
-}
adsynt2 :: Sig -> Sig -> Tab -> Tab -> Tab -> D -> Sig
adsynt2 b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unTab b3 <*> unTab b4 <*> unTab b5 <*> unD b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "adsynt2" [(Ar, [Kr, Kr, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
An oscillator which takes tonality and brightness as arguments.

An oscillator which takes tonality and brightness as arguments, relative to a base frequency.

> ares  hsboscil  kamp, ktone, kbrite, ibasfreq, iwfn, ioctfn \
>                    [, ioctcnt] [, iphs]

csound doc: <https://csound.com/docs/manual/hsboscil.html>
-}
hsboscil :: Sig -> Sig -> Sig -> D -> Tab -> Tab -> Sig
hsboscil b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unTab b5 <*> unTab b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "hsboscil" [(Ar, [Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

-- Basic Oscillators.

{- |
A low frequency oscillator of various shapes.

> kres  lfo  kamp, kcps [, itype]
> ares  lfo  kamp, kcps [, itype]

csound doc: <https://csound.com/docs/manual/lfo.html>
-}
lfo :: Sig -> Sig -> Sig
lfo b1 b2 =
  Sig $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = opcs "lfo" [(Kr, [Kr, Kr, Ir]), (Ar, [Kr, Kr, Ir])] [a1, a2]

{- |
Mixes the output of any number of oscillators.

This unit generator mixes the output of any number of oscillators. The frequency, phase, and amplitude of each oscillator can be modulated by two LFOs (all oscillators have a separate set of LFOs, with different phase and frequency); additionally, the output of each oscillator can be filtered through an optional parametric equalizer (also controlled by the LFOs).  This opcode is most useful for rendering ensemble (strings, choir, etc.) instruments.

> ares  oscbnk   kcps, kamd, kfmd, kpmd, iovrlap, iseed, kl1minf, kl1maxf, \
>           kl2minf, kl2maxf, ilfomode, keqminf, keqmaxf, keqminl, keqmaxl, \
>           keqminq, keqmaxq, ieqmode, kfn [, il1fn] [, il2fn] [, ieqffn]   \
>           [, ieqlfn] [, ieqqfn] [, itabl] [, ioutfn]

csound doc: <https://csound.com/docs/manual/oscbnk.html>
-}
oscbnk :: Sig -> Sig -> Sig -> Sig -> D -> D -> Sig -> Sig -> Sig -> Sig -> D -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> Tab -> Sig
oscbnk b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unD b5 <*> unD b6 <*> unSig b7 <*> unSig b8 <*> unSig b9 <*> unSig b10 <*> unD b11 <*> unSig b12 <*> unSig b13 <*> unSig b14 <*> unSig b15 <*> unSig b16 <*> unSig b17 <*> unD b18 <*> unTab b19
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 =
      opcs
        "oscbnk"
        [
          ( Ar
          , [Kr, Kr, Kr, Kr, Ir, Ir, Kr, Kr, Kr, Kr, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Ir, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir]
          )
        ]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        , a19
        ]

{- |
A simple oscillator.

oscil reads table ifn sequentially and repeatedly at a frequency xcps. The amplitude is scaled by xamp.

> ares  oscil  xamp, xcps [, ifn, iphs]
> kres  oscil  kamp, kcps [, ifn, iphs]

csound doc: <https://csound.com/docs/manual/oscil.html>
-}
oscil :: Sig -> Sig -> Tab -> Sig
oscil b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
  where
    f a1 a2 a3 = opcs "oscil" [(Ar, [Xr, Xr, Ir, Ir]), (Kr, [Kr, Kr, Ir, Ir])] [a1, a2, a3]

{- |
A simple oscillator with cubic interpolation.

oscil3 reads table ifn sequentially and repeatedly at a frequency xcps. The amplitude is scaled by xamp. Cubic interpolation is applied for table look up from internal phase values.

> ares  oscil3  xamp, xcps [, ifn, iphs]
> kres  oscil3  kamp, kcps [, ifn, iphs]

csound doc: <https://csound.com/docs/manual/oscil3.html>
-}
oscil3 :: Sig -> Sig -> Tab -> Sig
oscil3 b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
  where
    f a1 a2 a3 = opcs "oscil3" [(Ar, [Xr, Xr, Ir, Ir]), (Kr, [Kr, Kr, Ir, Ir])] [a1, a2, a3]

{- |
A simple oscillator with linear interpolation.

oscili reads table ifn sequentially and repeatedly at a frequency xcps. The amplitude is scaled by xamp. Linear interpolation is applied for table look up from internal phase values.

> ares  oscili  xamp, xcps[, ifn, iphs]
> kres  oscili  kamp, kcps[, ifn, iphs]

csound doc: <https://csound.com/docs/manual/oscili.html>
-}
oscili :: Sig -> Sig -> Tab -> Sig
oscili b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
  where
    f a1 a2 a3 = opcs "oscili" [(Ar, [Xr, Xr, Ir, Ir]), (Kr, [Kr, Kr, Ir, Ir])] [a1, a2, a3]

{- |
A linearly interpolated oscillator that allows changing the table number at k-rate.

oscilikt is very similar to oscili, but allows changing the table number at k-rate. It is slightly slower than oscili (especially with high control rate), although also more accurate as it uses a 31-bit phase accumulator, as opposed to the 24-bit one used by oscili.

> ares  oscilikt  xamp, xcps, kfn [, iphs] [, istor]
> kres  oscilikt  kamp, kcps, kfn [, iphs] [, istor]

csound doc: <https://csound.com/docs/manual/oscilikt.html>
-}
oscilikt :: Sig -> Sig -> Tab -> Sig
oscilikt b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
  where
    f a1 a2 a3 = opcs "oscilikt" [(Ar, [Xr, Xr, Kr, Ir, Ir]), (Kr, [Kr, Kr, Kr, Ir, Ir])] [a1, a2, a3]

{- |
A linearly interpolated oscillator that allows allows phase modulation.

osciliktp allows phase modulation (which is actually implemented as k-rate frequency modulation, by differentiating phase input). The disadvantage is that there is no amplitude control, and frequency can be varied only at the control-rate. This opcode can be faster or slower than oscilikt, depending on the control-rate.

> ares  osciliktp  kcps, kfn, kphs [, istor]

csound doc: <https://csound.com/docs/manual/osciliktp.html>
-}
osciliktp :: Sig -> Tab -> Sig -> Sig
osciliktp b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unTab b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "osciliktp" [(Ar, [Kr, Kr, Kr, Ir])] [a1, a2, a3]

{- |
A linearly interpolated oscillator with sync status that allows changing the table number at k-rate.

oscilikts is the same as oscilikt. Except it has a sync input that can be used to re-initialize the oscillator to a k-rate phase value. It is slower than oscilikt and osciliktp.

> ares  oscilikts  xamp, xcps, kfn, async, kphs [, istor]

csound doc: <https://csound.com/docs/manual/oscilikts.html>
-}
oscilikts :: Sig -> Sig -> Tab -> Sig -> Sig -> Sig
oscilikts b1 b2 b3 b4 b5 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unTab b3 <*> unSig b4 <*> unSig b5
  where
    f a1 a2 a3 a4 a5 = opcs "oscilikts" [(Ar, [Xr, Xr, Kr, Ar, Kr, Ir])] [a1, a2, a3, a4, a5]

{- |
Accesses table values at a user-defined frequency.

Accesses table values at a user-defined frequency. This opcode can also be written as oscilx.

> ares  osciln  kamp, ifrq, ifn, itimes

csound doc: <https://csound.com/docs/manual/osciln.html>
-}
osciln :: Sig -> D -> Tab -> D -> Sig
osciln b1 b2 b3 b4 =
  Sig $ f <$> unSig b1 <*> unD b2 <*> unTab b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "osciln" [(Ar, [Kr, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
A simple, fast sine oscillator

Simple, fast sine oscillator, that uses only one multiply, and two add operations to generate one sample of output, and does not require a function table.

> ares  oscils  iamp, icps, iphs [, iflg]

csound doc: <https://csound.com/docs/manual/oscils.html>
-}
oscils :: D -> D -> D -> Sig
oscils b1 b2 b3 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "oscils" [(Ar, [Ir, Ir, Ir, Ir])] [a1, a2, a3]

{- |
High precision oscillator.

> ares  poscil  aamp, acps [, ifn, iphs]
> ares  poscil  aamp, kcps [, ifn, iphs]
> ares  poscil  kamp, acps [, ifn, iphs]
> ares  poscil  kamp, kcps [, ifn, iphs]
> ires  poscil  kamp, kcps [, ifn, iphs]
> kres  poscil  kamp, kcps [, ifn, iphs]

csound doc: <https://csound.com/docs/manual/poscil.html>
-}
poscil :: Sig -> Sig -> Tab -> Sig
poscil b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
  where
    f a1 a2 a3 =
      opcs
        "poscil"
        [ (Ar, [Ar, Ar, Ir, Ir])
        , (Ar, [Ar, Kr, Ir, Ir])
        , (Ar, [Kr, Ar, Ir, Ir])
        , (Ar, [Kr, Kr, Ir, Ir])
        , (Ir, [Kr, Kr, Ir, Ir])
        , (Kr, [Kr, Kr, Ir, Ir])
        ]
        [a1, a2, a3]

{- |
High precision oscillator with cubic interpolation.

> ares  poscil3  aamp, acps [, ifn, iphs]
> ares  poscil3  aamp, kcps [, ifn, iphs]
> ares  poscil3  kamp, acps [, ifn, iphs]
> ares  poscil3  kamp, kcps [, ifn, iphs]
> ires  poscil3  kamp, kcps [, ifn, iphs]
> kres  poscil3  kamp, kcps [, ifn, iphs]

csound doc: <https://csound.com/docs/manual/poscil3.html>
-}
poscil3 :: Sig -> Sig -> Tab -> Sig
poscil3 b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
  where
    f a1 a2 a3 =
      opcs
        "poscil3"
        [ (Ar, [Ar, Ar, Ir, Ir])
        , (Ar, [Ar, Kr, Ir, Ir])
        , (Ar, [Kr, Ar, Ir, Ir])
        , (Ar, [Kr, Kr, Ir, Ir])
        , (Ir, [Kr, Kr, Ir, Ir])
        , (Kr, [Kr, Kr, Ir, Ir])
        ]
        [a1, a2, a3]

{- |
Easier-to-use user-controllable vibrato.

> kout  vibr  kAverageAmp, kAverageFreq, ifn

csound doc: <https://csound.com/docs/manual/vibr.html>
-}
vibr :: Sig -> Sig -> Tab -> Sig
vibr b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
  where
    f a1 a2 a3 = opcs "vibr" [(Kr, [Kr, Kr, Ir])] [a1, a2, a3]

{- |
Generates a natural-sounding user-controllable vibrato.

> kout  vibrato  kAverageAmp, kAverageFreq, kRandAmountAmp, kRandAmountFreq, kAmpMinRate, kAmpMaxRate, kcpsMinRate, kcpsMaxRate, ifn [, iphs

csound doc: <https://csound.com/docs/manual/vibrato.html>
-}
vibrato :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig
vibrato b1 b2 b3 b4 b5 b6 b7 b8 b9 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7 <*> unSig b8 <*> unTab b9
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
      opcs
        "vibrato"
        [(Kr, [Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        ]

-- Dynamic Spectrum Oscillators.

{- |
Output is a set of harmonically related sine partials.

> ares  buzz  xamp, xcps, knh, ifn [, iphs]

csound doc: <https://csound.com/docs/manual/buzz.html>
-}
buzz :: Sig -> Sig -> Sig -> Tab -> Sig
buzz b1 b2 b3 b4 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unTab b4
  where
    f a1 a2 a3 a4 = opcs "buzz" [(Ar, [Xr, Xr, Kr, Ir, Ir])] [a1, a2, a3, a4]

{- |
Output is a set of harmonically related cosine partials.

> ares  gbuzz  xamp, xcps, knh, klh, kmul, ifn [, iphs]

csound doc: <https://csound.com/docs/manual/gbuzz.html>
-}
gbuzz :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig
gbuzz b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unTab b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "gbuzz" [(Ar, [Xr, Xr, Kr, Kr, Kr, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
Generates a set of impulses.

Generates a set of impulses of amplitude kamp separated by kintvl seconds (or samples if kintvl is negative).  The first impulse is generated after a delay of ioffset seconds.

> ares  mpulse  kamp, kintvl [, ioffset]

csound doc: <https://csound.com/docs/manual/mpulse.html>
-}
mpulse :: Sig -> Sig -> Sig
mpulse b1 b2 =
  Sig $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = opcs "mpulse" [(Ar, [Kr, Kr, Ir])] [a1, a2]

{- |
A mostly bandlimited shape-shifting square-pulse-saw-sinewave oscillator with hardsync.

This oscillator generates a variable shape waveform that can morph freely between classical shapes sine, square, pulse and saw.
The shape is controlled by two interacting values: clip (squareness) and "skew" (symmetry).
All shapes use a minimum number of samples per transition (ie, the sharp end of a saw or a pulse uses minimum N samples), this makes output bandlimited.
At higher frequency, the minimum sweep rate takes over, so over a certain pitch all shapes "degrade" to sinewave. The minimum sweep rate is i-time configurable.
Hardsync (a very quick sweep to phase=0) is supported, and a sync signal is output once per cycle.

> aout [, asyncout]  squinewave  acps, aClip, aSkew, asyncin [, iMinSweep] [, iphase]
> aout [, asyncout]  squinewave  acps, aClip, aSkew [, ksyncin] [, iMinSweep] [, iphase]

csound doc: <https://csound.com/docs/manual/squinewave.html>
-}
squinewave :: forall a. (Tuple a) => Sig -> Sig -> Sig -> Sig -> a
squinewave b1 b2 b3 b4 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
  where
    f a1 a2 a3 a4 = mopcs "squinewave" ([Ar, Ar], [Ar, Ar, Ar, Kr, Ir, Ir]) [a1, a2, a3, a4]

{- |
Implementation of a band limited, analog modeled oscillator.

Implementation of a band limited, analog modeled oscillator, based on integration of band limited impulses. vco can be used to simulate a variety of analog wave forms.

> ares  vco  xamp, xcps, iwave, kpw [, ifn] [, imaxd] [, ileak] [, inyx] \
>           [, iphs] [, iskip]

csound doc: <https://csound.com/docs/manual/vco.html>
-}
vco :: Sig -> Sig -> D -> Sig -> Sig
vco b1 b2 b3 b4 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unSig b4
  where
    f a1 a2 a3 a4 = opcs "vco" [(Ar, [Xr, Xr, Ir, Kr, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
Implementation of a band-limited oscillator using pre-calculated tables.

vco2 is similar to vco. But the implementation uses pre-calculated tables of band-limited waveforms (see also GEN30) rather than integrating impulses. This opcode can be faster than vco (especially if a low control-rate is used) and also allows better sound quality. Additionally, there are more waveforms and oscillator phase can be modulated at k-rate. The disadvantage is increased memory usage. For more details about vco2 tables, see also vco2init and vco2ft.

> ares  vco2  kamp, kcps [, imode] [, kpw] [, kphs] [, inyx]

csound doc: <https://csound.com/docs/manual/vco2.html>
-}
vco2 :: Sig -> Sig -> Sig
vco2 b1 b2 =
  Sig $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = opcs "vco2" [(Ar, [Kr, Kr, Ir, Kr, Kr, Ir])] [a1, a2]

{- |
Returns a table number at k-time for a given oscillator frequency and wavform.

vco2ft returns the function table number to be used for generating the specified waveform at a given frequency. This function table number can be used by any Csound opcode that generates a signal by reading function tables (like oscilikt). The tables must be calculated by vco2init before vco2ft is called and shared as Csound ftables (ibasfn).

> kfn  vco2ft  kcps, iwave [, inyx]

csound doc: <https://csound.com/docs/manual/vco2ft.html>
-}
vco2ft :: Sig -> D -> Tab
vco2ft b1 b2 =
  Tab $ f <$> unSig b1 <*> unD b2
  where
    f a1 a2 = opcs "vco2ft" [(Kr, [Kr, Ir, Ir])] [a1, a2]

{- |
Returns a table number at i-time for a given oscillator frequency and wavform.

vco2ift is the same as vco2ft, but works at i-time. It is suitable for use with opcodes that expect an i-rate table number (for example, oscili).

> ifn  vco2ift  icps, iwave [, inyx]

csound doc: <https://csound.com/docs/manual/vco2ift.html>
-}
vco2ift :: D -> D -> Tab
vco2ift b1 b2 =
  Tab $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "vco2ift" [(Ir, [Ir, Ir, Ir])] [a1, a2]

{- |
Calculates tables for use by vco2 opcode.

vco2init calculates tables for use by vco2 opcode. Optionally, it is also possible to access these tables as standard Csound function tables. In this case, vco2ft can be used to find the correct table number for a given oscillator frequency.

> ifn  vco2init  iwave [, ibasfn] [, ipmul] [, iminsiz] [, imaxsiz] [, isrcft]

csound doc: <https://csound.com/docs/manual/vco2init.html>
-}
vco2init :: D -> SE Tab
vco2init b1 =
  fmap (Tab . return) $ SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep "vco2init" [(Ir, [Ir, Ir, Ir, Ir, Ir, Ir])] [a1]

-- FM Synthesis.

{- |
Two mutually frequency and/or phase modulated oscillators.

Two oscillators, mutually frequency and/or phase modulated by each other.

> a1, a2  crossfm  xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]

csound doc: <https://csound.com/docs/manual/crossfm.html>
-}
crossfm :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> (Sig, Sig)
crossfm b1 b2 b3 b4 b5 b6 b7 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unTab b6 <*> unTab b7
  where
    f a1 a2 a3 a4 a5 a6 a7 =
      mopcs
        "crossfm"
        ([Ar, Ar], [Xr, Xr, Xr, Xr, Kr, Ir, Ir, Ir, Ir])
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        ]

{- |
Two mutually frequency and/or phase modulated oscillators.

Two oscillators, mutually frequency and/or phase modulated by each other.

> a1, a2  crossfmi  xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]

csound doc: <https://csound.com/docs/manual/crossfm.html>
-}
crossfmi :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> (Sig, Sig)
crossfmi b1 b2 b3 b4 b5 b6 b7 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unTab b6 <*> unTab b7
  where
    f a1 a2 a3 a4 a5 a6 a7 =
      mopcs
        "crossfmi"
        ([Ar, Ar], [Xr, Xr, Xr, Xr, Kr, Ir, Ir, Ir, Ir])
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        ]

{- |
Two mutually frequency and/or phase modulated oscillators.

Two oscillators, mutually frequency and/or phase modulated by each other.

> a1, a2  crosspm  xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]

csound doc: <https://csound.com/docs/manual/crossfm.html>
-}
crosspm :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> (Sig, Sig)
crosspm b1 b2 b3 b4 b5 b6 b7 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unTab b6 <*> unTab b7
  where
    f a1 a2 a3 a4 a5 a6 a7 =
      mopcs
        "crosspm"
        ([Ar, Ar], [Xr, Xr, Xr, Xr, Kr, Ir, Ir, Ir, Ir])
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        ]

{- |
Two mutually frequency and/or phase modulated oscillators.

Two oscillators, mutually frequency and/or phase modulated by each other.

> a1, a2  crosspmi  xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]

csound doc: <https://csound.com/docs/manual/crossfm.html>
-}
crosspmi :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> (Sig, Sig)
crosspmi b1 b2 b3 b4 b5 b6 b7 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unTab b6 <*> unTab b7
  where
    f a1 a2 a3 a4 a5 a6 a7 =
      mopcs
        "crosspmi"
        ([Ar, Ar], [Xr, Xr, Xr, Xr, Kr, Ir, Ir, Ir, Ir])
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        ]

{- |
Two mutually frequency and/or phase modulated oscillators.

Two oscillators, mutually frequency and/or phase modulated by each other.

> a1, a2  crossfmpm  xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]

csound doc: <https://csound.com/docs/manual/crossfm.html>
-}
crossfmpm :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> (Sig, Sig)
crossfmpm b1 b2 b3 b4 b5 b6 b7 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unTab b6 <*> unTab b7
  where
    f a1 a2 a3 a4 a5 a6 a7 =
      mopcs
        "crossfmpm"
        ([Ar, Ar], [Xr, Xr, Xr, Xr, Kr, Ir, Ir, Ir, Ir])
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        ]

{- |
Two mutually frequency and/or phase modulated oscillators.

Two oscillators, mutually frequency and/or phase modulated by each other.

> a1, a2  crossfmpmi  xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]

csound doc: <https://csound.com/docs/manual/crossfm.html>
-}
crossfmpmi :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> (Sig, Sig)
crossfmpmi b1 b2 b3 b4 b5 b6 b7 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unTab b6 <*> unTab b7
  where
    f a1 a2 a3 a4 a5 a6 a7 =
      mopcs
        "crossfmpmi"
        ([Ar, Ar], [Xr, Xr, Xr, Xr, Kr, Ir, Ir, Ir, Ir])
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        ]

{- |
Uses FM synthesis to create a Hammond B3 organ sound.

Uses FM synthesis to create a Hammond B3 organ sound. It comes from a family of FM sounds, all using 4 basic oscillators and various architectures, as used in the TX81Z synthesizer.

> ares  fmb3  kamp, kfreq, kc1, kc2, kvdepth, kvrate[, ifn1, ifn2, ifn3, \
>           ifn4, ivfn]

csound doc: <https://csound.com/docs/manual/fmb3.html>
-}
fmb3 :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
fmb3 b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "fmb3" [(Ar, [Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
Uses FM synthesis to create a tublar bell sound.

Uses FM synthesis to create a tublar bell sound. It comes from a family of FM sounds, all using 4 basic oscillators and various architectures, as used in the TX81Z synthesizer.

> ares  fmbell  kamp, kfreq, kc1, kc2, kvdepth, kvrate[, ifn1, ifn2, ifn3, \
>           ifn4, ivfn, isus]

csound doc: <https://csound.com/docs/manual/fmbell.html>
-}
fmbell :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
fmbell b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6
  where
    f a1 a2 a3 a4 a5 a6 =
      opcs
        "fmbell"
        [(Ar, [Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        ]

{- |
Uses FM synthesis to create a âHeavy Metalâ sound.

Uses FM synthesis to create a âHeavy Metalâ sound. It comes from a family of FM sounds, all using 4 basic oscillators and various architectures, as used in the TX81Z synthesizer.

> ares  fmmetal  kamp, kfreq, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, \
>           ifn4, ivfn

csound doc: <https://csound.com/docs/manual/fmmetal.html>
-}
fmmetal :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig
fmmetal b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unTab b7 <*> unTab b8 <*> unTab b9 <*> unTab b10 <*> unTab b11
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
      opcs
        "fmmetal"
        [
          ( Ar
          , [Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir]
          )
        ]
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11]

{- |
Uses FM synthesis to create a percussive flute sound.

Uses FM synthesis to create a percussive flute sound. It comes from a family of FM sounds, all using 4 basic oscillators and various architectures, as used in the TX81Z synthesizer.

> ares  fmpercfl  kamp, kfreq, kc1, kc2, kvdepth, kvrate[, ifn1, ifn2, \
>           ifn3, ifn4, ivfn]

csound doc: <https://csound.com/docs/manual/fmpercfl.html>
-}
fmpercfl :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
fmpercfl b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6
  where
    f a1 a2 a3 a4 a5 a6 =
      opcs
        "fmpercfl"
        [(Ar, [Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        ]

{- |
Uses FM synthesis to create a Fender Rhodes electric piano sound.

Uses FM synthesis to create a Fender Rhodes electric piano sound. It comes from a family of FM sounds, all using 4 basic oscillators and various architectures, as used in the TX81Z synthesizer.

> ares  fmrhode  kamp, kfreq, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, \
>           ifn3, ifn4, ivfn

csound doc: <https://csound.com/docs/manual/fmrhode.html>
-}
fmrhode :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig
fmrhode b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unTab b7 <*> unTab b8 <*> unTab b9 <*> unTab b10 <*> unTab b11
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
      opcs
        "fmrhode"
        [
          ( Ar
          , [Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir]
          )
        ]
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11]

{- |
FM Singing Voice Synthesis

> ares  fmvoice  kamp, kfreq, kvowel, ktilt, kvibamt, kvibrate[, ifn1, \
>           ifn2, ifn3, ifn4, ivibfn]

csound doc: <https://csound.com/docs/manual/fmvoice.html>
-}
fmvoice :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
fmvoice b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6
  where
    f a1 a2 a3 a4 a5 a6 =
      opcs
        "fmvoice"
        [(Ar, [Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        ]

{- |
Uses FM synthesis to create a Wurlitzer electric piano sound.

Uses FM synthesis to create a Wurlitzer electric piano sound. It comes from a family of FM sounds, all using 4 basic oscillators and various architectures, as used in the TX81Z synthesizer.

> ares  fmwurlie  kamp, kfreq, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, \
>           ifn4, ivfn

csound doc: <https://csound.com/docs/manual/fmwurlie.html>
-}
fmwurlie :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig
fmwurlie b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unTab b7 <*> unTab b8 <*> unTab b9 <*> unTab b10 <*> unTab b11
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
      opcs
        "fmwurlie"
        [
          ( Ar
          , [Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir]
          )
        ]
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11]

{- |
A basic frequency modulated oscillator.

> ares  foscil  xamp, kcps, xcar, xmod, kndx, ifn [, iphs]

csound doc: <https://csound.com/docs/manual/foscil.html>
-}
foscil :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig
foscil b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unTab b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "foscil" [(Ar, [Xr, Kr, Xr, Xr, Kr, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
Basic frequency modulated oscillator with linear interpolation.

> ares  foscili  xamp, kcps, xcar, xmod, kndx, ifn [, iphs]

csound doc: <https://csound.com/docs/manual/foscili.html>
-}
foscili :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig
foscili b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unTab b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "foscili" [(Ar, [Xr, Kr, Xr, Xr, Kr, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

-- Granular Synthesis.

{- |
Synchronous granular synthesis, using a soundfile as source.

diskgrain implements synchronous granular synthesis. The source sound for the grains is obtained by reading a soundfile containing the samples of the source waveform.

> asig  diskgrain  Sfname, kamp, kfreq, kpitch, kgrsize, kprate, \
>           ifun, iolaps [,imaxgrsize , ioffset]

csound doc: <https://csound.com/docs/manual/diskgrain.html>
-}
diskgrain :: Str -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> D -> Sig
diskgrain b1 b2 b3 b4 b5 b6 b7 b8 =
  Sig $ f <$> unStr b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unD b7 <*> unD b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 =
      opcs
        "diskgrain"
        [(Ar, [Sr, Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        ]

{- |
Produces sinusoid bursts useful for formant and granular synthesis.

Audio output is a succession of sinusoid bursts initiated at frequency xfund with a spectral peak at xform. For xfund above 25 Hz these bursts produce a speech-like formant with spectral characteristics determined by the k-input parameters. For lower fundamentals this generator provides a special form of granular synthesis.

> ares  fof  xamp, xfund, xform, koct, kband, kris, kdur, kdec, iolaps, \
>           ifna, ifnb, itotdur [, iphs] [, ifmode] [, iskip]

csound doc: <https://csound.com/docs/manual/fof.html>
-}
fof :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> Tab -> Tab -> D -> Sig
fof b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7 <*> unSig b8 <*> unD b9 <*> unTab b10 <*> unTab b11 <*> unD b12
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
      opcs
        "fof"
        [
          ( Ar
          , [Xr, Xr, Xr, Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir]
          )
        ]
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12]

{- |
Produces sinusoid bursts including k-rate incremental indexing with each successive burst.

Audio output is a succession of sinusoid bursts initiated at frequency xfund with a spectral peak at xform. For xfund above 25 Hz these bursts produce a speech-like formant with spectral characteristics determined by the k-input parameters. For lower fundamentals this generator provides a special form of granular synthesis.

> ares  fof2  xamp, xfund, xform, koct, kband, kris, kdur, kdec, iolaps, \
>           ifna, ifnb, itotdur, kphs, kgliss [, iskip]

csound doc: <https://csound.com/docs/manual/fof2.html>
-}
fof2 :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> Tab -> Tab -> D -> Sig -> Sig -> Sig
fof2 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7 <*> unSig b8 <*> unD b9 <*> unTab b10 <*> unTab b11 <*> unD b12 <*> unSig b13 <*> unSig b14
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 =
      opcs
        "fof2"
        [
          ( Ar
          , [Xr, Xr, Xr, Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir, Kr, Kr, Ir]
          )
        ]
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14]

{- |
Audio output is a succession of grains derived from data in a stored function table

Audio output is a succession of grains derived from data in a stored function table ifna. The local envelope of these grains and their timing is based on the model of fof synthesis and permits detailed control of the granular synthesis.

> ares  fog  xamp, xdens, xtrans, aspd, koct, kband, kris, kdur, kdec, \
>           iolaps, ifna, ifnb, itotdur [, iphs] [, itmode] [, iskip]

csound doc: <https://csound.com/docs/manual/fog.html>
-}
fog :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> Tab -> Tab -> D -> Sig
fog b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7 <*> unSig b8 <*> unSig b9 <*> unD b10 <*> unTab b11 <*> unTab b12 <*> unD b13
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 =
      opcs
        "fog"
        [
          ( Ar
          , [Xr, Xr, Xr, Ar, Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir]
          )
        ]
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13]

{- |
Generates granular synthesis textures.

> ares  grain  xamp, xpitch, xdens, kampoff, kpitchoff, kgdur, igfn, \
>           iwfn, imgdur [, igrnd]

csound doc: <https://csound.com/docs/manual/grain.html>
-}
grain :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> D -> Sig
grain b1 b2 b3 b4 b5 b6 b7 b8 b9 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unTab b7 <*> unTab b8 <*> unD b9
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
      opcs
        "grain"
        [(Ar, [Xr, Xr, Xr, Kr, Kr, Kr, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        ]

{- |
Easy-to-use granular synthesis texture generator.

Generate granular synthesis textures. grain2 is simpler to use, but grain3 offers more control.

> ares  grain2  kcps, kfmd, kgdur, iovrlp, kfn, iwfn [, irpow] \
>           [, iseed] [, imode]

csound doc: <https://csound.com/docs/manual/grain2.html>
-}
grain2 :: Sig -> Sig -> Sig -> D -> Tab -> Tab -> Sig
grain2 b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unTab b5 <*> unTab b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "grain2" [(Ar, [Kr, Kr, Kr, Ir, Kr, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
Generate granular synthesis textures with more user control.

Generate granular synthesis textures. grain2 is simpler to use but grain3 offers more control.

> ares  grain3  kcps, kphs, kfmd, kpmd, kgdur, kdens, imaxovr, kfn, iwfn, \
>           kfrpow, kprpow [, iseed] [, imode]

csound doc: <https://csound.com/docs/manual/grain3.html>
-}
grain3 :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> Tab -> Tab -> Sig -> Sig -> Sig
grain3 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unD b7 <*> unTab b8 <*> unTab b9 <*> unSig b10 <*> unSig b11
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
      opcs
        "grain3"
        [
          ( Ar
          , [Kr, Kr, Kr, Kr, Kr, Kr, Ir, Kr, Ir, Kr, Kr, Ir, Ir]
          )
        ]
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11]

{- |
A more complex granular synthesis texture generator.

The granule unit generator is more complex than grain, but does add new possibilities.

> ares  granule  xamp, ivoice, iratio, imode, ithd, ifn, ipshift, igskip, igskip_os, ilength, kgap, igap_os, kgsize, igsize_os, iatt, idec \
>           [, iseed] [, ipitch1] [, ipitch2] [, ipitch3] [, ipitch4] [, ifnenv]

csound doc: <https://csound.com/docs/manual/granule.html>
-}
granule :: Sig -> D -> D -> D -> D -> Tab -> D -> D -> D -> D -> Sig -> D -> Sig -> D -> D -> D -> Sig
granule b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 =
  Sig $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unTab b6 <*> unD b7 <*> unD b8 <*> unD b9 <*> unD b10 <*> unSig b11 <*> unD b12 <*> unSig b13 <*> unD b14 <*> unD b15 <*> unD b16
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 =
      opcs
        "granule"
        [
          ( Ar
          , [Xr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Kr, Ir, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir]
          )
        ]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        ]

{- |
Granular synthesizer with "per grain" control
      over many of its parameters.  Has a sync input to
      sychronize its internal grain scheduler clock to an external
      clock source.

partikkel was conceived after reading Curtis Roads' book
      "Microsound", and the goal was to create an opcode that was
      capable of all time-domain varieties of granular synthesis
      described in this book. The idea being that most of the
      techniques only differ in parameter values, and by having a
      single opcode that can do all varieties of granular synthesis
      makes it possible to interpolate between techniques. Granular synthesis is sometimes dubbed particle
      synthesis, and it was thought apt to name the opcode partikkel
      to distinguish it from other granular opcodes.

> a1 [, a2, a3, a4, a5, a6, a7, a8]  partikkel  agrainfreq, \
>                   kdistribution, idisttab, async, kenv2amt, ienv2tab, ienv_attack, \
>                   ienv_decay, ksustain_amount, ka_d_ratio, kduration, kamp, igainmasks, \
>                   kwavfreq, ksweepshape, iwavfreqstarttab, iwavfreqendtab, awavfm, \
>                   ifmamptab, kfmenv, icosine, ktraincps, knumpartials, kchroma, \
>                   ichannelmasks, krandommask, kwaveform1, kwaveform2, kwaveform3, \
>                   kwaveform4, iwaveamptab, asamplepos1, asamplepos2, asamplepos3, \
>                   asamplepos4, kwavekey1, kwavekey2, kwavekey3, kwavekey4, imax_grains \
>                   [, iopcode_id, ipanlaws]

csound doc: <https://csound.com/docs/manual/partikkel.html>
-}
partikkel :: forall a. (Tuple a) => Sig -> Sig -> D -> Sig -> Sig -> D -> D -> D -> Sig -> Sig -> Sig -> Sig -> D -> Sig -> Sig -> D -> D -> Sig -> D -> Sig -> D -> Sig -> Sig -> Sig -> D -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> a
partikkel b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31 b32 b33 b34 b35 b36 b37 b38 b39 b40 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unSig b4 <*> unSig b5 <*> unD b6 <*> unD b7 <*> unD b8 <*> unSig b9 <*> unSig b10 <*> unSig b11 <*> unSig b12 <*> unD b13 <*> unSig b14 <*> unSig b15 <*> unD b16 <*> unD b17 <*> unSig b18 <*> unD b19 <*> unSig b20 <*> unD b21 <*> unSig b22 <*> unSig b23 <*> unSig b24 <*> unD b25 <*> unSig b26 <*> unSig b27 <*> unSig b28 <*> unSig b29 <*> unSig b30 <*> unD b31 <*> unSig b32 <*> unSig b33 <*> unSig b34 <*> unSig b35 <*> unSig b36 <*> unSig b37 <*> unSig b38 <*> unSig b39 <*> unD b40
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 =
      mopcs
        "partikkel"
        (
          [ Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          , Ar
          ]
        ,
          [ Ar
          , Kr
          , Ir
          , Ar
          , Kr
          , Ir
          , Ir
          , Ir
          , Kr
          , Kr
          , Kr
          , Kr
          , Ir
          , Kr
          , Kr
          , Ir
          , Ir
          , Ar
          , Ir
          , Kr
          , Ir
          , Kr
          , Kr
          , Kr
          , Ir
          , Kr
          , Kr
          , Kr
          , Kr
          , Kr
          , Ir
          , Ar
          , Ar
          , Ar
          , Ar
          , Kr
          , Kr
          , Kr
          , Kr
          , Ir
          , Ir
          , Ir
          ]
        )
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        , a19
        , a20
        , a21
        , a22
        , a23
        , a24
        , a25
        , a26
        , a27
        , a28
        , a29
        , a30
        , a31
        , a32
        , a33
        , a34
        , a35
        , a36
        , a37
        , a38
        , a39
        , a40
        ]

{- |
Get mask index for a specific mask parameter of a running partikkel instance.

partikkelget is an opcode for outputting partikkel mask index for a specific parameter.
      Used together with partikkelset, it can be used to synchronize partikkel masking between several running instances of the  partikkel opcode.
      It can also be used to control other processes based on the internal mask index, for example to create more complex masking patterns than is available with the regular grain masking system.

> kindex  partikkelget  kparameterindex, iopcode_id

csound doc: <https://csound.com/docs/manual/partikkelget.html>
-}
partikkelget :: Sig -> D -> Sig
partikkelget b1 b2 =
  Sig $ f <$> unSig b1 <*> unD b2
  where
    f a1 a2 = opcs "partikkelget" [(Kr, [Kr, Ir])] [a1, a2]

{- |
Set mask index for a specific mask parameter of a running partikkel instance.

partikkelset is an opcode for setting the partikkel mask index for a specific parameter.
      Used together with partikkelget, it can be used to synchronize partikkel masking between several running instances of the  partikkel opcode.
      It can also be used to set the internal mask index basaed on other processes, for example to create more complex masking patterns than is available with the regular grain masking system.

>  partikkelset  kparameterindex, kmaskindex, iopcode_id

csound doc: <https://csound.com/docs/manual/partikkelset.html>
-}
partikkelset :: Sig -> Sig -> D -> SE ()
partikkelset b1 b2 b3 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "partikkelset" [(Xr, [Kr, Kr, Ir])] [a1, a2, a3]

{- |
Outputs partikkel's grain
      scheduler clock pulse and phase to synchronize several instances of the partikkel
      opcode to the same clock source.

partikkelsync is an opcode for outputting partikkel's grain scheduler clock pulse and phase. partikkelsync's output can be used to synchronize other instances of the partikkel opcode to the same clock.

> async [,aphase]  partikkelsync  iopcode_id

csound doc: <https://csound.com/docs/manual/partikkelsync.html>
-}
partikkelsync :: forall a. (Tuple a) => D -> a
partikkelsync b1 =
  pureTuple $ f <$> unD b1
  where
    f a1 = mopcs "partikkelsync" ([Ar, Ar], [Ir]) [a1]

{- |
Reads a mono sound sample from a table and applies time-stretching and/or pitch modification.

sndwarp reads sound samples from a table and applies time-stretching and/or pitch modification. Time and frequency modification are independent from one another. For example, a sound can be stretched in time while raising the pitch!

> ares [, ac]  sndwarp  xamp, xtimewarp, xresample, ifn1, ibeg, iwsize, \
>           irandw, ioverlap, ifn2, itimemode

csound doc: <https://csound.com/docs/manual/sndwarp.html>
-}
sndwarp :: forall a. (Tuple a) => Sig -> Sig -> Sig -> Tab -> D -> D -> D -> D -> Tab -> D -> a
sndwarp b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unTab b4 <*> unD b5 <*> unD b6 <*> unD b7 <*> unD b8 <*> unTab b9 <*> unD b10
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
      mopcs
        "sndwarp"
        ([Ar, Ar], [Xr, Xr, Xr, Ir, Ir, Ir, Ir, Ir, Ir, Ir])
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        ]

{- |
Reads a stereo sound sample from a table and applies time-stretching and/or pitch modification.

sndwarpst reads stereo sound samples from a table and applies time-stretching and/or pitch modification. Time and frequency modification are independent from one another. For example, a sound can be stretched in time while raising the pitch!

> ar1, ar2 [,ac1] [, ac2]  sndwarpst  xamp, xtimewarp, xresample, ifn1, \
>           ibeg, iwsize, irandw, ioverlap, ifn2, itimemode

csound doc: <https://csound.com/docs/manual/sndwarpst.html>
-}
sndwarpst :: forall a. (Tuple a) => Sig -> Sig -> Sig -> Tab -> D -> D -> D -> D -> Tab -> D -> a
sndwarpst b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unTab b4 <*> unD b5 <*> unD b6 <*> unD b7 <*> unD b8 <*> unTab b9 <*> unD b10
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
      mopcs
        "sndwarpst"
        ( [Ar, Ar, Ar, Ar]
        , [Xr, Xr, Xr, Ir, Ir, Ir, Ir, Ir, Ir, Ir]
        )
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10]

{- |
Synchronous granular synthesis.

syncgrain implements synchronous granular synthesis. The source sound for the
grains is obtained by reading a function table containing the samples of the source waveform.
For sampled-sound sources, GEN01 is used.
syncgrain will accept deferred allocation tables.

> asig  syncgrain  kamp, kfreq, kpitch, kgrsize, kprate, ifun1, \
>           ifun2, iolaps

csound doc: <https://csound.com/docs/manual/syncgrain.html>
-}
syncgrain :: Sig -> Sig -> Sig -> Sig -> Sig -> D -> D -> D -> Sig
syncgrain b1 b2 b3 b4 b5 b6 b7 b8 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unD b6 <*> unD b7 <*> unD b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 =
      opcs
        "syncgrain"
        [(Ar, [Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        ]

{- |
Synchronous granular synthesis.

syncloop is a variation on syncgrain,
which implements synchronous granular synthesis.
syncloop adds loop start and end points and an optional start position. Loop start
and end control grain start positions, so the actual grains can go beyond the loop
points (if the loop points are not at the extremes of the table), enabling
seamless crossfading. For more information on the granular synthesis process,
check the syncgrain manual page.

> asig  syncloop  kamp, kfreq, kpitch, kgrsize, kprate, klstart, \
>           klend, ifun1, ifun2, iolaps[,istart, iskip]

csound doc: <https://csound.com/docs/manual/syncloop.html>
-}
syncloop :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> D -> D -> Sig
syncloop b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7 <*> unD b8 <*> unD b9 <*> unD b10
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
      opcs
        "syncloop"
        [
          ( Ar
          , [Kr, Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir]
          )
        ]
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10]

{- |
Simple vocal simulation based on glottal pulses with formant characteristics.

This opcode produces a simple vocal simulation based on glottal pulses with formant characteristics.
Output is a series of sound events, where each event is composed of a burst of squared sine pulses followed by silence.
The VOSIM (VOcal SIMulation) synthesis method was developed by Kaegi and Tempelaars in the 1970's.

> ar  vosim  kamp, kFund, kForm, kDecay, kPulseCount, kPulseFactor, ifn [, iskip]

csound doc: <https://csound.com/docs/manual/vosim.html>
-}
vosim :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig
vosim b1 b2 b3 b4 b5 b6 b7 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unTab b7
  where
    f a1 a2 a3 a4 a5 a6 a7 = opcs "vosim" [(Ar, [Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir])] [a1, a2, a3, a4, a5, a6, a7]

-- Hyper Vectorial Synthesis.

{- |
Allows one-dimensional Hyper Vectorial Synthesis (HVS) controlled by externally-updated k-variables.

hvs1 allows one-dimensional Hyper Vectorial Synthesis (HVS) controlled by externally-updated k-variables.

>  hvs1  kx, inumParms, inumPointsX, iOutTab, iPositionsTab, iSnapTab [, iConfigTab]

csound doc: <https://csound.com/docs/manual/hvs1.html>
-}
hvs1 :: Sig -> D -> D -> D -> D -> D -> SE ()
hvs1 b1 b2 b3 b4 b5 b6 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6
  where
    f a1 a2 a3 a4 a5 a6 = opcsDep_ "hvs1" [(Xr, [Kr, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
Allows two-dimensional Hyper Vectorial Synthesis (HVS) controlled by externally-updated k-variables.

hvs2 allows two-dimensional Hyper Vectorial Synthesis (HVS) controlled by externally-updated k-variables.

>  hvs2  kx, ky, inumParms, inumPointsX, inumPointsY, iOutTab, iPositionsTab, iSnapTab [, iConfigTab]

csound doc: <https://csound.com/docs/manual/hvs2.html>
-}
hvs2 :: Sig -> Sig -> D -> D -> D -> D -> D -> D -> SE ()
hvs2 b1 b2 b3 b4 b5 b6 b7 b8 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6 <*> (lift . unD) b7 <*> (lift . unD) b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 =
      opcsDep_
        "hvs2"
        [(Xr, [Kr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        ]

{- |
Allows three-dimensional Hyper Vectorial Synthesis (HVS) controlled by externally-updated k-variables.

hvs3 allows three-dimensional Hyper Vectorial Synthesis (HVS) controlled by externally-updated k-variables.

>  hvs3  kx, ky, kz, inumParms, inumPointsX, inumPointsY, inumPointsZ, iOutTab, iPositionsTab, iSnapTab [, iConfigTab]

csound doc: <https://csound.com/docs/manual/hvs3.html>
-}
hvs3 :: Sig -> Sig -> Sig -> D -> D -> D -> D -> D -> D -> D -> SE ()
hvs3 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6 <*> (lift . unD) b7 <*> (lift . unD) b8 <*> (lift . unD) b9 <*> (lift . unD) b10
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
      opcsDep_
        "hvs3"
        [(Xr, [Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        ]

-- Linear and Exponential Generators.

{- |
Break point function with linear interpolation

Break-point function with linear interpolation. Useful when
	  defining a table with GEN27 and scaling the x value would be
	  overkill.

> ky  bpf  kx, kx1, ky1, kx2, ..., kxn, kyn
> iy  bpf  ix, ix1, iy1, ix2, ..., ixn, iyn
> kys[]  bpf  kxs[], kx1, ky1, kx2, ..., kxn, kyn
> iys[]  bpf  ixs[], ix1, iy1, ix2, ..., ixn, iyn
> ky  bpf  kx, kxs[], kys[]
> iy  bpf  ix, ixs[], iys[]
> ay  bpf  ax, kx1, ky1, kx2, ..., kxn, kyn
> ay  bpf  ax, kxs[], kys[]
> ky, kw  bpf  kx, kxs[], kys[], kws[]

csound doc: <https://csound.com/docs/manual/bpf.html>
-}
bpf :: Sig -> Sig -> Sig -> [Sig] -> (Sig, Sig)
bpf b1 b2 b3 b4 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> mapM unSig b4
  where
    f a1 a2 a3 a4 = mopcs "bpf" ([Kr, Kr], [Kr, Kr, Kr, Kr]) ([a1, a2, a3] ++ a4)

--
-- > ky  bpfcos  kx, kx1, ky1, kx2, ..., kxn, kyn
-- > kys[]  bpfcos  kxs[], kx1, ky1, kx2, ..., kxn, kyn
-- > ky  bpfcos  kx, kxs[], kys[]
-- > ky  bpfcos  kx, ixs[], iys[]
-- > ky, kz  bpfcos  kx, kxs[], kys[], kzs[]
-- > ky, kz  bpfcos  kx, ixs[], iys[], izs[]
-- > kys[]  bpfcos  kxs[], kx1, ky1, kx2, ..., kxn, kyn
-- > ky  bpfcos  kx, ixs[], iys[]
-- > ky, kz  bpfcos  kx, kxs[], kys[], kzs[]
--
-- csound doc: <https://csound.com/docs/manual/bpfcos.html>
bpfcos :: Sig -> Sig -> Sig -> [Sig] -> (Sig, Sig)
bpfcos b1 b2 b3 b4 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> mapM unSig b4
  where
    f a1 a2 a3 a4 = mopcs "bpfcos" ([Kr, Kr], [Kr, Kr, Kr, Kr]) ([a1, a2, a3] ++ a4)

{- |
Trace a series of line segments between specified points with
      cosine interpolation.

> ares  cosseg  ia, idur1, ib [, idur2] [, ic] [...]
> kres  cosseg  ia, idur1, ib [, idur2] [, ic] [...]

csound doc: <https://csound.com/docs/manual/cosseg.html>
-}
cosseg :: [D] -> Sig
cosseg b1 =
  Sig $ f <$> mapM unD b1
  where
    f a1 = setRate Kr $ opcs "cosseg" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, last a1])

{- |
Trace a series of line segments between specified absolute points with
      cosine interpolation.

> ares  cossegb  ia, itim1, ib [, itim2] [, ic] [...]
> kres  cossegb  ia, itim1, ib [, itim2] [, ic] [...]

csound doc: <https://csound.com/docs/manual/cossegb.html>
-}
cossegb :: [D] -> Sig
cossegb b1 =
  Sig $ f <$> mapM unD b1
  where
    f a1 = setRate Kr $ opcs "cossegb" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, last a1])

{- |
Trace a series of line segments between specified points with
      cosine interpolation, including a release segment.

> ares  cossegr  ia, idur1, ib [, idur2] [, ic] [...], irel, iz
> kres  cossegr  ia, idur1, ib [, idur2] [, ic] [...], irel, iz

csound doc: <https://csound.com/docs/manual/cossegr.html>
-}
cossegr :: [D] -> D -> D -> Sig
cossegr b1 b2 b3 =
  Sig $ f <$> mapM unD b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = setRate Kr $ opcs "cossegr" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, last a1, a2, a3])

{- |
This opcode implements a formula for generating a normalised exponential curve in range 0 - 1. It is based on the Max / MSP work of Eric Singer (c) 1994.

Generates an exponential curve in range 0 to 1 of arbitrary steepness.
      Steepness index equal to or lower than 1.0 will result in Not-a-Number
      errors and cause unstable behavior.

> kout  expcurve  kindex, ksteepness

csound doc: <https://csound.com/docs/manual/expcurve.html>
-}
expcurve :: Sig -> Sig -> Sig
expcurve b1 b2 =
  Sig $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = opcs "expcurve" [(Kr, [Kr, Kr])] [a1, a2]

{- |
Trace an exponential curve between specified points.

> ares  expon  ia, idur, ib
> kres  expon  ia, idur, ib

csound doc: <https://csound.com/docs/manual/expon.html>
-}
expon :: D -> D -> D -> Sig
expon b1 b2 b3 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "expon" [(Ar, [Ir, Ir, Ir]), (Kr, [Ir, Ir, Ir])] [a1, a2, a3]

{- |
Trace a series of exponential segments between specified points.

> ares  expseg  ia, idur1, ib [, idur2] [, ic] [...]
> kres  expseg  ia, idur1, ib [, idur2] [, ic] [...]

csound doc: <https://csound.com/docs/manual/expseg.html>
-}
expseg :: [D] -> Sig
expseg b1 =
  Sig $ f <$> mapM unD b1
  where
    f a1 = setRate Kr $ opcs "expseg" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, last a1])

{- |
An exponential segment generator operating at a-rate.

An exponential segment generator operating at a-rate. This unit is almost identical to expseg, but more precise when defining segments with very short durations (i.e., in a percussive attack phase) at audio rate.

> ares  expsega  ia, idur1, ib [, idur2] [, ic] [...]

csound doc: <https://csound.com/docs/manual/expsega.html>
-}
expsega :: [D] -> Sig
expsega b1 =
  Sig $ f <$> mapM unD b1
  where
    f a1 = opcs "expsega" [(Ar, repeat Ir)] (a1 ++ [1, last a1])

{- |
Trace a series of exponential segments between specified
      absolute points.

> ares  expsegb  ia, itim1, ib [, itim2] [, ic] [...]
> kres  expsegb  ia, itim1, ib [, itim2] [, ic] [...]

csound doc: <https://csound.com/docs/manual/expsegb.html>
-}
expsegb :: [D] -> Sig
expsegb b1 =
  Sig $ f <$> mapM unD b1
  where
    f a1 = opcs "expsegb" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, last a1])

{- |
An exponential segment generator operating at a-rate with
      absolute times.

An exponential segment generator operating at a-rate. This unit
      is almost identical to expsegb, but
      more precise when defining segments with very short durations
      (i.e., in a percussive attack phase) at audio rate.

> ares  expsegba  ia, itim1, ib [, itim2] [, ic] [...]

csound doc: <https://csound.com/docs/manual/expsegba.html>
-}
expsegba :: D -> D -> D -> Sig
expsegba b1 b2 b3 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "expsegba" [(Ar, (repeat Ir))] [a1, a2, a3]

{- |
Trace a series of exponential segments between specified points including a release segment.

> ares  expsegr  ia, idur1, ib [, idur2] [, ic] [...], irel, iz
> kres  expsegr  ia, idur1, ib [, idur2] [, ic] [...], irel, iz

csound doc: <https://csound.com/docs/manual/expsegr.html>
-}
expsegr :: [D] -> D -> D -> Sig
expsegr b1 b2 b3 =
  Sig $ f <$> mapM unD b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = setRate Kr $ opcs "expsegr" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, last a1, a2, a3])

{- |
An implementation of a logarithmic gain curve which is similar to the gainslider~ object from Cycling 74 Max / MSP.

This opcode is intended for use to multiply by an audio signal to give a console mixer like feel. There is no bounds in the
      source code so you can for example give higher than 127 values for extra amplitude but possibly clipped audio.

> kout  gainslider  kindex

csound doc: <https://csound.com/docs/manual/gainslider.html>
-}
gainslider :: Sig -> Sig
gainslider b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "gainslider" [(Kr, [Kr])] [a1]

--
-- > ky  lincos  kx, ky0, ky1 [, kx0, kx1 ]
-- > iy  lincos  ix, iy0, iy1 [, ix0, ix1 ]
--
-- csound doc: <https://csound.com/docs/manual/lincos.html>
lincos :: Sig -> Sig -> Sig -> D
lincos b1 b2 b3 =
  D $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "lincos" [(Kr, [Kr, Kr, Kr, Kr, Kr]), (Ir, [Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3]

{- |
Trace a straight line between specified points.

> ares  line  ia, idur, ib
> kres  line  ia, idur, ib

csound doc: <https://csound.com/docs/manual/line.html>
-}
line :: D -> D -> D -> Sig
line b1 b2 b3 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "line" [(Ar, [Ir, Ir, Ir]), (Kr, [Ir, Ir, Ir])] [a1, a2, a3]

{- |
Linear to linear interpolation

Maps a linear range of values to another linear range of values.

> ky  linlin  kx, ky0, ky1 [, kx0, kx1 ]
> iy  linlin  ix, iy0, iy1 [, ix0, ix1 ]
> kys[]  linlin  kxs[], ky0, ky1 [, kx0, kx1 ]
> iys[]  linlin  ixs[], ky0, ky1, [ kx0, kx1 ]
> kC[]  linlin  kx, kA[], kB[] [, kx0, kx1 ]

csound doc: <https://csound.com/docs/manual/linlin.html>
-}
linlin :: Sig -> Sig -> Sig -> Sig
linlin b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 =
      opcs
        "linlin"
        [ (Kr, [Kr, Kr, Kr, Kr, Kr])
        , (Ir, [Ir, Ir, Ir, Ir, Ir])
        , (Kr, [Kr, Kr, Kr, Kr, Kr])
        , (Ir, [Ir, Kr, Kr, Kr, Kr])
        , (Kr, [Kr, Kr, Kr, Kr, Kr])
        ]
        [a1, a2, a3]

{- |
Trace a series of line segments between specified points.

> ares  linseg  ia, idur1, ib [, idur2] [, ic] [...]
> kres  linseg  ia, idur1, ib [, idur2] [, ic] [...]

csound doc: <https://csound.com/docs/manual/linseg.html>
-}
linseg :: [D] -> Sig
linseg b1 =
  Sig $ f <$> mapM unD b1
  where
    f a1 = setRate Kr $ opcs "linseg" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, last a1])

{- |
Trace a series of line segments between specified absolute points.

> ares  linsegb  ia, itim1, ib [, itim2] [, ic] [...]
> kres  linsegb  ia, itim1, ib [, itim2] [, ic] [...]

csound doc: <https://csound.com/docs/manual/linsegb.html>
-}
linsegb :: [D] -> Sig
linsegb b1 =
  Sig $ f <$> mapM unD b1
  where
    f a1 = setRate Kr $ opcs "linsegb" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, last a1])

{- |
Trace a series of line segments between specified points including a release segment.

> ares  linsegr  ia, idur1, ib [, idur2] [, ic] [...], irel, iz
> kres  linsegr  ia, idur1, ib [, idur2] [, ic] [...], irel, iz

csound doc: <https://csound.com/docs/manual/linsegr.html>
-}
linsegr :: [D] -> D -> D -> Sig
linsegr b1 b2 b3 =
  Sig $ f <$> mapM unD b1 <*> unD b2 <*> unD b3
  where
    f a1 a2 a3 = setRate Kr $ opcs "linsegr" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, last a1, a2, a3])

{- |
This opcode implements a formula for generating a normalised logarithmic curve in range 0 - 1. It is based on the Max / MSP work of Eric Singer (c) 1994.

Generates a logarithmic curve in range 0 to 1 of arbitrary steepness.
      Steepness index equal to or lower than 1.0 will result in Not-a-Number
      errors and cause unstable behavior.

> kout  logcurve  kindex, ksteepness

csound doc: <https://csound.com/docs/manual/logcurve.html>
-}
logcurve :: Sig -> Sig -> Sig
logcurve b1 b2 =
  Sig $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = opcs "logcurve" [(Kr, [Kr, Kr])] [a1, a2]

{- |
Generate control signal consisting of linear segments delimited by two or more specified points.

Generate control signal consisting of linear segments delimited by two or more specified points. The entire envelope is looped at kfreq rate. Each parameter can be varied at k-rate.

> ksig  loopseg  kfreq, ktrig, iphase, kvalue0, ktime0 [, kvalue1] [, ktime1] \
>     [, kvalue2] [, ktime2][...]

csound doc: <https://csound.com/docs/manual/loopseg.html>
-}
loopseg :: Sig -> Sig -> D -> [Sig] -> Sig
loopseg b1 b2 b3 b4 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> mapM unSig b4
  where
    f a1 a2 a3 a4 = opcs "loopseg" [(Kr, [Kr, Kr, Ir] ++ (repeat Kr))] ([a1, a2, a3] ++ a4)

{- |
Control signals based on linear segments.

Generate control signal consisiting of linear segments delimited
    by two or more specified points. The entire envelope can be looped
    at time-variant rate. Each segment coordinate can also be varied
    at k-rate.

> ksig  loopsegp   kphase, kvalue0, kdur0, kvalue1 \
>           [, kdur1, ... , kdurN-1, kvalueN]

csound doc: <https://csound.com/docs/manual/loopsegp.html>
-}
loopsegp :: Sig -> [Sig] -> Sig
loopsegp b1 b2 =
  Sig $ f <$> unSig b1 <*> mapM unSig b2
  where
    f a1 a2 = opcs "loopsegp" [(Kr, (repeat Kr))] ([a1] ++ a2)

{- |
Generate control signal consisting of exponential or linear segments delimited by two or more specified points.

Generate control signal consisting of controllable exponential segments or linear segments delimited by two or more specified points. The entire envelope is looped at kfreq rate. Each parameter can be varied at k-rate.

> ksig  looptseg  kfreq, ktrig, iphase, kvalue0, ktype0, ktime0, [, kvalue1] [,ktype1] [, ktime1] \
>           [, kvalue2] [,ktype2] [, ktime2] [...] [, kvalueN] [,ktypeN] [, ktimeN]

csound doc: <https://csound.com/docs/manual/looptseg.html>
-}
looptseg :: Sig -> Sig -> [Sig] -> Sig
looptseg b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> mapM unSig b3
  where
    f a1 a2 a3 = opcs "looptseg" [(Kr, [Kr, Kr, Ir] ++ (repeat Kr))] ([a1, a2] ++ a3)

{- |
Generate control signal consisting of exponential segments delimited by two or more specified points.

Generate control signal consisting of exponential segments delimited by two or more specified points. The entire envelope is looped at kfreq rate. Each parameter can be varied at k-rate.

> ksig  loopxseg  kfreq, ktrig, iphase, kvalue0, ktime0  [, kvalue1] [, ktime1] \
>           [, kvalue2] [, ktime2] [...]

csound doc: <https://csound.com/docs/manual/loopxseg.html>
-}
loopxseg :: Sig -> Sig -> D -> [Sig] -> Sig
loopxseg b1 b2 b3 b4 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> mapM unSig b4
  where
    f a1 a2 a3 a4 = opcs "loopxseg" [(Kr, [Kr, Kr, Ir] ++ (repeat Kr))] ([a1, a2, a3] ++ a4)

{- |
Generate control signal consisting of held segments.

Generate control signal consisting of held segments delimited by two or more specified points. The entire envelope is looped at kfreq rate. Each parameter can be varied at k-rate.

> ksig  lpshold  kfreq, ktrig, iphase, kvalue0, ktime0  [, kvalue1] [, ktime1] [, kvalue2] [, ktime2] [...]

csound doc: <https://csound.com/docs/manual/lpshold.html>
-}
lpshold :: Sig -> Sig -> D -> [Sig] -> Sig
lpshold b1 b2 b3 b4 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> mapM unSig b4
  where
    f a1 a2 a3 a4 = opcs "lpshold" [(Kr, [Kr, Kr, Ir] ++ (repeat Kr))] ([a1, a2, a3] ++ a4)

{- |
Control signals based on held segments.

Generate control signal consisiting of held segments delimited
    by two or more specified points. The entire envelope can be looped
    at time-variant rate. Each segment coordinate can also be varied
    at k-rate.

> ksig  lpsholdp   kphase, kvalue0, ktime0  [, kvalue1] [, ktime1] \
>           [, kvalue2] [, ktime2] [...]

csound doc: <https://csound.com/docs/manual/lpsholdp.html>
-}
lpsholdp :: Sig -> Sig -> [Sig] -> Sig
lpsholdp b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> mapM unSig b3
  where
    f a1 a2 a3 = opcs "lpsholdp" [(Kr, (repeat Kr))] ([a1, a2] ++ a3)

{- |
Arbitrary signal scaling.

Scales incoming value to user-definable range. Similar to scale object found in popular dataflow languages.

> kscl  scale  kinput, kmax, kmin[, kimax, kimin]

csound doc: <https://csound.com/docs/manual/scale.html>
-}
scale :: Sig -> Sig -> Sig -> Sig
scale b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "scale" [(Kr, [Kr, Kr, Kr, Kr, Kr])] [a1, a2, a3]

--
-- > kscl  scale2  kinput, kmin, kmax[, kimin, kimax][ihtime]
--
-- csound doc: <https://csound.com/docs/manual/scale2.html>
scale2 :: Sig -> Sig -> Sig -> Sig
scale2 b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "scale2" [(Kr, [Kr, Kr, Kr, Kr, Kr, Ir])] [a1, a2, a3]

{- |
Constructs a user-definable envelope.

> ares  transeg  ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
> kres  transeg  ia, idur, itype, ib [, idur2] [, itype] [, ic] ...

csound doc: <https://csound.com/docs/manual/transeg.html>
-}
transeg :: [D] -> Sig
transeg b1 =
  Sig $ f <$> mapM unD b1
  where
    f a1 = setRate Kr $ opcs "transeg" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, 0, last a1])

{- |
Constructs a user-definable envelope in absolute time.

> ares  transegb  ia, itim, itype, ib [, itim2] [, itype] [, ic] ...
> kres  transegb  ia, itim, itype, ib [, itim2] [, itype] [, ic] ...

csound doc: <https://csound.com/docs/manual/transegb.html>
-}
transegb :: [D] -> Sig
transegb b1 =
  Sig $ f <$> mapM unD b1
  where
    f a1 = setRate Kr $ opcs "transegb" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, 0, last a1])

{- |
Constructs a user-definable envelope with extended release segment.

Constructs a user-definable envelope. It is the same
      as transeg,
      with an extended release segment.

> ares  transegr  ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
> kres  transegr  ia, idur, itype, ib [, idur2] [, itype] [, ic] ...

csound doc: <https://csound.com/docs/manual/transegr.html>
-}
transegr :: [D] -> D -> D -> D -> Sig
transegr b1 b2 b3 b4 =
  Sig $ f <$> mapM unD b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 = setRate Kr $ opcs "transegr" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, 0, last a1, a2, a3, a4])

--
-- > ares  trigexpseg  kTrig, ia, idur1, ib [, idur2] [, ic] [...]
-- > kres  trigexpseg  kTrig, ia, idur1, ib [, idur2] [, ic] [...]
--
-- csound doc: <https://csound.com/docs/manual/trigexpseg.html>
trigexpseg :: Sig -> D -> D -> D -> Sig
trigexpseg b1 b2 b3 b4 =
  Sig $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 =
      opcs
        "trigexpseg"
        [(Ar, [Kr] ++ (repeat Ir)), (Kr, [Kr] ++ (repeat Ir))]
        [ a1
        , a2
        , a3
        , a4
        ]

--
-- > ares  triglinseg  kTrig, ia, idur1, ib [, idur2] [, ic] [...]
-- > kres  triglinseg  kTrig, ia, idur1, ib [, idur2] [, ic] [...]
--
-- csound doc: <https://csound.com/docs/manual/triglinseg.html>
triglinseg :: Sig -> D -> D -> D -> Sig
triglinseg b1 b2 b3 b4 =
  Sig $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 =
      opcs
        "triglinseg"
        [(Ar, [Kr] ++ (repeat Ir)), (Kr, [Kr] ++ (repeat Ir))]
        [ a1
        , a2
        , a3
        , a4
        ]

{- |
2D linear interpolation

2D linear interpolation between 4 points at (0,0), (1,0), (0,1),
	  (1,1)

> kout  xyscale  kx, ky, k00, k10, k01, k11

csound doc: <https://csound.com/docs/manual/xyscale.html>
-}
xyscale :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
xyscale b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "xyscale" [(Kr, [Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2, a3, a4, a5, a6]

-- Envelope Generators.

{- |
Calculates the classical ADSR envelope using linear segments.

> ares  adsr  iatt, idec, islev, irel [, idel]
> kres  adsr  iatt, idec, islev, irel [, idel]

csound doc: <https://csound.com/docs/manual/adsr.html>
-}
adsr :: D -> D -> D -> D -> Sig
adsr b1 b2 b3 b4 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "adsr" [(Ar, [Ir, Ir, Ir, Ir, Ir]), (Kr, [Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
Applies an envelope consisting of 3 segments.

envlpx -- apply an envelope consisting of 3 segments:

> ares  envlpx  xamp, irise, idur, idec, ifn, iatss, iatdec [, ixmod]
> kres  envlpx  kamp, irise, idur, idec, ifn, iatss, iatdec [, ixmod]

csound doc: <https://csound.com/docs/manual/envlpx.html>
-}
envlpx :: Sig -> D -> D -> D -> Tab -> D -> D -> Sig
envlpx b1 b2 b3 b4 b5 b6 b7 =
  Sig $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unTab b5 <*> unD b6 <*> unD b7
  where
    f a1 a2 a3 a4 a5 a6 a7 =
      opcs
        "envlpx"
        [ (Ar, [Xr, Ir, Ir, Ir, Ir, Ir, Ir, Ir])
        , (Kr, [Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir])
        ]
        [a1, a2, a3, a4, a5, a6, a7]

{- |
The envlpx opcode with a final release segment.

envlpxr is the same as envlpx except that the final segment is entered only on sensing a MIDI note release. The note is then extended by the decay time.

> ares  envlpxr  xamp, irise, idec, ifn, iatss, iatdec [, ixmod] [,irind]
> kres  envlpxr  kamp, irise, idec, ifn, iatss, iatdec [, ixmod] [,irind]

csound doc: <https://csound.com/docs/manual/envlpxr.html>
-}
envlpxr :: Sig -> D -> D -> Tab -> D -> D -> Sig
envlpxr b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unTab b4 <*> unD b5 <*> unD b6
  where
    f a1 a2 a3 a4 a5 a6 =
      opcs
        "envlpxr"
        [ (Ar, [Xr, Ir, Ir, Ir, Ir, Ir, Ir, Ir])
        , (Kr, [Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir])
        ]
        [a1, a2, a3, a4, a5, a6]

--
-- > ares  gtadsr  asig, katt, kdec, ksus,
-- >         krel, kgate
-- > xres  gtadsr  kamp, katt, kdec, ksus,
-- >         krel, kgate
--
-- csound doc: <https://csound.com/docs/manual/gtadsr.html>
gtadsr :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
gtadsr b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6
  where
    f a1 a2 a3 a4 a5 a6 =
      opcs
        "gtadsr"
        [(Ar, [Ar, Kr, Kr, Kr, Kr, Kr]), (Xr, [Kr, Kr, Kr, Kr, Kr, Kr])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        ]

{- |
Applies a straight line rise and decay pattern to an input amp signal.

linen -- apply a straight line rise and decay pattern to an input amp signal.

> ares  linen  xamp, irise, idur, idec
> kres  linen  kamp, irise, idur, idec

csound doc: <https://csound.com/docs/manual/linen.html>
-}
linen :: Sig -> D -> D -> D -> Sig
linen b1 b2 b3 b4 =
  Sig $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "linen" [(Ar, [Xr, Ir, Ir, Ir]), (Kr, [Kr, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
The linen opcode extended with a final release segment.

linenr -- same as linen except that the final segment is entered only on sensing a MIDI note release. The note is then extended by the decay time.

> ares  linenr  xamp, irise, idec, iatdec
> kres  linenr  kamp, irise, idec, iatdec

csound doc: <https://csound.com/docs/manual/linenr.html>
-}
linenr :: Sig -> D -> D -> D -> Sig
linenr b1 b2 b3 b4 =
  Sig $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "linenr" [(Ar, [Xr, Ir, Ir, Ir]), (Kr, [Kr, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
Calculates the classical ADSR envelope using the linsegr mechanism.

> ares  madsr  iatt, idec, islev, irel [, idel] [, ireltim]
> kres  madsr  iatt, idec, islev, irel [, idel] [, ireltim]

csound doc: <https://csound.com/docs/manual/madsr.html>
-}
madsr :: D -> D -> D -> D -> Sig
madsr b1 b2 b3 b4 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "madsr" [(Ar, [Ir, Ir, Ir, Ir, Ir, Ir]), (Kr, [Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
Calculates the classical ADSR envelope using the expsegr mechanism.

> ares  mxadsr  iatt, idec, islev, irel [, idel] [, ireltim]
> kres  mxadsr  iatt, idec, islev, irel [, idel] [, ireltim]

csound doc: <https://csound.com/docs/manual/mxadsr.html>
-}
mxadsr :: D -> D -> D -> D -> Sig
mxadsr b1 b2 b3 b4 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "mxadsr" [(Ar, [Ir, Ir, Ir, Ir, Ir, Ir]), (Kr, [Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
Calculates the classical ADSR envelope.

Calculates the classical ADSR envelope

> ares  xadsr  iatt, idec, islev, irel [, idel]
> kres  xadsr  iatt, idec, islev, irel [, idel]

csound doc: <https://csound.com/docs/manual/xadsr.html>
-}
xadsr :: D -> D -> D -> D -> Sig
xadsr b1 b2 b3 b4 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "xadsr" [(Ar, [Ir, Ir, Ir, Ir, Ir]), (Kr, [Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

-- Models and Emulations.

{- |
Semi-physical model of a bamboo sound.

bamboo is a semi-physical model of a bamboo sound. It is one of the PhISEM percussion opcodes. PhISEM (Physically Informed Stochastic Event Modeling) is an algorithmic approach for simulating collisions of multiple independent sound producing objects.

> ares  bamboo  kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] \
>           [, ifreq1] [, ifreq2]

csound doc: <https://csound.com/docs/manual/bamboo.html>
-}
bamboo :: Sig -> D -> Sig
bamboo b1 b2 =
  Sig $ f <$> unSig b1 <*> unD b2
  where
    f a1 a2 = opcs "bamboo" [(Ar, [Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2]

{- |
Creates a tone similar to a struck metal bar.

Audio output is a tone similar to a struck metal bar, using a
    physical model developed from solving the partial differential
    equation.  There are controls over the boundary conditions as
    well as the bar characteristics.

> ares  barmodel  kbcL, kbcR, iK, ib, kscan, iT30, ipos, ivel, iwid

csound doc: <https://csound.com/docs/manual/barmodel.html>
-}
barmodel :: Sig -> Sig -> D -> D -> Sig -> D -> D -> D -> D -> Sig
barmodel b1 b2 b3 b4 b5 b6 b7 b8 b9 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unD b4 <*> unSig b5 <*> unD b6 <*> unD b7 <*> unD b8 <*> unD b9
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
      opcs
        "barmodel"
        [(Ar, [Kr, Kr, Ir, Ir, Kr, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        ]

{- |
Semi-physical model of a cabasa sound.

cabasa is a semi-physical model of a cabasa sound. It is one of the PhISEM percussion opcodes. PhISEM (Physically Informed Stochastic Event Modeling) is an algorithmic approach for simulating collisions of multiple independent sound producing objects.

> ares  cabasa  iamp, idettack [, inum] [, idamp] [, imaxshake]

csound doc: <https://csound.com/docs/manual/cabasa.html>
-}
cabasa :: D -> D -> Sig
cabasa b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "cabasa" [(Ar, [Ir, Ir, Ir, Ir, Ir])] [a1, a2]

{- |
Simulates Chua's oscillator, an LRC oscillator with an active resistor, proved capable of bifurcation and chaotic attractors, with k-rate control of circuit elements.

> aI3, aV2, aV1  chuap  kL, kR0, kC2, kG, kGa, kGb, kE, kC1, iI3, iV2, iV1, ktime_step

csound doc: <https://csound.com/docs/manual/chuap.html>
-}
chuap ::
  Sig ->
  Sig ->
  Sig ->
  Sig ->
  Sig ->
  Sig ->
  Sig ->
  Sig ->
  D ->
  D ->
  D ->
  Sig ->
  ( Sig
  , Sig
  , Sig
  )
chuap b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7 <*> unSig b8 <*> unD b9 <*> unD b10 <*> unD b11 <*> unSig b12
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
      mopcs
        "chuap"
        ( [Ar, Ar, Ar]
        , [Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Kr]
        )
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12]

{- |
Semi-physical model of a crunch sound.

crunch is a semi-physical model of a crunch sound. It is one of the PhISEM percussion opcodes. PhISEM (Physically Informed Stochastic Event Modeling) is an algorithmic approach for simulating collisions of multiple independent sound producing objects.

> ares  crunch  iamp, idettack [, inum] [, idamp] [, imaxshake]

csound doc: <https://csound.com/docs/manual/crunch.html>
-}
crunch :: D -> D -> Sig
crunch b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "crunch" [(Ar, [Ir, Ir, Ir, Ir, Ir])] [a1, a2]

{- |
Semi-physical model of a water drop.

dripwater is a semi-physical model of a water drop. It is one of the PhISEM percussion opcodes. PhISEM (Physically Informed Stochastic Event Modeling) is an algorithmic approach for simulating collisions of multiple independent sound producing objects.

> ares  dripwater  kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] \
>           [, ifreq1] [, ifreq2]

csound doc: <https://csound.com/docs/manual/dripwater.html>
-}
dripwater :: Sig -> D -> Sig
dripwater b1 b2 =
  Sig $ f <$> unSig b1 <*> unD b2
  where
    f a1 a2 = opcs "dripwater" [(Ar, [Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2]

{- |
Dynamic stochastic approach to waveform synthesis conceived by Iannis Xenakis.

Implementation of the GÃ©nÃ©ration Dynamique Stochastique
      (GENDYN), a dynamic stochastic approach to waveform synthesis conceived
      by Iannis Xenakis.

> ares  gendy  kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, \
>                kampscl, kdurscl [, initcps] [, knum]
> kres  gendy  kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, \
>                kampscl, kdurscl [, initcps] [, knum]

csound doc: <https://csound.com/docs/manual/gendy.html>
-}
gendy :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
gendy b1 b2 b3 b4 b5 b6 b7 b8 b9 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7 <*> unSig b8 <*> unSig b9
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
      opcs
        "gendy"
        [ (Ar, [Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Ir, Kr])
        , (Kr, [Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Ir, Kr])
        ]
        [a1, a2, a3, a4, a5, a6, a7, a8, a9]

{- |
Dynamic stochastic approach to waveform synthesis using cubic interpolation.

Implementation with cubic interpolation of the
      GÃ©nÃ©ration Dynamique Stochastique (GENDYN),
      a dynamic stochastic approach to waveform synthesis conceived by
      Iannis Xenakis.

> ares  gendyc  kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, \
>                 kampscl, kdurscl [, initcps] [, knum]
> kres  gendyc  kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, \
>                 kampscl, kdurscl [, initcps] [, knum]

csound doc: <https://csound.com/docs/manual/gendyc.html>
-}
gendyc :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
gendyc b1 b2 b3 b4 b5 b6 b7 b8 b9 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7 <*> unSig b8 <*> unSig b9
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
      opcs
        "gendyc"
        [ (Ar, [Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Ir, Kr])
        , (Kr, [Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Ir, Kr])
        ]
        [a1, a2, a3, a4, a5, a6, a7, a8, a9]

{- |
Variation of the dynamic stochastic approach to waveform
      synthesis conceived by Iannis Xenakis.

gendyx (gendy eXtended) is an implementation
      of the GÃ©nÃ©ration Dynamique Stochastique
      (GENDYN), a dynamic stochastic approach to waveform synthesis
      conceived by Iannis Xenakis, using curves instead of segments.

> ares  gendyx  kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, \
>                 kampscl, kdurscl, kcurveup, kcurvedown [, initcps] [, knum]
> kres  gendyx  kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, \
>                 kampscl, kdurscl, kcurveup, kcurvedown [, initcps] [, knum]

csound doc: <https://csound.com/docs/manual/gendyx.html>
-}
gendyx :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
gendyx b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7 <*> unSig b8 <*> unSig b9 <*> unSig b10 <*> unSig b11
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
      opcs
        "gendyx"
        [
          ( Ar
          , [Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Ir, Kr]
          )
        , (Kr, [Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Ir, Kr])
        ]
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11]

{- |
Audio output is a tone related to the striking of a cow bell or similar.

Audio output is a tone related to the striking of a cow bell or similar. The method is a physical model developed from Perry Cook, but re-coded for Csound.

> ares  gogobel  kamp, kfreq, ihrd, ipos, imp, kvibf, kvamp, ivfn

csound doc: <https://csound.com/docs/manual/gogobel.html>
-}
gogobel :: Sig -> Sig -> D -> D -> D -> Sig -> Sig -> Tab -> Sig
gogobel b1 b2 b3 b4 b5 b6 b7 b8 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unSig b6 <*> unSig b7 <*> unTab b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 =
      opcs
        "gogobel"
        [(Ar, [Kr, Kr, Ir, Ir, Ir, Kr, Kr, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        ]

{- |
Semi-physical model of a guiro sound.

guiro is a semi-physical model of a guiro sound. It is one of the PhISEM percussion opcodes. PhISEM (Physically Informed Stochastic Event Modeling) is an algorithmic approach for simulating collisions of multiple independent sound producing objects.

> ares  guiro  kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] [, ifreq1]

csound doc: <https://csound.com/docs/manual/guiro.html>
-}
guiro :: Sig -> D -> Sig
guiro b1 b2 =
  Sig $ f <$> unSig b1 <*> unD b2
  where
    f a1 a2 = opcs "guiro" [(Ar, [Kr, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2]

{- |
Implements the Lorenz system of equations.

Implements the Lorenz system of equations.  The Lorenz system is a chaotic-dynamic system which was originally used to simulate the motion of a particle in convection currents and simplified weather systems. Small differences in initial conditions rapidly lead to diverging values. This is sometimes expressed as the butterfly effect. If a butterfly flaps its wings in Australia, it will have an effect on the weather in Alaska. This system is one of the milestones in the development of chaos theory. It is useful as a chaotic audio source or as a low frequency modulation source.

> ax, ay, az  lorenz  ksv, krv, kbv, kh, ix, iy, iz, iskip [, iskipinit]

csound doc: <https://csound.com/docs/manual/lorenz.html>
-}
lorenz :: Sig -> Sig -> Sig -> Sig -> D -> D -> D -> D -> (Sig, Sig, Sig)
lorenz b1 b2 b3 b4 b5 b6 b7 b8 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unD b5 <*> unD b6 <*> unD b7 <*> unD b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 =
      mopcs
        "lorenz"
        ([Ar, Ar, Ar], [Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir])
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        ]

{- |
Mandelbrot set

Returns the number of iterations corresponding to a given point of complex plane by applying the Mandelbrot set formula.

> kiter, koutrig  mandel   ktrig, kx, ky, kmaxIter

csound doc: <https://csound.com/docs/manual/mandel.html>
-}
mandel :: Sig -> Sig -> Sig -> Sig -> (Sig, Sig)
mandel b1 b2 b3 b4 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
  where
    f a1 a2 a3 a4 = mopcs "mandel" ([Kr, Kr], [Kr, Kr, Kr, Kr]) [a1, a2, a3, a4]

{- |
An emulation of a mandolin.

> ares  mandol  kamp, kfreq, kpluck, kdetune, kgain, ksize \
>         [, ifn] [, iminfreq]

csound doc: <https://csound.com/docs/manual/mandol.html>
-}
mandol :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
mandol b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "mandol" [(Ar, [Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
Physical model related to the striking of a wooden block.

Audio output is a tone related to the striking of a wooden block as found in a marimba. The method is a physical model developed from Perry Cook but re-coded for Csound.

> ares  marimba  kamp, kfreq, ihrd, ipos, imp, kvibf, kvamp, ivibfn, idec \
>           [, idoubles] [, itriples]

csound doc: <https://csound.com/docs/manual/marimba.html>
-}
marimba :: Sig -> Sig -> D -> D -> D -> Sig -> Sig -> Tab -> D -> Sig
marimba b1 b2 b3 b4 b5 b6 b7 b8 b9 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unSig b6 <*> unSig b7 <*> unTab b8 <*> unD b9
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
      opcs
        "marimba"
        [(Ar, [Kr, Kr, Ir, Ir, Ir, Kr, Kr, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        ]

{- |
An emulation of a mini-Moog synthesizer.

> ares  moog  kamp, kfreq, kfiltq, kfiltrate, kvibf, kvamp, iafn, iwfn, ivfn

csound doc: <https://csound.com/docs/manual/moog.html>
-}
moog :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Tab -> Sig
moog b1 b2 b3 b4 b5 b6 b7 b8 b9 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unTab b7 <*> unTab b8 <*> unTab b9
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
      opcs
        "moog"
        [(Ar, [Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        ]

{- |
Simulates a planet orbiting in a binary star system.

planet simulates a planet orbiting in a binary star system. The outputs are the x, y and z coordinates of the orbiting planet. It is possible for the planet to achieve escape velocity by a close encounter with a star. This makes this system somewhat unstable.

> ax, ay, az  planet  kmass1, kmass2, ksep, ix, iy, iz, ivx, ivy, ivz, idelta \
>           [, ifriction] [, iskip]

csound doc: <https://csound.com/docs/manual/planet.html>
-}
planet :: Sig -> Sig -> Sig -> D -> D -> D -> D -> D -> D -> D -> (Sig, Sig, Sig)
planet b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unD b5 <*> unD b6 <*> unD b7 <*> unD b8 <*> unD b9 <*> unD b10
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
      mopcs
        "planet"
        ( [Ar, Ar, Ar]
        , [Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir]
        )
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10]

{- |
Creates a tone similar to a piano string prepared in a Cageian fashion.

Audio output is a tone similar to a piano string, prepared with
    a number of rubbers and rattles. The method uses a
    physical model developed from solving the partial differential
    equation.

> ares  prepiano  ifreq, iNS, iD, iK, \
>         iT30,iB, kbcl, kbcr, imass, ihvfreq, iinit, ipos, ivel, isfreq, \
>         isspread[, irattles, irubbers]
> al,ar  prepiano  ifreq, iNS, iD, iK, \
>         iT30,iB, kbcl, kbcr, imass, ihvfreq, iinit, ipos, ivel, isfreq, \
>         isspread[, irattles, irubbers]

csound doc: <https://csound.com/docs/manual/prepiano.html>
-}
prepiano ::
  D ->
  D ->
  D ->
  D ->
  D ->
  D ->
  Sig ->
  Sig ->
  D ->
  D ->
  D ->
  D ->
  D ->
  D ->
  D ->
  ( Sig
  , Sig
  )
prepiano b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 =
  pureTuple $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unD b6 <*> unSig b7 <*> unSig b8 <*> unD b9 <*> unD b10 <*> unD b11 <*> unD b12 <*> unD b13 <*> unD b14 <*> unD b15
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 =
      mopcs
        "prepiano"
        ( [Ar, Ar]
        , [Ir, Ir, Ir, Ir, Ir, Ir, Kr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir]
        )
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        ]

{- |
Semi-physical model of a sandpaper sound.

sandpaper is a semi-physical model of a sandpaper sound. It is one of the PhISEM percussion opcodes. PhISEM (Physically Informed Stochastic Event Modeling) is an algorithmic approach for simulating collisions of multiple independent sound producing objects.

> ares  sandpaper  iamp, idettack [, inum] [, idamp] [, imaxshake]

csound doc: <https://csound.com/docs/manual/sandpaper.html>
-}
sandpaper :: D -> D -> Sig
sandpaper b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "sandpaper" [(Ar, [Ir, Ir, Ir, Ir, Ir])] [a1, a2]

{- |
Semi-physical model of a sekere sound.

sekere is a semi-physical model of a sekere sound. It is one of the PhISEM percussion opcodes. PhISEM (Physically Informed Stochastic Event Modeling) is an algorithmic approach for simulating collisions of multiple independent sound producing objects.

> ares  sekere  iamp, idettack [, inum] [, idamp] [, imaxshake]

csound doc: <https://csound.com/docs/manual/sekere.html>
-}
sekere :: D -> D -> Sig
sekere b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "sekere" [(Ar, [Ir, Ir, Ir, Ir, Ir])] [a1, a2]

{- |
Sounds like the shaking of a maraca or similar gourd instrument.

Audio output is a tone related to the shaking of a maraca or similar gourd instrument. The method is a physically inspired model developed from Perry Cook, but re-coded for Csound.

> ares  shaker  kamp, kfreq, kbeans, kdamp, ktimes [, idecay]

csound doc: <https://csound.com/docs/manual/shaker.html>
-}
shaker :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
shaker b1 b2 b3 b4 b5 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5
  where
    f a1 a2 a3 a4 a5 = opcs "shaker" [(Ar, [Kr, Kr, Kr, Kr, Kr, Ir])] [a1, a2, a3, a4, a5]

{- |
Semi-physical model of a sleighbell sound.

sleighbells is a semi-physical model of a sleighbell sound. It is one of the PhISEM percussion opcodes. PhISEM (Physically Informed Stochastic Event Modeling) is an algorithmic approach for simulating collisions of multiple independent sound producing objects.

> ares  sleighbells  kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] \
>           [, ifreq1] [, ifreq2]

csound doc: <https://csound.com/docs/manual/sleighbells.html>
-}
sleighbells :: Sig -> D -> Sig
sleighbells b1 b2 =
  Sig $ f <$> unSig b1 <*> unD b2
  where
    f a1 a2 = opcs "sleighbells" [(Ar, [Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2]

{- |
Semi-physical model of a stick sound.

stix is a semi-physical model of a stick sound. It is one of the PhISEM percussion opcodes. PhISEM (Physically Informed Stochastic Event Modeling) is an algorithmic approach for simulating collisions of multiple independent sound producing objects.

> ares  stix  iamp, idettack [, inum] [, idamp] [, imaxshake]

csound doc: <https://csound.com/docs/manual/stix.html>
-}
stix :: D -> D -> Sig
stix b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "stix" [(Ar, [Ir, Ir, Ir, Ir, Ir])] [a1, a2]

{- |
Semi-physical model of a tambourine sound.

tambourine is a semi-physical model of a tambourine sound. It is one of the PhISEM percussion opcodes. PhISEM (Physically Informed Stochastic Event Modeling) is an algorithmic approach for simulating collisions of multiple independent sound producing objects.

> ares  tambourine  kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] \
>           [, ifreq1] [, ifreq2]

csound doc: <https://csound.com/docs/manual/tambourine.html>
-}
tambourine :: Sig -> D -> Sig
tambourine b1 b2 =
  Sig $ f <$> unSig b1 <*> unD b2
  where
    f a1 a2 = opcs "tambourine" [(Ar, [Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2]

{- |
Physical model related to the striking of a metal block.

Audio output is a tone related to the striking of a metal block as found in a vibraphone. The method is a physical model developed from Perry Cook, but re-coded for Csound.

> ares  vibes  kamp, kfreq, ihrd, ipos, imp, kvibf, kvamp, ivibfn, idec

csound doc: <https://csound.com/docs/manual/vibes.html>
-}
vibes :: Sig -> Sig -> D -> D -> D -> Sig -> Sig -> Tab -> D -> Sig
vibes b1 b2 b3 b4 b5 b6 b7 b8 b9 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unSig b6 <*> unSig b7 <*> unTab b8 <*> unD b9
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
      opcs
        "vibes"
        [(Ar, [Kr, Kr, Ir, Ir, Ir, Kr, Kr, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        ]

{- |
An emulation of a human voice.

> ares  voice  kamp, kfreq, kphoneme, kform, kvibf, kvamp, ifn, ivfn

csound doc: <https://csound.com/docs/manual/voice.html>
-}
voice :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Sig
voice b1 b2 b3 b4 b5 b6 b7 b8 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unTab b7 <*> unTab b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 =
      opcs
        "voice"
        [(Ar, [Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        ]

-- Phasors.

--
-- > aexp,aph  ephasor  kfreq, kR
--
-- csound doc: <https://csound.com/docs/manual/ephasor.html>
ephasor :: Sig -> Sig -> (Sig, Sig)
ephasor b1 b2 =
  pureTuple $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = mopcs "ephasor" ([Ar, Ar], [Kr, Kr]) [a1, a2]

{- |
Produce a normalized moving phase value.

> ares  phasor  xcps [, iphs]
> kres  phasor  kcps [, iphs]

csound doc: <https://csound.com/docs/manual/phasor.html>
-}
phasor :: Sig -> Sig
phasor b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "phasor" [(Ar, [Xr, Ir]), (Kr, [Kr, Ir])] [a1]

{- |
Produce an arbitrary number of normalized moving phase values.

Produce an arbitrary number of normalized moving phase values, accessable by an index.

> ares  phasorbnk  xcps, kndx, icnt [, iphs]
> kres  phasorbnk  kcps, kndx, icnt [, iphs]

csound doc: <https://csound.com/docs/manual/phasorbnk.html>
-}
phasorbnk :: Sig -> Sig -> D -> Sig
phasorbnk b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "phasorbnk" [(Ar, [Xr, Kr, Ir, Ir]), (Kr, [Kr, Kr, Ir, Ir])] [a1, a2, a3]

{- |
A resettable linear ramp between two levels

A resettable linear ramp between two levels. Port of
	  Supercollider's Phasor.

> aindex  sc_phasor  xtrig, xrate, kstart, kend [, kresetPos]
> kindex  sc_phasor  xtrig, xrate, kstart, kend [, kresetPos]

csound doc: <https://csound.com/docs/manual/sc_phasor.html>
-}
sc_phasor :: Sig -> Sig -> Sig -> Sig -> Sig
sc_phasor b1 b2 b3 b4 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
  where
    f a1 a2 a3 a4 = opcs "sc_phasor" [(Ar, [Xr, Xr, Kr, Kr, Kr]), (Kr, [Xr, Xr, Kr, Kr, Kr])] [a1, a2, a3, a4]

{- |
Produces a normalized moving phase value with sync input and output.

Produces a moving phase value between zero and one and an extra impulse output ("sync out") whenever its phase value crosses or is reset to zero. The phase can be reset at any time by an impulse on the "sync in" parameter.

> aphase, asyncout  syncphasor  xcps, asyncin, [, iphs]

csound doc: <https://csound.com/docs/manual/syncphasor.html>
-}
syncphasor :: Sig -> Sig -> (Sig, Sig)
syncphasor b1 b2 =
  pureTuple $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = mopcs "syncphasor" ([Ar, Ar], [Xr, Ar, Ir]) [a1, a2]

--
-- > aindex  trigphasor  xtrig, xrate, kstart, kend [, kresetPos]
-- > kindex  trigphasor  xtrig, xrate, kstart, kend [, kresetPos]
--
-- csound doc: <https://csound.com/docs/manual/trigphasor.html>
trigphasor :: Sig -> Sig -> Sig -> Sig -> Sig
trigphasor b1 b2 b3 b4 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
  where
    f a1 a2 a3 a4 = opcs "trigphasor" [(Ar, [Xr, Xr, Kr, Kr, Kr]), (Kr, [Xr, Xr, Kr, Kr, Kr])] [a1, a2, a3, a4]

-- Random (Noise) Generators.

{- |
Beta distribution random number generator (positive values only).

Beta distribution random number generator (positive values only). This is an x-class noise generator.

> ares  betarand  krange, kalpha, kbeta
> ires  betarand  krange, kalpha, kbeta
> kres  betarand  krange, kalpha, kbeta

csound doc: <https://csound.com/docs/manual/betarand.html>
-}
betarand :: (SigOrD a) => a -> a -> a -> SE a
betarand b1 b2 b3 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1 <*> (lift . toGE) b2 <*> (lift . toGE) b3
  where
    f a1 a2 a3 = opcsDep "betarand" [(Ar, [Kr, Kr, Kr]), (Ir, [Kr, Kr, Kr]), (Kr, [Kr, Kr, Kr])] [a1, a2, a3]

{- |
Exponential distribution random number generator.

Exponential distribution random number generator. This is an x-class noise generator.

> ares  bexprnd  krange
> ires  bexprnd  krange
> kres  bexprnd  krange

csound doc: <https://csound.com/docs/manual/bexprnd.html>
-}
bexprnd :: (SigOrD a) => a -> SE a
bexprnd b1 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1
  where
    f a1 = opcsDep "bexprnd" [(Ar, [Kr]), (Ir, [Kr]), (Kr, [Kr])] [a1]

{- |
Cauchy distribution random number generator.

Cauchy distribution random number generator. This is an x-class noise generator.

> ares  cauchy  kalpha
> ires  cauchy  kalpha
> kres  cauchy  kalpha

csound doc: <https://csound.com/docs/manual/cauchy.html>
-}
cauchy :: (SigOrD a) => a -> SE a
cauchy b1 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1
  where
    f a1 = opcsDep "cauchy" [(Ar, [Kr]), (Ir, [Kr]), (Kr, [Kr])] [a1]

{- |
Cauchy distribution random number generator with
      interpolation.

Cauchy distribution random number generator with controlled
      interpolation between values. This is an x-class noise
      generator.

> ares  cauchyi  klambda, xamp, xcps
> ires  cauchyi  klambda, xamp, xcps
> kres  cauchyi  klambda, xamp, xcps

csound doc: <https://csound.com/docs/manual/cauchyi.html>
-}
cauchyi :: (SigOrD a) => a -> a -> a -> SE a
cauchyi b1 b2 b3 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1 <*> (lift . toGE) b2 <*> (lift . toGE) b3
  where
    f a1 a2 a3 = opcsDep "cauchyi" [(Ar, [Kr, Xr, Xr]), (Ir, [Kr, Xr, Xr]), (Kr, [Kr, Xr, Xr])] [a1, a2, a3]

{- |
Continuous USER-defined-distribution RaNDom generator.

> aout  cuserrnd  kmin, kmax, ktableNum
> iout  cuserrnd  imin, imax, itableNum
> kout  cuserrnd  kmin, kmax, ktableNum

csound doc: <https://csound.com/docs/manual/cuserrnd.html>
-}
cuserrnd :: (SigOrD a) => a -> a -> a -> SE a
cuserrnd b1 b2 b3 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1 <*> (lift . toGE) b2 <*> (lift . toGE) b3
  where
    f a1 a2 a3 = opcsDep "cuserrnd" [(Ar, [Kr, Kr, Kr]), (Ir, [Ir, Ir, Ir]), (Kr, [Kr, Kr, Kr])] [a1, a2, a3]

{- |
Discrete USER-defined-distribution RaNDom generator.

> aout  duserrnd  ktableNum
> iout  duserrnd  itableNum
> kout  duserrnd  ktableNum

csound doc: <https://csound.com/docs/manual/duserrnd.html>
-}
duserrnd :: (SigOrD a) => a -> SE a
duserrnd b1 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1
  where
    f a1 = opcsDep "duserrnd" [(Ar, [Kr]), (Ir, [Ir]), (Kr, [Kr])] [a1]

{- |
Random impulses.

Generates random impulses from 0 to 1.

> ares  dust  kamp, kdensity
> kres  dust  kamp, kdensity

csound doc: <https://csound.com/docs/manual/dust.html>
-}
dust :: Sig -> Sig -> SE Sig
dust b1 b2 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep "dust" [(Ar, [Kr, Kr]), (Kr, [Kr, Kr])] [a1, a2]

{- |
Random impulses.

Generates random impulses from -1 to 1.

> ares  dust2  kamp, kdensity
> kres  dust2  kamp, kdensity

csound doc: <https://csound.com/docs/manual/dust2.html>
-}
dust2 :: Sig -> Sig -> SE Sig
dust2 b1 b2 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep "dust2" [(Ar, [Kr, Kr]), (Kr, [Kr, Kr])] [a1, a2]

{- |
Exponential distribution random number generator (positive values only).

Exponential distribution random number generator (positive values only). This is an x-class noise generator.

> ares  exprand  klambda
> ires  exprand  klambda
> kres  exprand  klambda

csound doc: <https://csound.com/docs/manual/exprand.html>
-}
exprand :: (SigOrD a) => a -> SE a
exprand b1 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1
  where
    f a1 = opcsDep "exprand" [(Ar, [Kr]), (Ir, [Kr]), (Kr, [Kr])] [a1]

{- |
Exponential distribution random number generator with
      interpolation (positive values only).

Exponential distribution random number generator with controlled
      interpolation between values (positive values only). This is an
      x-class noise generator.

> ares  exprandi  klambda, xamp, xcps
> ires  exprandi  klambda, xamp, xcps
> kres  exprandi  klambda, xamp, xcps

csound doc: <https://csound.com/docs/manual/exprandi.html>
-}
exprandi :: (SigOrD a) => a -> a -> a -> SE a
exprandi b1 b2 b3 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1 <*> (lift . toGE) b2 <*> (lift . toGE) b3
  where
    f a1 a2 a3 = opcsDep "exprandi" [(Ar, [Kr, Xr, Xr]), (Ir, [Kr, Xr, Xr]), (Kr, [Kr, Xr, Xr])] [a1, a2, a3]

{- |
A fractal noise generator.

A fractal noise generator implemented as a white noise filtered
      by a cascade of 15 first-order filters.

> ares  fractalnoise  kamp, kbeta

csound doc: <https://csound.com/docs/manual/fractalnoise.html>
-}
fractalnoise :: Sig -> Sig -> SE Sig
fractalnoise b1 b2 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep "fractalnoise" [(Ar, [Kr, Kr])] [a1, a2]

{- |
Gaussian distribution random number generator.

Gaussian distribution random number generator. This is an x-class noise generator.

> ares  gauss  krange
> ires  gauss  irange
> kres  gauss  krange
> ares  gauss  kmean, ksdev
> ires  gauss  imean, isdev
> kres  gauss  kmean, ksdev

csound doc: <https://csound.com/docs/manual/gauss.html>
-}
gauss :: Sig -> SE Sig
gauss b1 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 =
      opcsDep
        "gauss"
        [ (Ar, [Kr])
        , (Ir, [Ir])
        , (Kr, [Kr])
        , (Ar, [Kr, Kr])
        , (Ir, [Ir, Ir])
        , (Kr, [Kr, Kr])
        ]
        [a1]

{- |
Gaussian distribution random number generator with
      interpolation.

Gaussian distribution random number generator with controlled
      interpolation between values. This is an
      x-class noise generator.

> ares  gaussi  krange, xamp, xcps
> ires  gaussi  krange, xamp, xcps
> kres  gaussi  krange, xamp, xcps

csound doc: <https://csound.com/docs/manual/gaussi.html>
-}
gaussi :: (SigOrD a) => a -> a -> a -> SE a
gaussi b1 b2 b3 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1 <*> (lift . toGE) b2 <*> (lift . toGE) b3
  where
    f a1 a2 a3 = opcsDep "gaussi" [(Ar, [Kr, Xr, Xr]), (Ir, [Kr, Xr, Xr]), (Kr, [Kr, Xr, Xr])] [a1, a2, a3]

{- |
Random impulses around a certain frequency.

Generates random impulses around a certain frequency.

> ares  gausstrig  kamp, kcps, kdev [, imode] [, ifrst1]
> kres  gausstrig  kamp, kcps, kdev [, imode] [, ifrst1]

csound doc: <https://csound.com/docs/manual/gausstrig.html>
-}
gausstrig :: Sig -> Sig -> Sig -> SE Sig
gausstrig b1 b2 b3 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep "gausstrig" [(Ar, [Kr, Kr, Kr, Ir, Ir]), (Kr, [Kr, Kr, Kr, Ir, Ir])] [a1, a2, a3]

{- |
Reads the global seed value.

Returns the global seed value used for all x-class
      noise generators.

> ians getseed
> kans getseed

csound doc: <https://csound.com/docs/manual/getseed.html>
-}
getseed :: SE Sig
getseed =
  fmap (Sig . return) $ SE $ join $ return $ f
  where
    f = opcsDep "getseed" [(Ir, []), (Kr, [])] []

{- |
Generates a segmented line whose segments are randomly generated.

> kout  jitter  kamp, kcpsMin, kcpsMax

csound doc: <https://csound.com/docs/manual/jitter.html>
-}
jitter :: Sig -> Sig -> Sig -> SE Sig
jitter b1 b2 b3 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep "jitter" [(Kr, [Kr, Kr, Kr])] [a1, a2, a3]

{- |
Generates a segmented line with user-controllable random segments.

> kout  jitter2  ktotamp, kamp1, kcps1,
>         kamp2, kcps2, kamp3, kcps3[ , iopt]

csound doc: <https://csound.com/docs/manual/jitter2.html>
-}
jitter2 :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> SE Sig
jitter2 b1 b2 b3 b4 b5 b6 b7 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5 <*> (lift . unSig) b6 <*> (lift . unSig) b7
  where
    f a1 a2 a3 a4 a5 a6 a7 =
      opcsDep
        "jitter2"
        [(Kr, [Kr, Kr, Kr, Kr, Kr, Kr, Kr, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        ]

{- |
A jitter-spline generator.

> ares  jspline  xamp, kcpsMin, kcpsMax
> kres  jspline  kamp, kcpsMin, kcpsMax

csound doc: <https://csound.com/docs/manual/jspline.html>
-}
jspline :: Sig -> Sig -> Sig -> SE Sig
jspline b1 b2 b3 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep "jspline" [(Ar, [Xr, Kr, Kr]), (Kr, [Kr, Kr, Kr])] [a1, a2, a3]

--
-- > knum  lfsr  ilen, iprob [, iseed]
--
-- csound doc: <https://csound.com/docs/manual/lfsr.html>
lfsr :: D -> D -> SE Sig
lfsr b1 b2 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep "lfsr" [(Kr, [Ir, Ir, Ir])] [a1, a2]

{- |
Linear distribution random number generator (positive values only).

Linear distribution random number generator (positive values only). This is an x-class noise generator.

> ares  linrand  krange
> ires  linrand  krange
> kres  linrand  krange

csound doc: <https://csound.com/docs/manual/linrand.html>
-}
linrand :: (SigOrD a) => a -> SE a
linrand b1 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1
  where
    f a1 = opcsDep "linrand" [(Ar, [Kr]), (Ir, [Kr]), (Kr, [Kr])] [a1]

{- |
A white noise generator with an IIR lowpass filter.

> ares  noise  xamp, kbeta

csound doc: <https://csound.com/docs/manual/noise.html>
-}
noise :: Sig -> Sig -> SE Sig
noise b1 b2 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep "noise" [(Ar, [Xr, Kr])] [a1, a2]

{- |
Cauchy distribution random number generator (positive values only).

Cauchy distribution random number generator (positive values only). This is an x-class noise generator.

> ares  pcauchy  kalpha
> ires  pcauchy  kalpha
> kres  pcauchy  kalpha

csound doc: <https://csound.com/docs/manual/pcauchy.html>
-}
pcauchy :: (SigOrD a) => a -> SE a
pcauchy b1 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1
  where
    f a1 = opcsDep "pcauchy" [(Ar, [Kr]), (Ir, [Kr]), (Kr, [Kr])] [a1]

{- |
Generates pink noise.

Generates pink noise (-3dB/oct response) by the New
      Shade of Pink algorithm of Stefan Stenzel.

> ares  pinker

csound doc: <https://csound.com/docs/manual/pinker.html>
-}
pinker :: SE Sig
pinker =
  fmap (Sig . return) $ SE $ join $ return $ f
  where
    f = opcsDep "pinker" [(Ar, [])] []

{- |
Generates approximate pink noise.

Generates approximate pink noise (-3dB/oct response) by one of two different methods:

> ares  pinkish  xin [, imethod] [, inumbands] [, iseed] [, iskip]

csound doc: <https://csound.com/docs/manual/pinkish.html>
-}
pinkish :: Sig -> SE Sig
pinkish b1 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep "pinkish" [(Ar, [Xr, Ir, Ir, Ir, Ir])] [a1]

{- |
Poisson distribution random number generator (positive values only).

Poisson distribution random number generator (positive values only). This is an x-class noise generator.

> ares  poisson  klambda
> ires  poisson  klambda
> kres  poisson  klambda

csound doc: <https://csound.com/docs/manual/poisson.html>
-}
poisson :: (SigOrD a) => a -> SE a
poisson b1 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1
  where
    f a1 = opcsDep "poisson" [(Ar, [Kr]), (Ir, [Kr]), (Kr, [Kr])] [a1]

{- |
Generates a controlled random number series.

Output is a controlled random number series between -amp and +amp

> ares  rand  xamp [, iseed] [, isel] [, ioffset]
> kres  rand  xamp [, iseed] [, isel] [, ioffset]

csound doc: <https://csound.com/docs/manual/rand.html>
-}
rand :: Sig -> SE Sig
rand b1 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep "rand" [(Ar, [Xr, Ir, Ir, Ir]), (Kr, [Xr, Ir, Ir, Ir])] [a1]

{- |
Generates random numbers and holds them for a period of time.

> ares  randh  xamp, xcps [, iseed] [, isize] [, ioffset]
> kres  randh  kamp, kcps [, iseed] [, isize] [, ioffset]

csound doc: <https://csound.com/docs/manual/randh.html>
-}
randh :: Sig -> Sig -> SE Sig
randh b1 b2 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep "randh" [(Ar, [Xr, Xr, Ir, Ir, Ir]), (Kr, [Kr, Kr, Ir, Ir, Ir])] [a1, a2]

{- |
Generates a controlled random number series with interpolation between each new number.

> ares  randi  xamp, xcps [, iseed] [, isize] [, ioffset]
> kres  randi  kamp, kcps [, iseed] [, isize] [, ioffset]

csound doc: <https://csound.com/docs/manual/randi.html>
-}
randi :: Sig -> Sig -> SE Sig
randi b1 b2 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep "randi" [(Ar, [Xr, Xr, Ir, Ir, Ir]), (Kr, [Kr, Kr, Ir, Ir, Ir])] [a1, a2]

{- |
Generates a controlled pseudo-random number series between min and max values.

Generates is a controlled pseudo-random number series between min and max values.

> ares  random  kmin, kmax
> ires  random  imin, imax
> kres  random  kmin, kmax

csound doc: <https://csound.com/docs/manual/random.html>
-}
random :: (SigOrD a) => a -> a -> SE a
random b1 b2 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1 <*> (lift . toGE) b2
  where
    f a1 a2 = opcsDep "random" [(Ar, [Kr, Kr]), (Ir, [Ir, Ir]), (Kr, [Kr, Kr])] [a1, a2]

{- |
Generates random numbers with a user-defined limit and holds them for a period of time.

> ares  randomh  kmin, kmax, xcps [,imode] [,ifirstval]
> kres  randomh  kmin, kmax, kcps [,imode] [,ifirstval]

csound doc: <https://csound.com/docs/manual/randomh.html>
-}
randomh :: Sig -> Sig -> Sig -> SE Sig
randomh b1 b2 b3 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep "randomh" [(Ar, [Kr, Kr, Xr, Ir, Ir]), (Kr, [Kr, Kr, Kr, Ir, Ir])] [a1, a2, a3]

{- |
Generates a user-controlled random number series with interpolation between each new number.

> ares  randomi  kmin, kmax, xcps [,imode] [,ifirstval]
> kres  randomi  kmin, kmax, kcps [,imode] [,ifirstval]

csound doc: <https://csound.com/docs/manual/randomi.html>
-}
randomi :: Sig -> Sig -> Sig -> SE Sig
randomi b1 b2 b3 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep "randomi" [(Ar, [Kr, Kr, Xr, Ir, Ir]), (Kr, [Kr, Kr, Kr, Ir, Ir])] [a1, a2, a3]

{- |
31-bit bipolar random opcodes with controllable distribution.

31-bit bipolar random opcodes with controllable distribution. These units are portable, i.e. using the same seed value will generate the same random sequence on all systems. The distribution of generated random numbers can be varied at k-rate.

> ax  rnd31  kscl, krpow [, iseed]
> ix  rnd31  iscl, irpow [, iseed]
> kx  rnd31  kscl, krpow [, iseed]

csound doc: <https://csound.com/docs/manual/rnd31.html>
-}
rnd31 :: (SigOrD a) => a -> a -> SE a
rnd31 b1 b2 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1 <*> (lift . toGE) b2
  where
    f a1 a2 = opcsDep "rnd31" [(Ar, [Kr, Kr, Ir]), (Ir, [Ir, Ir, Ir]), (Kr, [Kr, Kr, Ir])] [a1, a2]

--
-- >  rndseed  ival
--
-- csound doc: <https://csound.com/docs/manual/rndseed.html>
rndseed :: D -> SE ()
rndseed b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "rndseed" [(Xr, [Ir])] [a1]

{- |
Generate random spline curves.

> ares  rspline  xrangeMin, xrangeMax, kcpsMin, kcpsMax
> kres  rspline  krangeMin, krangeMax, kcpsMin, kcpsMax

csound doc: <https://csound.com/docs/manual/rspline.html>
-}
rspline :: Sig -> Sig -> Sig -> Sig -> SE Sig
rspline b1 b2 b3 b4 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4
  where
    f a1 a2 a3 a4 = opcsDep "rspline" [(Ar, [Xr, Xr, Kr, Kr]), (Kr, [Kr, Kr, Kr, Kr])] [a1, a2, a3, a4]

{- |
Sets the global seed value.

Sets the global seed value for all x-class noise generators, as well as other opcodes that use a random call, such as grain.

>  seed  ival

csound doc: <https://csound.com/docs/manual/seed.html>
-}
seed :: D -> SE ()
seed b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "seed" [(Xr, [Ir])] [a1]

{- |
Generates a controlled pseudo-random number series between min and max values according to a trigger.

Generates a controlled pseudo-random number series between min and max values at k-rate whenever the trigger parameter is different to 0.

> kout  trandom  ktrig, kmin, kmax

csound doc: <https://csound.com/docs/manual/trandom.html>
-}
trandom :: Sig -> Sig -> Sig -> SE Sig
trandom b1 b2 b3 =
  fmap (Sig . return) $ SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep "trandom" [(Kr, [Kr, Kr, Kr])] [a1, a2, a3]

{- |
Triangular distribution random number generator

Triangular distribution random number generator. This is an x-class noise generator.

> ares  trirand  krange
> ires  trirand  krange
> kres  trirand  krange

csound doc: <https://csound.com/docs/manual/trirand.html>
-}
trirand :: (SigOrD a) => a -> SE a
trirand b1 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1
  where
    f a1 = opcsDep "trirand" [(Ar, [Kr]), (Ir, [Kr]), (Kr, [Kr])] [a1]

{- |
Uniform distribution random number generator (positive values only).

Uniform distribution random number generator (positive values only). This is an x-class noise generator.

> ares  unirand  krange
> ires  unirand  krange
> kres  unirand  krange

csound doc: <https://csound.com/docs/manual/unirand.html>
-}
unirand :: (SigOrD a) => a -> SE a
unirand b1 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1
  where
    f a1 = opcsDep "unirand" [(Ar, [Kr]), (Ir, [Kr]), (Kr, [Kr])] [a1]

{- |
truly random opcodes with controllable range.

Truly random opcodes with controllable range. These
      units are for Unix-like systems only and use /dev/urandom to construct
      Csound random values

> ax  urandom  [imin, imax]
> ix  urandom  [imin, imax]
> kx  urandom  [imin, imax]

csound doc: <https://csound.com/docs/manual/urandom.html>
-}
urandom :: (SigOrD a) => SE a
urandom =
  fmap (fromGE . return) $ SE $ join $ return $ f
  where
    f = opcsDep "urandom" [(Ar, [Ir, Ir]), (Ir, [Ir, Ir]), (Kr, [Ir, Ir])] []

{- |
A discrete user-defined-distribution random generator that can be used as a function.

> aout =  urd (ktableNum)
> iout =  urd (itableNum)
> kout =  urd (ktableNum)

csound doc: <https://csound.com/docs/manual/urd.html>
-}
urd :: (SigOrD a) => a -> SE a
urd b1 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1
  where
    f a1 = oprByDep "urd" [(Ar, [Kr]), (Kr, [Kr]), (Ir, [Ir])] [a1]

{- |
Weibull distribution random number generator (positive values only).

Weibull distribution random number generator (positive values only). This is an x-class noise generator

> ares  weibull  ksigma, ktau
> ires  weibull  ksigma, ktau
> kres  weibull  ksigma, ktau

csound doc: <https://csound.com/docs/manual/weibull.html>
-}
weibull :: (SigOrD a) => a -> a -> SE a
weibull b1 b2 =
  fmap (fromGE . return) $ SE $ join $ f <$> (lift . toGE) b1 <*> (lift . toGE) b2
  where
    f a1 a2 = opcsDep "weibull" [(Ar, [Kr, Kr]), (Ir, [Kr, Kr]), (Kr, [Kr, Kr])] [a1, a2]

-- Sample Playback.

{- |
Generates breakbeat-style cut-ups of a mono audio stream.

The BreakBeat Cutter automatically generates cut-ups of a source audio stream in the style of drum and bass/jungle breakbeat manipulations.  There are two versions, for mono (bbcutm) or stereo (bbcuts) sources.  Whilst originally based on breakbeat cutting, the opcode can be applied to any type of source audio.

> a1  bbcutm  asource, ibps, isubdiv, ibarlength, iphrasebars, inumrepeats \
>           [, istutterspeed] [, istutterchance] [, ienvchoice ]

csound doc: <https://csound.com/docs/manual/bbcutm.html>
-}
bbcutm :: Sig -> D -> D -> D -> D -> D -> Sig
bbcutm b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unD b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "bbcutm" [(Ar, [Ar, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
Generates breakbeat-style cut-ups of a stereo audio stream.

The BreakBeat Cutter automatically generates cut-ups of a source audio stream in the style of drum and bass/jungle breakbeat manipulations.  There are two versions, for mono (bbcutm) or stereo (bbcuts) sources.  Whilst originally based on breakbeat cutting, the opcode can be applied to any type of source audio.

> a1,a2  bbcuts  asource1, asource2, ibps, isubdiv, ibarlength, iphrasebars, \
>           inumrepeats [, istutterspeed] [, istutterchance] [, ienvchoice]

csound doc: <https://csound.com/docs/manual/bbcuts.html>
-}
bbcuts :: Sig -> Sig -> D -> D -> D -> D -> D -> (Sig, Sig)
bbcuts b1 b2 b3 b4 b5 b6 b7 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unD b6 <*> unD b7
  where
    f a1 a2 a3 a4 a5 a6 a7 =
      mopcs
        "bbcuts"
        ([Ar, Ar], [Ar, Ar, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        ]

{- |
Function-table-based crossfading looper.

This opcode reads audio from a function table and plays it back in a loop with user-defined
   start time, duration and crossfade time. It also allows the pitch of the loop to be controlled,
   including reversed playback. It accepts non-power-of-two tables, such as deferred-allocation
   GEN01 tables, with one or two channels.

> asig1[, asig2]  flooper  kamp, kpitch, istart, idur, ifad, ifn

csound doc: <https://csound.com/docs/manual/flooper.html>
-}
flooper :: forall a. (Tuple a) => Sig -> Sig -> D -> D -> D -> Tab -> a
flooper b1 b2 b3 b4 b5 b6 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unTab b6
  where
    f a1 a2 a3 a4 a5 a6 = mopcs "flooper" ([Ar, Ar], [Kr, Kr, Ir, Ir, Ir, Ir]) [a1, a2, a3, a4, a5, a6]

{- |
Function-table-based crossfading looper.

This opcode implements a crossfading looper with variable loop parameters and three
  looping modes, optionally using a table for its crossfade shape. It accepts
  non-power-of-two tables for its source sounds, such as deferred-allocation
   GEN01 tables, with one or two channels.

> asig1[,asig2]  flooper2  kamp, kpitch, kloopstart, kloopend, kcrossfade, ifn \
>           [, istart, imode, ifenv, iskip]

csound doc: <https://csound.com/docs/manual/flooper2.html>
-}
flooper2 :: forall a. (Tuple a) => Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> a
flooper2 b1 b2 b3 b4 b5 b6 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unTab b6
  where
    f a1 a2 a3 a4 a5 a6 =
      mopcs
        "flooper2"
        ([Ar, Ar], [Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir])
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        ]

{- |
Collects all audio from all Fluidsynth engines in a performance

> aleft, aright  fluidAllOut

csound doc: <https://csound.com/docs/manual/fluidAllOut.html>
-}
fluidAllOut :: (Sig, Sig)
fluidAllOut =
  pureTuple $ return $ f
  where
    f = mopcs "fluidAllOut" ([Ar, Ar], []) []

{- |
Sends a MIDI controller data message to fluid.

Sends a MIDI controller data (MIDI controller number and value to use)
    message to a fluid engine by number on the user specified MIDI channel number.

>  fluidCCi  iEngineNumber, iChannelNumber, iControllerNumber, iValue

csound doc: <https://csound.com/docs/manual/fluidCCi.html>
-}
fluidCCi :: D -> D -> D -> D -> SE ()
fluidCCi b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "fluidCCi" [(Xr, [Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
Sends a MIDI controller data message to fluid.

Sends a MIDI controller data (MIDI controller number and value to use)
    message to a fluid engine by number on the user specified MIDI channel number.

>  fluidCCk  iEngineNumber, iChannelNumber, iControllerNumber, kValue

csound doc: <https://csound.com/docs/manual/fluidCCk.html>
-}
fluidCCk :: D -> D -> D -> Sig -> SE ()
fluidCCk b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unSig) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "fluidCCk" [(Xr, [Ir, Ir, Ir, Kr])] [a1, a2, a3, a4]

{- |
Sends MIDI note on, note off, and other messages to a SoundFont preset.

The fluid opcodes provide a simple
    Csound opcode wrapper around Peter Hanappe's Fluidsynth SoundFont2
    synthesizer. This implementation accepts any MIDI note on, note
    off, controller, pitch bend, or program change message at
    k-rate. Maximum polyphony is 4096 simultaneously sounding
    voices. Any number of SoundFonts may be loaded and played
    simultaneously.

>  fluidControl  ienginenum, kstatus, kchannel, \
>         kdata1, kdata2 [,imsgs]

csound doc: <https://csound.com/docs/manual/fluidControl.html>
-}
fluidControl :: D -> Sig -> Sig -> Sig -> Sig -> SE ()
fluidControl b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unSig) b2 <*> (lift . unSig) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "fluidControl" [(Xr, [Ir, Kr, Kr, Kr, Kr, Ir])] [a1, a2, a3, a4, a5]

{- |
Instantiates a fluidsynth engine.

Instantiates a fluidsynth engine, and returns ienginenum to identify the
    engine. ienginenum is passed to other other opcodes for loading
    and playing SoundFonts and gathering the generated sound.

> ienginenum  fluidEngine  [iChorusEnabled] [, iRevervEnabled] [, iNumChannels] [, iPolyphony]

csound doc: <https://csound.com/docs/manual/fluidEngine.html>
-}
fluidEngine :: D
fluidEngine =
  D $ return $ f
  where
    f = opcs "fluidEngine" [(Ir, [Ir, Ir, Ir, Ir])] []

--
-- > SPrograms[]  fluidInfo  ienginenum
--
-- csound doc: <https://csound.com/docs/manual/fluidInfo.html>
fluidInfo :: D -> Str
fluidInfo b1 =
  Str $ f <$> unD b1
  where
    f a1 = opcs "fluidInfo" [(Sr, [Ir])] [a1]

{- |
Loads a SoundFont into a fluidEngine, optionally listing SoundFont contents.

Loads a SoundFont into an instance of a fluidEngine, optionally
    listing banks and presets for SoundFont.

> isfnum  fluidLoad  soundfont, ienginenum[, ilistpresets]

csound doc: <https://csound.com/docs/manual/fluidLoad.html>
-}
fluidLoad :: D -> D -> Tab
fluidLoad b1 b2 =
  Tab $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "fluidLoad" [(Ir, [Ir, Ir, Ir])] [a1, a2]

{- |
Plays a note on a channel in a fluidSynth engine.

Plays a note at imidikey pitch and imidivel velocity
    on ichannelnum channel of number ienginenum fluidEngine.

>  fluidNote  ienginenum, ichannelnum, imidikey, imidivel

csound doc: <https://csound.com/docs/manual/fluidNote.html>
-}
fluidNote :: D -> D -> D -> D -> SE ()
fluidNote b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "fluidNote" [(Xr, [Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
Outputs sound from a given fluidEngine

Outputs the sound from a fluidEngine.

> aleft, aright  fluidOut  ienginenum

csound doc: <https://csound.com/docs/manual/fluidOut.html>
-}
fluidOut :: D -> (Sig, Sig)
fluidOut b1 =
  pureTuple $ f <$> unD b1
  where
    f a1 = mopcs "fluidOut" ([Ar, Ar], [Ir]) [a1]

{- |
Assigns a preset from a SoundFont to a channel on a fluidEngine.

>  fluidProgramSelect  ienginenum, ichannelnum, isfnum, ibanknum, ipresetnum

csound doc: <https://csound.com/docs/manual/fluidProgramSelect.html>
-}
fluidProgramSelect :: D -> D -> Tab -> D -> D -> SE ()
fluidProgramSelect b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unTab) b3 <*> (lift . unD) b4 <*> (lift . unD) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "fluidProgramSelect" [(Xr, [Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
Set interpolation method for channel in Fluid Engine

Set interpolation method for channel in Fluid Engine.  Lower
    order interpolation methods will render faster at lower fidelity while
    higher order interpolation methods will render slower at higher fidelity.
    Default interpolation for a channel is 4th order interpolation.

>  fluidSetInterpMethod  ienginenum, ichannelnum, iInterpMethod

csound doc: <https://csound.com/docs/manual/fluidSetInterpMethod.html>
-}
fluidSetInterpMethod :: D -> D -> D -> SE ()
fluidSetInterpMethod b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "fluidSetInterpMethod" [(Xr, [Ir, Ir, Ir])] [a1, a2, a3]

{- |
Read sampled sound from a table.

Read sampled sound (mono or stereo) from a table, with optional sustain and release looping.

> ar1 [,ar2]  loscil  xamp, kcps, ifn [, ibas] [, imod1] [, ibeg1] [, iend1] \
>         [, imod2] [, ibeg2] [, iend2]

csound doc: <https://csound.com/docs/manual/loscil.html>
-}
loscil :: forall a. (Tuple a) => Sig -> Sig -> Tab -> a
loscil b1 b2 b3 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
  where
    f a1 a2 a3 = mopcs "loscil" ([Ar, Ar], [Xr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir]) [a1, a2, a3]

--
-- > aph, ar1 [,ar2]  loscilphs  xamp, kcps, ifn [, ibas] [, imod1] [, ibeg1] [, iend1] \
-- >           [, imod2] [, ibeg2] [, iend2]
--
-- csound doc: <https://csound.com/docs/manual/loscil.html>
loscilphs :: forall a. (Tuple a) => Sig -> Sig -> Tab -> a
loscilphs b1 b2 b3 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
  where
    f a1 a2 a3 = mopcs "loscilphs" ([Ar, Ar, Ar], [Xr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir]) [a1, a2, a3]

{- |
Read sampled sound from a table using cubic interpolation.

Read sampled sound (mono or stereo) from a table, with optional sustain and release looping, using cubic interpolation.

> ar1 [,ar2]  loscil3  xamp, kcps, ifn [, ibas] [, imod1] [, ibeg1] [, iend1] \
>         [, imod2] [, ibeg2] [, iend2]

csound doc: <https://csound.com/docs/manual/loscil3.html>
-}
loscil3 :: forall a. (Tuple a) => Sig -> Sig -> Tab -> a
loscil3 b1 b2 b3 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
  where
    f a1 a2 a3 = mopcs "loscil3" ([Ar, Ar], [Xr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir]) [a1, a2, a3]

--
-- > aph, ar1 [,ar2]  loscil3phs  xamp, kcps, ifn [, ibas] [, imod1] [, ibeg1] [, iend1] \
-- >           [, imod2] [, ibeg2] [, iend2]
--
-- csound doc: <https://csound.com/docs/manual/loscil3.html>
loscil3phs :: forall a. (Tuple a) => Sig -> Sig -> Tab -> a
loscil3phs b1 b2 b3 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
  where
    f a1 a2 a3 = mopcs "loscil3phs" ([Ar, Ar, Ar], [Xr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir]) [a1, a2, a3]

{- |
Read multi-channel sampled sound from a table.

Read sampled sound (up to 16 channels) from a table, with
      optional sustain and release looping.

> ar1 [, ar2, ar3, ar4, ar5, ar6, ar7, ar8, ar9, ar10, ar11, ar12, ar13, ar14, \
>           ar15, ar16]  loscilx  xamp, kcps, ifn \
>         [, iwsize, ibas, istrt, imod, ibeg, iend]
> ar[]  loscilx  xamp, kcps, ifn \
>         [, iwsize, ibas, istrt, imod, ibeg, iend]

csound doc: <https://csound.com/docs/manual/loscilx.html>
-}
loscilx :: forall a. (Tuple a) => Sig -> Sig -> Tab -> a
loscilx b1 b2 b3 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
  where
    f a1 a2 a3 =
      mopcs
        "loscilx"
        ( [Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar]
        , [Xr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir]
        )
        [a1, a2, a3]

{- |
Generates a table index for sample playback

This opcode can be used to generate table index for sample playback (e.g. tablexkt).

> ares  lphasor  xtrns [, ilps] [, ilpe] [, imode] [, istrt] [, istor]

csound doc: <https://csound.com/docs/manual/lphasor.html>
-}
lphasor :: Sig -> Sig
lphasor b1 =
  Sig $ f <$> unSig b1
  where
    f a1 = opcs "lphasor" [(Ar, [Xr, Ir, Ir, Ir, Ir, Ir])] [a1]

{- |
Read sampled sound from a table with looping and high precision.

Read sampled sound (mono or stereo) from a table, with looping, and high precision.

> ares  lposcil  kamp, kfreqratio, kloop, kend, ifn [, iphs]

csound doc: <https://csound.com/docs/manual/lposcil.html>
-}
lposcil :: Sig -> Sig -> Sig -> Sig -> Tab -> Sig
lposcil b1 b2 b3 b4 b5 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unTab b5
  where
    f a1 a2 a3 a4 a5 = opcs "lposcil" [(Ar, [Kr, Kr, Kr, Kr, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
Read sampled sound from a table with high precision and cubic interpolation.

Read sampled sound (mono or stereo) from a table, with looping, and high precision. lposcil3 uses cubic interpolation.

> ares  lposcil3  kamp, kfreqratio, kloop, kend, ifn [, iphs]

csound doc: <https://csound.com/docs/manual/lposcil3.html>
-}
lposcil3 :: Sig -> Sig -> Sig -> Sig -> Tab -> Sig
lposcil3 b1 b2 b3 b4 b5 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unTab b5
  where
    f a1 a2 a3 a4 a5 = opcs "lposcil3" [(Ar, [Kr, Kr, Kr, Kr, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
Read sampled sound from a table with looping and high precision.

lposcila reads sampled sound from a table with looping and high precision.

> ar  lposcila  aamp, kfreqratio, kloop, kend, ift [,iphs]

csound doc: <https://csound.com/docs/manual/lposcila.html>
-}
lposcila :: Sig -> Sig -> Sig -> Sig -> D -> Sig
lposcila b1 b2 b3 b4 b5 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 = opcs "lposcila" [(Ar, [Ar, Kr, Kr, Kr, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
Read stereo sampled sound from a table with looping and high precision.

lposcilsa reads stereo sampled sound from a table with looping and high precision.

> ar1, ar2  lposcilsa  aamp, kfreqratio, kloop, kend, ift [,iphs]

csound doc: <https://csound.com/docs/manual/lposcilsa.html>
-}
lposcilsa :: Sig -> Sig -> Sig -> Sig -> D -> (Sig, Sig)
lposcilsa b1 b2 b3 b4 b5 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 = mopcs "lposcilsa" ([Ar, Ar], [Ar, Kr, Kr, Kr, Ir, Ir]) [a1, a2, a3, a4, a5]

{- |
Read stereo sampled sound from a table with looping and high precision.

lposcilsa2 reads stereo sampled sound from a table with looping and high precision.

> ar1, ar2  lposcilsa2  aamp, kfreqratio, kloop, kend, ift [,iphs]

csound doc: <https://csound.com/docs/manual/lposcilsa2.html>
-}
lposcilsa2 :: Sig -> Sig -> Sig -> Sig -> D -> (Sig, Sig)
lposcilsa2 b1 b2 b3 b4 b5 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 = mopcs "lposcilsa2" ([Ar, Ar], [Ar, Kr, Kr, Kr, Ir, Ir]) [a1, a2, a3, a4, a5]

{- |
Prints a list of all instruments of a previously loaded SoundFont2 (SF2) file.

Prints a list of all instruments of a previously loaded SoundFont2 (SF2) sample file. These opcodes allow management the sample-structure of SF2 files. In order to understand the usage of these opcodes, the user must have some knowledge of the SF2 format, so a brief description of this format can be found in the SoundFont2 File Format Appendix.

>  sfilist  ifilhandle [, Sprintprefix]

csound doc: <https://csound.com/docs/manual/sfilist.html>
-}
sfilist :: Sf -> SE ()
sfilist b1 =
  SE $ join $ f <$> (lift . unSf) b1
  where
    f a1 = opcsDep_ "sfilist" [(Xr, [Sr, Sr])] [a1]

{- |
Plays a SoundFont2 (SF2) sample instrument, generating a stereo sound.

Plays a SoundFont2 (SF2) sample instrument, generating a stereo sound. These opcodes allow management the sample-structure of SF2 files. In order to understand the usage of these opcodes, the user must have some knowledge of the SF2 format, so a brief description of this format can be found in the SoundFont2 File Format Appendix.

> ar1, ar2  sfinstr  ivel, inotenum, xamp, xfreq, instrnum, ifilhandle \
>           [, iflag] [, ioffset]

csound doc: <https://csound.com/docs/manual/sfinstr.html>
-}
sfinstr :: D -> D -> Sig -> Sig -> D -> Sf -> (Sig, Sig)
sfinstr b1 b2 b3 b4 b5 b6 =
  pureTuple $ f <$> unD b1 <*> unD b2 <*> unSig b3 <*> unSig b4 <*> unD b5 <*> unSf b6
  where
    f a1 a2 a3 a4 a5 a6 = mopcs "sfinstr" ([Ar, Ar], [Ir, Ir, Xr, Xr, Ir, Sr, Ir, Ir]) [a1, a2, a3, a4, a5, a6]

{- |
Plays a SoundFont2 (SF2) sample instrument, generating a stereo sound with cubic interpolation.

Plays a SoundFont2 (SF2) sample instrument, generating a stereo sound with cubic interpolation. These opcodes allow management the sample-structure of SF2 files. In order to understand the usage of these opcodes, the user must have some knowledge of the SF2 format, so a brief description of this format can be found in the SoundFont2 File Format Appendix.

> ar1, ar2  sfinstr3  ivel, inotenum, xamp, xfreq, instrnum, ifilhandle \
>           [, iflag] [, ioffset]

csound doc: <https://csound.com/docs/manual/sfinstr3.html>
-}
sfinstr3 :: D -> D -> Sig -> Sig -> D -> Sf -> (Sig, Sig)
sfinstr3 b1 b2 b3 b4 b5 b6 =
  pureTuple $ f <$> unD b1 <*> unD b2 <*> unSig b3 <*> unSig b4 <*> unD b5 <*> unSf b6
  where
    f a1 a2 a3 a4 a5 a6 = mopcs "sfinstr3" ([Ar, Ar], [Ir, Ir, Xr, Xr, Ir, Sr, Ir, Ir]) [a1, a2, a3, a4, a5, a6]

{- |
Plays a SoundFont2 (SF2) sample instrument, generating a mono sound with cubic interpolation.

Plays a SoundFont2 (SF2) sample instrument, generating a mono sound with cubic interpolation. These opcodes allow management the sample-structure of SF2 files. In order to understand the usage of these opcodes, the user must have some knowledge of the SF2 format, so a brief description of this format can be found in the SoundFont2 File Format Appendix.

> ares  sfinstr3m  ivel, inotenum, xamp, xfreq, instrnum, ifilhandle \
>           [, iflag] [, ioffset]

csound doc: <https://csound.com/docs/manual/sfinstr3m.html>
-}
sfinstr3m :: D -> D -> Sig -> Sig -> D -> Sf -> Sig
sfinstr3m b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unSig b3 <*> unSig b4 <*> unD b5 <*> unSf b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "sfinstr3m" [(Ar, [Ir, Ir, Xr, Xr, Ir, Sr, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
Plays a SoundFont2 (SF2) sample instrument, generating a mono sound.

Plays a SoundFont2 (SF2) sample instrument, generating a mono sound. These opcodes allow management the sample-structure of SF2 files. In order to understand the usage of these opcodes, the user must have some knowledge of the SF2 format, so a brief description of this format can be found in the SoundFont2 File Format Appendix.

> ares  sfinstrm  ivel, inotenum, xamp, xfreq, instrnum, ifilhandle \
>           [, iflag] [, ioffset]

csound doc: <https://csound.com/docs/manual/sfinstrm.html>
-}
sfinstrm :: D -> D -> Sig -> Sig -> D -> Sf -> Sig
sfinstrm b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unSig b3 <*> unSig b4 <*> unD b5 <*> unSf b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "sfinstrm" [(Ar, [Ir, Ir, Xr, Xr, Ir, Sr, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
Loads an entire SoundFont2 (SF2) sample file into memory.

Loads an entire SoundFont2 (SF2) sample file into memory. These opcodes allow management the sample-structure of SF2 files. In order to understand the usage of these opcodes, the user must have some knowledge of the SF2 format, so a brief description of this format can be found in the SoundFont2 File Format Appendix.

> ir  sfload  "filename"

csound doc: <https://csound.com/docs/manual/sfload.html>
-}
sfload :: Str -> D
sfload b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "sfload" [(Ir, [Sr])] [a1]

{- |
Plays a SoundFont2 (SF2) sample preset, generating a stereo sound, with user-defined
      time-varying crossfade looping.

Plays a SoundFont2 (SF2) sample preset, generating a stereo sound, similarly to sfplay. Unlike that opcode, though,
	  it ignores the looping points set in the SF2 file and substitutes them for a user-defined crossfade loop. It is
	  a cross between sfplay and
      flooper2.

> ar1, ar2  sflooper  ivel, inotenum, kamp, kpitch, ipreindex, kloopstart, kloopend, kcrossfade \
>           [, istart, imode, ifenv, iskip, iflag]

csound doc: <https://csound.com/docs/manual/sflooper.html>
-}
sflooper :: D -> D -> Sig -> Sig -> Sf -> Sig -> Sig -> Sig -> (Sig, Sig)
sflooper b1 b2 b3 b4 b5 b6 b7 b8 =
  pureTuple $ f <$> unD b1 <*> unD b2 <*> unSig b3 <*> unSig b4 <*> unSf b5 <*> unSig b6 <*> unSig b7 <*> unSig b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 =
      mopcs
        "sflooper"
        ( [Ar, Ar]
        , [Ir, Ir, Kr, Kr, Ir, Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir]
        )
        [a1, a2, a3, a4, a5, a6, a7, a8]

{- |
Assigns all presets of a SoundFont2 (SF2) sample file to a sequence of progressive index numbers.

Assigns all presets of a previously loaded SoundFont2 (SF2)
      sample file to a sequence of progressive index numbers. These
      opcodes allow management the sample-structure of SF2 files. In
      order to understand the usage of these opcodes, the user must
      have some knowledge of the SF2 format, so a brief description of
      this format can be found in
      the SoundFont2 File Format
      Appendix.

>  sfpassign  istartindex, ifilhandle[, imsgs]

csound doc: <https://csound.com/docs/manual/sfpassign.html>
-}
sfpassign :: D -> Sf -> SE ()
sfpassign b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unSf) b2
  where
    f a1 a2 = opcsDep_ "sfpassign" [(Xr, [Ir, Sr, Ir])] [a1, a2]

{- |
Plays a SoundFont2 (SF2) sample preset, generating a stereo sound.

Plays a SoundFont2 (SF2) sample preset, generating a stereo sound. These opcodes allow management the sample-structure of SF2 files. In order to understand the usage of these opcodes, the user must have some knowledge of the SF2 format, so a brief description of this format can be found in the SoundFont2 File Format Appendix.

> ar1, ar2  sfplay  ivel, inotenum, xamp, xfreq, ipreindex [, iflag] [, ioffset] [, ienv]

csound doc: <https://csound.com/docs/manual/sfplay.html>
-}
sfplay :: D -> D -> Sig -> Sig -> Sf -> (Sig, Sig)
sfplay b1 b2 b3 b4 b5 =
  pureTuple $ f <$> unD b1 <*> unD b2 <*> unSig b3 <*> unSig b4 <*> unSf b5
  where
    f a1 a2 a3 a4 a5 = mopcs "sfplay" ([Ar, Ar], [Ir, Ir, Xr, Xr, Ir, Ir, Ir, Ir]) [a1, a2, a3, a4, a5]

{- |
Plays a SoundFont2 (SF2) sample preset, generating a stereo sound with cubic interpolation.

Plays a SoundFont2 (SF2) sample preset, generating a stereo sound with cubic interpolation. These opcodes allow management the sample-structure of SF2 files. In order to understand the usage of these opcodes, the user must have some knowledge of the SF2 format, so a brief description of this format can be found in the SoundFont2 File Format Appendix.

> ar1, ar2  sfplay3  ivel, inotenum, xamp, xfreq, ipreindex [, iflag] [, ioffset] [, ienv]

csound doc: <https://csound.com/docs/manual/sfplay3.html>
-}
sfplay3 :: D -> D -> Sig -> Sig -> Sf -> (Sig, Sig)
sfplay3 b1 b2 b3 b4 b5 =
  pureTuple $ f <$> unD b1 <*> unD b2 <*> unSig b3 <*> unSig b4 <*> unSf b5
  where
    f a1 a2 a3 a4 a5 = mopcs "sfplay3" ([Ar, Ar], [Ir, Ir, Xr, Xr, Ir, Ir, Ir, Ir]) [a1, a2, a3, a4, a5]

{- |
Plays a SoundFont2 (SF2) sample preset, generating a mono sound with cubic interpolation.

Plays a SoundFont2 (SF2) sample preset, generating a mono sound with cubic interpolation. These opcodes allow management the sample-structure of SF2 files. In order to understand the usage of these opcodes, the user must have some knowledge of the SF2 format, so a brief description of this format can be found in the SoundFont2 File Format Appendix.

> ares  sfplay3m  ivel, inotenum, xamp, xfreq, ipreindex [, iflag] [, ioffset] [, ienv]

csound doc: <https://csound.com/docs/manual/sfplay3m.html>
-}
sfplay3m :: D -> D -> Sig -> Sig -> Sf -> Sig
sfplay3m b1 b2 b3 b4 b5 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unSig b3 <*> unSig b4 <*> unSf b5
  where
    f a1 a2 a3 a4 a5 = opcs "sfplay3m" [(Ar, [Ir, Ir, Xr, Xr, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
Plays a SoundFont2 (SF2) sample preset, generating a mono sound.

Plays a SoundFont2 (SF2) sample preset, generating a mono sound. These opcodes allow management the sample-structure of SF2 files. In order to understand the usage of these opcodes, the user must have some knowledge of the SF2 format, so a brief description of this format can be found in the SoundFont2 File Format Appendix.

> ares  sfplaym  ivel, inotenum, xamp, xfreq, ipreindex [, iflag] [, ioffset] [, ienv]

csound doc: <https://csound.com/docs/manual/sfplaym.html>
-}
sfplaym :: D -> D -> Sig -> Sig -> Sf -> Sig
sfplaym b1 b2 b3 b4 b5 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unSig b3 <*> unSig b4 <*> unSf b5
  where
    f a1 a2 a3 a4 a5 = opcs "sfplaym" [(Ar, [Ir, Ir, Xr, Xr, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
Prints a list of all presets of a SoundFont2 (SF2) sample file.

Prints a list of all presets of a previously loaded SoundFont2 (SF2) sample file. These opcodes allow management the sample-structure of SF2 files. In order to understand the usage of these opcodes, the user must have some knowledge of the SF2 format, so a brief description of this format can be found in the SoundFont2 File Format Appendix.

>  sfplist  ifilhandle

csound doc: <https://csound.com/docs/manual/sfplist.html>
-}
sfplist :: Sf -> SE ()
sfplist b1 =
  SE $ join $ f <$> (lift . unSf) b1
  where
    f a1 = opcsDep_ "sfplist" [(Xr, [Sr])] [a1]

{- |
Assigns an existing preset of a SoundFont2 (SF2) sample file to an index number.

Assigns an existing preset of a previously loaded SoundFont2 (SF2) sample file to an index number. These opcodes allow management the sample-structure of SF2 files. In order to understand the usage of these opcodes, the user must have some knowledge of the SF2 format, so a brief description of this format can be found in the SoundFont2 File Format Appendix.

> ir  sfpreset  iprog, ibank, ifilhandle, ipreindex

csound doc: <https://csound.com/docs/manual/sfpreset.html>
-}
sfpreset :: D -> D -> Sf -> Sf -> D
sfpreset b1 b2 b3 b4 =
  D $ f <$> unD b1 <*> unD b2 <*> unSf b3 <*> unSf b4
  where
    f a1 a2 a3 a4 = opcs "sfpreset" [(Ir, [Ir, Ir, Sr, Ir])] [a1, a2, a3, a4]

{- |
A sound looper with pitch control.

This opcode records input audio and plays it back in a loop with user-defined
   duration and crossfade time. It also allows the pitch of the loop to be controlled,
   including reversed playback.

> asig, krec  sndloop  ain, kpitch, ktrig, idur, ifad

csound doc: <https://csound.com/docs/manual/sndloop.html>
-}
sndloop :: Sig -> Sig -> Sig -> D -> D -> (Sig, Sig)
sndloop b1 b2 b3 b4 b5 =
  pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 = mopcs "sndloop" ([Ar, Kr], [Ar, Kr, Kr, Ir, Ir]) [a1, a2, a3, a4, a5]

{- |
A simple time stretch by repeating cycles.

> ares  waveset  ain, krep [, ilen]

csound doc: <https://csound.com/docs/manual/waveset.html>
-}
waveset :: Sig -> Sig -> Sig
waveset b1 b2 =
  Sig $ f <$> unSig b1 <*> unSig b2
  where
    f a1 a2 = opcs "waveset" [(Ar, [Ar, Kr, Ir])] [a1, a2]

-- Scanned Synthesis.

{- |
Copies from one table to another with a gain control.

This is is a variant of tablecopy, copying from one table to another, starting at ipos, and with a gain control. The number of points copied is determined by the length of the source. Other points are not changed. This opcode can be used to âhitâ a string in the scanned synthesis code.

>  scanhammer  isrc, idst, ipos, imult

csound doc: <https://csound.com/docs/manual/scanhammer.html>
-}
scanhammer :: D -> D -> D -> D -> SE ()
scanhammer b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "scanhammer" [(Xr, [Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

--
-- > kpos, kvel  scanmap  iscan, kamp, kvamp [, iwhich]
--
-- csound doc: <https://csound.com/docs/manual/scanmap.html>
scanmap :: D -> Sig -> Sig -> (Sig, Sig)
scanmap b1 b2 b3 =
  pureTuple $ f <$> unD b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = mopcs "scanmap" ([Kr, Kr], [Ir, Kr, Kr, Ir]) [a1, a2, a3]

{- |
Generate audio output using scanned synthesis.

> ares  scans  kamp, kfreq, ifn, id [, iorder]

csound doc: <https://csound.com/docs/manual/scans.html>
-}
scans :: Sig -> Sig -> Tab -> D -> Sig
scans b1 b2 b3 b4 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unTab b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "scans" [(Ar, [Kr, Kr, Ir, Ir, Ir])] [a1, a2, a3, a4]

--
-- >  scansmap  kpos, kvel, iscan, kamp, kvamp [, iwhich]
--
-- csound doc: <https://csound.com/docs/manual/scansmap.html>
scansmap :: Sig -> Sig -> D -> Sig -> Sig -> SE ()
scansmap b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unD) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "scansmap" [(Xr, [Kr, Kr, Ir, Kr, Kr, Ir])] [a1, a2, a3, a4, a5]

{- |
A simpler scanned synthesis implementation.

A simpler scanned synthesis implementation. This is an implementation of a circular string scanned using external tables. This opcode will allow direct modification and reading of values with the table opcodes.

> aout  scantable  kamp, kpch, ipos, imass, istiff, idamp, ivel

csound doc: <https://csound.com/docs/manual/scantable.html>
-}
scantable :: Sig -> Sig -> D -> D -> D -> D -> D -> Sig
scantable b1 b2 b3 b4 b5 b6 b7 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unD b6 <*> unD b7
  where
    f a1 a2 a3 a4 a5 a6 a7 = opcs "scantable" [(Ar, [Kr, Kr, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5, a6, a7]

{- |
Compute the waveform and the wavetable for use in scanned synthesis.

>  scanu  init, irate, ifndisplace,
>         ifnmass, ifnmatrix, ifncentr, ifndamp, kmass, kmtrxstiff, kcentr,
>         kdamp, ileft, iright, kpos, kdisplace, ain, idisp, id

csound doc: <https://csound.com/docs/manual/scanu.html>
-}
scanu :: D -> D -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig -> Sig -> Sig -> Sig -> D -> D -> Sig -> Sig -> Sig -> D -> D -> SE ()
scanu b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unTab) b3 <*> (lift . unTab) b4 <*> (lift . unTab) b5 <*> (lift . unTab) b6 <*> (lift . unTab) b7 <*> (lift . unSig) b8 <*> (lift . unSig) b9 <*> (lift . unSig) b10 <*> (lift . unSig) b11 <*> (lift . unD) b12 <*> (lift . unD) b13 <*> (lift . unSig) b14 <*> (lift . unSig) b15 <*> (lift . unSig) b16 <*> (lift . unD) b17 <*> (lift . unD) b18
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 =
      opcsDep_
        "scanu"
        [
          ( Xr
          , [Ir, Ir, Ir, Ir, Ir, Ir, Ir, Kr, Kr, Kr, Kr, Ir, Ir, Kr, Kr, Ar, Ir, Ir]
          )
        ]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        ]

--
-- >  scanu2  init, irate, ifndisplace,
-- >         ifnmass, ifnmatrix, ifncentr, ifndamp, kmass, kmtrxstiff, kcentr,
-- >         kdamp, ileft, iright, kpos, kdisplace, ain, idisp, id
--
-- csound doc: <https://csound.com/docs/manual/scanu2.html>
scanu2 :: D -> D -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig -> Sig -> Sig -> Sig -> D -> D -> Sig -> Sig -> Sig -> D -> D -> SE ()
scanu2 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unTab) b3 <*> (lift . unTab) b4 <*> (lift . unTab) b5 <*> (lift . unTab) b6 <*> (lift . unTab) b7 <*> (lift . unSig) b8 <*> (lift . unSig) b9 <*> (lift . unSig) b10 <*> (lift . unSig) b11 <*> (lift . unD) b12 <*> (lift . unD) b13 <*> (lift . unSig) b14 <*> (lift . unSig) b15 <*> (lift . unSig) b16 <*> (lift . unD) b17 <*> (lift . unD) b18
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 =
      opcsDep_
        "scanu2"
        [
          ( Xr
          , [Ir, Ir, Ir, Ir, Ir, Ir, Ir, Kr, Kr, Kr, Kr, Ir, Ir, Kr, Kr, Ar, Ir, Ir]
          )
        ]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        ]

{- |
Allows the position and velocity of a node in a scanned process to be read.

> kpos, kvel  xscanmap  iscan, kamp, kvamp [, iwhich]

csound doc: <https://csound.com/docs/manual/xscanmap.html>
-}
xscanmap :: D -> Sig -> Sig -> (Sig, Sig)
xscanmap b1 b2 b3 =
  pureTuple $ f <$> unD b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = mopcs "xscanmap" ([Kr, Kr], [Ir, Kr, Kr, Ir]) [a1, a2, a3]

{- |
Fast scanned synthesis waveform and the wavetable generator.

Experimental version of scans.  Allows much larger matrices and is faster and smaller but removes some (unused?) flexibility.  If liked, it will replace the older opcode as it is syntax compatible but extended.

> ares  xscans  kamp, kfreq, ifntraj, id [, iorder]

csound doc: <https://csound.com/docs/manual/xscans.html>
-}
xscans :: Sig -> Sig -> Tab -> D -> Sig
xscans b1 b2 b3 b4 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unTab b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "xscans" [(Ar, [Kr, Kr, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
Allows the position and velocity of a node in a scanned process to be read.

>  xscansmap  kpos, kvel, iscan, kamp, kvamp [, iwhich]

csound doc: <https://csound.com/docs/manual/xscansmap.html>
-}
xscansmap :: Sig -> Sig -> D -> Sig -> Sig -> SE ()
xscansmap b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unD) b3 <*> (lift . unSig) b4 <*> (lift . unSig) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "xscansmap" [(Xr, [Kr, Kr, Ir, Kr, Kr, Ir])] [a1, a2, a3, a4, a5]

{- |
Compute the waveform and the wavetable for use in scanned synthesis.

Experimental version of scanu. Allows much larger matrices and is faster and smaller but removes some (unused?) flexibility.  If liked, it will replace the older opcode as it is syntax compatible but extended.

>  xscanu  init, irate, ifndisplace, ifnmass, ifnmatrix, ifncentr, ifndamp, kmass, \
>           kmtrxstiff, kcentr, kdamp, ileft, iright, kpos, kdisplace, ain, idisp, id

csound doc: <https://csound.com/docs/manual/xscanu.html>
-}
xscanu :: D -> D -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig -> Sig -> Sig -> Sig -> D -> D -> Sig -> Sig -> Sig -> D -> D -> SE ()
xscanu b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unTab) b3 <*> (lift . unTab) b4 <*> (lift . unTab) b5 <*> (lift . unTab) b6 <*> (lift . unTab) b7 <*> (lift . unSig) b8 <*> (lift . unSig) b9 <*> (lift . unSig) b10 <*> (lift . unSig) b11 <*> (lift . unD) b12 <*> (lift . unD) b13 <*> (lift . unSig) b14 <*> (lift . unSig) b15 <*> (lift . unSig) b16 <*> (lift . unD) b17 <*> (lift . unD) b18
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 =
      opcsDep_
        "xscanu"
        [
          ( Xr
          , [Ir, Ir, Ir, Ir, Ir, Ir, Ir, Kr, Kr, Kr, Kr, Ir, Ir, Kr, Kr, Ar, Ir, Ir]
          )
        ]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        ]

-- STK Opcodes.

{- |
STKBandedWG uses banded waveguide techniques to model a variety of sounds.

This opcode uses banded waveguide techniques to model a variety of sounds, including bowed bars, glasses, and bowls.

> asignal  STKBandedWG  ifrequency, iamplitude, [kpress, kv1[, kmot, kv2[, klfo, kv3[, klfodepth, kv4[, kvel, kv5[, kstrk, kv6[, kinstr, kv7]]]]]]]

csound doc: <https://csound.com/docs/manual/STKBandedWG.html>
-}
stkBandedWG :: D -> D -> Sig
stkBandedWG b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKBandedWG" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STK Hammond-oid organ-like FM synthesis instrument.

> asignal  STKBeeThree  ifrequency, iamplitude, [kop4, kv1[, kop3, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKBeeThree.html>
-}
stkBeeThree :: D -> D -> Sig
stkBeeThree b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKBeeThree" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKBlowBotl uses a helmholtz resonator (biquad filter) with a polynomial jet excitation.

This opcode implements a helmholtz resonator (biquad filter) with a polynomial jet excitation (a la Cook).

> asignal  STKBlowBotl  ifrequency, iamplitude, [knoise, kv1[, klfo, kv2[, klfodepth, kv3[, kvol, kv4]]]]

csound doc: <https://csound.com/docs/manual/STKBlowBotl.html>
-}
stkBlowBotl :: D -> D -> Sig
stkBlowBotl b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKBlowBotl" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STK clarinet physical model with one register hole and one tonehole.

This opcode is based on the clarinet model, with the addition of a two-port register hole and a three-port dynamic tonehole implementation.

> asignal  STKBlowHole  ifrequency, iamplitude, [kreed, kv1[, knoise, kv2[, khole, kv3[, kreg, kv4[, kbreath, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKBlowHole.html>
-}
stkBlowHole :: D -> D -> Sig
stkBlowHole b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKBlowHole" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKBowed is a bowed string instrument.

STKBowed is a bowed string instrument, using a waveguide model.

> asignal  STKBowed  ifrequency, iamplitude, [kpress, kv1[, kpos, kv2[, klfo, kv3[, klfodepth, kv4[, kvol, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKBowed.html>
-}
stkBowed :: D -> D -> Sig
stkBowed b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKBowed" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKBrass is a simple brass instrument.

STKBrass uses a simple brass instrument waveguide model, a la Cook.

> asignal  STKBrass  ifrequency, iamplitude, [klip, kv1[, kslide, kv2[, klfo, kv3[, klfodepth, kv4[, kvol, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKBrass.html>
-}
stkBrass :: D -> D -> Sig
stkBrass b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKBrass" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKClarinet uses a simple clarinet physical model.

> asignal  STKClarinet  ifrequency, iamplitude, [kstiff, kv1[, knoise, kv2[, klfo, kv3[, klfodepth, kv4[, kbreath, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKClarinet.html>
-}
stkClarinet :: D -> D -> Sig
stkClarinet b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKClarinet" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKDrummer is a drum sampling synthesizer.

STKDrummer is a drum sampling synthesizer using raw waves and one-pole filters,
      The drum rawwave files are sampled at 22050 Hz, but will be appropriately interpolated for other sample rates.

> asignal  STKDrummer  ifrequency, iamplitude

csound doc: <https://csound.com/docs/manual/STKDrummer.html>
-}
stkDrummer :: D -> D -> Sig
stkDrummer b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKDrummer" [(Ar, [Ir, Ir])] [a1, a2]

{- |
STKFMVoices is a singing FM synthesis instrument.

STKFMVoices is a singing FM synthesis instrument. It has 3 carriers and a common modulator, also referred to as algorithm 6 of the TX81Z.

> asignal  STKFMVoices  ifrequency, iamplitude, [kvowel, kv1[, kspec, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKFMVoices.html>
-}
stkFMVoices :: D -> D -> Sig
stkFMVoices b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKFMVoices" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKFlute uses a simple flute physical model.

STKFlute uses a simple flute physical model. The jet model uses a polynomial, a la Cook.

> asignal  STKFlute  ifrequency, iamplitude, [kjet, kv1[, knoise, kv2[, klfo, kv3[, klfodepth, kv4[, kbreath, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKFlute.html>
-}
stkFlute :: D -> D -> Sig
stkFlute b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKFlute" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKHevyMetl produces metal sounds.

STKHevyMetl produces metal sounds, using FM synthesis.
      It uses 3 cascade operators with feedback modulation, also referred to as algorithm 3 of the TX81Z.

> asignal  STKHevyMetl  ifrequency, iamplitude, [kmod, kv1[, kcross, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKHevyMetl.html>
-}
stkHevyMetl :: D -> D -> Sig
stkHevyMetl b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKHevyMetl" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKMandolin produces mamdolin-like sounds.

STKMandolin produces mamdolin-like sounds, using "commuted synthesis" techniques to model a mandolin instrument.

> asignal  STKMandolin  ifrequency, iamplitude, [kbody, kv1[, kpos, kv2[, ksus, kv3[, kdetune, kv4[, kmic, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKMandolin.html>
-}
stkMandolin :: D -> D -> Sig
stkMandolin b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKMandolin" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKModalBar is a resonant bar instrument.

This opcode is a resonant bar instrument.It has a number of different struck bar instruments.

> asignal  STKModalBar  ifrequency, iamplitude, [khard, kv1[, kpos, kv2[, klfo, kv3[, klfodepth, kv4[, kmix, kv5[, kvol, kv6[, kinstr, kv7]]]]]]]

csound doc: <https://csound.com/docs/manual/STKModalBar.html>
-}
stkModalBar :: D -> D -> Sig
stkModalBar b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKModalBar" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKMoog produces moog-like swept filter sounds.

STKMoog produces moog-like swept filter sounds, using one attack wave, one looped wave, and an ADSR envelope and adds two sweepable formant filters.

> asignal  STKMoog  ifrequency, iamplitude, [kq, kv1[, krate, kv2[, klfo, kv3[, klfodepth, kv4[, kvol, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKMoog.html>
-}
stkMoog :: D -> D -> Sig
stkMoog b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKMoog" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKPercFlut is a percussive flute FM synthesis instrument.

STKPercFlut is a percussive flute FM synthesis instrument. The instrument uses an algorithm like the algorithm 4 of the TX81Z.

> asignal  STKPercFlut  ifrequency, iamplitude, [kmod, kv1[, kcross, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKPercFlut.html>
-}
stkPercFlut :: D -> D -> Sig
stkPercFlut b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKPercFlut" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKPlucked uses a plucked string physical model.

STKPlucked uses a plucked string physical model based on the Karplus-Strong algorithm.

> asignal  STKPlucked  ifrequency, iamplitude

csound doc: <https://csound.com/docs/manual/STKPlucked.html>
-}
stkPlucked :: D -> D -> Sig
stkPlucked b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKPlucked" [(Ar, [Ir, Ir])] [a1, a2]

{- |
STKResonate is a noise driven formant filter.

STKResonate is a noise driven formant filter. This instrument contains a noise source, which excites a biquad resonance filter, with volume controlled by an ADSR.

> asignal  STKResonate  ifrequency, iamplitude, [kfreq, kv1[, kpole, kv2[, knotch, kv3[, kzero, kv4[, kenv, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKResonate.html>
-}
stkResonate :: D -> D -> Sig
stkResonate b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKResonate" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STK Fender Rhodes-like electric piano FM synthesis instrument.

> asignal  STKRhodey  ifrequency, iamplitude, [kmod, kv1[, kcross, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKRhodey.html>
-}
stkRhodey :: D -> D -> Sig
stkRhodey b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKRhodey" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKSaxofony is a faux conical bore reed instrument.

STKSaxofony is a faux conical bore reed instrument.
      This opcode uses a "hybrid" digital waveguide instrument that can generate a variety of wind-like sounds. It has also been referred to as the "blowed string" model.
      The waveguide section is essentially that of a string, with one rigid and one lossy termination. The non-linear function is a reed table.
      The string can be "blown" at any point between the terminations, though just as with strings, it is impossible to excite the system at either end.
      If the excitation is placed at the string mid-point, the sound is that of a clarinet. At points closer to the "bridge", the sound is closer to that of a saxophone.

> asignal  STKSaxofony  ifrequency, iamplitude, [kstiff, kv1[, kapert, kv2[, kblow, kv3[, knoise, kv4[, klfo, kv5[, klfodepth, kv6[, kbreath, kv7]]]]]]]

csound doc: <https://csound.com/docs/manual/STKSaxofony.html>
-}
stkSaxofony :: D -> D -> Sig
stkSaxofony b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKSaxofony" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKShakers is an instrument that simulates environmental sounds or collisions of multiple independent sound producing objects.

STKShakers are a set of PhISEM and PhOLIES instruments:
      PhISEM (Physically Informed Stochastic Event Modeling) is an algorithmic approach for simulating collisions of multiple independent sound producing objects.
      It can simulate a Maraca, Sekere, Cabasa, Bamboo Wind Chimes, Water Drops, Tambourine, Sleighbells, and a Guiro. On http://soundlab.cs.princeton.edu/research/controllers/shakers/
PhOLIES (Physically-Oriented Library of Imitated Environmental Sounds) there is a similar approach for the synthesis of environmental sounds.
It simulates of breaking sticks, crunchy snow (or not), a wrench, sandpaper, and more..

> asignal  STKShakers  ifrequency, iamplitude, [kenerg, kv1[, kdecay, kv2[, kshake, kv3[, knum, kv4[, kres, kv5[, kinstr, kv6]]]]]]

csound doc: <https://csound.com/docs/manual/STKShakers.html>
-}
stkShakers :: D -> D -> Sig
stkShakers b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKShakers" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKSimple is a wavetable/noise instrument.

STKSimple is a wavetable/noise instrument.
      It combines a looped wave, a noise source, a biquad resonance filter, a one-pole filter, and an ADSR envelope to create some interesting sounds.

> asignal  STKSimple  ifrequency, iamplitude, [kpos, kv1[, kcross, kv2[, kenv, kv3[, kgain, kv4]]]]

csound doc: <https://csound.com/docs/manual/STKSimple.html>
-}
stkSimple :: D -> D -> Sig
stkSimple b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKSimple" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKSitar uses a plucked string physical model.

STKSitar uses a plucked string physical model based on the Karplus-Strong algorithm.

> asignal  STKSitar  ifrequency, iamplitude

csound doc: <https://csound.com/docs/manual/STKSitar.html>
-}
stkSitar :: D -> D -> Sig
stkSitar b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKSitar" [(Ar, [Ir, Ir])] [a1, a2]

{- |
STKStifKarp is a plucked stiff string instrument.

STKStifKarp is a plucked stiff string instrument.
      It a simple plucked string algorithm (Karplus Strong) with enhancements, including string stiffness and pluck position controls. The stiffness is modeled with allpass filters.

> asignal  STKStifKarp  ifrequency, iamplitude, [kpos, kv1[, ksus, kv2[, kstretch, kv3]]]

csound doc: <https://csound.com/docs/manual/STKStifKarp.html>
-}
stkStifKarp :: D -> D -> Sig
stkStifKarp b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKStifKarp" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKTubeBell is a  tubular bell (orchestral chime) FM synthesis instrument.

STKTubeBell is a tubular bell (orchestral chime) FM synthesis instrument.
      It uses two simple FM Pairs summed together, also referred to as algorithm 5 of the TX81Z.

> asignal  STKTubeBell  ifrequency, iamplitude, [kmod, kv1[, kcross, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKTubeBell.html>
-}
stkTubeBell :: D -> D -> Sig
stkTubeBell b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKTubeBell" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKVoicForm is a  four formant synthesis instrument.

STKVoicForm is a four formant synthesis instrument.
This instrument contains an excitation singing wavetable (looping wave with random and periodic vibrato, smoothing on frequency, etc.), excitation noise, and four sweepable complex resonances.
Measured formant data is included, and enough data is there to support either parallel or cascade synthesis. In the floating point case cascade synthesis is the most natural so that's what you'll find here.

> asignal  STKVoicForm  ifrequency, iamplitude, [kmix, kv1[, ksel, kv2[, klfo, kv3[, klfodepth, kv4[, kloud, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKVoicForm.html>
-}
stkVoicForm :: D -> D -> Sig
stkVoicForm b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKVoicForm" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKWhistle produces whistle sounds.

STKWhistle produces (police) whistle sounds. It uses a hybrid physical/spectral model of a police whistle (a la Cook).

> asignal  STKWhistle  ifrequency, iamplitude, [kmod, kv1[, knoise, kv2[, kfipfreq, kv3[, kfipgain, kv4[, kvol, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKWhistle.html>
-}
stkWhistle :: D -> D -> Sig
stkWhistle b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKWhistle" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

{- |
STKWurley simulates a Wurlitzer electric piano FM synthesis instrument.

STKWurley simulates a Wurlitzer electric piano FM synthesis instrument.
      It uses two simple FM Pairs summed together, also referred to as algorithm 5 of the TX81Z.

> asignal  STKWurley  ifrequency, iamplitude, [kmod, kv1[, kcross, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]

csound doc: <https://csound.com/docs/manual/STKWurley.html>
-}
stkWurley :: D -> D -> Sig
stkWurley b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "STKWurley" [(Ar, [Ir, Ir, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [a1, a2]

-- Table Access.

{- |
Accesses table values by incremental sampling.

> kres  oscil1  idel, kamp, idur [, ifn]

csound doc: <https://csound.com/docs/manual/oscil1.html>
-}
oscil1 :: D -> Sig -> D -> Sig
oscil1 b1 b2 b3 =
  Sig $ f <$> unD b1 <*> unSig b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "oscil1" [(Kr, [Ir, Kr, Ir, Ir])] [a1, a2, a3]

{- |
Accesses table values by incremental sampling with linear interpolation.

> kres  oscil1i  idel, kamp, idur [, ifn]

csound doc: <https://csound.com/docs/manual/oscil1i.html>
-}
oscil1i :: D -> Sig -> D -> Sig
oscil1i b1 b2 b3 =
  Sig $ f <$> unD b1 <*> unSig b2 <*> unD b3
  where
    f a1 a2 a3 = opcs "oscil1i" [(Kr, [Ir, Kr, Ir, Ir])] [a1, a2, a3]

{- |
Accesses table values by direct indexing.

> ares  ptable  andx, ifn [, ixmode] [, ixoff] [, iwrap]
> ires  ptable  indx, ifn [, ixmode] [, ixoff] [, iwrap]
> kres  ptable  kndx, ifn [, ixmode] [, ixoff] [, iwrap]

csound doc: <https://csound.com/docs/manual/ptable.html>
-}
ptable :: Sig -> Tab -> Sig
ptable b1 b2 =
  Sig $ f <$> unSig b1 <*> unTab b2
  where
    f a1 a2 =
      opcs
        "ptable"
        [(Ar, [Ar, Ir, Ir, Ir, Ir]), (Ir, [Ir, Ir, Ir, Ir, Ir]), (Kr, [Kr, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        ]

{- |
Accesses table values by direct indexing with cubic interpolation.

> ares  ptable3  andx, ifn [, ixmode] [, ixoff] [, iwrap]
> ires  ptable3  indx, ifn [, ixmode] [, ixoff] [, iwrap]
> kres  ptable3  kndx, ifn [, ixmode] [, ixoff] [, iwrap]

csound doc: <https://csound.com/docs/manual/ptable3.html>
-}
ptable3 :: Sig -> Tab -> Sig
ptable3 b1 b2 =
  Sig $ f <$> unSig b1 <*> unTab b2
  where
    f a1 a2 =
      opcs
        "ptable3"
        [(Ar, [Ar, Ir, Ir, Ir, Ir]), (Ir, [Ir, Ir, Ir, Ir, Ir]), (Kr, [Kr, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        ]

{- |
Accesses table values by direct indexing with linear interpolation.

> ares  ptablei  andx, ifn [, ixmode] [, ixoff] [, iwrap]
> ires  ptablei  indx, ifn [, ixmode] [, ixoff] [, iwrap]
> kres  ptablei  kndx, ifn [, ixmode] [, ixoff] [, iwrap]

csound doc: <https://csound.com/docs/manual/ptablei.html>
-}
ptablei :: Sig -> Tab -> Sig
ptablei b1 b2 =
  Sig $ f <$> unSig b1 <*> unTab b2
  where
    f a1 a2 =
      opcs
        "ptablei"
        [(Ar, [Ar, Ir, Ir, Ir, Ir]), (Ir, [Ir, Ir, Ir, Ir, Ir]), (Kr, [Kr, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        ]

{- |
Fast table opcodes.

Fast table opcodes. Faster than
    table and
    tablew because don't
    allow wrap-around and limit and don't check index validity. Have
    been implemented in order to provide fast access to
    arrays. Support non-power of two tables (can be generated by any
    GEN function by giving a negative length value).

> kr  tab  kndx, ifn[, ixmode]
> ar  tab  xndx, ifn[, ixmode]

csound doc: <https://csound.com/docs/manual/tab.html>
-}
tab :: Sig -> Tab -> Sig
tab b1 b2 =
  Sig $ f <$> unSig b1 <*> unTab b2
  where
    f a1 a2 = opcs "tab" [(Kr, [Kr, Ir, Ir]), (Ar, [Xr, Ir, Ir])] [a1, a2]

{- |
Fast table opcodes.

Fast table opcodes. Faster than
    table and
    tablew because don't
    allow wrap-around and limit and don't check index validity. Have
    been implemented in order to provide fast access to
    arrays. Support non-power of two tables (can be generated by any
    GEN function by giving a negative length value).

> ir  tab_i  indx, ifn[, ixmode]

csound doc: <https://csound.com/docs/manual/tab_i.html>
-}
tab_i :: D -> Tab -> D
tab_i b1 b2 =
  D $ f <$> unD b1 <*> unTab b2
  where
    f a1 a2 = opcs "tab_i" [(Ir, [Ir, Ir, Ir])] [a1, a2]

{- |
Accesses table values by direct indexing.

> ares  table  andx, ifn [, ixmode] [, ixoff] [, iwrap]
> ires  table  indx, ifn [, ixmode] [, ixoff] [, iwrap]
> kres  table  kndx, ifn [, ixmode] [, ixoff] [, iwrap]

csound doc: <https://csound.com/docs/manual/table.html>
-}
table :: (SigOrD a) => a -> Tab -> a
table b1 b2 =
  fromGE $ f <$> toGE b1 <*> unTab b2
  where
    f a1 a2 =
      opcs
        "table"
        [(Ar, [Ar, Ir, Ir, Ir, Ir]), (Ir, [Ir, Ir, Ir, Ir, Ir]), (Kr, [Kr, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        ]

{- |
Accesses table values by direct indexing with cubic interpolation.

> ares  table3  andx, ifn [, ixmode] [, ixoff] [, iwrap]
> ires  table3  indx, ifn [, ixmode] [, ixoff] [, iwrap]
> kres  table3  kndx, ifn [, ixmode] [, ixoff] [, iwrap]

csound doc: <https://csound.com/docs/manual/table3.html>
-}
table3 :: (SigOrD a) => a -> Tab -> a
table3 b1 b2 =
  fromGE $ f <$> toGE b1 <*> unTab b2
  where
    f a1 a2 =
      opcs
        "table3"
        [(Ar, [Ar, Ir, Ir, Ir, Ir]), (Ir, [Ir, Ir, Ir, Ir, Ir]), (Kr, [Kr, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        ]

{- |
Accesses table values by direct indexing with linear interpolation.

> ares  tablei  andx, ifn [, ixmode] [, ixoff] [, iwrap]
> ires  tablei  indx, ifn [, ixmode] [, ixoff] [, iwrap]
> kres  tablei  kndx, ifn [, ixmode] [, ixoff] [, iwrap]

csound doc: <https://csound.com/docs/manual/tablei.html>
-}
tablei :: (SigOrD a) => a -> Tab -> a
tablei b1 b2 =
  fromGE $ f <$> toGE b1 <*> unTab b2
  where
    f a1 a2 =
      opcs
        "tablei"
        [(Ar, [Ar, Ir, Ir, Ir, Ir]), (Ir, [Ir, Ir, Ir, Ir, Ir]), (Kr, [Kr, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        ]

{- |
Fast table opcodes.

Fast table opcodes. Faster than
    table and
    tablew because don't
    allow wrap-around and limit and don't check index validity. Have
    been implemented in order to provide fast access to
    arrays. Support non-power of two tables (can be generated by any
    GEN function by giving a negative length value).

>  tabw_i  isig, indx, ifn [,ixmode]

csound doc: <https://csound.com/docs/manual/tabw_i.html>
-}
tabw_i :: D -> D -> Tab -> SE ()
tabw_i b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unTab) b3
  where
    f a1 a2 a3 = opcsDep_ "tabw_i" [(Xr, [Ir, Ir, Ir, Ir])] [a1, a2, a3]

-- Wave Terrain Synthesis.

--
-- > aout  sterrain  kamp, kcps, kx, ky, krx, kry, krot, ktab0, ktab1, km1, km2, kn1, kn2, kn3, ka, kb, kperiod
-- >
--
-- csound doc: <https://csound.com/docs/manual/sterrain.html>
sterrain :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
sterrain b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7 <*> unTab b8 <*> unTab b9 <*> unSig b10 <*> unSig b11 <*> unSig b12 <*> unSig b13 <*> unSig b14 <*> unSig b15 <*> unSig b16 <*> unSig b17
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 =
      opcs
        "sterrain"
        [
          ( Ar
          , [Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr]
          )
        ]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        ]

{- |
A simple wave-terrain synthesis opcode.

> aout  wterrain  kamp, kpch, k_xcenter, k_ycenter, k_xradius, k_yradius, \
>           itabx, itaby

csound doc: <https://csound.com/docs/manual/wterrain.html>
-}
wterrain :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> D -> Sig
wterrain b1 b2 b3 b4 b5 b6 b7 b8 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unD b7 <*> unD b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 =
      opcs
        "wterrain"
        [(Ar, [Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        ]

--
-- > aout  wterrain2  kamp, kcps, kx, ky, krx, kry, krot, \
-- >           ktab0, ktab1, kcurve, kcurveparam
-- >
--
-- csound doc: <https://csound.com/docs/manual/wterrain2.html>
wterrain2 :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> Sig -> Sig -> Sig
wterrain2 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6 <*> unSig b7 <*> unTab b8 <*> unTab b9 <*> unSig b10 <*> unSig b11
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
      opcs
        "wterrain2"
        [
          ( Ar
          , [Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr]
          )
        ]
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11]

-- Waveguide Physical Modeling.

{- |
Produces a naturally decaying plucked string or drum sound.

Audio output is a naturally decaying plucked string or drum sound based on the Karplus-Strong algorithms.

> ares  pluck  kamp, kcps, icps, ifn, imeth [, iparm1] [, iparm2]

csound doc: <https://csound.com/docs/manual/pluck.html>
-}
pluck :: Sig -> Sig -> D -> Tab -> D -> Sig
pluck b1 b2 b3 b4 b5 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unTab b4 <*> unD b5
  where
    f a1 a2 a3 a4 a5 = opcs "pluck" [(Ar, [Kr, Kr, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
Physical model of the plucked string.

repluck is an implementation of the physical model of the plucked string. A user can control the pluck point, the pickup point, the filter, and an additional audio signal, axcite. axcite is used to excite the 'string'. Based on the Karplus-Strong algorithm.

> ares  repluck  iplk, kamp, icps, kpick, krefl, axcite

csound doc: <https://csound.com/docs/manual/repluck.html>
-}
repluck :: D -> Sig -> D -> Sig -> Sig -> Sig -> Sig
repluck b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unD b1 <*> unSig b2 <*> unD b3 <*> unSig b4 <*> unSig b5 <*> unSig b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "repluck" [(Ar, [Ir, Kr, Ir, Kr, Kr, Ar])] [a1, a2, a3, a4, a5, a6]

{- |
A string resonator with variable fundamental frequency.

An audio signal is modified by a string resonator with variable fundamental frequency.

> ares  streson  asig, kfr, kfdbgain

csound doc: <https://csound.com/docs/manual/streson.html>
-}
streson :: Sig -> Sig -> Sig -> Sig
streson b1 b2 b3 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "streson" [(Ar, [Ar, Kr, Kr])] [a1, a2, a3]

{- |
Creates a tone similar to a bowed string.

Audio output is a tone similar to a bowed string, using a physical model developed from Perry Cook, but re-coded for Csound.

> ares  wgbow  kamp, kfreq, kpres, krat, kvibf, kvamp \
>         [, ifn] [, iminfreq]

csound doc: <https://csound.com/docs/manual/wgbow.html>
-}
wgbow :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
wgbow b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unSig b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "wgbow" [(Ar, [Kr, Kr, Kr, Kr, Kr, Kr, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
A physical model of a bowed bar.

A physical model of a bowed bar, belonging to the Perry Cook family of waveguide instruments.

> ares  wgbowedbar  kamp, kfreq, kpos, kbowpres, kgain [, iconst] [, itvel] \
>           [, ibowpos] [, ilow]

csound doc: <https://csound.com/docs/manual/wgbowedbar.html>
-}
wgbowedbar :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
wgbowedbar b1 b2 b3 b4 b5 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5
  where
    f a1 a2 a3 a4 a5 = opcs "wgbowedbar" [(Ar, [Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
Creates a tone related to a brass instrument.

Audio output is a tone related to a brass instrument, using a physical model developed from Perry Cook, but re-coded for Csound.

> ares  wgbrass  kamp, kfreq, ktens, iatt, kvibf, kvamp \
>         [, ifn] [, iminfreq]

csound doc: <https://csound.com/docs/manual/wgbrass.html>
-}
wgbrass :: Sig -> Sig -> Sig -> D -> Sig -> Sig -> Sig
wgbrass b1 b2 b3 b4 b5 b6 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unSig b5 <*> unSig b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "wgbrass" [(Ar, [Kr, Kr, Kr, Ir, Kr, Kr, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
Creates a tone similar to a clarinet.

Audio output is a tone similar to a clarinet, using a physical model developed from Perry Cook, but re-coded for Csound.

> ares  wgclar  kamp, kfreq, kstiff, \
>         iatt, idetk, kngain, kvibf, kvamp [, ifn] [, iminfreq]

csound doc: <https://csound.com/docs/manual/wgclar.html>
-}
wgclar :: Sig -> Sig -> Sig -> D -> D -> Sig -> Sig -> Sig -> Sig
wgclar b1 b2 b3 b4 b5 b6 b7 b8 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unD b5 <*> unSig b6 <*> unSig b7 <*> unSig b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 =
      opcs
        "wgclar"
        [(Ar, [Kr, Kr, Kr, Ir, Ir, Kr, Kr, Kr, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        ]

{- |
Creates a tone similar to a flute.

Audio output is a tone similar to a flute, using a physical model developed from Perry Cook, but re-coded for Csound.

> ares  wgflute  kamp, kfreq, kjet, iatt,
>         idetk, kngain, kvibf, kvamp [, ifn] [, iminfreq] [, ijetrf] [, iendrf]

csound doc: <https://csound.com/docs/manual/wgflute.html>
-}
wgflute :: Sig -> Sig -> Sig -> D -> D -> Sig -> Sig -> Sig -> Sig
wgflute b1 b2 b3 b4 b5 b6 b7 b8 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unD b5 <*> unSig b6 <*> unSig b7 <*> unSig b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 =
      opcs
        "wgflute"
        [(Ar, [Kr, Kr, Kr, Ir, Ir, Kr, Kr, Kr, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        ]

{- |
A high fidelity simulation of a plucked string.

A high fidelity simulation of a plucked string, using interpolating delay-lines.

> ares  wgpluck  icps, iamp, kpick, iplk, idamp, ifilt, axcite

csound doc: <https://csound.com/docs/manual/wgpluck.html>
-}
wgpluck :: D -> D -> Sig -> D -> D -> D -> Sig -> Sig
wgpluck b1 b2 b3 b4 b5 b6 b7 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unSig b3 <*> unD b4 <*> unD b5 <*> unD b6 <*> unSig b7
  where
    f a1 a2 a3 a4 a5 a6 a7 = opcs "wgpluck" [(Ar, [Ir, Ir, Kr, Ir, Ir, Ir, Ar])] [a1, a2, a3, a4, a5, a6, a7]

{- |
Physical model of the plucked string.

wgpluck2 is an implementation of the physical model of the plucked string, with control over the pluck point, the pickup point and the filter. Based on the Karplus-Strong algorithm.

> ares  wgpluck2  iplk, kamp, icps, kpick, krefl

csound doc: <https://csound.com/docs/manual/wgpluck2.html>
-}
wgpluck2 :: D -> Sig -> D -> Sig -> Sig -> Sig
wgpluck2 b1 b2 b3 b4 b5 =
  Sig $ f <$> unD b1 <*> unSig b2 <*> unD b3 <*> unSig b4 <*> unSig b5
  where
    f a1 a2 a3 a4 a5 = opcs "wgpluck2" [(Ar, [Ir, Kr, Ir, Kr, Kr])] [a1, a2, a3, a4, a5]
