{-# LANGUAGE InstanceSigs #-}

{- | Essential opcodes. Top 100 opcodes.
See the package csound-expression-opcodes for full list of Csound opcodes
-}
module Csound.Core.Opcode (
  -- * Oscillators / Phasors
  poscil,
  poscil3,
  oscil,
  oscil3,
  vco2,
  foscili,
  -- , vcoInit, VcoInit (..), VcoShape (..)
  -- , osc
  -- , oscilikt, sqr, tri, saw, vcoTab
  buzz,
  gbuzz,
  mpulse,
  phasor,

  -- * Modifiers
  integ,
  dcblock,
  diff,

  -- * Random generators
  rand,
  rnd,
  birnd,
  random,
  randomi,
  randomh,
  seed,
  gauss,
  pinkish,
  noise,
  randi,
  randh,
  dust,
  dust2,
  gausstrig,

  -- * Envelopes
  linen,
  linenr,
  madsr,
  adsr,
  mxadsr,

  -- * Line generators
  linseg,
  expseg,
  linsegr,
  expsegr,
  rspline,
  lpshold,
  loopseg,
  loopxseg,
  lineto,

  -- * Line Smooth
  lag,
  lagud,

  -- * Sound Files / Samples
  diskin,
  diskin2,
  mp3in,
  loscil,
  loscil3,
  loscilx,
  lphasor,
  flooper,
  flooper2,
  filescal,
  mincer,
  filelen,
  filesr,
  bbcutm,
  bbcuts,

  -- * Audio I/O

  -- for outs and ins use functions writeOuts and readIns
  monitor,
  inch,
  outch,
  outs,

  -- * Tables (Buffers)
  table,
  tablei,
  table3,
  tablew,

  -- ** Read-only tables (pure)
  getTable,
  getTablei,
  getTable3,
  -- TODO
  -- ftgen, ftsamplebank

  -- * Arrays - check the list

  -- * Program Control
  changed,
  changed2,
  trigger,

  -- * Time
  metro,
  metro2,
  timeinsts,
  {-
  -- * Channels
  , Chan, newChan, getChanName
  , chnmix, chnclear
  -}

  -- * MIDI
  massign,
  pgmassign,
  midiin,
  notnum,
  veloc,
  release,
  ampmidi,
  cpsmidi,
  cpstmid,
  midictrl,
  ctrlinit,
  CtrlInit (..),
  ctrl7,
  ctrl14,
  initc7,
  initc14,

  -- * Panning / Spatialization
  pan2,
  vbap,
  vbaplsinit,

  -- * Reverb
  freeverb,
  reverbsc,

  -- * Dynamic processing
  compress,
  compress2,
  dam,

  -- * Spectral Processing
  pvsanal,
  pvsynth,
  pvscale,
  tradsyn,
  pvshift,
  pvsifd,
  trcross,
  partials,

  -- * Convolution
  pconvolve,

  -- * Physical Models
  pluck,

  -- * Delay
  vdelayx,
  comb,
  vcomb,

  -- * Distortion
  distort,
  distort1,
  powershape,

  -- * Filter
  tone,
  atone,
  reson,
  butlp,
  buthp,
  butbp,
  butbr,
  mode,
  zdfLadder,
  moogvcf,
  moogvcf2,
  k35_lpf,
  k35_hpf,
  pareq,
  bqrez,
  diode_ladder,
  zdf_ladder,
  zdf_1pole,
  zdf_1pole_mode,
  zdf_2pole,
  zdf_2pole_mode,
  statevar,
  svfilter,
  tbvcf,
  clfilt,
  lowres,
  vlowres,
  rezzy,
  moogladder,
  lpf18,
  areson,
  mvchpf,
  mvclpf1,
  mvclpf2,
  mvclpf3,
  mvclpf4,
  portk,

  -- * Level
  rms,
  balance,
  balance2,

  -- * Math / Conversion
  ampdb,
  dbamp,
  ampdbfs,
  dbfsamp,
  dbfs,
  gainslider,
  expcurve,
  scale,
  cent,
  semitone,
  downsamp,

  -- * Amplitude / Pitch Tracking
  follow,
  follow2,
  ptrack,

  -- * Print
  printi,
  printk,
  prints,
  printks,
  printk2,
  fprint,
  print',

  -- * File IO
  fout,
  ftsave,
  fprints,
  readf,
  readfi,

  -- * Signal Type Conversion

  -- * schedule instruments
  eventD,
  eventStr,
) where

import Csound.Core.Types

-- import Csound.Core.Opcodes.Instr
-- import Csound.Core.Opcodes.Osc
-- import Csound.Core.Opcodes.Vco
import Csound.Core.Render.Options (setMa)

----------------------------------------------------------------------------------
-- Oscillators / Phasors

{- |
High precision oscillator.

> ares  poscil  aamp, acps [, ifn, iphs]
> ares  poscil  aamp, kcps [, ifn, iphs]
> ares  poscil  kamp, acps [, ifn, iphs]
> ares  poscil  kamp, kcps [, ifn, iphs]
> ires  poscil  kamp, kcps [, ifn, iphs]
> kres  poscil  kamp, kcps [, ifn, iphs]

csound doc: <http://csound.com/docs/manual/poscil.html>
-}
poscil :: Sig -> Sig -> Tab -> Sig
poscil b1 b2 b3 = liftOpc "poscil" rates (b1, b2, b3)
  where
    rates =
      [ (Ar, [Ar, Ar, Ir, Ir])
      , (Ar, [Ar, Kr, Ir, Ir])
      , (Ar, [Kr, Ar, Ir, Ir])
      , (Ar, [Kr, Kr, Ir, Ir])
      , (Ir, [Kr, Kr, Ir, Ir])
      , (Kr, [Kr, Kr, Ir, Ir])
      ]

{- |
High precision oscillator with cubic interpolation.

> ares  poscil3  aamp, acps [, ifn, iphs]
> ares  poscil3  aamp, kcps [, ifn, iphs]
> ares  poscil3  kamp, acps [, ifn, iphs]
> ares  poscil3  kamp, kcps [, ifn, iphs]
> ires  poscil3  kamp, kcps [, ifn, iphs]
> kres  poscil3  kamp, kcps [, ifn, iphs]

csound doc: <http://csound.com/docs/manual/poscil3.html>
-}
poscil3 :: Sig -> Sig -> Tab -> Sig
poscil3 b1 b2 b3 = liftOpc "poscil3" rates (b1, b2, b3)
  where
    rates =
      [ (Ar, [Ar, Ar, Ir, Ir])
      , (Ar, [Ar, Kr, Ir, Ir])
      , (Ar, [Kr, Ar, Ir, Ir])
      , (Ar, [Kr, Kr, Ir, Ir])
      , (Ir, [Kr, Kr, Ir, Ir])
      , (Kr, [Kr, Kr, Ir, Ir])
      ]

{-
osc :: Sig -> Sig
osc cps = poscil3 1 cps (sines [1])
-}

{- |
A simple oscillator.

oscil reads table ifn sequentially and repeatedly at a frequency xcps. The amplitude is scaled by xamp.

> ares  oscil  xamp, xcps [, ifn, iphs]
> kres  oscil  kamp, kcps [, ifn, iphs]

csound doc: <http://csound.com/docs/manual/oscil.html>
-}
oscil :: Sig -> Sig -> Tab -> Sig
oscil amp cps t = liftOpc "oscil" [(Ar, [Xr, Xr, Ir, Ir]), (Kr, [Kr, Kr, Ir, Ir])] (amp, cps, t)

{- |
A simple oscillator with cubic interpolation.

oscil3 reads table ifn sequentially and repeatedly at a frequency xcps. The amplitude is scaled by xamp.

> ares  oscil3  xamp, xcps [, ifn, iphs]
> kres  oscil3  kamp, kcps [, ifn, iphs]

csound doc: <http://csound.com/docs/manual/oscil.html>
-}
oscil3 :: Sig -> Sig -> Tab -> Sig
oscil3 amp cps t = liftOpc "oscil3" [(Ar, [Xr, Xr, Ir, Ir]), (Kr, [Kr, Kr, Ir, Ir])] (amp, cps, t)

-- Dynamic Spectrum Oscillators.

{- |
Output is a set of harmonically related sine partials.

> ares  buzz  xamp, xcps, knh, ifn [, iphs]

csound doc: <http://csound.com/docs/manual/buzz.html>
-}
buzz :: Sig -> Sig -> Sig -> Tab -> Sig
buzz b1 b2 b3 b4 = liftOpc "buzz" rates (b1, b2, b3, b4)
  where
    rates = [(Ar, [Xr, Xr, Kr, Ir, Ir])]

{- |
Output is a set of harmonically related cosine partials.

> ares  gbuzz  xamp, xcps, knh, klh, kmul, ifn [, iphs]

csound doc: <http://csound.com/docs/manual/gbuzz.html>
-}
gbuzz :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig
gbuzz b1 b2 b3 b4 b5 b6 = liftOpc "gbuzz" rates (b1, b2, b3, b4, b5, b6)
  where
    rates = [(Ar, [Xr, Xr, Kr, Kr, Kr, Ir, Ir])]

{- |
Generates a set of impulses.

Generates a set of impulses of amplitude kamp separated by kintvl seconds (or samples if kintvl is negative).  The first impulse is generated after a delay of ioffset seconds.

> ares  mpulse  kamp, kintvl [, ioffset]

csound doc: <http://csound.com/docs/manual/mpulse.html>
-}
mpulse :: Sig -> Sig -> Sig
mpulse b1 b2 = liftOpc "mpulse" rates (b1, b2)
  where
    rates = [(Ar, [Kr, Kr, Ir])]

{- |
Produce a normalized moving phase value.

> ares  phasor  xcps [, iphs]
> kres  phasor  kcps [, iphs]

csound doc: <http://csound.com/docs/manual/phasor.html>
-}
phasor :: Sig -> Sig
phasor b1 = liftOpc "phasor" rates b1
  where
    rates = [(Ar, [Xr, Ir]), (Kr, [Kr, Ir])]

----------------------------------------------------------------------------------
-- random generators

{- |
Generates a controlled random number series.

Output is a controlled random number series between -amp and +amp

> ares  rand  xamp [, iseed] [, isel] [, ioffset]
> kres  rand  xamp [, iseed] [, isel] [, ioffset]

csound doc: <http://csound.com/docs/manual/rand.html>
-}
rand :: Sig -> SE Sig
rand b1 = liftOpcDep "rand" rates b1
  where
    rates = [(Ar, [Xr, Ir, Ir, Ir]), (Kr, [Xr, Ir, Ir, Ir])]

{- |
Returns a random number in a unipolar range at the rate given by the input argument.

>  rnd (x) (init- or control-rate only)

csound doc: <http://csound.com/docs/manual/rnd.html>
-}
rnd :: (SigOrD a) => a -> SE a
rnd b1 = liftOpr1kDep "rnd" b1

{- |
Returns a random number in a unipolar range at the rate given by the input argument.

>  rnd (x) (init- or control-rate only)

csound doc: <http://csound.com/docs/manual/rnd.html>
-}
birnd :: (SigOrD a) => a -> SE a
birnd b1 = liftOpr1kDep "birnd" b1

{- |
Generates a controlled pseudo-random number series between min and max values.

Generates is a controlled pseudo-random number series between min and max values.

> ares  random  kmin, kmax
> ires  random  imin, imax
> kres  random  kmin, kmax

csound doc: <http://csound.com/docs/manual/random.html>
-}
random :: (SigOrD a) => a -> a -> SE a
random b1 b2 = liftOpcDep "random" rates (b1, b2)
  where
    rates = [(Ar, [Kr, Kr]), (Ir, [Ir, Ir]), (Kr, [Kr, Kr])]

{- |
Generates random numbers with a user-defined limit and holds them for a period of time.

> ares  randomh  kmin, kmax, xcps [,imode] [,ifirstval]
> kres  randomh  kmin, kmax, kcps [,imode] [,ifirstval]

csound doc: <http://csound.com/docs/manual/randomh.html>
-}
randomh :: Sig -> Sig -> Sig -> SE Sig
randomh b1 b2 b3 = liftOpcDep "randomh" rates (b1, b2, b3)
  where
    rates = [(Ar, [Kr, Kr, Xr, Ir, Ir]), (Kr, [Kr, Kr, Kr, Ir, Ir])]

{- |
Generates a user-controlled random number series with interpolation between each new number.

> ares  randomi  kmin, kmax, xcps [,imode] [,ifirstval]
> kres  randomi  kmin, kmax, kcps [,imode] [,ifirstval]

csound doc: <http://csound.com/docs/manual/randomi.html>
-}
randomi :: Sig -> Sig -> Sig -> SE Sig
randomi b1 b2 b3 = liftOpcDep "randomi" rates (b1, b2, b3)
  where
    rates = [(Ar, [Kr, Kr, Xr, Ir, Ir]), (Kr, [Kr, Kr, Kr, Ir, Ir])]

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
  liftOpcDep "gauss" rates b1
  where
    rates =
      [ (Ar, [Kr])
      , (Ir, [Ir])
      , (Kr, [Kr])
      , (Ar, [Kr, Kr])
      , (Ir, [Ir, Ir])
      , (Kr, [Kr, Kr])
      ]

{- |
Sets the global seed value.

Sets the global seed value for all x-class noise generators, as well as other opcodes that use a random call, such as grain.

>  seed  ival

csound doc: <http://csound.com/docs/manual/seed.html>
-}
seed :: D -> SE ()
seed b1 = global $ liftOpcDep_ "seed" rates b1
  where
    rates = [(Xr, [Ir])]

----------------------------------------------------------------------------------
-- line generators

{- |
Applies a straight line rise and decay pattern to an input amp signal.

linen -- apply a straight line rise and decay pattern to an input amp signal.

> ares  linen  xamp, irise, idur, idec
> kres  linen  kamp, irise, idur, idec

csound doc: <http://csound.com/docs/manual/linen.html>
-}
linen :: Sig -> D -> D -> D -> Sig
linen b1 b2 b3 b4 = liftOpc "linen" rates (b1, b2, b3, b4)
  where
    rates = [(Ar, [Xr, Ir, Ir, Ir]), (Kr, [Kr, Ir, Ir, Ir])]

{- |
The linen opcode extended with a final release segment.

linenr -- same as linen except that the final segment is entered only on sensing a MIDI note release. The note is then extended by the decay time.

> ares  linenr  xamp, irise, idec, iatdec
> kres  linenr  kamp, irise, idec, iatdec

csound doc: <http://csound.com/docs/manual/linenr.html>
-}
linenr :: Sig -> D -> D -> D -> Sig
linenr b1 b2 b3 b4 = liftOpc "linenr" rates (b1, b2, b3, b4)
  where
    rates = [(Ar, [Xr, Ir, Ir, Ir]), (Kr, [Kr, Ir, Ir, Ir])]

{- |
Calculates the classical ADSR envelope using the linsegr mechanism.

> ares  madsr  iatt, idec, islev, irel [, idel] [, ireltim]
> kres  madsr  iatt, idec, islev, irel [, idel] [, ireltim]

csound doc: <http://csound.com/docs/manual/madsr.html>
-}
madsr :: D -> D -> D -> D -> Sig
madsr b1 b2 b3 b4 = liftOpc "madsr" rates (b1, b2, b3, b4)
  where
    rates = [(Ar, [Ir, Ir, Ir, Ir, Ir, Ir]), (Kr, [Ir, Ir, Ir, Ir, Ir, Ir])]

{- |
Calculates the classical ADSR envelope using linear segments.

> ares  adsr  iatt, idec, islev, irel [, idel]
> kres  adsr  iatt, idec, islev, irel [, idel]

csound doc: <http://csound.com/docs/manual/adsr.html>
-}
adsr :: D -> D -> D -> D -> Sig
adsr b1 b2 b3 b4 = liftOpc "adsr" rates (b1, b2, b3, b4)
  where
    rates = [(Ar, [Ir, Ir, Ir, Ir, Ir]), (Kr, [Ir, Ir, Ir, Ir, Ir])]

----------------------------------------------------------------------------------
-- line generators

{- |
Trace a series of line segments between specified points.

> ares  linseg  ia, idur1, ib [, idur2] [, ic] [...]
> kres  linseg  ia, idur1, ib [, idur2] [, ic] [...]

csound doc: <http://csound.com/docs/manual/linseg.html>
-}
linseg :: [D] -> Sig
linseg bs = setRate Kr $ liftOpc "linseg" [(Kr, repeat Ir), (Ar, repeat Ir)] (bs ++ [1, last bs])

{- |
Trace a series of exponential segments between specified points.

> ares  expseg  ia, idur1, ib [, idur2] [, ic] [...]
> kres  expseg  ia, idur1, ib [, idur2] [, ic] [...]

csound doc: <http://csound.com/docs/manual/linseg.html>
-}
expseg :: [D] -> Sig
expseg bs = setRate Kr $ liftOpc "expseg" [(Kr, repeat Ir), (Ar, repeat Ir)] (bs ++ [1, last bs])

{- |
Trace a series of line segments between specified points including a release segment.

> ares  linsegr  ia, idur1, ib [, idur2] [, ic] [...], irel, iz
> kres  linsegr  ia, idur1, ib [, idur2] [, ic] [...], irel, iz

csound doc: <http://csound.com/docs/manual/linsegr.html>
-}
linsegr :: [D] -> D -> D -> Sig
linsegr b1 b2 b3 = setRate Kr $ liftOpc "linsegr" [(Kr, repeat Ir), (Ar, repeat Ir)] (b1 ++ [1, last b1], b2, b3)

{- |
Trace a series of exponential segments between specified points including a release segment.

> ares  expsegr  ia, idur1, ib [, idur2] [, ic] [...], irel, iz
> kres  expsegr  ia, idur1, ib [, idur2] [, ic] [...], irel, iz

csound doc: <http://csound.com/docs/manual/expsegr.html>
-}
expsegr :: [D] -> D -> D -> Sig
expsegr b1 b2 b3 = setRate Kr $ liftOpc "expsegr" [(Kr, repeat Ir), (Ar, repeat Ir)] (b1 ++ [1, last b1], b2, b3)

----------------------------------------------------------------------------------
-- Line Smooth

{- |
Exponential lag with 60dB lag time. Port of Supercollider's Lag. This is essentially
a one pole filter except that instead of supplying the coefficient directly, it is calculated from a 60 dB lag time. This is the time required for the filter to converge to within 0.01% of a value. This is useful for smoothing out control signals.

> aout lag ain, klagtime [, initialvalue]
> kout lag kin, klagtime [, initialvalue]
-}
lag :: Sig -> Sig -> Sig
lag b1 b2 = liftOpc "lag" rates (b1, b2)
  where
    rates = [(Ar, [Ar, Kr, Ir]), (Kr, [Kr, Kr, Ir])]

{- |
Exponential lag with different smoothing time for up- and downgoing signals. Port of Supercollider's LagUD.

> aout lagud ain, klagup, klagdown [, initialvalue]
> kout lagud kin, klagup, klagdown [, initialvalue]
-}
lagud :: Sig -> Sig -> Sig -> Sig
lagud b1 b2 b3 = liftOpc "lagud" rates (b1, b2, b3)
  where
    rates = [(Ar, [Ar, Kr, Kr, Ir]), (Kr, [Kr, Kr, Kr, Ir])]

----------------------------------------------------------------------------------
--- Sound Files / Samples

diskin :: Str -> Sig2
diskin a = liftMulti "diskin2" ((repeat Ar), [Sr, Kr, Ir, Ir, Ir, Ir, Ir, Ir]) a

diskin2 :: (Tuple a) => Str -> a
diskin2 a = liftMulti "diskin2" ((repeat Ar), [Sr, Kr, Ir, Ir, Ir, Ir, Ir, Ir]) a

{- |
Reads mono or stereo audio data from an external MP3 file.

> ar1, ar2  mp3in  ifilcod[, iskptim, iformat, iskipinit, ibufsize]
> ar1  mp3in  ifilcod[, iskptim, iformat, iskipinit, ibufsize]

csound doc: <http://csound.com/docs/manual/mp3in.html>
-}
mp3in :: Str -> (Sig, Sig)
mp3in b1 = liftMulti "mp3in" rates b1
  where
    rates = ([Ar, Ar], [Sr, Ir, Ir, Ir, Ir])

{- |
Read sampled sound from a table.

Read sampled sound (mono or stereo) from a table, with optional sustain and release looping.

> ar1 [,ar2]  loscil  xamp, kcps, ifn [, ibas] [, imod1] [, ibeg1] [, iend1] \
>           [, imod2] [, ibeg2] [, iend2]

csound doc: <http://csound.com/docs/manual/loscil.html>
-}
loscil :: (Tuple a) => Sig -> Sig -> Tab -> a
loscil b1 b2 b3 = liftMulti "loscil" rates (b1, b2, b3)
  where
    rates = ([Ar, Ar], [Xr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])

{- |
Read sampled sound from a table using cubic interpolation.

Read sampled sound (mono or stereo) from a table, with optional sustain and release looping, using cubic interpolation.

> ar1 [,ar2]  loscil3  xamp, kcps, ifn [, ibas] [, imod1] [, ibeg1] [, iend1] \
>           [, imod2] [, ibeg2] [, iend2]

csound doc: <http://csound.com/docs/manual/loscil3.html>
-}
loscil3 :: (Tuple a) => Sig -> Sig -> Tab -> a
loscil3 b1 b2 b3 = liftMulti "loscil3" rates (b1, b2, b3)
  where
    rates = ([Ar, Ar], [Xr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])

{- |
Read multi-channel sampled sound from a table.

Read sampled sound (up to 16 channels) from a table, with
      optional sustain and release looping.

> ar1 [, ar2, ar3, ar4, ar5, ar6, ar7, ar8, ar9, ar10, ar11, ar12, ar13, ar14, \
>           ar15, ar16]  loscilx  xamp, kcps, ifn \
>           [, iwsize, ibas, istrt, imod, ibeg, iend]

csound doc: <http://csound.com/docs/manual/loscilx.html>
-}
loscilx :: (Tuple a) => Sig -> Sig -> Tab -> a
loscilx b1 b2 b3 = liftMulti "loscilx" rates (b1, b2, b3)
  where
    rates =
      ( [Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar]
      , [Xr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir]
      )

{- |
Generates a table index for sample playback

This opcode can be used to generate table index for sample playback (e.g. tablexkt).

> ares  lphasor  xtrns [, ilps] [, ilpe] [, imode] [, istrt] [, istor]

csound doc: <http://csound.com/docs/manual/lphasor.html>
-}
lphasor :: Sig -> Sig
lphasor b1 = liftOpc "lphasor" rates b1
  where
    rates = [(Ar, [Xr, Ir, Ir, Ir, Ir, Ir])]

{- |
Function-table-based crossfading looper.

This opcode reads audio from a function table and plays it back in a loop with user-defined
   start time, duration and crossfade time. It also allows the pitch of the loop to be controlled,
   including reversed playback. It accepts non-power-of-two tables, such as deferred-allocation
   GEN01 tables, with one or two channels.

> asig1[, asig2]  flooper  kamp, kpitch, istart, idur, ifad, ifn

csound doc: <http://csound.com/docs/manual/flooper.html>
-}
flooper :: (Tuple a) => Sig -> Sig -> D -> D -> D -> Tab -> a
flooper b1 b2 b3 b4 b5 b6 = liftMulti "flooper" rates (b1, b2, b3, b4, b5, b6)
  where
    rates = ([Ar, Ar], [Kr, Kr, Ir, Ir, Ir, Ir])

{- |
Function-table-based crossfading looper.

This opcode implements a crossfading looper with variable loop parameters and three
  looping modes, optionally using a table for its crossfade shape. It accepts
  non-power-of-two tables for its source sounds, such as deferred-allocation
   GEN01 tables, with one or two channels.

> asig1[,asig2]  flooper2  kamp, kpitch, kloopstart, kloopend, kcrossfade, ifn \
>           [, istart, imode, ifenv, iskip]

csound doc: <http://csound.com/docs/manual/flooper2.html>
-}
flooper2 :: (Tuple a) => Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> a
flooper2 b1 b2 b3 b4 b5 b6 = liftMulti "flooper2" rates (b1, b2, b3, b4, b5, b6)
  where
    rates = ([Ar, Ar], [Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir])

{- |
Returns the length of a sound file.

> ir  filelen  ifilcod, [iallowraw]

csound doc: <http://csound.com/docs/manual/filelen.html>
-}
filelen :: Str -> D
filelen b1 = liftOpc "filelen" rates b1
  where
    rates = [(Ir, [Sr, Ir])]

{- |
Returns the sample rate of a sound file.

> ir  filesr  ifilcod [, iallowraw]

csound doc: <http://csound.com/docs/manual/filesr.html>
-}
filesr :: Str -> D
filesr a1 = liftOpc "filesr" [(Ir, [Sr, Ir])] a1

{- |
Phase-locked vocoder processing with onset detection/processing, 'tempo-scaling'.

filescal implements phase-locked vocoder
      processing from disk files, resampling if necessary.

> asig[,asig2]  filescal  ktimescal, kamp, kpitch, Sfile, klock [,ifftsize, idecim, ithresh]
>

csound doc: <http://csound.com/docs/manual/filescal.html>
-}
filescal :: (Tuple a) => Sig -> Sig -> Sig -> Str -> Sig -> a
filescal b1 b2 b3 b4 b5 = liftMulti "filescal" rates (b1, b2, b3, b4, b5)
  where
    rates = ([Ar, Ar], [Kr, Kr, Kr, Sr, Kr, Ir, Ir, Ir])

{- |
Phase-locked vocoder processing.

mincer implements phase-locked vocoder processing using function tables
containing sampled-sound sources, with GEN01, and
mincer will accept deferred allocation tables.

> asig  mincer  atimpt, kamp, kpitch, ktab, klock[,ifftsize,idecim]
>

csound doc: <http://csound.com/docs/manual/mincer.html>
-}
mincer :: Sig -> Sig -> Sig -> Tab -> Sig -> Sig
mincer b1 b2 b3 b4 b5 = liftOpc "mincer" rates (b1, b2, b3, b4, b5)
  where
    rates = [(Ar, [Ar, Kr, Kr, Kr, Kr, Ir, Ir])]

{- |
Returns the audio spout frame.

Returns the audio spout frame (if active), otherwise it returns zero.

> aout1 [,aout2 ... aoutX]  monitor
> aarra  monitor

csound doc: <http://csound.com/docs/manual/monitor.html>
-}
monitor :: forall a. (Sigs a) => SE a
monitor = liftMultiDep "monitor" ((repeat Ar), []) ()

{- |
Reads from numbered channels in an external audio signal or stream.

> ain1[, ...]  inch  kchan1[,...]

csound doc: <http://csound.com/docs/manual/inch.html>
-}
inch :: (Tuple a) => Sig -> SE a
inch b1 = liftMultiDep "inch" ((repeat Ar), (repeat Kr)) b1

{- | Writes multi-channel audio data, with user-controllable channels, to an external device or stream.

> outch kchan1, asig1 [, kchan2] [, asig2] [...]

User several invokations of outch to write to multiple channels
-}
outch :: Sig -> Sig -> SE ()
outch index aout = liftOpcDep_ "outch" [(Xr, [Kr, Ar])] (index, aout)

{- |
Writes stereo audio data to an external device or stream.

>  outs  asig1, asig2

csound doc: <http://csound.com/docs/manual/outs.html>
-}
outs :: Sig2 -> SE ()
outs a = liftOpcDep_ "outs" [(Xr, [Ar, Ar])] a

-------------------------------------------------------------------------------------
-- Tables (Buffers)

{- |
Accesses table values by direct indexing.

> ares  table  andx, ifn [, ixmode] [, ixoff] [, iwrap]
> ires  table  indx, ifn [, ixmode] [, ixoff] [, iwrap]
> kres  table  kndx, ifn [, ixmode] [, ixoff] [, iwrap]

csound doc: <http://csound.com/docs/manual/table.html>
-}
table :: (SigOrD a) => a -> Tab -> SE a
table b1 b2 = liftOpcDep "table" rates (b1, b2)
  where
    rates =
      [ (Ar, [Ar, Ir, Ir, Ir, Ir])
      , (Ir, [Ir, Ir, Ir, Ir, Ir])
      , (Kr, [Kr, Ir, Ir, Ir, Ir])
      ]

-- | The same as table but pure version for read-only tables
getTable :: (SigOrD a) => a -> Tab -> a
getTable b1 b2 = liftOpc "table" rates (b1, b2)
  where
    rates =
      [ (Ar, [Ar, Ir, Ir, Ir, Ir])
      , (Ir, [Ir, Ir, Ir, Ir, Ir])
      , (Kr, [Kr, Ir, Ir, Ir, Ir])
      ]

{- |
Accesses table values by direct indexing with cubic interpolation.

> ares  table3  andx, ifn [, ixmode] [, ixoff] [, iwrap]
> ires  table3  indx, ifn [, ixmode] [, ixoff] [, iwrap]
> kres  table3  kndx, ifn [, ixmode] [, ixoff] [, iwrap]

csound doc: <http://csound.com/docs/manual/table3.html>
-}
table3 :: (SigOrD a) => a -> Tab -> SE a
table3 b1 b2 = liftOpcDep "table3" rates (b1, b2)
  where
    rates =
      [ (Ar, [Ar, Ir, Ir, Ir, Ir])
      , (Ir, [Ir, Ir, Ir, Ir, Ir])
      , (Kr, [Kr, Ir, Ir, Ir, Ir])
      ]

-- | The same as table3 but pure version for read-only tables
getTable3 :: (SigOrD a) => a -> Tab -> a
getTable3 b1 b2 = liftOpc "table3" rates (b1, b2)
  where
    rates =
      [ (Ar, [Ar, Ir, Ir, Ir, Ir])
      , (Ir, [Ir, Ir, Ir, Ir, Ir])
      , (Kr, [Kr, Ir, Ir, Ir, Ir])
      ]

{- |
Accesses table values by direct indexing with linear interpolation.

> ares  tablei  andx, ifn [, ixmode] [, ixoff] [, iwrap]
> ires  tablei  indx, ifn [, ixmode] [, ixoff] [, iwrap]
> kres  tablei  kndx, ifn [, ixmode] [, ixoff] [, iwrap]

csound doc: <http://csound.com/docs/manual/tablei.html>
-}
tablei :: (SigOrD a) => a -> Tab -> SE a
tablei b1 b2 = liftOpcDep "tablei" rates (b1, b2)
  where
    rates =
      [ (Ar, [Ar, Ir, Ir, Ir, Ir])
      , (Ir, [Ir, Ir, Ir, Ir, Ir])
      , (Kr, [Kr, Ir, Ir, Ir, Ir])
      ]

-- | The same as tablei but pure version for read-only tables
getTablei :: (SigOrD a) => a -> Tab -> a
getTablei b1 b2 = liftOpc "tablei" rates (b1, b2)
  where
    rates =
      [ (Ar, [Ar, Ir, Ir, Ir, Ir])
      , (Ir, [Ir, Ir, Ir, Ir, Ir])
      , (Kr, [Kr, Ir, Ir, Ir, Ir])
      ]

{- | tablew — Change the contents of existing function tables.

> tablew asig, andx, ifn [, ixmode] [, ixoff] [, iwgmode]
> tablew isig, indx, ifn [, ixmode] [, ixoff] [, iwgmode]
> tablew ksig, kndx, ifn [, ixmode] [, ixoff] [, iwgmode]
-}
tablew :: (SigOrD a) => a -> a -> Tab -> SE ()
tablew b1 b2 b3 = liftOpcDep_ "tablew" rates (b1, b2, b3)
  where
    rates =
      [ (Ar, [Ar, Ar, Ir, Ir, Ir, Ir])
      , (Ir, [Ir, Ir, Ir, Ir, Ir, Ir])
      , (Kr, [Kr, Kr, Ir, Ir, Ir, Ir])
      ]

-------------------------------------------------------------------------------------

-- * Program Control

{- | changed — k-rate signal change detector

> ktrig changed kvar1 [, kvar2,..., kvarN]
-}
changed :: [Sig] -> Sig
changed as = liftOpc "changed" [(Kr, repeat Kr), (Ar, repeat Ir)] as

-- | Like @changed@ but it does not trigger the first cycle if any of the input signals is non-zero.
changed2 :: [Sig] -> Sig
changed2 as = liftOpc "changed" [(Kr, repeat Kr), (Ar, repeat Ir)] as

{- |
Informs when a krate signal crosses a threshold.

> kout  trigger  ksig, kthreshold, kmode

csound doc: <http://csound.com/docs/manual/trigger.html>
-}
trigger :: Sig -> Sig -> Sig -> Sig
trigger b1 b2 b3 = liftOpc "trigger" rates (b1, b2, b3)
  where
    rates = [(Kr, [Kr, Kr, Kr])]

-------------------------------------------------------------------------------------
-- Time

{- | metro — Trigger Metronome.

Generate a metronomic signal to be used in any circumstance an isochronous trigger is needed.

> ktrig  metro  kfreq [, initphase]
-}
metro :: Sig -> Sig
metro = liftOpc "metro" [(Kr, [Kr, Ir])]

{- | metro2 — Trigger Metronome with Swing and Accents.

> ktrig  metro2  kfreq, kswing [, iamp, initphase]
-}
metro2 :: Sig -> Sig -> Sig
metro2 a b = liftOpc "metro2" [(Kr, [Kr, Kr, Ir, Ir])] (a, b)

{- | timeinsts — Read absolute time in seconds.

> kres timeinsts
-}
timeinsts :: SE Sig
timeinsts = liftOpcDep "timeinsts" [(Kr, [])] ()

{-
-------------------------------------------------------------------------------------
-- Channels

newtype Chan a = Chan { unChan :: Str }

newChan :: Val a => Str -> Chan a
newChan = Chan

getChanName :: Chan a -> Str
getChanName = unChan

instance IsRef Chan where
  readRef :: forall a . Tuple a => Chan a -> SE a
  readRef (Chan str) = liftOpcDep "chnget" rates str
    where rates = [(head (tupleRates @a), [Sr])]

  writeRef :: forall a . Tuple a => Chan a -> a -> SE ()
  writeRef (Chan str) val = liftOpcDep_ "chnset" rates (val, str)
    where rates = [(Xr, [head (tupleRates @a), Sr])]

  readInitRef :: forall a . (Tuple a, Val (InitType a)) => Chan a -> SE (InitType a)
  readInitRef (Chan str) = liftOpcDep "chnget" rates str
    where rates = [(Ir, [Sr])]

  writeInitRef :: forall a . (Tuple a, Tuple (InitType a)) => Chan a -> InitType a -> SE ()
  writeInitRef (Chan str) val = liftOpcDep_ "chnseti" rates (val, str)
    where rates = [(Xr, [Ir, Sr])]

chnclear :: Chan Sig -> SE ()
chnclear (Chan str) = liftOpcDep_ "chnclear" [(Xr, [Sr])] str

chnmix :: Chan Sig -> Sig -> SE ()
chnmix (Chan str) val = liftOpcDep_ "chnset" rates (val, str)
  where rates = [(Xr, [Ar, Sr])]
-}

-------------------------------------------------------------------------------------
-- Space

{- | pan2 — Distribute an audio signal across two channels.
For Csound users: order of arguments is reversed for better Haskell experience

> a1, a2 pan2 xp, asig [, imode]
-}
pan2 :: Sig -> Sig -> Sig2
pan2 xp asig = liftMulti "pan2" ([Ar, Ar], [Ar, Xr, Ir]) (asig, xp)

{- | vbap — Distributes an audio signal among many channels.

> ar1[, ar2...] vbap asig, kazim [,
   kelev] [, kspread] [, ilayout]

Csound docs: <https://csound.com/docs/manual/vbap.html>
-}
vbap :: (Sigs a) => Sig -> Sig -> a
vbap asig kazim = liftMulti "vbap" (repeat Ar, [Ar, Kr, Kr, Kr, Ir]) (asig, kazim)

{- | vbaplsinit — Configures VBAP output according to loudspeaker parameters.

> vbaplsinit idim, ilsnum [, idir1] [, idir2] [...] [, idir32]
-}
vbaplsinit :: D -> D -> [D] -> SE ()
vbaplsinit b1 b2 bs = liftOpcDep_ "vbaplsinit" rates (b1, b2, bs)
  where
    rates = [(Xr, repeat Ir)]

-------------------------------------------------------------------------------------
-- Reverbs

{- |
Opcode version of Jezar's Freeverb

freeverb is a stereo reverb unit based on Jezar's public domain
		C++ sources, composed of eight parallel comb filters on both
		channels, followed by four allpass units in series. The filters
		on the right channel are slightly detuned compared to the left
		channel in order to create a stereo effect.

> aoutL, aoutR  freeverb  ainL, ainR, kRoomSize, kHFDamp[, iSRate[, iSkip]]

csound doc: <http://csound.com/docs/manual/freeverb.html>
order of arguments is reversed comared to the Csound version it's:

> freeverb kRoomSize kHFDamp (ainL, ainR) = (aoutL, aoutR)
-}
freeverb :: Sig -> Sig -> (Sig, Sig) -> (Sig, Sig)
freeverb roomSize damp (ainL, ainR) = liftMulti "freeverb" rates (ainL, ainR, roomSize, damp)
  where
    rates = ([Ar, Ar], [Ar, Ar, Kr, Kr, Ir, Ir])

{- |
8 delay line stereo FDN reverb, based on work by Sean Costello

8 delay line stereo FDN reverb, with feedback matrix based upon physical
		modeling scattering junction of 8 lossless waveguides of equal characteristic
		impedance. Based on Csound orchestra version by Sean Costello.

> aoutL, aoutR  reverbsc  ainL, ainR, kfblvl, kfco[, israte[, ipitchm[, iskip]]]

csound doc: <http://csound.com/docs/manual/reverbsc.html>
order of arguments is reversed comared to the Csound version it's:

> reverbsc kFeedbackLevel kfco (ainL, ainR) = (aoutL, aoutR)
-}
reverbsc :: Sig -> Sig -> (Sig, Sig) -> (Sig, Sig)
reverbsc kFeedbackLevel kfco (ainL, ainR) = liftMulti "reverbsc" rates (ainL, ainR, kFeedbackLevel, kfco)
  where
    rates = ([Ar, Ar], [Ar, Ar, Kr, Kr, Ir, Ir, Ir])

-------------------------------------------------------------------------------------
-- Convolution

{- |
Convolution based on a uniformly partitioned overlap-save algorithm

Convolution based on a uniformly partitioned overlap-save algorithm. Compared to the convolve opcode, pconvolve has these benefits:

> ar1 [, ar2] [, ar3] [, ar4]  pconvolve  ain, ifilcod [, ipartitionsize, ichannel]

csound doc: <http://csound.com/docs/manual/pconvolve.html>
-}
pconvolve :: (Tuple a) => Sig -> Str -> a
pconvolve b1 b2 = liftMulti "pconvolve" rates (b1, b2)
  where
    rates = ([Ar, Ar, Ar, Ar], [Ar, Sr, Ir, Ir])

-------------------------------------------------------------------------------------
-- Physical modeling

{- |
Produces a naturally decaying plucked string or drum sound.

Audio output is a naturally decaying plucked string or drum sound based on the Karplus-Strong algorithms.

> ares  pluck  kamp, kcps, icps, ifn, imeth [, iparm1] [, iparm2]

csound doc: <http://csound.com/docs/manual/pluck.html>
-}
pluck :: Sig -> Sig -> D -> Tab -> D -> Sig
pluck b1 b2 b3 b4 b5 = liftOpc "pluck" rates (b1, b2, b3, b4, b5)
  where
    rates = [(Ar, [Kr, Kr, Ir, Ir, Ir, Ir, Ir])]

-------------------------------------------------------------------------------------
-- Delay / Comb

{- |
A variable delay opcode with high quality interpolation.

> aout  vdelayx  ain, adl, imd, iws [, ist]

csound doc: <http://csound.com/docs/manual/vdelayx.html>
-}
vdelayx :: Sig -> Sig -> D -> D -> Sig
vdelayx b1 b2 b3 b4 = liftOpc "vdelayx" rates (b1, b2, b3, b4)
  where
    rates = [(Ar, [Ar, Ar, Ir, Ir, Ir])]

{- |
Reverberates an input signal with a âcoloredâ frequency response.

> ares  comb  asig, krvt, ilpt [, iskip] [, insmps]

csound doc: <http://csound.com/docs/manual/comb.html>
-}
comb :: Sig -> Sig -> D -> Sig
comb b1 b2 b3 = liftOpc "comb" rates (b1, b2, b3)
  where
    rates = [(Ar, [Ar, Kr, Ir, Ir, Ir])]

{- |
Variably reverberates an input signal with a âcoloredâ frequency response.

> ares  vcomb  asig, krvt, xlpt, imaxlpt [, iskip] [, insmps]

csound doc: <http://csound.com/docs/manual/vcomb.html>
-}
vcomb :: Sig -> Sig -> Sig -> D -> Sig
vcomb b1 b2 b3 b4 = liftOpc "vcomb" rates (b1, b2, b3, b4)
  where
    rates = [(Ar, [Ar, Kr, Xr, Ir, Ir, Ir])]

-------------------------------------------------------------------------------------
-- Filters

{- |
A first-order recursive low-pass filter with variable frequency response.

> ares  tone  asig, khp [, iskip]

csound doc: <http://csound.com/docs/manual/tone.html>

Note that first arguments goes last (as it convenient for Haskell)
-}
tone :: Sig -> Sig -> Sig
tone khp asig = liftOpc "tone" rates (asig, khp)
  where
    rates = [(Ar, [Ar, Kr, Ir])]

{- |
A first-order recursive high-pass filter with variable frequency response.

> ares  atone  asig, khp [, iskip]

csound doc: <http://csound.com/docs/manual/tone.html>

Note that first arguments goes last (as it convenient for Haskell)
-}
atone :: Sig -> Sig -> Sig
atone khp asig = liftOpc "atone" rates (asig, khp)
  where
    rates = [(Ar, [Ar, Kr, Ir])]

{- |
A second-order resonant filter.

> ares  reson  asig, xcf, xbw [, iscl] [, iskip]

csound doc: <http://csound.com/docs/manual/reson.html>

Note that first arguments goes last (as it convenient for Haskell)
-}
reson :: Sig -> Sig -> Sig -> Sig
reson xcf xbw asig = liftOpc "reson" rates (asig, xcf, xbw)
  where
    rates = [(Ar, [Ar, Xr, Xr, Ir, Ir])]

{- |
Same as the butterbp opcode.

> ares  butbp  asig, kfreq, kband [, iskip]

csound doc: <http://csound.com/docs/manual/butbp.html>

Note that first arguments goes last (as it convenient for Haskell)
-}
butbp :: Sig -> Sig -> Sig -> Sig
butbp kfreq kband asig = liftOpc "butbp" rates (asig, kfreq, kband)
  where
    rates = [(Ar, [Ar, Kr, Kr, Ir])]

{- |
Same as the butterbr opcode.

> ares  butbr  asig, kfreq, kband [, iskip]

csound doc: <http://csound.com/docs/manual/butbr.html>

Note that first arguments goes last (as it convenient for Haskell)
-}
butbr :: Sig -> Sig -> Sig -> Sig
butbr kfreq kband asig = liftOpc "butbr" rates (asig, kfreq, kband)
  where
    rates = [(Ar, [Ar, Kr, Kr, Ir])]

{- |
Same as the butterhp opcode.

> ares  buthp  asig, kfreq [, iskip]
> ares  buthp  asig, afreq [, iskip]

csound doc: <http://csound.com/docs/manual/buthp.html>

Note that first arguments goes last (as it convenient for Haskell)
-}
buthp :: Sig -> Sig -> Sig
buthp kfreq asig = liftOpc "buthp" rates (asig, kfreq)
  where
    rates = [(Ar, [Ar, Kr, Ir]), (Ar, [Ar, Ar, Ir])]

{- |
Same as the butterlp opcode.

> ares  butlp  asig, kfreq [, iskip]
> ares  butlp  asig, afreq [, iskip]

csound doc: <http://csound.com/docs/manual/butlp.html>

Note that first arguments goes last (as it convenient for Haskell)
-}
butlp :: Sig -> Sig -> Sig
butlp kfreq asig = liftOpc "butlp" rates (asig, kfreq)
  where
    rates = [(Ar, [Ar, Kr, Ir]), (Ar, [Ar, Ar, Ir])]

{- |
A filter that simulates a mass-spring-damper system

Filters the incoming signal with the specified resonance frequency and
      quality factor. It can also be seen as a signal generator for high quality
      factor, with an impulse for the excitation. You can combine several modes
      to built complex instruments such as bells or guitar tables.

> aout  mode  ain, xfreq, xQ [, iskip]

csound doc: <http://csound.com/docs/manual/mode.html>

Note that first arguments goes last (as it convenient for Haskell)
-}
mode :: Sig -> Sig -> Sig -> Sig
mode xfreq xQ ain = liftOpc "mode" rates (ain, xfreq, xQ)
  where
    rates = [(Ar, [Ar, Xr, Xr, Ir])]

{- |
Zero-delay feedback implementation of 4 pole ladder filter.

Zero-delay feedback implementation of a 4 pole (24 dB/oct) low-pass filter based on the Moog ladder filter.

> asig  zdf_ladder  ain, xcf, xQ [, istor]

csound doc: <http://csound.com/docs/manual/zdf_ladder.html>

Note that first arguments goes last (as it convenient for Haskell)
-}
zdfLadder :: Sig -> Sig -> Sig -> Sig
zdfLadder ain xcf xQ = liftOpc "zdf_ladder" rates (ain, xcf, xQ)
  where
    rates = [(Ar, [Ar, Xr, Xr, Ir])]

{- |
A digital emulation of the Moog diode ladder filter configuration.

> ares  moogvcf2  asig, xfco, xres [,iscale, iskip]

csound doc: <http://csound.com/docs/manual/moogvcf2.html>

Note that first arguments goes last (as it convenient for Haskell)
-}
moogvcf2 :: Sig -> Sig -> Sig -> Sig
moogvcf2 asig xfco xres = liftOpc "moogvcf2" rates (asig, xfco, xres)
  where
    rates = [(Ar, [Ar, Xr, Xr, Ir, Ir])]

{- |
A digital emulation of Korg 35 filter

> ares k35_lpf asig, xcutoff, xresonance, [..]
-}
k35_lpf :: Sig -> Sig -> Sig -> Sig
k35_lpf asig xfco xres = liftOpc "K35_lpf" rates (asig, xfco, xres)
  where
    rates = [(Ar, [Ar, Xr, Xr, Ir, Ir, Ir])]

{- |
A digital emulation of Korg 35 filter

> ares k35_hpf asig, xcutoff, xresonance, [..]
-}
k35_hpf :: Sig -> Sig -> Sig -> Sig
k35_hpf asig xfco xres = liftOpc "K35_hpf" rates (asig, xfco, xres)
  where
    rates = [(Ar, [Ar, Xr, Xr, Ir, Ir, Ir])]

{- |
Implementation of Zoelzer's parametric equalizer filters.

Implementation of Zoelzer's parametric equalizer filters, with some modifications by the author.

> ares  pareq  asig, kc, kv, kq [, imode] [, iskip]

csound doc: <http://csound.com/docs/manual/pareq.html>
-}
pareq :: Sig -> Sig -> Sig -> Sig -> Sig
pareq a1 a2 a3 a4 = liftOpc "pareq" [(Ar, [Ar, Kr, Kr, Kr, Ir, Ir])] (a1, a2, a3, a4)

-------------------------------------------------------------------------------------
-- Level

{- |
Determines the root-mean-square amplitude of an audio signal.

Determines the root-mean-square amplitude of an audio signal. It low-pass filters the actual value, to average in the manner of a VU meter.

> kres  rms  asig [, ihp] [, iskip]

csound doc: <http://csound.com/docs/manual/rms.html>
-}
rms :: Sig -> Sig
rms = liftOpc "rms" [(Kr, [Ar, Ir, Ir])]

{- |
Adjust one audio signal according to the values of another.

The rms power of asig can be interrogated, set, or adjusted to match that of a comparator signal.

> ares  balance  asig, acomp [, ihp] [, iskip]

csound doc: <http://csound.com/docs/manual/balance.html>
-}
balance :: Sig -> Sig -> Sig
balance b1 b2 = liftOpc "balance" rates (b1, b2)
  where
    rates = [(Ar, [Ar, Ar, Ir, Ir])]

{- |
Adjust one audio signal according to the values of another.

The rms power of asig can be interrogated, set, or adjusted to match that of a comparator signal.

> ares  balance  asig, acomp [, ihp] [, iskip]

csound doc: <http://csound.com/docs/manual/balance2.html>

Note that balance2 is just like balance except the gain is recalculated for every sample rather than interpolating k-rate values.
-}
balance2 :: Sig -> Sig -> Sig
balance2 b1 b2 = liftOpc "balance2" rates (b1, b2)
  where
    rates = [(Ar, [Ar, Ar, Ir, Ir])]

-------------------------------------------------------------------------------------
-- Amplitude / Pitch tracking

{- |
Envelope follower unit generator.

> ares  follow  asig, idt

csound doc: <http://csound.com/docs/manual/follow.html>
-}
follow :: Sig -> D -> Sig
follow b1 b2 = liftOpc "follow" rates (b1, b2)
  where
    rates = [(Ar, [Ar, Ir])]

{- |
Another controllable envelope extractor.

A controllable envelope extractor using the algorithm attributed to Jean-Marc Jot.

> ares  follow2  asig, katt, krel

csound doc: <http://csound.com/docs/manual/follow2.html>
-}
follow2 :: Sig -> Sig -> Sig -> Sig
follow2 b1 b2 b3 = liftOpc "follow2" rates (b1, b2, b3)
  where
    rates = [(Ar, [Ar, Kr, Kr])]

{- |
Tracks the pitch of a signal.

ptrack takes an input signal, splits it into ihopsize blocks and using a STFT method, extracts an estimated pitch for its fundamental frequency as well as estimating the total amplitude of the signal in dB, relative to full-scale (0dB). The method implies an analysis window size of 2*ihopsize samples (overlaping by 1/2 window), which has to be a power-of-two, between 128 and 8192 (hopsizes between 64 and 4096). Smaller windows will give better time precision, but worse frequency accuracy (esp. in low fundamentals).This opcode is based on an original algorithm by M. Puckette.

> kcps, kamp  ptrack  asig, ihopsize[,ipeaks]

csound doc: <http://csound.com/docs/manual/ptrack.html>
-}
ptrack :: Sig -> D -> (Sig, Sig)
ptrack b1 b2 = liftMulti "ptrack" rates (b1, b2)
  where
    rates = ([Kr, Kr], [Ar, Ir, Ir])

-------------------------------------------------------------------------------------
-- Distortion

{- |
Waveshapes a signal by raising it to a variable exponent.

The powershape opcode raises an input signal to a power with pre- and post-scaling of the signal so that the output will be in a predictable range.  It also processes negative inputs in a symmetrical way to positive inputs, calculating a dynamic transfer function that is useful for waveshaping.

> aout  powershape  ain, kShapeAmount [, ifullscale]

csound doc: <http://csound.com/docs/manual/powershape.html>

Arguments are reversed:

> powershape kShape ain
-}
powershape :: Sig -> Sig -> Sig
powershape kShape ain = liftOpc "powershape" rates (ain, kShape)
  where
    rates = [(Ar, [Ar, Kr, Ir])]

{- |
Distort an audio signal via waveshaping and optional clipping.

> ar  distort  asig, kdist, ifn[, ihp, istor]

csound doc: <http://csound.com/docs/manual/distort.html>

Arguments are reversed, the audio input goes last
-}
distort :: Sig -> Tab -> Sig -> Sig
distort b1 b2 ain = liftOpc "distort" rates (ain, b1, b2)
  where
    rates = [(Ar, [Ar, Kr, Ir, Ir, Ir])]

{- |
Modified hyperbolic tangent distortion.

Implementation of modified hyperbolic tangent distortion. distort1 can be used to generate wave shaping distortion based on a modification of the tanh function.

> ares  distort1  asig, kpregain, kpostgain, kshape1, kshape2[, imode]

csound doc: <http://csound.com/docs/manual/distort1.html>

Arguments are reversed, the audio input goes last
-}
distort1 :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
distort1 b1 b2 b3 b4 ain = liftOpc "distort1" rates (ain, b1, b2, b3, b4)
  where
    rates = [(Ar, [Ar, Kr, Kr, Kr, Kr, Ir])]

-------------------------------------------------------------------------------------
-- print and display

{- |
Displays the values init (i-rate) variables.

These units will print orchestra init-values.

>  print  iarg [, iarg1] [, iarg2] [...]

csound doc: <http://csound.com/docs/manual/print.html>
-}
printi :: [D] -> SE ()
printi ds = liftOpcDep_ "print" rates ds
  where
    rates = [(Xr, (repeat Ir))]

{- |
Prints one k-rate value at specified intervals.

>  printk  itime, kval [, ispace]

csound doc: <http://csound.com/docs/manual/printk.html>
-}
printk :: D -> Sig -> SE ()
printk b1 b2 = liftOpcDep_ "printk" rates (b1, b2)
  where
    rates = [(Xr, [Ir, Kr, Ir])]

{- |
Prints at init-time using a printf() style syntax.

>  prints  "string" [, kval1] [, kval2] [...]

csound doc: <http://csound.com/docs/manual/prints.html>
-}
prints :: forall a. (Tuple a) => Str -> a -> SE ()
prints b1 b2 = liftOpcDep_ "prints" rates (b1, b2)
  where
    rates = [(Xr, Sr : tupleRates @a)]

{- |
Prints at init-time using a printf() style syntax.

>  prints  "string" [, kval1] [, kval2] [...]

csound doc: <http://csound.com/docs/manual/prints.html>
-}
printks :: (Sigs a) => Str -> D -> a -> SE ()
printks b1 b2 b3 = liftOpcDep_ "printks" rates (b1, b2, b3)
  where
    rates = [(Xr, Sr : Ir : repeat Kr)]

{- |
Prints a new value every time a control variable changes.

>  printk2  kvar [, inumspaces]

csound doc: <http://csound.com/docs/manual/printk2.html>
-}
printk2 :: Sig -> SE ()
printk2 = liftOpcDep_ "printk2" [(Xr, [Kr, Ir])]

-- TODO
-- printarray

fprint :: Tab -> SE ()
fprint = liftOpcDep_ "fprint" [(Xr, [Ir, Kr, Kr, Kr, Kr, Ir])]

-------------------------------------------------------------------------------------
-- File ouptut

{- |
Outputs a-rate signals to an arbitrary number of channels.

fout outputs N a-rate signals to a specified file of N channels.

>  fout  ifilename, iformat, aout1 [, aout2, aout3,...,aoutN]
>  fout  ifilename, iformat, array[]

csound doc: <http://csound.com/docs/manual/fout.html>
-}
fout :: (Sigs a) => Str -> D -> a -> SE ()
fout b1 b2 b3 = liftOpcDep_ "fout" rates (b1, b2, b3)
  where
    rates = [(Xr, [Sr, Ir] ++ (repeat Ar))]

ftsave :: Str -> D -> [Tab] -> SE ()
ftsave file imode tabs = liftOpcDep_ "ftsave" rates (file, imode, tabs)
  where
    rates = [(Xr, Sr : Ir : (repeat Ir))]

fprints :: Str -> Str -> [D] -> SE ()
fprints file format vals = liftOpcDep_ "fprints" rates (file, format, vals)
  where
    rates = [(Xr, Sr : Sr : repeat Ir)]

readf :: Str -> SE (Str, Sig)
readf = liftMultiDep "readf" ([Sr, Kr], [Sr])

readfi :: Str -> SE (Str, Sig)
readfi = liftMultiDep "readfi" ([Sr, Ir], [Sr])

-- TODO
-- directory: <https://csound.com/docs/manual/directory.html>

-------------------------------------------------------------------------------------
-- Math

{- |
Returns the amplitude equivalent of the decibel value x.

Returns the amplitude equivalent of the decibel value x. Thus:

>  ampdb (x)  (no rate restriction)

csound doc: <http://csound.com/docs/manual/ampdb.html>
-}
ampdb :: (SigOrD a) => a -> a
ampdb b1 = liftOpr1 "ampdb" b1

{- |
Returns the decibel equivalent of the raw amplitude x.

>  dbamp (x)  (init-rate or control-rate args only)

csound doc: <http://csound.com/docs/manual/dbamp.html>
-}
dbamp :: (SigOrD a) => a -> a
dbamp b1 = liftOpr1k "dbamp" b1

{- |
Returns the amplitude equivalent (in 16-bit signed integer scale) of the full scale decibel (dB FS) value x.

Returns the amplitude equivalent of the full scale decibel (dB FS) value x. The logarithmic full scale decibel values will be converted to linear 16-bit signed integer values from â32,768 to +32,767.

>  ampdbfs (x)  (no rate restriction)

csound doc: <http://csound.com/docs/manual/ampdbfs.html>
-}
ampdbfs :: (SigOrD a) => a -> a
ampdbfs b1 = liftOpr1 "ampdbfs" b1

{- |
Returns the decibel equivalent of the raw amplitude x, relative to full scale amplitude.

Returns the decibel equivalent of the raw amplitude x, relative to full scale amplitude. Full scale is assumed to be 16 bit. New is Csound version 4.10.

>  dbfsamp (x)  (init-rate or control-rate args only)

csound doc: <http://csound.com/docs/manual/dbfsamp.html>
-}
dbfsamp :: (SigOrD a) => a -> a
dbfsamp b1 = liftOpr1k "dbfsamp" b1

{- |
An implementation of a logarithmic gain curve which is similar to the gainslider~ object from Cycling 74 Max / MSP.

This opcode is intended for use to multiply by an audio signal to give a console mixer like feel. There is no bounds in the
      source code so you can for example give higher than 127 values for extra amplitude but possibly clipped audio.

> kout  gainslider  kindex

csound doc: <http://csound.com/docs/manual/gainslider.html>
-}
gainslider :: Sig -> Sig
gainslider b1 = liftOpc "gainslider" [(Kr, [Kr])] b1

dbfs :: Sig -> Sig
dbfs x = gainslider (x * 127)

-------------------------------------------------------------------------------------
-- MIDI

{- |
Returns a generic MIDI message received by the MIDI IN port.

Returns a generic MIDI message received by the MIDI IN port

> kstatus, kchan, kdata1, kdata2  midiin

csound doc: <http://csound.com/docs/manual/midiin.html>
-}
midiin :: SE (Sig, Sig, Sig, Sig)
midiin = liftMultiDep "midiin" ([Kr, Kr, Kr, Kr], []) ()

{- | massign — Assigns a MIDI channel number to a Csound instrument.

> massign ichnl, insnum[, ireset]

csound doc <https://csound.com/docs/manual/massign.html>
-}
massign :: (Arg a) => D -> InstrRef a -> SE ()
massign ichnl instrRef = do
  setDefaultOption setMa
  case getInstrRefId instrRef of
    Left strId -> liftOpcDep_ "massign" strRates (ichnl, strId)
    Right intId -> liftOpcDep_ "massign" intRates (ichnl, intId)
  where
    strRates = [(Xr, [Ir, Sr, Ir])]
    intRates = [(Xr, [Ir, Ir, Ir])]

{- |
Assigns an instrument number to a specified MIDI program.

Assigns an instrument number to a specified (or all) MIDI program(s).

>  pgmassign  ipgm, inst[, ichn]
>  pgmassign  ipgm, "insname"[, ichn]

csound doc: <https://csound.com/docs/manual/pgmassign.html>
-}
pgmassign :: (Arg a) => D -> InstrRef a -> D -> SE ()
pgmassign ipgm instrRef chn = do
  setDefaultOption setMa
  case getInstrRefId instrRef of
    Left strId -> liftOpcDep_ "pgmassign" strRates (ipgm, strId, chn)
    Right intId -> liftOpcDep_ "pgmassign" intRates (ipgm, intId, chn)
  where
    strRates = [(Xr, [Ir, Sr, Ir])]
    intRates = [(Xr, [Ir, Ir, Ir])]

{- |
Get a note number from a MIDI event.

> ival  notnum

csound doc: <http://csound.com/docs/manual/notnum.html>
-}
notnum :: SE D
notnum = liftOpcDep "notnum" [(Ir, [])] ()

{- |
Get the velocity of the current MIDI event.

> iamp  ampmidi  iscal [, ifn]

csound doc: <https://csound.com/docs/manual/ampmidi.html>
-}
ampmidi :: D -> D
ampmidi b1 = liftOpc "ampmidi" [(Ir, [Ir, Ir])] b1

{- |
Get the note number of the current MIDI event, expressed in cycles-per-second.

> icps  cpsmidi

csound doc: <https://csound.com/docs/manual/cpsmidi.html>
-}
cpsmidi :: D
cpsmidi = liftOpc "cpsmidi" [(Ir, [])] ()

-- | release — Indicates whether a note is in its “release” stage.
release :: SE Sig
release = liftOpcDep "release" [(Kr, [])] ()

{- |
Get the velocity from a MIDI event.

> ival  veloc  [ilow] [, ihigh]

csound doc: <http://csound.com/docs/manual/veloc.html>
-}
veloc :: SE D
veloc = liftOpcDep "veloc" [(Ir, [Ir, Ir])] ()

{- | midictrl — Get the current value (0-127) of a specified MIDI controller.

> ival midictrl inum [, imin] [, imax]
-}
midictrl :: D -> SE D
midictrl inum = liftOpcDep "midictrl" [(Ir, [Ir, Ir, Ir])] inum

data CtrlInit = CtrlInit
  { ctrlInitChan :: D -- midi channel
  , ctrlInitId :: D -- control number
  , ctrlInitValue :: D -- init value
  }

instance FromTuple CtrlInit where
  fromTuple (CtrlInit chan num val) = fromTuple (chan, num, val)

instance Tuple CtrlInit where
  tupleArity = 3
  tupleRates = [Ir, Ir, Ir]
  defTuple = CtrlInit defTuple defTuple defTuple
  toTuple = (\(chan, num, val) -> CtrlInit chan num val) . toTuple

-- | ctrlinit — Sets the initial values for a set of MIDI controllers.
ctrlinit :: [CtrlInit] -> SE ()
ctrlinit inits = liftOpcDep_ "ctrlinit" rates inits
  where
    rates = [(Xr, repeat Ir)]

{- |
Allows a floating-point 7-bit MIDI signal scaled with a minimum and a maximum range.

> idest  ctrl7  ichan, ictlno, imin, imax [, ifn]
> kdest  ctrl7  ichan, ictlno, kmin, kmax [, ifn]
> adest  ctrl7  ichan, ictlno, kmin, kmax [, ifn] [, icutoff]

csound doc: <http://csound.com/docs/manual/ctrl7.html>
-}
ctrl7 :: D -> D -> D -> D -> Sig
ctrl7 b1 b2 b3 b4 = liftOpc "ctrl7" rates (b1, b2, b3, b4)
  where
    rates =
      [ (Ir, [Ir, Ir, Ir, Ir, Ir])
      , (Kr, [Ir, Ir, Kr, Kr, Ir])
      , (Ar, [Ir, Ir, Kr, Kr, Ir, Ir])
      ]

{- |
Allows a floating-point 7-bit MIDI signal scaled with a minimum and a maximum range.

> idest  ctrl7  ichan, ictlno, imin, imax [, ifn]
> kdest  ctrl7  ichan, ictlno, kmin, kmax [, ifn]
> adest  ctrl7  ichan, ictlno, kmin, kmax [, ifn] [, icutoff]

csound doc: <http://csound.com/docs/manual/ctrl7.html>
-}
ctrl14 :: D -> D -> D -> D -> Sig
ctrl14 b1 b2 b3 b4 = liftOpc "ctrl14" rates (b1, b2, b3, b4)
  where
    rates =
      [ (Ir, [Ir, Ir, Ir, Ir, Ir])
      , (Kr, [Ir, Ir, Kr, Kr, Ir])
      , (Ar, [Ir, Ir, Kr, Kr, Ir, Ir])
      ]

{- |
Initializes the controller used to create a 7-bit MIDI value.

Initializes MIDI controller ictlno with ivalue

>  initc7  ichan, ictlno, ivalue

csound doc: <http://csound.com/docs/manual/initc7.html>
-}
initc7 :: CtrlInit -> SE ()
initc7 b1 = liftOpcDep_ "initc7" rates b1
  where
    rates = [(Xr, [Ir, Ir, Ir])]

{- |
Initializes the controller used to create a 7-bit MIDI value.

Initializes MIDI controller ictlno with ivalue

>  initc7  ichan, ictlno, ivalue

csound doc: <http://csound.com/docs/manual/initc7.html>
-}
initc14 :: CtrlInit -> SE ()
initc14 b1 = liftOpcDep_ "initc14" rates b1
  where
    rates = [(Xr, [Ir, Ir, Ir])]

{- |
Generates breakbeat-style cut-ups of a mono audio stream.

The BreakBeat Cutter automatically generates cut-ups of a source audio stream in the style of drum and bass/jungle breakbeat manipulations.  There are two versions, for mono (bbcutm) or stereo (bbcuts) sources.  Whilst originally based on breakbeat cutting, the opcode can be applied to any type of source audio.

> a1  bbcutm  asource, ibps, isubdiv, ibarlength, iphrasebars, inumrepeats \
>           [, istutterspeed] [, istutterchance] [, ienvchoice ]

csound doc: <http://csound.com/docs/manual/bbcutm.html>
-}
bbcutm :: Sig -> D -> D -> D -> D -> D -> Sig
bbcutm b1 b2 b3 b4 b5 b6 = liftOpc "bbcutm" rates (b1, b2, b3, b4, b5, b6)
  where
    rates = [(Ar, [Ar, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])]

{- |
Generates breakbeat-style cut-ups of a stereo audio stream.

The BreakBeat Cutter automatically generates cut-ups of a source audio stream in the style of drum and bass/jungle breakbeat manipulations.  There are two versions, for mono (bbcutm) or stereo (bbcuts) sources.  Whilst originally based on breakbeat cutting, the opcode can be applied to any type of source audio.

> a1,a2  bbcuts  asource1, asource2, ibps, isubdiv, ibarlength, iphrasebars, \
>           inumrepeats [, istutterspeed] [, istutterchance] [, ienvchoice]

csound doc: <http://csound.com/docs/manual/bbcuts.html>
-}
bbcuts :: Sig -> Sig -> D -> D -> D -> D -> D -> (Sig, Sig)
bbcuts b1 b2 b3 b4 b5 b6 b7 =
  liftMulti "bbcuts" ([Ar, Ar], [Ar, Ar, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir]) (b1, b2, b3, b4, b5, b6, b7)

{- |
This opcode implements a formula for generating a normalised exponential curve in range 0 - 1. It is based on the Max / MSP work of Eric Singer (c) 1994.

Generates an exponential curve in range 0 to 1 of arbitrary steepness.
      Steepness index equal to or lower than 1.0 will result in Not-a-Number
      errors and cause unstable behavior.

> kout  expcurve  kindex, ksteepness

csound doc: <http://csound.com/docs/manual/expcurve.html>
-}
expcurve :: Sig -> Sig -> Sig
expcurve b1 b2 = liftOpc "expcurve" rates (b1, b2)
  where
    rates = [(Kr, [Kr, Kr])]

{- |
Arbitrary signal scaling.

Scales incoming value to user-definable range. Similar to scale object found in popular dataflow languages.

> kscl  scale  kinput, kmax, kmin

csound doc: <http://csound.com/docs/manual/scale.html>
-}
scale :: Sig -> Sig -> Sig -> Sig
scale b1 b2 b3 = liftOpc "scale" rates (b1, b2, b3)
  where
    rates = [(Kr, [Kr, Kr, Kr])]

{- |
Compress, limit, expand, duck or gate an audio signal.

This unit functions as an audio
    compressor, limiter, expander, or noise gate, using either
    soft-knee or hard-knee mapping, and with dynamically variable
    performance characteristics.  It takes two audio input signals,
    aasig and acsig, the first of which is modified by a running
    analysis of the second. Both signals can be the same, or the first
    can be modified by a different controlling signal.

> ar  compress2  aasig, acsig, kthresh, kloknee, khiknee, kratio, katt, krel, ilook

csound doc: <http://csound.com/docs/manual/compress2.html>
-}
compress2 :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> Sig
compress2 a1 a2 a3 a4 a5 a6 a7 a8 a9 = liftOpc "compress2" rates args
  where
    rates = [(Ar, [Ar, Ar, Kr, Kr, Kr, Kr, Kr, Kr, Ir])]
    args = ((a1, a2, a3, a4), (a5, a6, a7, a8, a9))

{- |
A dynamic compressor/expander.

This opcode dynamically modifies a gain value applied to the input sound ain by comparing its power level to a given threshold level. The signal will be compressed/expanded with different factors regarding that it is over or under the threshold.

> ares  dam  asig, kthreshold, icomp1, icomp2, irtime, iftime

csound doc: <http://csound.com/docs/manual/dam.html>
-}
dam :: Sig -> Sig -> D -> D -> D -> D -> Sig
dam a1 a2 a3 a4 a5 a6 = liftOpc "dam" [(Ar, [Ar, Kr, Ir, Ir, Ir, Ir])] (a1, a2, a3, a4, a5, a6)

{- |
Compress, limit, expand, duck or gate an audio signal.

This unit functions as an audio
    compressor, limiter, expander, or noise gate, using either
    soft-knee or hard-knee mapping, and with dynamically variable
    performance characteristics.  It takes two audio input signals,
    aasig and acsig, the first of which is modified by a running
    analysis of the second. Both signals can be the same, or the first
    can be modified by a different controlling signal.

> ar  compress  aasig, acsig, kthresh, kloknee, khiknee, kratio, katt, krel, ilook

csound doc: <http://csound.com/docs/manual/compress.html>
-}
compress :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> Sig
compress a1 a2 a3 a4 a5 a6 a7 a8 a9 =
  liftOpc "compress" [(Ar, [Ar, Ar, Kr, Kr, Kr, Kr, Kr, Kr, Ir])] ((a1, a2, a3, a4), (a5, a6, a7, a8, a9))

{- |
Calculates a factor to raise/lower a frequency by a given amount of cents.

>  cent (x)

csound doc: <https://csound.com/docs/manual/cent.html>
-}
cent :: (SigOrD a) => a -> a
cent b1 = liftOpr1 "cent" b1

{- |
Modify a signal by integration.

> ares  integ  asig [, iskip]
> kres  integ  ksig [, iskip]

csound doc: <https://csound.com/docs/manual/integ.html>
-}
integ :: Sig -> Sig
integ b1 = liftOpc "integ" rates b1
  where
    rates = [(Ar, [Ar, Ir]), (Kr, [Kr, Ir])]

{- |
A DC blocking filter.

Implements the DC blocking filter

> ares  dcblock  ain [, igain]

csound doc: <https://csound.com/docs/manual/dcblock.html>
-}
dcblock :: Sig -> Sig
dcblock b1 = liftOpc "dcblock" [(Ar, [Ar, Ir])] b1

{- |
Generates random numbers and holds them for a period of time.

> ares  randh  xamp, xcps [, iseed] [, isize] [, ioffset]
> kres  randh  kamp, kcps [, iseed] [, isize] [, ioffset]

csound doc: <https://csound.com/docs/manual/randh.html>
-}
randh :: Sig -> Sig -> SE Sig
randh b1 b2 = liftOpcDep "randh" rates (b1, b2)
  where
    rates = [(Ar, [Xr, Xr, Ir, Ir, Ir]), (Kr, [Kr, Kr, Ir, Ir, Ir])]

{- |
Generates a controlled random number series with interpolation between each new number.

> ares  randi  xamp, xcps [, iseed] [, isize] [, ioffset]
> kres  randi  kamp, kcps [, iseed] [, isize] [, ioffset]

csound doc: <https://csound.com/docs/manual/randi.html>
-}
randi :: Sig -> Sig -> SE Sig
randi b1 b2 = liftOpcDep "randi" rates (b1, b2)
  where
    rates = [(Ar, [Xr, Xr, Ir, Ir, Ir]), (Kr, [Kr, Kr, Ir, Ir, Ir])]

{- |
A white noise generator with an IIR lowpass filter.

> ares  noise  xamp, kbeta

csound doc: <https://csound.com/docs/manual/noise.html>
-}
noise :: Sig -> Sig -> SE Sig
noise b1 b2 = liftOpcDep "noise" rates (b1, b2)
  where
    rates = [(Ar, [Xr, Kr])]

{- |
Generates approximate pink noise.

Generates approximate pink noise (-3dB/oct response) by one of two different methods:

> ares  pinkish  xin [, imethod] [, inumbands] [, iseed] [, iskip]

csound doc: <https://csound.com/docs/manual/pinkish.html>
-}
pinkish :: Sig -> SE Sig
pinkish b1 = liftOpcDep "pinkish" rates b1
  where
    rates = [(Ar, [Xr, Ir, Ir, Ir, Ir])]

{- |
Generate random spline curves.

> ares  rspline  xrangeMin, xrangeMax, kcpsMin, kcpsMax
> kres  rspline  krangeMin, krangeMax, kcpsMin, kcpsMax

csound doc: <https://csound.com/docs/manual/rspline.html>
-}
rspline :: Sig -> Sig -> Sig -> Sig -> SE Sig
rspline b1 b2 b3 b4 = liftOpcDep "rspline" rates (b1, b2, b3, b4)
  where
    rates = [(Ar, [Xr, Xr, Kr, Kr]), (Kr, [Kr, Kr, Kr, Kr])]

{- |
Implementation of a band-limited oscillator using pre-calculated tables.

vco2 is similar to vco. But the implementation uses pre-calculated tables of band-limited waveforms (see also GEN30) rather than integrating impulses. This opcode can be faster than vco (especially if a low control-rate is used) and also allows better sound quality. Additionally, there are more waveforms and oscillator phase can be modulated at k-rate. The disadvantage is increased memory usage. For more details about vco2 tables, see also vco2init and vco2ft.

> ares  vco2  kamp, kcps [, imode] [, kpw] [, kphs] [, inyx]

csound doc: <https://csound.com/docs/manual/vco2.html>
-}
vco2 :: Sig -> Sig -> Sig
vco2 b1 b2 = liftOpc "vco2" rates (b1, b2)
  where
    rates = [(Ar, [Kr, Kr, Ir, Kr, Kr, Ir])]

{- |
Basic frequency modulated oscillator with linear interpolation.

> ares  foscili  xamp, kcps, xcar, xmod, kndx, ifn [, iphs]

csound doc: <https://csound.com/docs/manual/foscili.html>
-}
foscili :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig
foscili b1 b2 b3 b4 b5 b6 = liftOpc "foscili" rates (b1, b2, b3, b4, b5, b6)
  where
    rates = [(Ar, [Xr, Kr, Xr, Xr, Kr, Ir, Ir])]

{- |
Generate control signal consisting of exponential segments delimited by two or more specified points.

Generate control signal consisting of exponential segments delimited by two or more specified points. The entire envelope is looped at kfreq rate. Each parameter can be varied at k-rate.

> ksig  loopxseg  kfreq, ktrig, iphase, kvalue0, ktime0  [, kvalue1] [, ktime1] \
>           [, kvalue2] [, ktime2] [...]

csound doc: <https://csound.com/docs/manual/loopxseg.html>
-}
loopxseg :: Sig -> Sig -> D -> [Sig] -> Sig
loopxseg b1 b2 b3 b4 =
  liftOpc "loopxseg" [(Kr, [Kr, Kr, Ir] ++ (repeat Kr))] (b1, b2, b3, b4, 0 :: Sig)

{- |
Generate control signal consisting of linear segments delimited by two or more specified points.

Generate control signal consisting of linear segments delimited by two or more specified points. The entire envelope is looped at kfreq rate. Each parameter can be varied at k-rate.

> ksig  loopseg  kfreq, ktrig, iphase, kvalue0, ktime0 [, kvalue1] [, ktime1] \
>     [, kvalue2] [, ktime2][...]

csound doc: <https://csound.com/docs/manual/loopseg.html>
-}
loopseg :: Sig -> Sig -> D -> [Sig] -> Sig
loopseg b1 b2 b3 b4 =
  liftOpc "loopseg" [(Kr, [Kr, Kr, Ir] ++ (repeat Kr))] (b1, b2, b3, b4, 0 :: Sig)

{- |
Generate control signal consisting of held segments.

Generate control signal consisting of held segments delimited by two or more specified points. The entire envelope is looped at kfreq rate. Each parameter can be varied at k-rate.

> ksig  lpshold  kfreq, ktrig, iphase, kvalue0, ktime0  [, kvalue1] [, ktime1] [, kvalue2] [, ktime2] [...]

csound doc: <https://csound.com/docs/manual/lpshold.html>
-}
lpshold :: Sig -> Sig -> D -> [Sig] -> Sig
lpshold b1 b2 b3 b4 =
  liftOpc "lpshold" [(Kr, [Kr, Kr, Ir] ++ (repeat Kr))] (b1, b2, b3, b4, 0 :: Sig)

{- |
Applies portamento to a step-valued control signal.

> kres  portk  ksig, khtim [, isig]

csound doc: <https://csound.com/docs/manual/portk.html>
-}
portk :: Sig -> Sig -> Sig
portk b1 b2 = liftOpc "portk" [(Kr, [Kr, Kr, Ir])] (b1, b2)

{- |
Calculates the classical ADSR envelope using the expsegr mechanism.

> ares  mxadsr  iatt, idec, islev, irel [, idel] [, ireltim]
> kres  mxadsr  iatt, idec, islev, irel [, idel] [, ireltim]

csound doc: <https://csound.com/docs/manual/mxadsr.html>
-}
mxadsr :: D -> D -> D -> D -> Sig
mxadsr b1 b2 b3 b4 =
  liftOpc
    "mxadsr"
    [(Ar, [Ir, Ir, Ir, Ir, Ir, Ir]), (Kr, [Ir, Ir, Ir, Ir, Ir, Ir])]
    (b1, b2, b3, b4)

{- |
A second-order multi-mode filter.

> ares  bqrez  asig, xfco, xres [, imode] [, iskip]

csound doc: <https://csound.com/docs/manual/bqrez.html>
-}
bqrez :: Sig -> Sig -> Sig -> Sig
bqrez b1 b2 b3 = liftOpc "bqrez" [(Ar, [Ar, Xr, Xr, Ir, Ir])] (b1, b2, b3)

{- |
Zero-delay feedback implementation of 4 pole diode ladder filter.

Zero-delay feedback implementation of a 4 pole (24 dB/oct) diode low-pass filter. This filter design was originally used in the EMS VCS3 and was the resonant filter in the Roland TB-303.

> asig  diode_ladder  ain, xcf, xk [, inlp, isaturation, istor]

csound doc: <https://csound.com/docs/manual/diode_ladder.html>
-}
diode_ladder :: Sig -> Sig -> Sig -> Sig
diode_ladder b1 b2 b3 =
  liftOpc "diode_ladder" [(Ar, [Ar, Xr, Xr, Ir, Ir, Ir])] (b1, b2, b3)

{- |
Zero-delay feedback implementation of 4 pole ladder filter.

Zero-delay feedback implementation of a 4 pole (24 dB/oct) low-pass filter based on the Moog ladder filter.

> asig  zdf_ladder  ain, xcf, xQ [, istor]

csound doc: <https://csound.com/docs/manual/zdf_ladder.html>
-}
zdf_ladder :: Sig -> Sig -> Sig -> Sig
zdf_ladder b1 b2 b3 =
  liftOpc "zdf_ladder" [(Ar, [Ar, Xr, Xr, Ir])] (b1, b2, b3)

{- |
Zero-delay feedback implementation of 1 pole filter.

Zero-delay feedback implementation of a 1 pole (6 dB/oct) filter. Offers low-pass (default), high-pass, and allpass output modes.

> asig  zdf_1pole  ain, xcf [, kmode, istor]

csound doc: <https://csound.com/docs/manual/zdf_1pole.html>
-}
zdf_1pole :: Sig -> Sig -> Sig
zdf_1pole b1 b2 =
  liftOpc "zdf_1pole" [(Ar, [Ar, Xr, Kr, Ir])] (b1, b2)

{- |
Zero-delay feedback implementation of 1 pole filter with multimode output.

Zero-delay feedback implementation of a 1 pole (6 dB/oct) filter. Offers low-pass and high-pass output.

> alp, ahp  zdf_1pole_mode  ain, xcf [, istor]

csound doc: <https://csound.com/docs/manual/zdf_1pole_mode.html>
-}
zdf_1pole_mode :: Sig -> Sig -> (Sig, Sig)
zdf_1pole_mode b1 b2 =
  liftMulti "zdf_1pole_mode" ([Ar, Ar], [Ar, Xr, Ir]) (b1, b2)

{- |
Zero-delay feedback implementation of 2 pole filter.

Zero-delay feedback implementation of a 2 pole (12 dB/oct) filter. Offers low-pass (default), high-pass, and allpass output modes.

> asig  zdf_2pole  ain, xcf, xQ [, kmode, istor]

csound doc: <https://csound.com/docs/manual/zdf_2pole.html>
-}
zdf_2pole :: Sig -> Sig -> Sig -> Sig
zdf_2pole b1 b2 b3 =
  liftOpc "zdf_2pole" [(Ar, [Ar, Xr, Xr, Kr, Ir])] (b1, b2, b3)

{- |
Zero-delay feedback implementation of 2 pole filter with multimode output.

Zero-delay feedback implementation of a 2 pole (12 dB/oct) filter. Offers low-pass,
      band-pass, and high-pass output.

> alp, abp, ahp  zdf_2pole_mode  ain, xcf, Q [, istor]

csound doc: <https://csound.com/docs/manual/zdf_2pole_mode.html>
-}
zdf_2pole_mode :: Sig -> Sig -> Sig -> (Sig, Sig, Sig)
zdf_2pole_mode b1 b2 b3 =
  liftMulti "zdf_2pole_mode" ([Ar, Ar, Ar], [Ar, Xr, Xr, Ir]) (b1, b2, b3)

{- |
State-variable filter.

Statevar is a new digital implementation of the analogue state-variable filter.
This filter has four simultaneous outputs: high-pass, low-pass,
band-pass and band-reject. This filter uses oversampling for sharper
resonance (default: 3 times oversampling). It includes a
resonance limiter that prevents the filter from getting unstable.

> ahp,alp,abp,abr  statevar  ain, xcf, xq [, iosamps, istor]

csound doc: <https://csound.com/docs/manual/statevar.html>
-}
statevar :: Sig -> Sig -> Sig -> (Sig, Sig, Sig, Sig)
statevar b1 b2 b3 =
  liftMulti "statevar" ([Ar, Ar, Ar, Ar], [Ar, Xr, Xr, Ir, Ir]) (b1, b2, b3)

{- |
A resonant second order filter, with simultaneous lowpass, highpass and bandpass outputs.

Implementation of a resonant second order filter, with simultaneous lowpass, highpass and bandpass outputs.

> alow, ahigh, aband  svfilter   asig, kcf, kq [, iscl] [, iskip]

csound doc: <https://csound.com/docs/manual/svfilter.html>
-}
svfilter :: Sig -> Sig -> Sig -> (Sig, Sig, Sig)
svfilter b1 b2 b3 =
  liftMulti "svfilter" ([Ar, Ar, Ar], [Ar, Kr, Kr, Ir, Ir]) (b1, b2, b3)

{- |
Models some of the filter characteristics of a Roland TB303 voltage-controlled filter.

This opcode attempts to model some of the filter characteristics of a Roland TB303 voltage-controlled filter. Euler's method is used to approximate the system, rather than traditional filter methods. Cutoff frequency, Q, and distortion are all coupled. Empirical methods were used to try to unentwine,  but frequency is only approximate as a result. Future fixes for some problems with this opcode may break existing orchestras relying on this version of tbvcf.

> ares  tbvcf  asig, xfco, xres, kdist, kasym [, iskip]

csound doc: <https://csound.com/docs/manual/tbvcf.html>
-}
tbvcf :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
tbvcf b1 b2 b3 b4 b5 =
  liftOpc "tbvcf" [(Ar, [Ar, Xr, Xr, Kr, Kr, Ir])] (b1, b2, b3, b4, b5)

{- |
Implements low-pass and high-pass filters of different styles.

Implements the classical standard analog filter types: low-pass and high-pass. They are implemented with the four classical kinds of filters: Butterworth, Chebyshev Type I, Chebyshev Type II, and Elliptical.  The number of poles may be any even number from 2 to 80.

> ares  clfilt  asig, kfreq, itype, inpol [, ikind] [, ipbr] [, isba] [, iskip]

csound doc: <https://csound.com/docs/manual/clfilt.html>
-}
clfilt :: Sig -> Sig -> D -> D -> Sig
clfilt b1 b2 b3 b4 =
  liftOpc "clfilt" [(Ar, [Ar, Kr, Ir, Ir, Ir, Ir, Ir, Ir])] (b1, b2, b3, b4)

{- |
Another resonant lowpass filter.

lowres is a resonant lowpass filter.

> ares  lowres  asig, xcutoff, xresonance [, iskip]

csound doc: <https://csound.com/docs/manual/lowres.html>
-}
lowres :: Sig -> Sig -> Sig -> Sig
lowres b1 b2 b3 =
  liftOpc "lowres" [(Ar, [Ar, Xr, Xr, Ir])] (b1, b2, b3)

{- |
A bank of filters in which the cutoff frequency can be separated under user control.

A bank of filters in which the cutoff frequency can be separated under user control

> ares  vlowres  asig, kfco, kres, iord, ksep

csound doc: <https://csound.com/docs/manual/vlowres.html>
-}
vlowres :: Sig -> Sig -> Sig -> D -> Sig -> Sig
vlowres b1 b2 b3 b4 b5 =
  liftOpc "vlowres" [(Ar, [Ar, Kr, Kr, Ir, Kr])] (b1, b2, b3, b4, b5)

{- |
A resonant low-pass filter.

> ares  rezzy  asig, xfco, xres [, imode, iskip]

csound doc: <https://csound.com/docs/manual/rezzy.html>
-}
rezzy :: Sig -> Sig -> Sig -> Sig
rezzy b1 b2 b3 = liftOpc "rezzy" [(Ar, [Ar, Xr, Xr, Ir, Ir])] (b1, b2, b3)

{- |
Moog ladder lowpass filter.

Moogladder is an new digital implementation of the Moog ladder filter based on
the work of Antti Huovilainen, described in the paper "Non-Linear Digital
Implementation of the Moog Ladder Filter" (Proceedings of DaFX04, Univ of Napoli).
This implementation is probably a more accurate digital representation of
the original analogue filter.

> asig  moogladder  ain, kcf, kres[, istor]
> asig  moogladder  ain, acf, kres[, istor]
> asig  moogladder  ain, kcf, ares[, istor]
> asig  moogladder  ain, acf, ares[, istor]

csound doc: <https://csound.com/docs/manual/moogladder.html>
-}
moogladder :: Sig -> Sig -> Sig -> Sig
moogladder b1 b2 b3 =
  liftOpc
    "moogladder"
    [ (Ar, [Ar, Kr, Kr, Ir])
    , (Ar, [Ar, Ar, Kr, Ir])
    , (Ar, [Ar, Kr, Ar, Ir])
    , (Ar, [Ar, Ar, Ar, Ir])
    ]
    (b1, b2, b3)

{- |
A digital emulation of the Moog diode ladder filter configuration.

> ares  moogvcf  asig, xfco, xres [,iscale, iskip]

csound doc: <https://csound.com/docs/manual/moogvcf.html>
-}
moogvcf :: Sig -> Sig -> Sig -> Sig
moogvcf b1 b2 b3 =
  liftOpc "moogvcf" [(Ar, [Ar, Xr, Xr, Ir, Ir])] (b1, b2, b3)

{- |
Generate glissandos starting from a control signal.

> kres  lineto  ksig, ktime

csound doc: <https://csound.com/docs/manual/lineto.html>
-}
lineto :: Sig -> Sig -> Sig
lineto b1 b2 = liftOpc "lineto" [(Kr, [Kr, Kr])] (b1, b2)

{- |
A 3-pole sweepable resonant lowpass filter.

Implementation of a 3 pole sweepable resonant lowpass filter.

> ares  lpf18  asig, xfco, xres, xdist [, iskip]

csound doc: <https://csound.com/docs/manual/lpf18.html>
-}
lpf18 :: Sig -> Sig -> Sig -> Sig -> Sig
lpf18 b1 b2 b3 b4 =
  liftOpc "lpf18" [(Ar, [Ar, Xr, Xr, Xr, Ir])] (b1, b2, b3, b4)

{- |
A notch filter whose transfer functions are the complements of
      the reson opcode.

> ares  areson  asig, kcf, kbw [, iscl] [, iskip]
> ares  areson  asig, acf, kbw [, iscl] [, iskip]
> ares  areson  asig, kcf, abw [, iscl] [, iskip]
> ares  areson  asig, acf, abw [, iscl] [, iskip]

csound doc: <https://csound.com/docs/manual/areson.html>
-}
areson :: Sig -> Sig -> Sig -> Sig
areson b1 b2 b3 =
  liftOpc
    "areson"
    [ (Ar, [Ar, Kr, Kr, Ir, Ir])
    , (Ar, [Ar, Ar, Kr, Ir, Ir])
    , (Ar, [Ar, Kr, Ar, Ir, Ir])
    , (Ar, [Ar, Ar, Ar, Ir, Ir])
    ]
    (b1, b2, b3)

{- |
Moog voltage-controlled highpass filter emulation.

Mvchpf is an digital implementation of the 4th-order (24 dB/oct)  Moog
high-pass filter, originally written by Fons Andriaensen. According to the author,
mvchpf "...is based on the voltage controlled highpass filter by Robert Moog.
again with some attention to the nonlinear effects."

> asig  mvchpf  ain, xcf[, istor]

csound doc: <https://csound.com/docs/manual/mvchpf.html>
-}
mvchpf :: Sig -> Sig -> Sig
mvchpf b1 b2 = liftOpc "mvchpf" [(Ar, [Ar, Xr, Ir])] (b1, b2)

{- |
Moog voltage-controlled lowpass filter emulation.

Mvclpf1 is an digital implementation of the 4th-order (24 dB/oct)  Moog ladder filter
originally written by Fons Andriaensen. According to the author,
mvclpf1 "is a fairly simple design, and it does not even pretend to come
close the 'real thing'. It uses a very crude approximation of the non-linear
resistor in the first filter section only. [...] [I]t's [a] cheap (in
terms of CPU usage) general purpose 24 dB/oct lowpass
filter that could be useful".

> asig  mvclpf1  ain, xcf, xres[,istor]

csound doc: <https://csound.com/docs/manual/mvclpf1.html>
-}
mvclpf1 :: Sig -> Sig -> Sig -> Sig
mvclpf1 b1 b2 b3 =
  liftOpc "mvclpf1" [(Ar, [Ar, Xr, Xr, Ir])] (b1, b2, b3)

{- |
Moog voltage-controlled lowpass filter emulation.

Mvclpf2 is an digital implementation of the 4th-order (24 dB/oct) Moog ladder filter
originally written by Fons Andriaensen. According to the author,
mvclpf2 "uses five non-linear elements, in the input and in all four filter
sections. It works by using the derivative of the nonlinearity (for which
1 / (1 + x * x) is reasonable approximation). The main advantage of this is
that only one evaluation of the non-linear function is required for each
section".

> asig  mvclpf2  ain, xcf, xres[, istor]

csound doc: <https://csound.com/docs/manual/mvclpf2.html>
-}
mvclpf2 :: Sig -> Sig -> Sig -> Sig
mvclpf2 b1 b2 b3 =
  liftOpc "mvclpf2" [(Ar, [Ar, Xr, Xr, Ir])] (b1, b2, b3)

{- |
Moog voltage-controlled lowpass filter emulation.

Mvclpf3 is an digital implementation of the 4th-order (24 dB/oct) Moog ladder filter
originally written by Fons Andriaensen. According to the author,
mvclpf3 "is based on mvclpf2 , with two differences. It uses the
the technique described by Stilson and Smith to extend the constant-Q
range, and the internal sample frequency is doubled, giving a better
approximation to the non-linear behaviour at high freqencies.
This version has high Q over the entire frequency range and will
oscillate up to above 10 kHz, while the two others show a decreasing
Q at high frequencies. Mvclpf3  is reasonably well tuned, and can be
'played' as a VCO up to at least 5 kHz".

> asig  mvclpf3  ain, xcf, xres[, istor]

csound doc: <https://csound.com/docs/manual/mvclpf3.html>
-}
mvclpf3 :: Sig -> Sig -> Sig -> Sig
mvclpf3 b1 b2 b3 =
  liftOpc "mvclpf3" [(Ar, [Ar, Xr, Xr, Ir])] (b1, b2, b3)

{- |
Moog voltage-controlled lowpass filter emulation.

Mvclpf4 is an digital implementation of the 4th-order (24 dB/oct) Moog ladder filter
originally written by Fons Andriaensen. It is a version of the
mvclpf3 opcode with four outputs, for 6dB, 12dB, 18dB, and
24 dB/octave responses.

> asig1,asig2,asig3,asig4  mvclpf4  ain, xcf, xres[, istor]

csound doc: <https://csound.com/docs/manual/mvclpf4.html>
-}
mvclpf4 :: Sig -> Sig -> Sig -> (Sig, Sig, Sig, Sig)
mvclpf4 b1 b2 b3 =
  liftMulti "mvclpf4" ([Ar, Ar, Ar, Ar], [Ar, Xr, Xr, Ir]) (b1, b2, b3)

{- |
Generate an fsig from a mono audio source ain, using phase vocoder overlap-add analysis.

> fsig  pvsanal  ain, ifftsize, ioverlap, iwinsize, iwintype [, iformat] [, iinit]

csound doc: <https://csound.com/docs/manual/pvsanal.html>
-}
pvsanal :: Sig -> D -> D -> D -> D -> Spec
pvsanal b1 b2 b3 b4 b5 =
  liftOpc "pvsanal" [(Fr, [Ar, Ir, Ir, Ir, Ir, Ir, Ir])] (b1, b2, b3, b4, b5)

{- |
Resynthesise using a FFT overlap-add.

Resynthesise phase vocoder data (f-signal) using a FFT overlap-add.

> ares  pvsynth  fsrc, [iinit]

csound doc: <https://csound.com/docs/manual/pvsynth.html>
-}
pvsynth :: Spec -> Sig
pvsynth b1 = liftOpc "pvsynth" [(Ar, [Fr, Ir])] b1

{- |
Scale the frequency components of a pv stream.

Scale the frequency components of a pv stream, resulting
      in pitch shift. Output amplitudes can be optionally modified in order
      to attempt formant preservation.

> fsig  pvscale  fsigin, kscal[, kkeepform, kgain, kcoefs]

csound doc: <https://csound.com/docs/manual/pvscale.html>
-}
pvscale :: Spec -> Sig -> Spec
pvscale b1 b2 = liftOpc "pvscale" [(Fr, [Fr, Kr, Kr, Kr, Kr])] (b1, b2)

{- |
Shift the frequency components of a pv stream, stretching/compressing
      its spectrum.

> fsig  pvshift  fsigin, kshift, klowest[, kkeepform, igain, kcoefs]

csound doc: <https://csound.com/docs/manual/pvshift.html>
-}
pvshift :: Spec -> Sig -> Sig -> Spec
pvshift b1 b2 b3 = liftOpc "pvshift" [(Fr, [Fr, Kr, Kr, Kr, Ir, Kr])] (b1, b2, b3)

{- |
Streaming partial track additive synthesis

The tradsyn opcode takes an input containg a TRACKS pv streaming signal (as generated,
      for instance by partials),as described in Lazzarini et al, "Time-stretching using the Instantaneous Frequency Distribution and Partial
      Tracking", Proc.of ICMC05, Barcelona. It resynthesises the signal using linear amplitude and frequency
      interpolation to drive a bank of interpolating oscillators with amplitude and pitch scaling controls.

> asig  tradsyn  fin, kscal, kpitch, kmaxtracks, ifn

csound doc: <https://csound.com/docs/manual/tradsyn.html>
-}
tradsyn :: Spec -> Sig -> Sig -> Sig -> Tab -> Sig
tradsyn b1 b2 b3 b4 b5 =
  liftOpc "tradsyn" [(Ar, [Fr, Kr, Kr, Kr, Ir])] (b1, b2, b3, b4, b5)

{- |
Instantaneous Frequency Distribution, magnitude and phase analysis.

The pvsifd opcode takes an input a-rate signal and performs an Instantaneous Frequency,
  magnitude and phase analysis, using the STFT and pvsifd (Instantaneous Frequency Distribution),
  as described in Lazzarini et al, "Time-stretching using the Instantaneous Frequency Distribution and Partial
  Tracking", Proc.of ICMC05, Barcelona. It generates two PV streaming signals, one containing the
  amplitudes and frequencies (a similar output to pvsanal) and another containing amplitudes and
  unwrapped phases.

> ffr,fphs  pvsifd  ain, ifftsize, ihopsize, iwintype[,iscal]

csound doc: <https://csound.com/docs/manual/pvsifd.html>
-}
pvsifd :: Sig -> D -> D -> D -> (Spec, Spec)
pvsifd b1 b2 b3 b4 =
  liftMulti "pvsifd" ([Fr, Fr], [Ar, Ir, Ir, Ir, Ir]) (b1, b2, b3, b4)

{- |
Calculates a factor to raise/lower a frequency by a given amount of semitones.

>  semitone (x)

csound doc: <https://csound.com/docs/manual/semitone.html>
-}
semitone :: (SigOrD a) => a -> a
semitone b1 = liftOpr1 "semitone" b1

{- |
Streaming partial track cross-synthesis.

The trcross opcode takes two inputs containg TRACKS pv streaming signals (as generated,
      for instance by partials) and cross-synthesises them into a single TRACKS stream. Two
      different modes of operation are used: mode 0, cross-synthesis by multiplication of
      the amplitudes of the two inputs and mode 1, cross-synthesis by the substititution of
      the amplitudes of input 1 by the input 2. Frequencies and phases of input 1 are preserved
      in the output. The cross-synthesis is done by matching tracks between the two inputs using
      a 'search interval'. The matching algorithm will look for tracks in the second input that
      are within the search interval around each track in the first input. This interval can be changed
      at the control rate. Wider search intervals will find more matches.

> fsig  trcross  fin1, fin2, ksearch, kdepth [, kmode]

csound doc: <https://csound.com/docs/manual/trcross.html>
-}
trcross :: Spec -> Spec -> Sig -> Sig -> Spec
trcross b1 b2 b3 b4 = liftOpc "trcross" [(Fr, [Fr, Fr, Kr, Kr, Kr])] (b1, b2, b3, b4)

{- |
Partial track spectral analysis.

The partials opcode takes two input PV streaming signals containg AMP_FREQ and AMP_PHASE signals (as generated
  for instance by pvsifd or in the first case, by pvsanal) and performs partial track analysis,
  as described in Lazzarini et al, "Time-stretching using the Instantaneous Frequency Distribution and Partial
  Tracking", Proc.of ICMC05, Barcelona. It generates a TRACKS PV streaming signal, containing amplitude, frequency,
  phase and track ID for each output track. This type of signal will contain a variable number of output tracks,
  up to the total number of analysis bins contained in the inputs (fftsize/2 + 1 bins). The second input (AMP_PHASE)
  is optional, as it can take the same signal as the first input. In this case, however, all phase information will
  be NULL and resynthesis using phase information cannot be performed.

> ftrks  partials  ffr, fphs, kthresh, kminpts, kmaxgap, imaxtracks

csound doc: <https://csound.com/docs/manual/partials.html>
-}
partials :: Spec -> Spec -> Sig -> Sig -> Sig -> D -> Spec
partials b1 b2 b3 b4 b5 b6 =
  liftOpc "partials" [(Fr, [Fr, Fr, Kr, Kr, Kr, Ir])] (b1, b2, b3, b4, b5, b6)

{- |
Random impulses.

Generates random impulses from 0 to 1.

> ares  dust  kamp, kdensity
> kres  dust  kamp, kdensity

csound doc: <https://csound.com/docs/manual/dust.html>
-}
dust :: Sig -> Sig -> SE Sig
dust b1 b2 = liftOpcDep "dust" [(Ar, [Kr, Kr]), (Kr, [Kr, Kr])] (b1, b2)

{- |
Random impulses.

Generates random impulses from -1 to 1.

> ares  dust2  kamp, kdensity
> kres  dust2  kamp, kdensity

csound doc: <https://csound.com/docs/manual/dust2.html>
-}
dust2 :: Sig -> Sig -> SE Sig
dust2 b1 b2 = liftOpcDep "dust2" [(Ar, [Kr, Kr]), (Kr, [Kr, Kr])] (b1, b2)

{- |
Modify a signal by down-sampling.

> kres  downsamp  asig [, iwlen]

csound doc: <https://csound.com/docs/manual/downsamp.html>
-}
downsamp :: Sig -> Sig
downsamp b1 = liftOpc "downsamp" [(Kr, [Ar, Ir])] b1

{- |
Random impulses around a certain frequency.

Generates random impulses around a certain frequency.

> ares  gausstrig  kamp, kcps, kdev [, imode] [, ifrst1]
> kres  gausstrig  kamp, kcps, kdev [, imode] [, ifrst1]

csound doc: <https://csound.com/docs/manual/gausstrig.html>
-}
gausstrig :: Sig -> Sig -> Sig -> SE Sig
gausstrig b1 b2 b3 =
  liftOpcDep "gausstrig" [(Ar, [Kr, Kr, Kr, Ir, Ir]), (Kr, [Kr, Kr, Kr, Ir, Ir])] (b1, b2, b3)

{- |
Displays the values init (i-rate) variables.

These units will print orchestra init-values.

>  print  iarg [, iarg1] [, iarg2] [...]

csound doc: <https://csound.com/docs/manual/print.html>
-}
print' :: [D] -> SE ()
print' b1 = liftOpcDep_ "print" [(Xr, (repeat Ir))] b1

{- |
Modify a signal by differentiation.

> ares  diff  asig [, iskip]
> kres  diff  ksig [, iskip]

csound doc: <https://csound.com/docs/manual/diff.html>
-}
diff :: Sig -> Sig
diff b1 = liftOpc "diff" [(Ar, [Ar, Ir]), (Kr, [Kr, Ir])] b1

{- |
Get a MIDI note number (allows customized micro-tuning scales).

This unit is similar to cpsmidi, but allows fully customized micro-tuning scales.

> icps  cpstmid  ifn

csound doc: <https://csound.com/docs/manual/cpstmid.html>
-}
cpstmid :: Tab -> D
cpstmid b1 = liftOpc "cpstmid" [(Ir, [Ir])] b1

eventD :: forall a. (Tuple a) => Str -> D -> Sig -> Sig -> a -> SE ()
eventD eventType instrNum kdel kdur args =
  liftOpcDep_ "event" rates (eventType, instrNum, kdel, kdur, args)
  where
    rates = [(Xr, [Sr, Kr, Kr, Kr] <> replicate (tupleArity @a) Kr)]

eventStr :: forall a. (Tuple a) => Str -> Str -> Sig -> Sig -> a -> SE ()
eventStr eventType instrNum kdel kdur args =
  liftOpcDep_ "event" rates (eventType, instrNum, kdel, kdur, args)
  where
    rates = [(Xr, [Sr, Kr, Kr, Kr] <> replicate (tupleArity @a) Kr)]
