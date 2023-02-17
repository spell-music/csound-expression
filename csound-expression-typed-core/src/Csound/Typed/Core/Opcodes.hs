{-# Language InstanceSigs #-}
-- | Essential opcodes. Top 100 opcodes.
-- See the package csound-expression-opcodes for full list of Csound opcodes
module Csound.Typed.Core.Opcodes
  (
  -- * Oscillators / Phasors
    poscil, poscil3
  , oscil, oscil3
  , vcoInit, VcoInit (..), VcoShape (..)
  , oscilikt
  , osc, sqr, tri, saw, vcoTab
  , buzz, gbuzz
  , mpulse
  , phasor

  -- * Random generators
  , rand
  , rnd, birnd
  , random, randomi, randomh
  , seed

  -- * Envelopes
  , linen, linenr
  , madsr, adsr

  -- * Line generators
  , linseg, expseg
  , linsegr, expsegr

  -- * Line Smooth
  , lag, lagud

  -- * Sound Files / Samples
  , diskin, diskin2
  , mp3in
  , loscil, loscil3, loscilx, lphasor
  , flooper, flooper2
  , filescal, mincer
  , filelen

  -- * Audio I/O
  -- for outs and ins use functions writeOuts and readIns
  , monitor
  , inch
  , outch

  -- * Tables (Buffers)
  , table, tablei, table3
  , tablew
  -- ** Read-only tables (pure)
  , getTable, getTablei, getTable3
  -- TODO
  -- ftgen, ftsamplebank

  -- * Arrays - check the list

  -- * Program Control
  , changed, changed2
  , trigger

  -- * Instrument Control
  , schedule
  , active
  , maxalloc
  , nstrnum
  , turnoff2
  , turnoff2_i
  , turnoffSelf
  , turnoffSelf_i
  , stopSelf
  , stopInstr
  , stopInstr_i

  -- * Time
  , metro, metro2
  , timeinsts

  -- * Channels
  , Chan, newChan, getChanName
  , chnmix, chnclear

  -- * MIDI
  , massign
  , notnum, veloc
  , midictrl, ctrlinit, CtrlInit (..)
  , ctrl7, ctrl14
  , initc7, initc14

  -- * OSC
  , OscHandle, oscInit, oscListen, oscSend

  -- * Panning / Spatialization
  , pan2, vbap, vbaplsinit

  -- * Reverb
  , freeverb, reverbsc

  -- * Spectral Processing

  -- * Convolution
  , pconvolve

  -- * Physical Models
  , pluck

  -- * Delay
  , vdelayx, comb, vcomb

  -- * Distortion
  , distort, distort1, powershape

  -- * Filter
  , tone, atone, reson, butlp, buthp, butbp, butbr, mode, zdfLadder, moogvcf2

  -- * Level
  , rms, balance, balance2

  -- * Math / Conversion
  , ampdb, dbamp

  -- * Amplitude / Pitch Tracking
  , follow, follow2, ptrack

  -- * Print
  , printi, printk, prints, printks, printk2, fprint
  -- * File IO
  , fout, ftsave, fprints, readf, readfi
  -- * Signal Type Conversion
  ) where

import Control.Monad.Trans.Class (lift)

import Csound.Dynamic qualified as Dynamic
import Csound.Dynamic (Rate (..))

import Csound.Typed.Core.Types
import Csound.Typed.Core.Opcodes.Instr
import Csound.Typed.Core.Opcodes.Osc
import Csound.Typed.Core.Opcodes.Vco

----------------------------------------------------------------------------------
-- Oscillators / Phasors

-- |
-- High precision oscillator.
--
-- > ares  poscil  aamp, acps [, ifn, iphs]
-- > ares  poscil  aamp, kcps [, ifn, iphs]
-- > ares  poscil  kamp, acps [, ifn, iphs]
-- > ares  poscil  kamp, kcps [, ifn, iphs]
-- > ires  poscil  kamp, kcps [, ifn, iphs]
-- > kres  poscil  kamp, kcps [, ifn, iphs]
--
-- csound doc: <http://csound.com/docs/manual/poscil.html>
poscil ::  Sig -> Sig -> Tab -> Sig
poscil b1 b2 b3 = liftOpc "poscil" rates (b1, b2, b3)
  where
    rates =
      [(Ar,[Ar,Ar,Ir,Ir])
      ,(Ar,[Ar,Kr,Ir,Ir])
      ,(Ar,[Kr,Ar,Ir,Ir])
      ,(Ar,[Kr,Kr,Ir,Ir])
      ,(Ir,[Kr,Kr,Ir,Ir])
      ,(Kr,[Kr,Kr,Ir,Ir])]

-- |
-- High precision oscillator with cubic interpolation.
--
-- > ares  poscil3  aamp, acps [, ifn, iphs]
-- > ares  poscil3  aamp, kcps [, ifn, iphs]
-- > ares  poscil3  kamp, acps [, ifn, iphs]
-- > ares  poscil3  kamp, kcps [, ifn, iphs]
-- > ires  poscil3  kamp, kcps [, ifn, iphs]
-- > kres  poscil3  kamp, kcps [, ifn, iphs]
--
-- csound doc: <http://csound.com/docs/manual/poscil3.html>
poscil3 ::  Sig -> Sig -> Tab -> Sig
poscil3 b1 b2 b3 = liftOpc "poscil3" rates (b1, b2, b3)
  where
    rates =
      [(Ar,[Ar,Ar,Ir,Ir])
      ,(Ar,[Ar,Kr,Ir,Ir])
      ,(Ar,[Kr,Ar,Ir,Ir])
      ,(Ar,[Kr,Kr,Ir,Ir])
      ,(Ir,[Kr,Kr,Ir,Ir])
      ,(Kr,[Kr,Kr,Ir,Ir])]

osc :: Sig -> Sig
osc cps = poscil3 1 cps (sines [1])


-- |
-- A simple oscillator.
--
-- oscil reads table ifn sequentially and repeatedly at a frequency xcps. The amplitude is scaled by xamp.
--
-- > ares  oscil  xamp, xcps [, ifn, iphs]
-- > kres  oscil  kamp, kcps [, ifn, iphs]
--
-- csound doc: <http://csound.com/docs/manual/oscil.html>
oscil ::  Sig -> Sig -> Tab -> Sig
oscil amp cps t =  liftOpc "oscil" [(Ar,[Xr,Xr,Ir,Ir]),(Kr,[Kr,Kr,Ir,Ir])] (amp, cps, t)

-- |
-- A simple oscillator with cubic interpolation.
--
-- oscil3 reads table ifn sequentially and repeatedly at a frequency xcps. The amplitude is scaled by xamp.
--
-- > ares  oscil3  xamp, xcps [, ifn, iphs]
-- > kres  oscil3  kamp, kcps [, ifn, iphs]
--
-- csound doc: <http://csound.com/docs/manual/oscil.html>
oscil3 ::  Sig -> Sig -> Tab -> Sig
oscil3 amp cps t =  liftOpc "oscil3" [(Ar,[Xr,Xr,Ir,Ir]),(Kr,[Kr,Kr,Ir,Ir])] (amp, cps, t)

-- Dynamic Spectrum Oscillators.

-- |
-- Output is a set of harmonically related sine partials.
--
-- > ares  buzz  xamp, xcps, knh, ifn [, iphs]
--
-- csound doc: <http://csound.com/docs/manual/buzz.html>
buzz ::  Sig -> Sig -> Sig -> Tab -> Sig
buzz b1 b2 b3 b4 = liftOpc "buzz" rates (b1, b2, b3, b4)
  where rates = [(Ar,[Xr,Xr,Kr,Ir,Ir])]

-- |
-- Output is a set of harmonically related cosine partials.
--
-- > ares  gbuzz  xamp, xcps, knh, klh, kmul, ifn [, iphs]
--
-- csound doc: <http://csound.com/docs/manual/gbuzz.html>
gbuzz ::  Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig
gbuzz b1 b2 b3 b4 b5 b6 = liftOpc "gbuzz" rates (b1, b2, b3, b4, b5, b6)
  where rates = [(Ar,[Xr,Xr,Kr,Kr,Kr,Ir,Ir])]

-- |
-- Generates a set of impulses.
--
-- Generates a set of impulses of amplitude kamp separated by kintvl seconds (or samples if kintvl is negative).  The first impulse is generated after a delay of ioffset seconds.
--
-- > ares  mpulse  kamp, kintvl [, ioffset]
--
-- csound doc: <http://csound.com/docs/manual/mpulse.html>
mpulse ::  Sig -> Sig -> Sig
mpulse b1 b2 = liftOpc "mpulse" rates (b1, b2)
  where rates = [(Ar,[Kr,Kr,Ir])]

-- |
-- Produce a normalized moving phase value.
--
-- > ares  phasor  xcps [, iphs]
-- > kres  phasor  kcps [, iphs]
--
-- csound doc: <http://csound.com/docs/manual/phasor.html>
phasor ::  Sig -> Sig
phasor b1 = liftOpc "phasor" rates b1
  where rates = [(Ar,[Xr,Ir]),(Kr,[Kr,Ir])]

----------------------------------------------------------------------------------
-- random generators

-- |
-- Generates a controlled random number series.
--
-- Output is a controlled random number series between -amp and +amp
--
-- > ares  rand  xamp [, iseed] [, isel] [, ioffset]
-- > kres  rand  xamp [, iseed] [, isel] [, ioffset]
--
-- csound doc: <http://csound.com/docs/manual/rand.html>
rand ::  Sig -> SE Sig
rand b1 = liftOpcDep "rand" rates b1
  where rates = [(Ar,[Xr,Ir,Ir,Ir]),(Kr,[Xr,Ir,Ir,Ir])]

-- |
-- Returns a random number in a unipolar range at the rate given by the input argument.
--
-- >  rnd (x) (init- or control-rate only)
--
-- csound doc: <http://csound.com/docs/manual/rnd.html>
rnd :: SigOrD a => a -> SE a
rnd b1 = liftOpr1kDep "rnd" b1

-- |
-- Returns a random number in a unipolar range at the rate given by the input argument.
--
-- >  rnd (x) (init- or control-rate only)
--
-- csound doc: <http://csound.com/docs/manual/rnd.html>
birnd :: SigOrD a => a -> SE a
birnd b1 = liftOpr1kDep "birnd" b1

-- |
-- Generates a controlled pseudo-random number series between min and max values.
--
-- Generates is a controlled pseudo-random number series between min and max values.
--
-- > ares  random  kmin, kmax
-- > ires  random  imin, imax
-- > kres  random  kmin, kmax
--
-- csound doc: <http://csound.com/docs/manual/random.html>
random :: SigOrD a => a -> a -> SE a
random b1 b2 = liftOpcDep "random" rates (b1, b2)
  where rates = [(Ar,[Kr,Kr]),(Ir,[Ir,Ir]),(Kr,[Kr,Kr])]

-- |
-- Generates random numbers with a user-defined limit and holds them for a period of time.
--
-- > ares  randomh  kmin, kmax, xcps [,imode] [,ifirstval]
-- > kres  randomh  kmin, kmax, kcps [,imode] [,ifirstval]
--
-- csound doc: <http://csound.com/docs/manual/randomh.html>
randomh ::  Sig -> Sig -> Sig -> SE Sig
randomh b1 b2 b3 = liftOpcDep "randomh" rates (b1, b2, b3)
  where rates = [(Ar,[Kr,Kr,Xr,Ir,Ir]),(Kr,[Kr,Kr,Kr,Ir,Ir])]

-- |
-- Generates a user-controlled random number series with interpolation between each new number.
--
-- > ares  randomi  kmin, kmax, xcps [,imode] [,ifirstval]
-- > kres  randomi  kmin, kmax, kcps [,imode] [,ifirstval]
--
-- csound doc: <http://csound.com/docs/manual/randomi.html>
randomi ::  Sig -> Sig -> Sig -> SE Sig
randomi b1 b2 b3 = liftOpcDep "randomi" rates (b1, b2, b3)
  where rates = [(Ar,[Kr,Kr,Xr,Ir,Ir]),(Kr,[Kr,Kr,Kr,Ir,Ir])]

-- |
-- Sets the global seed value.
--
-- Sets the global seed value for all x-class noise generators, as well as other opcodes that use a random call, such as grain.
--
-- >  seed  ival
--
-- csound doc: <http://csound.com/docs/manual/seed.html>
seed ::  D -> SE ()
seed b1 = global $ liftOpcDep_ "seed" rates b1
    where rates = [(Xr,[Ir])]

----------------------------------------------------------------------------------
-- line generators

-- |
-- Applies a straight line rise and decay pattern to an input amp signal.
--
-- linen -- apply a straight line rise and decay pattern to an input amp signal.
--
-- > ares  linen  xamp, irise, idur, idec
-- > kres  linen  kamp, irise, idur, idec
--
-- csound doc: <http://csound.com/docs/manual/linen.html>
linen ::  Sig -> D -> D -> D -> Sig
linen b1 b2 b3 b4 = liftOpc "linen" rates (b1,b2,b3,b4)
  where rates = [(Ar,[Xr,Ir,Ir,Ir]),(Kr,[Kr,Ir,Ir,Ir])]

-- |
-- The linen opcode extended with a final release segment.
--
-- linenr -- same as linen except that the final segment is entered only on sensing a MIDI note release. The note is then extended by the decay time.
--
-- > ares  linenr  xamp, irise, idec, iatdec
-- > kres  linenr  kamp, irise, idec, iatdec
--
-- csound doc: <http://csound.com/docs/manual/linenr.html>
linenr ::  Sig -> D -> D -> D -> Sig
linenr b1 b2 b3 b4 = liftOpc "linenr" rates (b1,b2,b3,b4)
  where rates = [(Ar,[Xr,Ir,Ir,Ir]),(Kr,[Kr,Ir,Ir,Ir])]

-- |
-- Calculates the classical ADSR envelope using the linsegr mechanism.
--
-- > ares  madsr  iatt, idec, islev, irel [, idel] [, ireltim]
-- > kres  madsr  iatt, idec, islev, irel [, idel] [, ireltim]
--
-- csound doc: <http://csound.com/docs/manual/madsr.html>
madsr ::  D -> D -> D -> D -> Sig
madsr b1 b2 b3 b4 = liftOpc "madsr" rates (b1,b2,b3,b4)
  where rates = [(Ar,[Ir,Ir,Ir,Ir,Ir,Ir]),(Kr,[Ir,Ir,Ir,Ir,Ir,Ir])]

-- |
-- Calculates the classical ADSR envelope using linear segments.
--
-- > ares  adsr  iatt, idec, islev, irel [, idel]
-- > kres  adsr  iatt, idec, islev, irel [, idel]
--
-- csound doc: <http://csound.com/docs/manual/adsr.html>
adsr ::  D -> D -> D -> D -> Sig
adsr b1 b2 b3 b4 = liftOpc "adsr" rates (b1,b2,b3,b4)
  where rates = [(Ar,[Ir,Ir,Ir,Ir,Ir]),(Kr,[Ir,Ir,Ir,Ir,Ir])]

----------------------------------------------------------------------------------
-- line generators

-- |
-- Trace a series of line segments between specified points.
--
-- > ares  linseg  ia, idur1, ib [, idur2] [, ic] [...]
-- > kres  linseg  ia, idur1, ib [, idur2] [, ic] [...]
--
-- csound doc: <http://csound.com/docs/manual/linseg.html>
linseg ::  [D] -> Sig
linseg b1 = Sig $ f <$> mapM unD b1
    where f a1 = Dynamic.setRate Kr $ Dynamic.opcs "linseg" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, last a1])

-- |
-- Trace a series of exponential segments between specified points.
--
-- > ares  expseg  ia, idur1, ib [, idur2] [, ic] [...]
-- > kres  expseg  ia, idur1, ib [, idur2] [, ic] [...]
--
-- csound doc: <http://csound.com/docs/manual/linseg.html>
expseg ::  [D] -> Sig
expseg b1 = Sig $ f <$> mapM unD b1
    where f a1 = Dynamic.setRate Kr $ Dynamic.opcs "expseg" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, last a1])

-- |
-- Trace a series of line segments between specified points including a release segment.
--
-- > ares  linsegr  ia, idur1, ib [, idur2] [, ic] [...], irel, iz
-- > kres  linsegr  ia, idur1, ib [, idur2] [, ic] [...], irel, iz
--
-- csound doc: <http://csound.com/docs/manual/linsegr.html>
linsegr ::  [D] -> D -> D -> Sig
linsegr b1 b2 b3 = Sig $ f <$> mapM unD b1 <*> unD b2 <*> unD b3
    where f a1 a2 a3 = Dynamic.setRate Kr $ Dynamic.opcs "linsegr" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, last a1, a2, a3])

-- |
-- Trace a series of exponential segments between specified points including a release segment.
--
-- > ares  expsegr  ia, idur1, ib [, idur2] [, ic] [...], irel, iz
-- > kres  expsegr  ia, idur1, ib [, idur2] [, ic] [...], irel, iz
--
-- csound doc: <http://csound.com/docs/manual/expsegr.html>
expsegr ::  [D] -> D -> D -> Sig
expsegr b1 b2 b3 = Sig $ f <$> mapM unD b1 <*> unD b2 <*> unD b3
    where f a1 a2 a3 = Dynamic.setRate Kr $ Dynamic.opcs "expsegr" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, last a1, a2, a3])

----------------------------------------------------------------------------------
-- Line Smooth

-- |
-- Exponential lag with 60dB lag time. Port of Supercollider's Lag. This is essentially
-- a one pole filter except that instead of supplying the coefficient directly, it is calculated from a 60 dB lag time. This is the time required for the filter to converge to within 0.01% of a value. This is useful for smoothing out control signals.
--
-- > aout lag ain, klagtime [, initialvalue]
-- > kout lag kin, klagtime [, initialvalue]
lag :: Sig -> Sig -> Sig
lag b1 b2 = liftOpc "lag" rates (b1, b2)
  where rates = [(Ar, [Ar, Kr, Ir]), (Kr, [Kr, Kr, Ir])]

-- |
-- Exponential lag with different smoothing time for up- and downgoing signals. Port of Supercollider's LagUD.
--
-- > aout lagud ain, klagup, klagdown [, initialvalue]
-- > kout lagud kin, klagup, klagdown [, initialvalue]
lagud :: Sig -> Sig -> Sig -> Sig
lagud b1 b2 b3 = liftOpc "lagud" rates (b1,b2,b3)
  where rates = [(Ar, [Ar,Kr,Kr,Ir]), (Kr, [Kr,Kr,Kr,Ir])]

----------------------------------------------------------------------------------
--- Sound Files / Samples

diskin :: Str -> Sig2
diskin a = liftMulti "diskin2" ((repeat Ar),[Sr,Kr,Ir,Ir,Ir,Ir,Ir,Ir]) a

diskin2 :: Tuple a => Str -> a
diskin2 a = liftMulti "diskin2" ((repeat Ar),[Sr,Kr,Ir,Ir,Ir,Ir,Ir,Ir]) a

-- |
-- Reads mono or stereo audio data from an external MP3 file.
--
-- > ar1, ar2  mp3in  ifilcod[, iskptim, iformat, iskipinit, ibufsize]
-- > ar1  mp3in  ifilcod[, iskptim, iformat, iskipinit, ibufsize]
--
-- csound doc: <http://csound.com/docs/manual/mp3in.html>
mp3in ::  Str -> (Sig, Sig)
mp3in b1 = liftMulti "mp3in" rates b1
  where rates = ([Ar,Ar],[Sr,Ir,Ir,Ir,Ir])

-- |
-- Read sampled sound from a table.
--
-- Read sampled sound (mono or stereo) from a table, with optional sustain and release looping.
--
-- > ar1 [,ar2]  loscil  xamp, kcps, ifn [, ibas] [, imod1] [, ibeg1] [, iend1] \
-- >           [, imod2] [, ibeg2] [, iend2]
--
-- csound doc: <http://csound.com/docs/manual/loscil.html>
loscil :: Tuple a => Sig -> Sig -> Tab -> a
loscil b1 b2 b3 = liftMulti "loscil" rates (b1,b2,b3)
  where rates =([Ar,Ar],[Xr,Kr,Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir])

-- |
-- Read sampled sound from a table using cubic interpolation.
--
-- Read sampled sound (mono or stereo) from a table, with optional sustain and release looping, using cubic interpolation.
--
-- > ar1 [,ar2]  loscil3  xamp, kcps, ifn [, ibas] [, imod1] [, ibeg1] [, iend1] \
-- >           [, imod2] [, ibeg2] [, iend2]
--
-- csound doc: <http://csound.com/docs/manual/loscil3.html>
loscil3 :: Tuple a => Sig -> Sig -> Tab -> a
loscil3 b1 b2 b3 = liftMulti "loscil3" rates (b1, b2, b3)
  where rates = ([Ar,Ar],[Xr,Kr,Ir,Ir,Ir,Ir,Ir,Ir,Ir,Ir])

-- |
-- Read multi-channel sampled sound from a table.
--
-- Read sampled sound (up to 16 channels) from a table, with
--       optional sustain and release looping.
--
-- > ar1 [, ar2, ar3, ar4, ar5, ar6, ar7, ar8, ar9, ar10, ar11, ar12, ar13, ar14, \
-- >           ar15, ar16]  loscilx  xamp, kcps, ifn \
-- >           [, iwsize, ibas, istrt, imod, ibeg, iend]
--
-- csound doc: <http://csound.com/docs/manual/loscilx.html>
loscilx :: Tuple a => Sig -> Sig -> Tab -> a
loscilx b1 b2 b3 = liftMulti "loscilx" rates (b1, b2, b3)
  where rates = ( [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
                , [Xr,Kr,Ir,Ir,Ir,Ir,Ir,Ir,Ir])

-- |
-- Generates a table index for sample playback
--
-- This opcode can be used to generate table index for sample playback (e.g. tablexkt).
--
-- > ares  lphasor  xtrns [, ilps] [, ilpe] [, imode] [, istrt] [, istor]
--
-- csound doc: <http://csound.com/docs/manual/lphasor.html>
lphasor ::  Sig -> Sig
lphasor b1 = liftOpc "lphasor" rates b1
  where rates = [(Ar,[Xr,Ir,Ir,Ir,Ir,Ir])]

-- |
-- Function-table-based crossfading looper.
--
-- This opcode reads audio from a function table and plays it back in a loop with user-defined
--    start time, duration and crossfade time. It also allows the pitch of the loop to be controlled,
--    including reversed playback. It accepts non-power-of-two tables, such as deferred-allocation
--    GEN01 tables, with one or two channels.
--
-- > asig1[, asig2]  flooper  kamp, kpitch, istart, idur, ifad, ifn
--
-- csound doc: <http://csound.com/docs/manual/flooper.html>
flooper :: Tuple a => Sig -> Sig -> D -> D -> D -> Tab -> a
flooper b1 b2 b3 b4 b5 b6 = liftMulti "flooper" rates (b1, b2, b3, b4, b5, b6)
    where rates = ([Ar,Ar],[Kr,Kr,Ir,Ir,Ir,Ir])

-- |
-- Function-table-based crossfading looper.
--
-- This opcode implements a crossfading looper with variable loop parameters and three
--   looping modes, optionally using a table for its crossfade shape. It accepts
--   non-power-of-two tables for its source sounds, such as deferred-allocation
--    GEN01 tables, with one or two channels.
--
-- > asig1[,asig2]  flooper2  kamp, kpitch, kloopstart, kloopend, kcrossfade, ifn \
-- >           [, istart, imode, ifenv, iskip]
--
-- csound doc: <http://csound.com/docs/manual/flooper2.html>
flooper2 :: Tuple a => Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> a
flooper2 b1 b2 b3 b4 b5 b6 = liftMulti "flooper2" rates (b1, b2, b3, b4, b5, b6)
  where rates = ([Ar,Ar],[Kr,Kr,Kr,Kr,Kr,Ir,Ir,Ir,Ir,Ir])

-- |
-- Returns the length of a sound file.
--
-- > ir  filelen  ifilcod, [iallowraw]
--
-- csound doc: <http://csound.com/docs/manual/filelen.html>
filelen ::  Str -> D
filelen b1 = liftOpc "filelen" rates b1
  where rates = [(Ir,[Sr,Ir])]

-- |
-- Phase-locked vocoder processing with onset detection/processing, 'tempo-scaling'.
--
-- filescal implements phase-locked vocoder
--       processing from disk files, resampling if necessary.
--
-- > asig[,asig2]  filescal  ktimescal, kamp, kpitch, Sfile, klock [,ifftsize, idecim, ithresh]
-- >
--
-- csound doc: <http://csound.com/docs/manual/filescal.html>
filescal :: Tuple a => Sig -> Sig -> Sig -> Str -> Sig -> a
filescal b1 b2 b3 b4 b5 = liftMulti "filescal" rates (b1, b2, b3, b4, b5)
  where rates = ([Ar,Ar],[Kr,Kr,Kr,Sr,Kr,Ir,Ir,Ir])

-- |
-- Phase-locked vocoder processing.
--
-- mincer implements phase-locked vocoder processing using function tables
-- containing sampled-sound sources, with GEN01, and
-- mincer will accept deferred allocation tables.
--
-- > asig  mincer  atimpt, kamp, kpitch, ktab, klock[,ifftsize,idecim]
-- >
--
-- csound doc: <http://csound.com/docs/manual/mincer.html>
mincer ::  Sig -> Sig -> Sig -> Tab -> Sig -> Sig
mincer b1 b2 b3 b4 b5 = liftOpc "mincer" rates (b1, b2, b3, b4, b5)
  where rates = [(Ar,[Ar,Kr,Kr,Kr,Kr,Ir,Ir])]

-- |
-- Returns the audio spout frame.
--
-- Returns the audio spout frame (if active), otherwise it returns zero.
--
-- > aout1 [,aout2 ... aoutX]  monitor
-- > aarra  monitor
--
-- csound doc: <http://csound.com/docs/manual/monitor.html>
monitor :: forall a. Sigs a => SE a
monitor = liftMultiDep "monitor" ((repeat Ar),[]) ()

-- |
-- Reads from numbered channels in an external audio signal or stream.
--
-- > ain1[, ...]  inch  kchan1[,...]
--
-- csound doc: <http://csound.com/docs/manual/inch.html>
inch :: Tuple a => Sig -> SE a
inch b1 = liftMultiDep "inch" ((repeat Ar),(repeat Kr)) b1

-- | Writes multi-channel audio data, with user-controllable channels, to an external device or stream.
--
-- > outch kchan1, asig1 [, kchan2] [, asig2] [...]
--
-- User several invokations of outch to write to multiple channels
outch :: Sig -> Sig -> SE ()
outch index aout = liftOpcDep_ "outch" [(Xr, [Kr,Ar])] (index, aout)

-------------------------------------------------------------------------------------
-- Tables (Buffers)

-- |
-- Accesses table values by direct indexing.
--
-- > ares  table  andx, ifn [, ixmode] [, ixoff] [, iwrap]
-- > ires  table  indx, ifn [, ixmode] [, ixoff] [, iwrap]
-- > kres  table  kndx, ifn [, ixmode] [, ixoff] [, iwrap]
--
-- csound doc: <http://csound.com/docs/manual/table.html>
table :: SigOrD a => a -> Tab -> SE a
table b1 b2 = liftOpcDep "table" rates (b1, b2)
  where rates = [(Ar,[Ar,Ir,Ir,Ir,Ir])
                ,(Ir,[Ir,Ir,Ir,Ir,Ir])
                ,(Kr,[Kr,Ir,Ir,Ir,Ir])]

-- | The same as table but pure version for read-only tables
getTable :: SigOrD a => a -> Tab -> a
getTable b1 b2 = liftOpc "table" rates (b1, b2)
  where rates = [(Ar,[Ar,Ir,Ir,Ir,Ir])
                ,(Ir,[Ir,Ir,Ir,Ir,Ir])
                ,(Kr,[Kr,Ir,Ir,Ir,Ir])]

-- |
-- Accesses table values by direct indexing with cubic interpolation.
--
-- > ares  table3  andx, ifn [, ixmode] [, ixoff] [, iwrap]
-- > ires  table3  indx, ifn [, ixmode] [, ixoff] [, iwrap]
-- > kres  table3  kndx, ifn [, ixmode] [, ixoff] [, iwrap]
--
-- csound doc: <http://csound.com/docs/manual/table3.html>
table3 :: SigOrD a => a -> Tab -> SE a
table3 b1 b2 = liftOpcDep "table3" rates (b1, b2)
  where rates = [(Ar,[Ar,Ir,Ir,Ir,Ir])
                ,(Ir,[Ir,Ir,Ir,Ir,Ir])
                ,(Kr,[Kr,Ir,Ir,Ir,Ir])]

-- | The same as table3 but pure version for read-only tables
getTable3 :: SigOrD a => a -> Tab -> a
getTable3 b1 b2 = liftOpc "table3" rates (b1, b2)
  where rates = [(Ar,[Ar,Ir,Ir,Ir,Ir])
                ,(Ir,[Ir,Ir,Ir,Ir,Ir])
                ,(Kr,[Kr,Ir,Ir,Ir,Ir])]

-- |
-- Accesses table values by direct indexing with linear interpolation.
--
-- > ares  tablei  andx, ifn [, ixmode] [, ixoff] [, iwrap]
-- > ires  tablei  indx, ifn [, ixmode] [, ixoff] [, iwrap]
-- > kres  tablei  kndx, ifn [, ixmode] [, ixoff] [, iwrap]
--
-- csound doc: <http://csound.com/docs/manual/tablei.html>
tablei :: SigOrD a => a -> Tab -> SE a
tablei b1 b2 = liftOpcDep "tablei" rates (b1, b2)
  where rates = [(Ar,[Ar,Ir,Ir,Ir,Ir])
                ,(Ir,[Ir,Ir,Ir,Ir,Ir])
                ,(Kr,[Kr,Ir,Ir,Ir,Ir])]

-- | The same as tablei but pure version for read-only tables
getTablei :: SigOrD a => a -> Tab -> a
getTablei b1 b2 = liftOpc "tablei" rates (b1, b2)
  where rates = [(Ar,[Ar,Ir,Ir,Ir,Ir])
                ,(Ir,[Ir,Ir,Ir,Ir,Ir])
                ,(Kr,[Kr,Ir,Ir,Ir,Ir])]

-- | tablew — Change the contents of existing function tables.
--
-- > tablew asig, andx, ifn [, ixmode] [, ixoff] [, iwgmode]
-- > tablew isig, indx, ifn [, ixmode] [, ixoff] [, iwgmode]
-- > tablew ksig, kndx, ifn [, ixmode] [, ixoff] [, iwgmode]
tablew :: SigOrD a => a -> a -> Tab -> SE ()
tablew b1 b2 b3 = liftOpcDep_ "tablew" rates (b1,b2,b3)
  where rates = [(Ar,[Ar,Ar,Ir,Ir,Ir,Ir])
                ,(Ir,[Ir,Ir,Ir,Ir,Ir,Ir])
                ,(Kr,[Kr,Kr,Ir,Ir,Ir,Ir])]

-------------------------------------------------------------------------------------
-- * Program Control

-- | changed — k-rate signal change detector
--
-- > ktrig changed kvar1 [, kvar2,..., kvarN]
changed :: [Sig] -> Sig
changed as = Sig $ f <$> mapM toE as
  where f a1 = Dynamic.opcs "changed" [(Kr, repeat Kr), (Ar, repeat Ir)] a1

-- | Like @changed@ but it does not trigger the first cycle if any of the input signals is non-zero.
changed2 :: [Sig] -> Sig
changed2 as = Sig $ f <$> mapM toE as
  where f a1 = Dynamic.opcs "changed" [(Kr, repeat Kr), (Ar, repeat Ir)] a1

-- |
-- Informs when a krate signal crosses a threshold.
--
-- > kout  trigger  ksig, kthreshold, kmode
--
-- csound doc: <http://csound.com/docs/manual/trigger.html>
trigger ::  Sig -> Sig -> Sig -> Sig
trigger b1 b2 b3 = liftOpc "trigger" rates (b1, b2, b3)
    where rates = [(Kr,[Kr,Kr,Kr])]

-------------------------------------------------------------------------------------
-- Time

-- | metro — Trigger Metronome.
--
-- Generate a metronomic signal to be used in any circumstance an isochronous trigger is needed.
--
-- > ktrig  metro  kfreq [, initphase]
metro :: Sig -> Sig
metro = liftOpc "metro" [(Kr, [Kr,Ir])]

-- | metro2 — Trigger Metronome with Swing and Accents.
--
-- > ktrig  metro2  kfreq, kswing [, iamp, initphase]
metro2 :: Sig -> Sig -> Sig
metro2 a b = liftOpc "metro2" [(Kr, [Kr,Kr,Ir,Ir])] (a, b)

-- | timeinsts — Read absolute time in seconds.
--
-- > kres timeinsts
timeinsts :: SE Sig
timeinsts = liftOpcDep "timeinsts" [(Kr,[])] ()

-------------------------------------------------------------------------------------
-- Channels

newtype Chan a = Chan { unChan :: Str }

newChan :: Val a => Str -> Chan a
newChan = Chan

getChanName :: Chan a -> Str
getChanName = unChan

instance IsRef Chan where
  readRef :: forall a . Tuple a => Chan a -> SE a
  readRef (Chan str) = SE $ fmap (toTuple . pure . pure) $ Dynamic.opcsDep "chnget" rates =<< lift (fromTuple str)
    where rates = [(head (tupleRates @a), [Sr])]

  writeRef :: forall a . Tuple a => Chan a -> a -> SE ()
  writeRef (Chan str) val = liftOpcDep_ "chnset" rates (val, str)
    where rates = [(Xr, [head (tupleRates @a), Sr])]

chnclear :: Chan Sig -> SE ()
chnclear (Chan str) = liftOpcDep_ "chnclear" [(Xr, [Sr])] str

chnmix :: Chan Sig -> Sig -> SE ()
chnmix (Chan str) val = liftOpcDep_ "chnset" rates (val, str)
  where rates = [(Xr, [Ar, Sr])]

-------------------------------------------------------------------------------------
-- Space

-- | pan2 — Distribute an audio signal across two channels.
-- For Csound users: order of arguments is reversed for better Haskell experience
--
-- > a1, a2 pan2 xp, asig [, imode]
pan2 :: Sig -> Sig -> Sig2
pan2 xp asig = liftMulti "pan2" ([Ar, Ar], [Ar, Xr, Ir]) (asig, xp)

-- | vbap — Distributes an audio signal among many channels.
--
-- > ar1[, ar2...] vbap asig, kazim [,
--    kelev] [, kspread] [, ilayout]
--
-- Csound docs: <https://csound.com/docs/manual/vbap.html>
vbap :: Sigs a => Sig -> Sig -> a
vbap asig kazim = liftMulti "vbap" (repeat Ar, [Ar, Kr, Kr, Kr, Ir]) (asig, kazim)

-- | vbaplsinit — Configures VBAP output according to loudspeaker parameters.
--
-- > vbaplsinit idim, ilsnum [, idir1] [, idir2] [...] [, idir32]
vbaplsinit :: D -> D -> [D] -> SE ()
vbaplsinit b1 b2 bs = SE $ Dynamic.opcsDep_ "vbaplsinit" rates =<< lift ((\a1 a2 as -> a1 : a2 : as) <$> toE b1 <*> toE b2 <*> mapM toE bs)
  where rates = [(Xr, repeat Ir)]

-------------------------------------------------------------------------------------
-- Reverbs

-- |
-- Opcode version of Jezar's Freeverb
--
-- freeverb is a stereo reverb unit based on Jezar's public domain
-- 		C++ sources, composed of eight parallel comb filters on both
-- 		channels, followed by four allpass units in series. The filters
-- 		on the right channel are slightly detuned compared to the left
-- 		channel in order to create a stereo effect.
--
-- > aoutL, aoutR  freeverb  ainL, ainR, kRoomSize, kHFDamp[, iSRate[, iSkip]]
--
-- csound doc: <http://csound.com/docs/manual/freeverb.html>
-- order of arguments is reversed comared to the Csound version it's:
--
-- > freeverb kRoomSize kHFDamp (ainL, ainR) = (aoutL, aoutR)
freeverb ::  Sig -> Sig -> (Sig, Sig) -> (Sig, Sig)
freeverb roomSize damp (ainL, ainR) = liftMulti "freeverb" rates (ainL, ainR, roomSize, damp)
  where rates = ([Ar,Ar],[Ar,Ar,Kr,Kr,Ir,Ir])

-- |
-- 8 delay line stereo FDN reverb, based on work by Sean Costello
--
-- 8 delay line stereo FDN reverb, with feedback matrix based upon physical
-- 		modeling scattering junction of 8 lossless waveguides of equal characteristic
-- 		impedance. Based on Csound orchestra version by Sean Costello.
--
-- > aoutL, aoutR  reverbsc  ainL, ainR, kfblvl, kfco[, israte[, ipitchm[, iskip]]]
--
-- csound doc: <http://csound.com/docs/manual/reverbsc.html>
-- order of arguments is reversed comared to the Csound version it's:
--
-- > reverbsc kFeedbackLevel kfco (ainL, ainR) = (aoutL, aoutR)
reverbsc ::  Sig -> Sig -> (Sig, Sig) -> (Sig,Sig)
reverbsc kFeedbackLevel kfco (ainL, ainR) = liftMulti "reverbsc" rates (ainL, ainR, kFeedbackLevel, kfco)
    where rates = ([Ar,Ar],[Ar,Ar,Kr,Kr,Ir,Ir,Ir])

-------------------------------------------------------------------------------------
-- Convolution

-- |
-- Convolution based on a uniformly partitioned overlap-save algorithm
--
-- Convolution based on a uniformly partitioned overlap-save algorithm. Compared to the convolve opcode, pconvolve has these benefits:
--
-- > ar1 [, ar2] [, ar3] [, ar4]  pconvolve  ain, ifilcod [, ipartitionsize, ichannel]
--
-- csound doc: <http://csound.com/docs/manual/pconvolve.html>
pconvolve :: Tuple a => Sig -> Str -> a
pconvolve b1 b2 = liftMulti "pconvolve" rates (b1 , b2)
  where rates = ([Ar,Ar,Ar,Ar],[Ar,Sr,Ir,Ir])

-------------------------------------------------------------------------------------
-- Physical modeling

-- |
-- Produces a naturally decaying plucked string or drum sound.
--
-- Audio output is a naturally decaying plucked string or drum sound based on the Karplus-Strong algorithms.
--
-- > ares  pluck  kamp, kcps, icps, ifn, imeth [, iparm1] [, iparm2]
--
-- csound doc: <http://csound.com/docs/manual/pluck.html>
pluck ::  Sig -> Sig -> D -> Tab -> D -> Sig
pluck b1 b2 b3 b4 b5 = liftOpc "pluck" rates (b1, b2, b3, b4, b5)
  where rates = [(Ar,[Kr,Kr,Ir,Ir,Ir,Ir,Ir])]

-------------------------------------------------------------------------------------
-- Delay / Comb

-- |
-- A variable delay opcode with high quality interpolation.
--
-- > aout  vdelayx  ain, adl, imd, iws [, ist]
--
-- csound doc: <http://csound.com/docs/manual/vdelayx.html>
vdelayx ::  Sig -> Sig -> D -> D -> Sig
vdelayx b1 b2 b3 b4 = liftOpc "vdelayx" rates (b1,b2,b3,b4)
    where rates = [(Ar,[Ar,Ar,Ir,Ir,Ir])]

-- |
-- Reverberates an input signal with a âcoloredâ frequency response.
--
-- > ares  comb  asig, krvt, ilpt [, iskip] [, insmps]
--
-- csound doc: <http://csound.com/docs/manual/comb.html>
comb ::  Sig -> Sig -> D -> Sig
comb b1 b2 b3 = liftOpc "comb" rates (b1,b2,b3)
  where rates = [(Ar,[Ar,Kr,Ir,Ir,Ir])]

-- |
-- Variably reverberates an input signal with a âcoloredâ frequency response.
--
-- > ares  vcomb  asig, krvt, xlpt, imaxlpt [, iskip] [, insmps]
--
-- csound doc: <http://csound.com/docs/manual/vcomb.html>
vcomb ::  Sig -> Sig -> Sig -> D -> Sig
vcomb b1 b2 b3 b4 = liftOpc "vcomb" rates (b1,b2,b3,b4)
  where rates = [(Ar,[Ar,Kr,Xr,Ir,Ir,Ir])]

-------------------------------------------------------------------------------------
-- Filters

-- |
-- A first-order recursive low-pass filter with variable frequency response.
--
-- > ares  tone  asig, khp [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/tone.html>
--
-- Note that first arguments goes last (as it convenient for Haskell)
tone ::  Sig -> Sig -> Sig
tone khp asig = liftOpc "tone" rates (asig, khp)
  where rates = [(Ar,[Ar,Kr,Ir])]

-- |
-- A first-order recursive high-pass filter with variable frequency response.
--
-- > ares  atone  asig, khp [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/tone.html>
--
-- Note that first arguments goes last (as it convenient for Haskell)
atone ::  Sig -> Sig -> Sig
atone khp asig = liftOpc "atone" rates (asig, khp)
  where rates = [(Ar,[Ar,Kr,Ir])]

-- |
-- A second-order resonant filter.
--
-- > ares  reson  asig, xcf, xbw [, iscl] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/reson.html>
--
-- Note that first arguments goes last (as it convenient for Haskell)
reson ::  Sig -> Sig -> Sig -> Sig
reson xcf xbw asig = liftOpc "reson" rates (asig, xcf, xbw)
  where rates = [(Ar,[Ar,Xr,Xr,Ir,Ir])]

-- |
-- Same as the butterbp opcode.
--
-- > ares  butbp  asig, kfreq, kband [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/butbp.html>
--
-- Note that first arguments goes last (as it convenient for Haskell)
butbp ::  Sig -> Sig -> Sig -> Sig
butbp kfreq kband asig = liftOpc "butbp" rates (asig, kfreq, kband)
  where rates = [(Ar,[Ar,Kr,Kr,Ir])]

-- |
-- Same as the butterbr opcode.
--
-- > ares  butbr  asig, kfreq, kband [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/butbr.html>
--
-- Note that first arguments goes last (as it convenient for Haskell)
butbr ::  Sig -> Sig -> Sig -> Sig
butbr kfreq kband asig = liftOpc "butbr" rates (asig, kfreq, kband)
  where rates = [(Ar,[Ar,Kr,Kr,Ir])]

-- |
-- Same as the butterhp opcode.
--
-- > ares  buthp  asig, kfreq [, iskip]
-- > ares  buthp  asig, afreq [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/buthp.html>
--
-- Note that first arguments goes last (as it convenient for Haskell)
buthp ::  Sig -> Sig -> Sig
buthp kfreq asig = liftOpc "buthp" rates (asig, kfreq)
  where rates = [(Ar,[Ar,Kr,Ir]),(Ar,[Ar,Ar,Ir])]

-- |
-- Same as the butterlp opcode.
--
-- > ares  butlp  asig, kfreq [, iskip]
-- > ares  butlp  asig, afreq [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/butlp.html>
--
-- Note that first arguments goes last (as it convenient for Haskell)
butlp ::  Sig -> Sig -> Sig
butlp kfreq asig = liftOpc "butlp" rates (asig, kfreq)
  where rates = [(Ar,[Ar,Kr,Ir]),(Ar,[Ar,Ar,Ir])]

-- |
-- A filter that simulates a mass-spring-damper system
--
-- Filters the incoming signal with the specified resonance frequency and
--       quality factor. It can also be seen as a signal generator for high quality
--       factor, with an impulse for the excitation. You can combine several modes
--       to built complex instruments such as bells or guitar tables.
--
-- > aout  mode  ain, xfreq, xQ [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/mode.html>
--
-- Note that first arguments goes last (as it convenient for Haskell)
mode ::  Sig -> Sig -> Sig -> Sig
mode xfreq xQ ain = liftOpc "mode" rates (ain, xfreq, xQ)
  where rates = [(Ar,[Ar,Xr,Xr,Ir])]

-- |
-- Zero-delay feedback implementation of 4 pole ladder filter.
--
-- Zero-delay feedback implementation of a 4 pole (24 dB/oct) low-pass filter based on the Moog ladder filter.
--
-- > asig  zdf_ladder  ain, xcf, xQ [, istor]
--
-- csound doc: <http://csound.com/docs/manual/zdf_ladder.html>
--
-- Note that first arguments goes last (as it convenient for Haskell)
zdfLadder ::  Sig -> Sig -> Sig -> Sig
zdfLadder xcf xQ ain = liftOpc "zdf_ladder" rates (ain, xcf, xQ)
  where rates = [(Ar,[Ar,Xr,Xr,Ir])]

-- |
-- A digital emulation of the Moog diode ladder filter configuration.
--
-- > ares  moogvcf2  asig, xfco, xres [,iscale, iskip]
--
-- csound doc: <http://csound.com/docs/manual/moogvcf2.html>
--
-- Note that first arguments goes last (as it convenient for Haskell)
moogvcf2 ::  Sig -> Sig -> Sig -> Sig
moogvcf2 xfco xres asig = liftOpc "moogvcf2" rates (asig, xfco, xres)
  where rates = [(Ar,[Ar,Xr,Xr,Ir,Ir])]

-------------------------------------------------------------------------------------
-- Level

-- |
-- Determines the root-mean-square amplitude of an audio signal.
--
-- Determines the root-mean-square amplitude of an audio signal. It low-pass filters the actual value, to average in the manner of a VU meter.
--
-- > kres  rms  asig [, ihp] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/rms.html>
rms ::  Sig -> Sig
rms = liftOpc "rms" [(Kr,[Ar,Ir,Ir])]

-- |
-- Adjust one audio signal according to the values of another.
--
-- The rms power of asig can be interrogated, set, or adjusted to match that of a comparator signal.
--
-- > ares  balance  asig, acomp [, ihp] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/balance.html>
balance ::  Sig -> Sig -> Sig
balance b1 b2 = liftOpc "balance" rates (b1, b2)
  where rates = [(Ar,[Ar,Ar,Ir,Ir])]

-- |
-- Adjust one audio signal according to the values of another.
--
-- The rms power of asig can be interrogated, set, or adjusted to match that of a comparator signal.
--
-- > ares  balance  asig, acomp [, ihp] [, iskip]
--
-- csound doc: <http://csound.com/docs/manual/balance2.html>
--
-- Note that balance2 is just like balance except the gain is recalculated for every sample rather than interpolating k-rate values.
balance2 ::  Sig -> Sig -> Sig
balance2 b1 b2 = liftOpc "balance2" rates (b1, b2)
  where rates = [(Ar,[Ar,Ar,Ir,Ir])]

-------------------------------------------------------------------------------------
-- Amplitude / Pitch tracking

-- |
-- Envelope follower unit generator.
--
-- > ares  follow  asig, idt
--
-- csound doc: <http://csound.com/docs/manual/follow.html>
follow ::  Sig -> D -> Sig
follow b1 b2 = liftOpc "follow" rates (b1, b2)
  where rates = [(Ar,[Ar,Ir])]

-- |
-- Another controllable envelope extractor.
--
-- A controllable envelope extractor using the algorithm attributed to Jean-Marc Jot.
--
-- > ares  follow2  asig, katt, krel
--
-- csound doc: <http://csound.com/docs/manual/follow2.html>
follow2 ::  Sig -> Sig -> Sig -> Sig
follow2 b1 b2 b3 = liftOpc "follow2" rates (b1, b2, b3)
  where rates = [(Ar,[Ar,Kr,Kr])]

-- |
-- Tracks the pitch of a signal.
--
-- ptrack takes an input signal, splits it into ihopsize blocks and using a STFT method, extracts an estimated pitch for its fundamental frequency as well as estimating the total amplitude of the signal in dB, relative to full-scale (0dB). The method implies an analysis window size of 2*ihopsize samples (overlaping by 1/2 window), which has to be a power-of-two, between 128 and 8192 (hopsizes between 64 and 4096). Smaller windows will give better time precision, but worse frequency accuracy (esp. in low fundamentals).This opcode is based on an original algorithm by M. Puckette.
--
-- > kcps, kamp  ptrack  asig, ihopsize[,ipeaks]
--
-- csound doc: <http://csound.com/docs/manual/ptrack.html>
ptrack ::  Sig -> D -> (Sig,Sig)
ptrack b1 b2 = liftMulti "ptrack" rates (b1, b2)
  where rates = ([Kr,Kr],[Ar,Ir,Ir])

-------------------------------------------------------------------------------------
-- Distortion

-- |
-- Waveshapes a signal by raising it to a variable exponent.
--
-- The powershape opcode raises an input signal to a power with pre- and post-scaling of the signal so that the output will be in a predictable range.  It also processes negative inputs in a symmetrical way to positive inputs, calculating a dynamic transfer function that is useful for waveshaping.
--
-- > aout  powershape  ain, kShapeAmount [, ifullscale]
--
-- csound doc: <http://csound.com/docs/manual/powershape.html>
--
-- Arguments are reversed:
--
-- > powershape kShape ain
powershape ::  Sig -> Sig -> Sig
powershape kShape ain = liftOpc "powershape" rates (ain, kShape)
    where rates = [(Ar,[Ar,Kr,Ir])]

-- |
-- Distort an audio signal via waveshaping and optional clipping.
--
-- > ar  distort  asig, kdist, ifn[, ihp, istor]
--
-- csound doc: <http://csound.com/docs/manual/distort.html>
--
-- Arguments are reversed, the audio input goes last
distort ::  Sig -> Tab -> Sig -> Sig
distort b1 b2 ain = liftOpc "distort" rates (ain, b1, b2)
  where rates = [(Ar,[Ar,Kr,Ir,Ir,Ir])]

-- |
-- Modified hyperbolic tangent distortion.
--
-- Implementation of modified hyperbolic tangent distortion. distort1 can be used to generate wave shaping distortion based on a modification of the tanh function.
--
-- > ares  distort1  asig, kpregain, kpostgain, kshape1, kshape2[, imode]
--
-- csound doc: <http://csound.com/docs/manual/distort1.html>
--
-- Arguments are reversed, the audio input goes last
distort1 ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig
distort1 b1 b2 b3 b4 ain = liftOpc "distort1" rates (ain, b1,b2,b3,b4)
  where rates = [(Ar,[Ar,Kr,Kr,Kr,Kr,Ir])]




-------------------------------------------------------------------------------------
-- print and display

-- |
-- Displays the values init (i-rate) variables.
--
-- These units will print orchestra init-values.
--
-- >  print  iarg [, iarg1] [, iarg2] [...]
--
-- csound doc: <http://csound.com/docs/manual/print.html>
printi ::  [D] -> SE ()
printi ds = SE $ Dynamic.opcsDep_ "print" rates =<< lift (mapM toE ds)
    where rates = [(Xr,(repeat Ir))]


-- |
-- Prints one k-rate value at specified intervals.
--
-- >  printk  itime, kval [, ispace]
--
-- csound doc: <http://csound.com/docs/manual/printk.html>
printk ::  D -> Sig -> SE ()
printk b1 b2 = liftOpcDep_ "printk" rates (b1, b2)
    where rates = [(Xr,[Ir,Kr,Ir])]


-- |
-- Prints at init-time using a printf() style syntax.
--
-- >  prints  "string" [, kval1] [, kval2] [...]
--
-- csound doc: <http://csound.com/docs/manual/prints.html>
prints :: forall a . Tuple a => Str -> a -> SE ()
prints b1 b2 = liftOpcDep_ "prints" rates (b1, b2)
  where rates = [(Xr, Sr : tupleRates @a)]

-- |
-- Prints at init-time using a printf() style syntax.
--
-- >  prints  "string" [, kval1] [, kval2] [...]
--
-- csound doc: <http://csound.com/docs/manual/prints.html>
printks :: Sigs a => Str -> D -> a -> SE ()
printks b1 b2 b3 = liftOpcDep_ "printks" rates (b1, b2, b3)
  where rates = [(Xr, Sr : Ir : repeat Kr)]

-- |
-- Prints a new value every time a control variable changes.
--
-- >  printk2  kvar [, inumspaces]
--
-- csound doc: <http://csound.com/docs/manual/printk2.html>
printk2 ::  Sig -> SE ()
printk2 = liftOpcDep_ "printk2" [(Xr,[Kr,Ir])]

-- TODO
-- printarray

fprint :: Tab -> SE ()
fprint = liftOpcDep_ "fprint" [(Xr, [Ir,Kr,Kr,Kr,Kr,Ir])]

-------------------------------------------------------------------------------------
-- File ouptut

-- |
-- Outputs a-rate signals to an arbitrary number of channels.
--
-- fout outputs N a-rate signals to a specified file of N channels.
--
-- >  fout  ifilename, iformat, aout1 [, aout2, aout3,...,aoutN]
-- >  fout  ifilename, iformat, array[]
--
-- csound doc: <http://csound.com/docs/manual/fout.html>
fout ::  Sigs a => Str -> D -> a -> SE ()
fout b1 b2 b3 = liftOpcDep_ "fout" rates (b1, b2, b3)
  where rates = [(Xr,[Sr,Ir] ++ (repeat Ar))]

ftsave :: Str -> D -> [Tab] -> SE ()
ftsave file imode tabs = SE $ Dynamic.opcsDep_ "ftsave" rates =<< lift ((\a1 a2 as -> a1 : a2 : as) <$> toE file <*> toE imode <*> mapM toE tabs)
  where rates = [(Xr,Sr : Ir : (repeat Ir))]

fprints :: Str -> Str -> [D] -> SE ()
fprints file format vals = SE $ Dynamic.opcsDep_ "fprints" rates =<< lift ((\a1 a2 as -> a1 : a2 : as) <$> toE file <*> toE format <*> mapM toE vals)
  where rates = [(Xr, Sr : Sr : repeat Ir)]

readf :: Str -> SE (Str, Sig)
readf = liftMultiDep "readf" ([Sr, Kr], [Sr])

readfi :: Str -> SE (Str, Sig)
readfi = liftMultiDep "readfi" ([Sr, Ir], [Sr])

-- TODO
-- directory: <https://csound.com/docs/manual/directory.html>

-------------------------------------------------------------------------------------
-- Math

-- |
-- Returns the amplitude equivalent of the decibel value x.
--
-- Returns the amplitude equivalent of the decibel value x. Thus:
--
-- >  ampdb (x)  (no rate restriction)
--
-- csound doc: <http://csound.com/docs/manual/ampdb.html>
ampdb :: SigOrD a => a -> a
ampdb b1 = fromE $ f <$> toE b1
    where f a1 = Dynamic.opr1 "ampdb" a1

-- |
-- Returns the decibel equivalent of the raw amplitude x.
--
-- >  dbamp (x)  (init-rate or control-rate args only)
--
-- csound doc: <http://csound.com/docs/manual/dbamp.html>
dbamp :: SigOrD a => a -> a
dbamp b1 = fromE $ f <$> toE b1
    where f a1 = Dynamic.opr1k "dbamp" a1

-------------------------------------------------------------------------------------
-- MIDI

-- | massign — Assigns a MIDI channel number to a Csound instrument.
--
-- > massign ichnl, insnum[, ireset]
--
-- csound doc <https://csound.com/docs/manual/massign.html>
massign :: Arg a => D -> InstrRef a -> SE ()
massign ichnl instrRef = case getInstrRefId instrRef of
  Left strId  -> liftOpcDep_ "massign" strRates (ichnl, strId)
  Right intId -> liftOpcDep_ "massign" intRates (ichnl, intId)
  where
    strRates = [(Xr, [Ir,Sr,Ir])]
    intRates = [(Xr, [Ir,Ir,Ir])]

-- |
-- Get a note number from a MIDI event.
--
-- > ival  notnum
--
-- csound doc: <http://csound.com/docs/manual/notnum.html>
notnum ::  SE D
notnum = liftOpcDep "notnum" [(Ir,[])] ()

-- |
-- Get the velocity from a MIDI event.
--
-- > ival  veloc  [ilow] [, ihigh]
--
-- csound doc: <http://csound.com/docs/manual/veloc.html>
veloc :: SE D
veloc = liftOpcDep "veloc" [(Ir,[Ir,Ir])] ()

-- | midictrl — Get the current value (0-127) of a specified MIDI controller.
--
-- > ival midictrl inum [, imin] [, imax]
midictrl :: D -> SE D
midictrl inum = liftOpcDep "midictrl" [(Ir, [Ir,Ir,Ir])] inum

data CtrlInit = CtrlInit
  { ctrlInitChan  :: D -- midi channel
  , ctrlInitId    :: D -- control number
  , ctrlInitValue :: D -- init value
  }

instance Tuple CtrlInit where
  tupleMethods = makeTupleMethods  (\(a,b,c) -> CtrlInit a b c) (\(CtrlInit a b c) -> (a, b, c))

-- | ctrlinit — Sets the initial values for a set of MIDI controllers.
ctrlinit :: [CtrlInit] -> SE ()
ctrlinit inits = SE $ Dynamic.opcsDep_ "ctrlinit" rates =<< lift (concat <$> mapM fromTuple inits)
  where rates = [(Xr, repeat Ir)]

-- |
-- Allows a floating-point 7-bit MIDI signal scaled with a minimum and a maximum range.
--
-- > idest  ctrl7  ichan, ictlno, imin, imax [, ifn]
-- > kdest  ctrl7  ichan, ictlno, kmin, kmax [, ifn]
-- > adest  ctrl7  ichan, ictlno, kmin, kmax [, ifn] [, icutoff]
--
-- csound doc: <http://csound.com/docs/manual/ctrl7.html>
ctrl7 ::  D -> D -> D -> D -> SE Sig
ctrl7 b1 b2 b3 b4 = liftOpcDep "ctrl7" rates (b1,b2,b3,b4)
  where rates = [(Ir,[Ir,Ir,Ir,Ir,Ir])
                ,(Kr,[Ir,Ir,Kr,Kr,Ir])
                ,(Ar,[Ir,Ir,Kr,Kr,Ir,Ir])]

-- |
-- Allows a floating-point 7-bit MIDI signal scaled with a minimum and a maximum range.
--
-- > idest  ctrl7  ichan, ictlno, imin, imax [, ifn]
-- > kdest  ctrl7  ichan, ictlno, kmin, kmax [, ifn]
-- > adest  ctrl7  ichan, ictlno, kmin, kmax [, ifn] [, icutoff]
--
-- csound doc: <http://csound.com/docs/manual/ctrl7.html>
ctrl14 ::  D -> D -> D -> D -> SE Sig
ctrl14 b1 b2 b3 b4 = liftOpcDep "ctrl14" rates (b1,b2,b3,b4)
  where rates = [(Ir,[Ir,Ir,Ir,Ir,Ir])
                ,(Kr,[Ir,Ir,Kr,Kr,Ir])
                ,(Ar,[Ir,Ir,Kr,Kr,Ir,Ir])]

-- |
-- Initializes the controller used to create a 7-bit MIDI value.
--
-- Initializes MIDI controller ictlno with ivalue
--
-- >  initc7  ichan, ictlno, ivalue
--
-- csound doc: <http://csound.com/docs/manual/initc7.html>
initc7 ::  CtrlInit -> SE ()
initc7 b1  = liftOpcDep_ "initc7" rates b1
    where rates = [(Xr,[Ir,Ir,Ir])]

-- |
-- Initializes the controller used to create a 7-bit MIDI value.
--
-- Initializes MIDI controller ictlno with ivalue
--
-- >  initc7  ichan, ictlno, ivalue
--
-- csound doc: <http://csound.com/docs/manual/initc7.html>
initc14 ::  CtrlInit -> SE ()
initc14 b1  = liftOpcDep_ "initc14" rates b1
    where rates = [(Xr,[Ir,Ir,Ir])]


