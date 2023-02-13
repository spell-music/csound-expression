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
  , active
  , maxalloc
  , nstrnum
  , turnoff2
  ) where

import Data.Maybe
import Csound.Dynamic (E, Gen, IfRate (..))
import Csound.Dynamic qualified as Dynamic
import Csound.Dynamic (Rate (..))
import Csound.Typed.Core.Types hiding (setRate)
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.State qualified as State

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

newtype VcoTab = VcoTab { unVcoTab :: Run E }

instance Val VcoTab where
  fromE = VcoTab
  toE = unVcoTab
  valRate = Ir

instance Tuple VcoTab where
  tupleMethods = primTuple

data VcoShape = Saw | Pulse | Square | Triangle | IntegratedSaw | UserGen Tab

data VcoInit = VcoInit
  { vcoShape   :: VcoShape
  , vcoMul     :: Maybe Double
  , vcoMinSize :: Maybe Int
  , vcoMaxSize :: Maybe Int
  }

toInternalVcoShape :: VcoShape -> Run State.VcoShape
toInternalVcoShape = \case
  Saw   -> pure State.Saw
  Pulse -> pure State.Pulse
  Square -> pure State.Square
  Triangle -> pure State.Triangle
  IntegratedSaw -> pure State.IntegratedSaw
  UserGen t -> State.UserGen <$> tab2gen "VCO tab should be primitive" t

toInternalVcoInit :: VcoInit -> Run State.VcoInit
toInternalVcoInit inits = do
  shape <- toInternalVcoShape (vcoShape inits)
  pure $ State.VcoInit shape (vcoMul inits) (vcoMinSize inits) (vcoMaxSize inits)

vcoInit :: VcoInit -> VcoTab
vcoInit inits = VcoTab $ State.saveVco =<< toInternalVcoInit inits

vco2ft :: Sig -> VcoTab -> Tab
vco2ft kcps t = liftOpc "vco2ft" [(Kr, [Kr, Ir, Ir])] (kcps, t)

-- ares oscilikt xamp, xcps, kfn [, iphs] [, istor]
-- kres oscilikt kamp, kcps, kfn [, iphs] [, istor]
oscilikt :: Sig -> Sig -> Tab -> Sig -> Sig
oscilikt amp cps fn mphase = liftOpc "oscilikt" rates (amp, cps, fn, mphase)
  where
    rates = [ (Ar, [Xr, Xr, Kr, Ir, Ir]), (Kr, [Kr, Kr, Kr, Ir, Ir])]

osc :: Sig -> Sig
osc cps = poscil3 1 cps (sines [1])

saw :: Sig -> Sig
saw cps = oscilikt 1 cps (vco2ft cps $ vcoInit (VcoInit Saw Nothing Nothing Nothing)) 0

tri :: Sig -> Sig
tri cps = oscilikt 1 cps (vco2ft cps $ vcoInit (VcoInit Triangle Nothing Nothing Nothing)) 0

sqr :: Sig -> Sig
sqr cps = oscilikt 1 cps (vco2ft cps $ vcoInit (VcoInit Square Nothing Nothing Nothing)) 0

tab2gen :: String -> Tab -> Run Gen
tab2gen msg t = fromPreTab $ getPreTabUnsafe msg t

vcoTab :: Tab -> Sig -> Sig
vcoTab t cps = oscilikt 1 cps (vco2ft cps $ vcoInit (VcoInit (UserGen t) Nothing Nothing Nothing)) 0

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

-- | active — Returns the number of active instances of an instrument.
active :: (Arg a, SigOrD b) => InstrRef a -> SE b
active instrRef = case getInstrRefId instrRef  of
  Left strId  -> liftOpcDep "active" strRates strId
  Right intId -> liftOpcDep "active" intRates intId
  where
    intRates = [(Ir, [Ir,Ir,Ir]), (Kr, [Kr,Ir,Ir])]
    strRates = [(Ir, [Sr,Ir,Ir])]

-- | maxalloc — Limits the number of allocations of an instrument.
-- It's often used with @global@
maxalloc :: (Arg a) => InstrRef a -> D -> SE ()
maxalloc instrRef val = case getInstrRefId instrRef of
  Left strId -> liftOpcDep_ "maxalloc" strRates (strId, val)
  Right intId -> liftOpcDep_ "maxalloc" intRates (intId, val)
  where
    strRates = [(Xr, [Sr,Ir])]
    intRates = [(Xr, [Ir,Ir])]

-- | nstrnum — Returns the number of a named instrument
nstrnum :: Arg a => InstrRef a -> D
nstrnum instrRef = case getInstrRefId instrRef of
  Left strId -> liftOpc "nstrnum" [(Ir,[Sr])] strId
  Right intId -> intId

-- | turnoff2 — Turn off instance(s) of other instruments at performance time.
turnoff2 :: Arg a => InstrRef a -> Sig -> Sig -> SE ()
turnoff2 instrRef kmode krelease = do
  curRate <- fromMaybe IfIr <$> getCurrentRate
  case curRate of
    IfIr ->
      case getInstrRefId instrRef of
        Left strId  -> liftOpcDep_ "turnoff2" strRates (strId, kmode, krelease)
        Right intId -> liftOpcDep_ "turnoff2" intRates (intId, kmode, krelease)
    IfKr ->
      case getInstrRefId instrRef of
        Left strId  -> liftOpcDep_ "turnoff2_i" strRates_i (strId, kmode, krelease)
        Right intId -> liftOpcDep_ "turnoff2_i" intRates_i (intId, kmode, krelease)
  where
    strRates = [(Xr, [Sr,Kr,Kr])]
    intRates = [(Xr, [Kr,Kr,Kr])]
    strRates_i = [(Xr, [Sr,Ir,Ir])]
    intRates_i = [(Xr, [Ir,Ir,Ir])]
