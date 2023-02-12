-- | Essential opcodes. Top 100 opcodes
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

  , linseg
  , diskin2
  ) where

import Csound.Dynamic (E, Gen)
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

----------------------------------------------------------------------------------
-- envelopes


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

diskin2 :: Tuple a => Str -> a
diskin2 a = liftMulti "diskin2" ((repeat Ar),[Sr,Kr,Ir,Ir,Ir,Ir,Ir,Ir]) a
