-- | Basic waveforms that are used most often. 
-- A waveform function takes in a time varied frequency (in Hz).
module Csound.Air.Wave (
	 -- * Bipolar
    osc, oscBy, saw, isaw, pulse, sqr, pw, tri, ramp, blosc,

    -- ** With phase control
    osc', oscBy', saw', isaw', pulse', sqr', pw', tri', ramp', blosc',

    -- ** With random phase
    rndOsc, rndOscBy, rndSaw, rndIsaw, rndPulse, rndSqr, rndPw, rndTri, rndRamp, rndBlosc,    

    -- * Unipolar
    unipolar, bipolar, uosc, uoscBy, usaw, uisaw, upulse, usqr, upw, utri, uramp, ublosc,

    -- ** With phase control
    uosc', uoscBy', usaw', uisaw', upulse', usqr', upw', utri', uramp', ublosc',

    -- ** With random phase
    urndOsc, urndOscBy, urndSaw, urndIsaw, urndPulse, urndSqr, urndPw, urndTri, urndRamp, urndBlosc,        

    -- * Noise
    rndh, urndh, rndi, urndi, white, pink,

    -- * Frequency modulation
    fosc,

    -- * Low frequency oscillators
    Lfo, lfo
) where

import Csound.Typed
import Csound.Typed.Opcode hiding (lfo)
import Csound.Tab(sine, sines4)
import Csound.SigSpace

-- | A pure tone (sine wave).
osc :: Sig -> Sig
osc cps = oscil3 1 cps sine 

-- | A pure tone (sine wave) with initial phase (the first argiment).
osc' :: D -> Sig -> Sig
osc' phase cps = oscil3 1 cps sine `withD` phase

-- | An oscillator with user provided waveform.
oscBy :: Tab -> Sig -> Sig
oscBy tb cps = oscil3 1 cps tb

-- | An oscillator with user provided waveform with initial phase (the second argiment).
oscBy' :: Tab -> D -> Sig -> Sig
oscBy' tb phase cps = oscil3 1 cps tb `withD` phase

-- unipolar waveforms

-- | Turns a bipolar sound (ranges from -1 to 1) to unipolar (ranges from 0 to 1)
unipolar :: Sig -> Sig
unipolar a = 0.5 + 0.5 * a

-- | Turns an unipolar sound (ranges from 0 to 1) to bipolar (ranges from -1 to 1)
bipolar :: Sig -> Sig
bipolar a = 2 * a - 1

-- | Unipolar pure tone.
uosc :: Sig -> Sig
uosc = unipolar . osc

-- | Unipolar 'Csound.Air.oscBy'.
uoscBy :: Tab -> Sig -> Sig
uoscBy tb = unipolar . oscBy tb

-- | Unipolar sawtooth.
usaw :: Sig -> Sig
usaw = unipolar . saw

-- | Unipolar integrated sawtooth.
uisaw :: Sig -> Sig
uisaw = unipolar . isaw

-- | Unipolar square wave.
usqr :: Sig -> Sig
usqr = unipolar . sqr

-- | Unipolar triangle wave.
utri :: Sig -> Sig
utri = unipolar . tri

-- | Unipolar pulse.
upulse :: Sig -> Sig
upulse = unipolar . pulse

-- | Unipolar band-limited oscillator.
ublosc :: Tab -> Sig -> Sig
ublosc tb = unipolar . blosc tb

-----------------------

-- | Frequency modulation
--
-- > fosc carrierFreq modulatorFreq modIndex cps
fosc :: Sig -> Sig -> Sig -> Sig -> Sig
fosc car mod ndx cps = foscili 1 cps car mod ndx sine

-- | Pulse width modulation (width range is 0 to 1)
--
-- > pw dutyCycle cps
pw :: Sig -> Sig -> Sig
pw duty cps = vco2 1 cps `withD` 2 `withSig` duty

-- | Pulse width modulation (width range is 0 to 1)
--
-- > pw' dutyCycle phase cps
pw' :: Sig -> D -> Sig -> Sig
pw' duty phase cps = vco2 1 cps `withD` 2 `withSig` duty `withD` phase

-- | Triangle wave with ramp factor (factor's range is 0 to 1)
--
-- > ramp factor cps
ramp :: Sig -> Sig -> Sig
ramp duty cps = vco2 1 cps `withD` 4 `withSig` (uon 0.01 0.99 $ duty)

-- | Triangle wave with ramp factor (factor's range is 0 to 1)
--
-- > ramp' factor phase cps
ramp' :: Sig -> D -> Sig -> Sig
ramp' duty phase cps = vco2 1 cps `withD` 4 `withSig` (uon 0.01 0.99 $ duty) `withD` phase

-- | Unipolar pulse width modulation wave.
upw :: Sig -> Sig -> Sig
upw duty cps = unipolar $ pw duty cps

-- | Unipolar triangle wave with ram factor.
uramp :: Sig -> Sig -> Sig
uramp duty cps = unipolar $ ramp duty cps

--------------------------------------------------------------------------
-- unipolar oscils with phase control

unipolar' :: (D -> Sig -> Sig) -> (D -> Sig -> Sig)
unipolar' f phs cps = unipolar $ f phs cps 

uosc' = unipolar' osc'
uoscBy' a = unipolar' (oscBy' a) 
usaw' = unipolar' saw'
uisaw' = unipolar' isaw' 
upulse' = unipolar' pulse' 
usqr' = unipolar' sqr'
upw' a = unipolar' (pw' a)
utri' = unipolar' tri'
uramp' a = unipolar' (ramp' a)
ublosc' a = unipolar' (blosc' a)

--------------------------------------------------------------------------
-- random phase

rndPhs :: (D -> Sig -> Sig) -> (Sig -> SE Sig)
rndPhs f cps = fmap (\x -> f x cps) $ rnd 1

rndOsc = rndPhs osc'
rndOscBy a = rndPhs (oscBy' a)
rndSaw = rndPhs saw' 
rndIsaw = rndPhs isaw'
rndPulse = rndPhs pulse'
rndSqr = rndPhs sqr'
rndPw a = rndPhs (pw' a)
rndTri = rndPhs tri'
rndRamp a = rndPhs (ramp' a)
rndBlosc a = rndPhs (blosc' a)

urndOsc = rndPhs uosc'
urndOscBy a = rndPhs (uoscBy' a)
urndSaw = rndPhs usaw' 
urndIsaw = rndPhs uisaw'
urndPulse = rndPhs upulse'
urndSqr = rndPhs usqr'
urndPw a = rndPhs (upw' a)
urndTri = rndPhs utri'
urndRamp a = rndPhs (uramp' a)
urndBlosc a = rndPhs (ublosc' a)

--------------------------------------------------------------------------
-- unipolar random phase

--------------------------------------------------------------------------
-- noise

-- | Constant random signal. It updates random numbers with given frequency.
--
-- > constRnd freq 
rndh :: Sig -> SE Sig
rndh = randh 1

-- | Linear random signal. It updates random numbers with given frequency.
--
-- > rndi freq 
rndi :: Sig -> SE Sig
rndi = randi 1

-- | Unipolar @rndh@
urndh :: Sig -> SE Sig
urndh = fmap unipolar . rndh

-- | Unipolar @rndi@
urndi :: Sig -> SE Sig
urndi = fmap unipolar . rndi

-- | White noise.
white :: SE Sig 
white = noise 1 0

-- | Pink noise.
pink :: SE Sig
pink = pinkish 1

--------------------------------------------------------------------------
-- lfo

-- | Low frequency oscillator
type Lfo = Sig

-- | Low frequency oscillator
--
-- > lfo shape depth rate
lfo :: (Sig -> Sig) -> Sig -> Sig -> Sig
lfo shape depth rate = depth * shape rate


