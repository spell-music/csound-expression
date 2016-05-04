-- | Basic waveforms that are used most often. 
-- A waveform function takes in a time varied frequency (in Hz).
module Csound.Air.Wave (
	 -- * Bipolar
    osc, oscBy, saw, isaw, pulse, sqr, pw, tri, ramp, blosc,

    -- ** With phase control
    osc', oscBy', saw', isaw', pulse', sqr', pw', tri', ramp', blosc',

    -- ** With random phase
    rndOsc, rndOscBy, rndSaw, rndIsaw, rndPulse, rndSqr, rndPw, rndTri, rndRamp, rndBlosc,    
    rndPhs,

    -- ** Raw analog waves (no band limiting)
    -- | Analogue-like waves with no band-limiting. Can be useful for LFOs.
    rawTri, rawSaw, rawSqr, rawPw, rawTri', rawSaw', rawSqr', rawPw', rndRawTri, rndRawSaw, rndRawSqr, rndRawPw,

    -- ** With hard sync (band-limited waves)
    SyncSmooth(..),

    sawSync, isawSync, pulseSync, sqrSync, triSync, bloscSync,
    sawSync', isawSync', pulseSync', sqrSync', triSync', bloscSync',

    -- ** With hard sync (non bandlimited waves)
    rawTriSync, rawSqrSync, rawSawSync, rawPwSync, oscSyncBy,

    -- * Unipolar
    unipolar, bipolar, uosc, uoscBy, usaw, uisaw, upulse, usqr, upw, utri, uramp, ublosc,

    -- ** With phase control
    uosc', uoscBy', usaw', uisaw', upulse', usqr', upw', utri', uramp', ublosc',

    -- ** With random phase
    urndOsc, urndOscBy, urndSaw, urndIsaw, urndPulse, urndSqr, urndPw, urndTri, urndRamp, urndBlosc,        

    -- ** Raw analog waves (no band limiting)
    -- | Analogue-like waves with no band-limiting. Can be useful for LFOs.
    urawTri, urawSaw, urawSqr, urawPw, urawTri', urawSaw', urawSqr', urawPw', urndRawTri, urndRawSaw, urndRawSqr, urndRawPw,

    -- * Noise
    rndh, urndh, rndi, urndi, white, pink,

    -- * Frequency modulation
    fosc,

    -- * Low frequency oscillators
    Lfo, lfo,

    -- * Detune
    detune,

    -- * Unision
    multiHz, multiCent, multiRnd, multiGauss, multiRndSE, multiGaussSE,

    -- * Random splines
    urspline, birspline,

    -- * Buzzes
    buz, gbuz, buz', gbuz'    
) where

import Csound.Typed
import Csound.Typed.Opcode hiding (lfo)
import Csound.Tab(setSize, elins, sine, cosine, sines4, triTab, pwTab, sawTab, sqrTab)
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

-- | Unipolar random splines.
-- It generates the splines with unipolar output (ranges from 0 to 1).
-- Arguments affect the frequency for generation of new values. 
--
-- > urspline cpsMin cpsMax
urspline :: Sig -> Sig -> SE Sig
urspline cpsMin cpsMax = rspline 0 1 cpsMin cpsMax

-- | Bipolar random splines.
-- It generates the splines with bipolar output (ranges from -1 to 1).
-- Arguments affect the frequency for generation of new values. 
--
-- > birspline cpsMin cpsMax
birspline :: Sig -> Sig -> SE Sig
birspline cpsMin cpsMax = rspline (-1) 1 cpsMin cpsMax

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

-- | Generic random smoothTypephase oscil
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

--------------------------------------------------------------------------

-- | Scales the oscillator by frequency.
-- That's how we can rise the pitch by 2 semitones and 15 cents:
--
-- > detune (semitone 2 * cent 15) osc
detune :: Sig -> (Sig -> a) -> (Sig -> a)
detune k f cps = f (k * cps) 

--------------------------------------------------------------------------

linRange n amount = fmap (\x -> amount * sig (2 * double x - 1)) [0, (1 / fromIntegral n) .. 1] 

-- | Unision by Hertz. It creates n oscillators that are playing 
-- the same pitch slightly detuned. The oscillatos's pitch is evenly distributed in Hz.
--
-- > multiHz numberOfUnits amountHz wave
multiHz :: Fractional a => Int -> Sig -> (Sig -> a) -> (Sig -> a) 
multiHz n amount f cps = mean $ fmap (f . (cps + )) $ linRange n amount

-- | Unision by Cents. It creates n oscillators that are playing 
-- the same pitch slightly detuned. The oscillatos's pitch is evenly distributed in cents.
--
-- > multiCent numberOfUnits amountCent wave
multiCent :: Fractional a => Int -> Sig -> (Sig -> a) -> (Sig -> a) 
multiCent n amount f cps = mean $ fmap (f . (cps * ) . cent) $ linRange n amount
    
-- | Oscillators are detuned randomly in the given interval.
--
-- > multiRnd numberOfUnits amountCent wave
multiRnd :: Fractional a => Int -> Sig -> (Sig -> a) -> (Sig -> SE a)
multiRnd = genMultiRnd (rnd 1)

-- | Oscillators are detuned randomly with Gauss distribution in the given interval.
--
-- > multiGauss numberOfUnits amountCent wave
multiGauss :: Fractional a => Int -> Sig -> (Sig -> a) -> (Sig -> SE a)
multiGauss = genMultiRnd (fmap ((+ 0.5) . ir) $ gauss 0.5)

genMultiRnd :: Fractional a => (SE D) -> Int -> Sig -> (Sig -> a) -> (Sig -> SE a)
genMultiRnd gen n amount f cps = fmap mean $ mapM (const go) $ replicate n ()
    where go = fmap (\dx -> f $ cps + amount * (sig $ 2 * dx - 1)) gen

-- | Oscillators are detuned randomly in the given interval.
-- Useful for waves that return a signals with Side Effects.
--
-- > multiRnd numberOfUnits amountCent wave
multiRndSE :: Fractional a => Int -> Sig -> (Sig -> SE a) -> (Sig -> SE a)
multiRndSE = genMultiRndSE (rnd 1)

-- | Oscillators are detuned randomly with Gauss distribution in the given interval.
-- Useful for waves that return a signals with Side Effects.
--
-- > multiGauss numberOfUnits amountCent wave
multiGaussSE :: Fractional a => Int -> Sig -> (Sig -> SE a) -> (Sig -> SE a)
multiGaussSE = genMultiRndSE (fmap ((+ 0.5) . ir) $ gauss 0.5)

genMultiRndSE :: Fractional a => (SE D) -> Int -> Sig -> (Sig -> SE a) -> (Sig -> SE a)
genMultiRndSE gen n amount f cps = fmap mean $ mapM (const go) $ replicate n ()
    where go = (\dx -> f $ cps * cent (amount * (sig $ 2 * dx - 1))) =<< gen

-- | Mean value.
mean :: Fractional a => [a] -> a
mean xs = sum xs / (fromIntegral $ length xs)

---------------------------------------------
-- buzzes

-- |  Output is a set of harmonically related sine partials.
--
-- > buz numOfHarmonics frequency
buz :: Sig -> Sig -> Sig
buz kh x = buzz 1 x kh sine

-- | Buz with phase
buz' :: D -> Sig -> Sig -> Sig
buz' phs kh x = buz kh x `withD` phs

-- |  Output is a set of harmonically related cosine partials.
--
-- > gbuz (minHarm, maxHarm) ratio frequency
gbuz :: (Sig, Sig) -> Sig -> Sig -> Sig
gbuz (hmin, hmax) hratio x = gbuzz 1 x hmax hmin hratio cosine

-- | Gbuz with phase
gbuz' :: D -> (Sig, Sig) -> Sig -> Sig -> Sig
gbuz' phs hs hratio x = gbuz hs hratio x `withD` phs

---------------------------------------------
-- raw waveforms

-- bipolar

rawTri :: Sig -> Sig
rawTri = oscBy triTab

rawSaw :: Sig -> Sig
rawSaw = oscBy sawTab

rawSqr :: Sig -> Sig
rawSqr = oscBy sqrTab

rawPw :: Double -> Sig -> Sig
rawPw duty = oscBy (pwTab duty)

rawTri' :: D -> Sig -> Sig
rawTri' = oscBy' triTab

rawSaw' :: D -> Sig -> Sig
rawSaw' = oscBy' sawTab

rawSqr' :: D -> Sig -> Sig
rawSqr' = oscBy' sqrTab

rawPw' :: Double -> D -> Sig -> Sig
rawPw' duty = oscBy' (pwTab duty)

rndRawTri :: Sig -> SE Sig
rndRawTri = rndOscBy triTab

rndRawSaw :: Sig -> SE Sig
rndRawSaw = rndOscBy sawTab

rndRawSqr :: Sig -> SE Sig
rndRawSqr = rndOscBy sqrTab

rndRawPw :: Double -> Sig -> SE Sig
rndRawPw duty = rndOscBy (pwTab duty)

-- unipolar

urawTri :: Sig -> Sig
urawTri = uoscBy triTab

urawSaw :: Sig -> Sig
urawSaw = uoscBy sawTab

urawSqr :: Sig -> Sig
urawSqr = uoscBy sqrTab

urawPw :: Double -> Sig -> Sig
urawPw duty = uoscBy (pwTab duty)

urawTri' :: D -> Sig -> Sig
urawTri' = uoscBy' triTab

urawSaw' :: D -> Sig -> Sig
urawSaw' = uoscBy' sawTab

urawSqr' :: D -> Sig -> Sig
urawSqr' = uoscBy' sqrTab

urawPw' :: Double -> D -> Sig -> Sig
urawPw' duty = uoscBy' (pwTab duty)

urndRawTri :: Sig -> SE Sig
urndRawTri = urndOscBy triTab

urndRawSaw :: Sig -> SE Sig
urndRawSaw = urndOscBy sawTab

urndRawSqr :: Sig -> SE Sig
urndRawSqr = urndOscBy sqrTab

urndRawPw :: Double -> Sig -> SE Sig
urndRawPw duty = urndOscBy (pwTab duty)

--------------------------------------
-- Hard-sync for simple non-bandlimited waveforms

-- | Hard-sync with non-bandlimited triangle wave.
rawTriSync :: SyncSmooth -> Sig -> Sig -> Sig
rawTriSync = oscSyncBy triTab

-- | Hard-sync with non-bandlimited square wave.
rawSqrSync :: SyncSmooth -> Sig -> Sig -> Sig
rawSqrSync = oscSyncBy sqrTab

-- | Hard-sync with non-bandlimited sawtooth wave.
rawSawSync :: SyncSmooth -> Sig -> Sig -> Sig
rawSawSync = oscSyncBy sawTab

-- | Hard-sync with non-bandlimited pulse-width wave.
rawPwSync  :: Double -> SyncSmooth -> Sig -> Sig -> Sig
rawPwSync duty = oscSyncBy (pwTab duty)

-- | Hard-sync with non-bandlimited waves.
oscSyncBy :: Tab -> SyncSmooth -> Sig -> Sig -> Sig
oscSyncBy tab smoothType cpsRatio cps = (\smoothFun -> syncOsc smoothFun tab cpsRatio cps) $ case smoothType of
    RawSync      -> (\_ _ -> 1)                   
    SawSync      -> (\amaster _ -> (1 - amaster)) 
    TriSync      -> (const $ readSync uniTriTab)  
    TrapSync     -> (const $ readSync uniTrapTab) 
    UserSync gen -> (const $ readSync gen)        
    where
        readSync ft async = table3 async ft `withD` 1        
        uniTriTab  = setSize 4097 $ elins [0, 1, 0]
        uniTrapTab = setSize 4097 $ elins [1, 1, 0]

syncOsc smoothFun ftab ratio cps = dcblock $ aout
    where
        (amaster, asyncMaster) = syncphasor cps 0
        (aslave,  asyncSlave)  = syncphasor (cps * ratio) asyncMaster
        aosc = table3 aslave ftab `withD` 1
        aout = aosc * smoothFun amaster asyncMaster


