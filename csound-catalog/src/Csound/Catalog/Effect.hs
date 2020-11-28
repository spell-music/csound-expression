-- | A gallery of sound processors (effects).
module Csound.Catalog.Effect(
    nightChorus, nightReverb, 
    vibroDelay, delayLine, bassEnhancment, declick,
    sweepFilter, loopSweepFilter,
    -- * Presets
    bayAtNight, vestigeOfTime
) where

import Control.Monad

import Csound.Base

-- | A signal goes throgh the chain of varible delays. 
-- Delay time is affected by vibrato.
--
-- > aout = vibroDelay n delayBufferSize vibDepth vibRate asig
--
-- * @n@ -- number of delay lines
--
-- * @delayBufSize@ -- buffer size for the delay lines (it should be greater
-- than absolute maximum of the depth of the vibrato)
--
-- * @vibDepth@ -- the amplitude of the delay line time vibrato
--
-- * @vibRate@ -- the frequency of the delay lie time vibrato
vibroDelay :: Int -> D -> Sig -> Sig -> Sig -> Sig
vibroDelay order delayBufSize vibDepth vibRate asig = balance aout asig
    where aout = mean $ take order $ iterate del asig
          del x = vdelay x (vibDepth * uosc vibRate) delayBufSize

-- | Chorus effect, borrowed from http://www.jlpublishing.com/Csound.htm
-- I made some of its parameters accesible trhough score
-- delay in milliseconds (by John Lato in Csound)
--
-- > nightChorus idlym iscale asig
--
-- * idlym  -- delay in milliseconds
--
-- * iscale -- amplitude of the vibrato on delay time (in milliseconds).
nightChorus :: D -> D -> Sig -> Sig
nightChorus idlym iscale asig = 0.5 * aout
    where 
        phi cps maxDel = vdelay3 asig (sig (idlym / 5) + sig (idlym / iscale) * osc cps) maxDel
        aout = sum $ zipWith phi 
            [1, 0.995, 1.05, 1]
            [900, 700, 700, 900]

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Reverb
--
-- A bunch of delay lines FDN reverb, with feedback matrix based upon 
-- physical modeling scattering junction of 8 lossless waveguides
-- of equal characteristic impedance. Based on Julius O. Smith III, 
-- \"A New Approach to Digital Reverberation using Closed Waveguide
-- Networks,\" Proceedings of the International Computer Music 
-- Conference 1985, p. 47-53 (also available as a seperate
-- publication from CCRMA), as well as some more recent papers by
-- Smith and others.
--
-- Coded by Sean Costello, October 1999 (in Csound)
--
-- > nightReverb n igain ipitchmod itone cps
--
--  * @n@               -- a number of delay lines (typical value is 8)
--
--  * @igain@           -- gain of reverb. adjust empirically
--                      for desired reverb time. 0.6 gives
--                      a good small \"live\" room sound, 0.8
--                      a small hall, 0.9 a large hall,
--                      0.99 an enormous stone cavern.
--  
--  * @ipitchmod@      -- amount of random pitch modulation
--                     for the delay lines. 1 is the \"normal\"
--                     amount, but this may be too high for
--                     held pitches such as piano tones.
--                     adjust to taste.
--  
--  * @itone@           -- cutoff frequency of lowpass filters
--                       in feedback loops of delay lines,
--                       in hz. lower cutoff frequencies results
--                       in a sound with more high-frequency
--                       damping.
--
nightReverb :: Int -> D -> D -> D -> Sig -> SE (Sig, Sig)
nightReverb n igain ipitchmod itone asig = do
    afiltRefs   <- mapM newRef $ replicate n 0
    afilts1     <- mapM readRef afiltRefs 
    let apj     = (2 / fromIntegral n) * sum afilts1
    adels       <- sequence $ zipWith3 (del apj) idels ks afilts1
    zipWithM_ (\ref x -> writeRef ref $ filt x) afiltRefs adels
    afilts2     <- mapM readRef afiltRefs
    return (mean $ odds afilts2, mean $ evens afilts2)    
    where
        idels = cycle $ fmap ( / getSampleRate) [2473, 2767, 3217, 3557, 3907, 4127, 2143, 1933]
        ks    = cycle $ zipWith3 (\a b c -> randi a b `withSeed` c) 
            [0.001, 0.0011, 0.0017, 0.0006, 0.001, 0.0011, 0.0017, 0.0006]
            [3.1,   3.5,    1.11,   3.973,  2.341, 1.897,  0.891,  3.221]
            [0.06,  0.9,    0.7,    0.3,    0.63,  0.7,    0.9,    0.44]

        del apj idel k afilt = do
            _ <- delayr 1
            adel1 <- deltapi $ sig idel + k * sig ipitchmod
            delayw $ asig  + apj - afilt
            return adel1

        filt adel = tone (adel * sig igain) (sig itone)

-- | Enhances all frequencies below the give frequency by the given coefficient.
-- Original signal is added to the filtered signal with low-pass filter and scaled.
--
-- > bassEnhancment centerFrequency coefficient asig
bassEnhancment :: D -> D -> Sig -> Sig
bassEnhancment cfq k asig = sig k * butlp asig (sig cfq) + asig

-- | A chain of delay lines.
--
-- > delayLine n k dt asig
-- 
-- A signal (@asig@) is passed through the chain of fixed time delays (A @dt@ is the delay time 
-- @n@ is a number of filters, k - is scale of the signals that is passed through each delay line).
delayLine :: Int -> D -> D -> Sig -> (Sig, Sig)
delayLine n k dt asig = (mean $ asig : odds asigs, mean $ asig : evens asigs)
    where phi x = delaySig dt (x * sig k)
          asigs = take n $ iterate phi (delaySig dt asig)

-- | Adds a very short fade in to remove the click at the beggining of the note.
declick :: Sig -> Sig
declick = (fadeIn 0.01 * )

-- | Sweep band pass filter (center frequency ramps from one value to another)
--
-- > sweepFilter dur startCps endCps bandWidth asignal
sweepFilter :: D -> D -> D -> Sig -> Sig -> Sig
sweepFilter dur start end bandWidth = bp centerFreq bandWidth
    where centerFreq = linseg [start, dur, end]

-- | Sweep band pass filter in loops (center frequency ramps from one value to another and back)
--
-- > sweepFilter dur startCps endCps bandWidth asignal
loopSweepFilter :: D -> D -> D -> Sig -> Sig -> Sig
loopSweepFilter dur start end bandWidth = bp centerFreq bandWidth
    where centerFreq = loopseg [sig start, 1, sig end, 1, sig start] (1 / sig dur)

-- | The effect that was used in the piece \"Bay at night\".
bayAtNight :: Sig -> SE (Sig, Sig)
bayAtNight
    = mapOut (bassEnhancment 100 1.5)
    . nightReverb 8 0.98 0.8 20000 
    . nightChorus 2 30
    where mapOut f = fmap (\(a, b) -> (f a, f b))

-- | The effect that was used in the piece \"Vestige of time\".
vestigeOfTime :: Sig -> (Sig, Sig)
vestigeOfTime 
    = mapOut ((* 0.3) . (\x -> reverb2 x 2 0.2))     
    . delayLine 6 1.2 0.9
    where mapOut f (a, b) = (f a, f b)

