module Csound.Air (
    -- * Basic waveforms
    -- | Basic waveforms that are used most often. A waveform function take in a time varied frequency (in Hz).

    -- ** Bipolar
    osc, oscBy, saw, isaw, pulse, sqr, tri, blosc,

    -- ** Unipolar
    unipolar, bipolar, uosc, uoscBy, usaw, uisaw, upulse, usqr, utri, ublosc,

    -- * Envelopes

    -- ** Relative duration
    onIdur, lindur, expdur, linendur,
    onDur, lindurBy, expdurBy, linendurBy,
    once, onceBy, several, 
    -- ** Looping envelopes
    oscLins, oscElins, oscExps, oscEexps, oscLine, 
    -- ** Faders
    fadeIn, fadeOut, fades, expFadeIn, expFadeOut, expFades,

    -- * Filters
    -- | Arguemnts are inversed to get most out of curruing. First come parameters and the last one is the signal.
    
    -- ** Simple filters
    lp, hp, bp, br,
    
    -- ** Butterworth filters
    blp, bhp, bbp, bbr,

    -- ** Balanced filters
    -- | Applies filter and balances the output by the input signal.
    lpb, hpb, bpb, brb, blpb, bhpb, bbpb, bbrb,

    -- ** Specific filters
    mlp,

    -- * Patterns
    mean, vibrate, randomPitch, chorus, resons, resonsBy, modes, dryWet,    

    -- ** List functions
    odds, evens,

    -- * Widgets
    AdsrBound(..), AdsrInit(..),
    linAdsr, expAdsr, 
    classicWaves,

    -- * Other
    reverbsc1

) where

import Data.List(intersperse)
import Data.Boolean

import Csound.Typed
import Csound.Typed.Opcode hiding (display)
import Csound.Typed.Gui
import Csound.Control.Gui(funnyRadio)

import Csound.Tab(sine)

-------------------------------------------------------------------
-- waveforms

-- | A pure tone (sine wave).
osc :: Sig -> Sig
osc cps = oscil3 1 cps sine

-- | An oscillator with user provided waveform.
oscBy :: Tab -> Sig -> Sig
oscBy tb cps = oscil3 1 cps tb

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

--------------------------------------------------------------------------
-- envelopes

-- | Makes time intervals relative to the note's duration. So that:
--
-- > onIdur [a, t1, b, t2, c]
--
-- becomes: 
--
-- > [a, t1 * idur, b, t2 * idur, c]
onIdur :: [D] -> [D]
onIdur = onDur idur

-- | Makes time intervals relative to the note's duration. So that:
--
-- > onDur dt [a, t1, b, t2, c]
--
-- becomes: 
--
-- > [a, t1 * dt, b, t2 * dt, c]
onDur :: D -> [D] -> [D]
onDur dur xs = case xs of
    a:b:as -> a : b * dur : onDur dur as
    _ -> xs

-- | The opcode 'Csound.Opcode.linseg' with time intervals 
-- relative to the total duration of the note.
lindur :: [D] -> Sig
lindur = linseg . onIdur

-- | The opcode 'Csound.Opcode.expseg' with time intervals 
-- relative to the total duration of the note.
expdur :: [D] -> Sig
expdur = expseg . onIdur

-- | The opcode 'Csound.Opcode.linseg' with time intervals 
-- relative to the total duration of the note given by the user.
lindurBy :: D -> [D] -> Sig
lindurBy dt = linseg . onDur dt

-- | The opcode 'Csound.Opcode.expseg' with time intervals 
-- relative to the total duration of the note given by the user.
expdurBy :: D -> [D] -> Sig
expdurBy dt = expseg . onDur dt

-- | The opcode 'Csound.Opcode.linen' with time intervals relative to the total duration of the note. Total time is set to the value of idur.
--
-- > linendur asig rise decay
linendur :: Sig -> D -> D -> Sig
linendur = linendurBy idur

-- | The opcode 'Csound.Opcode.linen' with time intervals relative to the total duration of the note. Total time is set to the value of
-- the first argument.
--
-- > linendurBy dt asig rise decay
linendurBy :: D -> Sig -> D -> D -> Sig
linendurBy dt asig ris dec = linen asig (ris * dt) dt (dec * dt)

        
-- | Fades in with the given attack time.
fadeIn :: D -> Sig
fadeIn att = linseg [0, att, 1]

-- | Fades out with the given attack time.
fadeOut :: D -> Sig
fadeOut dec = linsegr [1] dec 0
        
-- | Fades in by exponent with the given attack time.
expFadeIn :: D -> Sig
expFadeIn att = expseg [0.0001, att, 1]

-- | Fades out by exponent with the given attack time.
expFadeOut :: D -> Sig
expFadeOut dec = expsegr [1] dec 0.0001

-- | A combination of fade in and fade out.
--
-- > fades attackDuration decayDuration
fades :: D -> D -> Sig
fades att dec = fadeIn att * fadeOut dec

-- | A combination of exponential fade in and fade out.
--
-- > expFades attackDuration decayDuration
expFades :: D -> D -> Sig
expFades att dec = expFadeIn att * expFadeOut dec

--------------------------------------------------------------------------
-- filters

-- | High-pass filter.
--
-- > hp cutoff sig
hp :: Sig -> Sig -> Sig
hp = flip atone

-- | Low-pass filter.
--
-- > lp cutoff sig
lp :: Sig -> Sig -> Sig
lp = flip tone

-- | Band-pass filter.
--
-- > bp cutoff bandwidth sig
bp :: Sig -> Sig -> Sig -> Sig
bp freq band a = reson a freq band

-- | Band-regect filter.
--
-- > br cutoff bandwidth sig
br :: Sig -> Sig -> Sig -> Sig 
br freq band a = areson a freq band

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

-- Balanced filters

balance1 :: (Sig -> Sig -> Sig) -> (Sig -> Sig -> Sig)
balance1 f = \cfq asig -> balance (f cfq asig) asig

balance2 :: (Sig -> Sig -> Sig -> Sig) -> (Sig -> Sig -> Sig -> Sig)
balance2 f = \cfq bw asig -> balance (f cfq bw asig) asig

-- | Balanced low-pass filter.
lpb :: Sig -> Sig -> Sig
lpb = balance1 lp

-- | Balanced high-pass filter.
hpb :: Sig -> Sig -> Sig
hpb = balance1 hp

-- | Balanced band-pass filter.
bpb :: Sig -> Sig -> Sig -> Sig
bpb = balance2 bp

-- | Balanced band-reject filter.
brb :: Sig -> Sig -> Sig -> Sig
brb = balance2 br

-- | Balanced butterworth low-pass filter.
blpb :: Sig -> Sig -> Sig
blpb = balance1 blp

-- | Balanced butterworth high-pass filter.
bhpb :: Sig -> Sig -> Sig
bhpb = balance1 bhp

-- | Balanced butterworth band-pass filter.
bbpb :: Sig -> Sig -> Sig -> Sig
bbpb = balance2 bbp

-- | Balanced butterworth band-reject filter.
bbrb :: Sig -> Sig -> Sig -> Sig
bbrb = balance2 bbr

-- | Moog's low-pass filter.
--
-- > mlp centerFrequency qResonance signal
mlp :: Sig -> Sig -> Sig -> Sig
mlp cf q asig = moogladder asig cf q

--------------------------------------------------------------------------
-- patterns

-- | Selects odd elements from the list.
odds :: [a] -> [a]
odds as = fmap snd $ filter fst $ zip (cycle [True, False]) as 

-- | Selects even elements from the list.
evens :: [a] -> [a]
evens as 
    | null as   = []
    | otherwise = odds $ tail as

-- | Reads table once during the note length. 
once :: Tab -> Sig
once = onceBy idur

-- | Reads table once during a given period of time. 
onceBy :: D -> Tab -> Sig
onceBy dt tb = kr $ oscBy tb (1 / sig dt) 

-- | Reads table several times during the note length.  
several :: Tab -> Sig -> Sig
several tb rate = kr $ oscil3 1 (rate / sig idur) tb

-- | Loops over line segments with the given rate.
--
-- > oscLins [a, durA, b, durB, c, durC ..] cps
--
-- where 
--
-- * @a@, @b@, @c@ ... -- values
--
-- * durA, durB, durC -- durations of the segments relative to the current frequency.
oscLins :: [D] -> Sig -> Sig
oscLins points cps = loopseg cps 0 0 (fmap sig points) 

-- | Loops over equally spaced line segments with the given rate.
--
-- > oscElins [a, b, c] === oscLins [a, 1, b, 1, c]
oscElins :: [D] -> Sig -> Sig
oscElins points = oscLins (intersperse 1 points)

-- | 
--
-- > oscLine a b cps
--
-- Goes from @a@ to @b@ and back by line segments. One period is equal to @2\/cps@ so that one period is passed by @1\/cps@ seconds.
oscLine :: D -> D -> Sig -> Sig
oscLine a b cps = oscElins [a, b, a] (cps / 2)

-- | Loops over exponential segments with the given rate.
--
-- > oscLins [a, durA, typeA, b, durB, typeB, c, durC, typeC ..] cps
--
-- where 
--
-- * @a@, @b@, @c@ ... -- values
--
-- * durA, durB, durC -- durations of the segments relative to the current frequency.
--
-- * typeA, typeB, typeC, ... -- shape of the envelope. If the value is 0 then the shap eis linear; otherwise it is an concave exponential (positive type) or a convex exponential (negative type).
oscExps :: [D] -> Sig -> Sig
oscExps points cps = looptseg cps 0 (fmap sig points)

-- | Loops over equally spaced exponential segments with the given rate.
--
-- > oscLins [a, typeA, b, typeB, c, typeC ..] === oscLins [a, 1, typeA, b, 1, typeB, c, 1, typeC ..]
oscEexps :: [D] -> Sig -> Sig
oscEexps points = oscExps (insertOnes points)
    where insertOnes xs = case xs of
            a:b:as  -> a:1:b:insertOnes as
            _       -> xs

-- | Mean value.
mean :: Fractional a => [a] -> a
mean xs = sum xs / (fromIntegral $ length xs)


-- | Adds vibrato to the sound unit. Sound units is a function that takes in a frequency. 
vibrate :: Sig -> Sig -> (Sig -> a) -> (Sig -> a)
vibrate vibDepth vibRate f cps = f (cps * (1 + kvib))
    where kvib = vibDepth * kr (osc vibRate) 

-- | Adds a random vibrato to the sound unit. Sound units is a function that takes in a frequency. 
randomPitch :: Sig -> Sig -> (Sig -> a) -> (Sig -> SE a)
randomPitch rndAmp rndCps f cps = fmap go $ randh (cps * rndAmp) rndCps
    where go krand = f (cps + krand)


-- | Chorus takes a list of displacments from the base frequencies and a sound unit.
-- Output is mean of signals with displacments that is applied to the base frequency. 
chorus :: Fractional a => [Sig] -> (Sig -> a) -> Sig -> a
chorus ks f = \cps -> mean $ fmap (f . (+ cps)) ks

-- | Applies a resonator to the signals. A resonator is
-- a list of band pass filters. A list contains the parameters for the filters:
--
-- > [(centerFrequency, bandWidth)]
resons :: [(Sig, Sig)] -> Sig -> Sig
resons = resonsBy bp

-- | A resonator with user defined band pass filter.
-- Warning: a filter takes in a center frequency, band width and the signal.
-- The signal comes last (this order is not standard in the Csound but it's more
-- convinient to use with Haskell).
resonsBy :: (cps -> bw -> Sig -> Sig) -> [(cps, bw)] -> Sig -> Sig
resonsBy filt ps asig = mean $ fmap (( $ asig) . uncurry filt) ps

-- | Mixes dry and wet signals. 
--
-- > dryWet ratio effect asig
--
-- * @ratio@ - of dry signal to wet
--
-- * @effect@ - means to wet the signal
--
-- * @asig@ -- processed signal
dryWet :: Sig -> (Sig -> Sig) -> Sig -> Sig
dryWet k ef asig = k * asig + (1 - k) * ef asig


-- | Chain of mass-spring-damping filters.
--
-- > modes params baseCps exciter 
--
-- * params - a list of pairs @(resonantFrequencyRatio, filterQuality)@
--
-- * @baseCps@ - base frequency of the resonator
--
-- * exciter - an impulse that starts a resonator.
modes :: [(Sig, Sig)] -> Sig -> Sig -> Sig
modes = relResonsBy (\cf q asig -> mode asig cf q)

relResonsBy :: (Sig -> a -> Sig -> Sig) -> [(Sig, a)] -> Sig -> Sig -> Sig
relResonsBy resonator ms baseCps apulse = (recip normFactor * ) $ sum $ fmap (\(cf, q) -> harm cf q apulse) ms
    where 
        -- limit modal frequency to prevent explosions by 
        -- skipping if the maximum value is exceeded (with a little headroom)
        gate :: Sig -> Sig
        gate cps = ifB (sig getSampleRate >* pi * cps) 1 0        

        normFactor = sum $ fmap (gate . (* baseCps) . fst) ms

                                    -- an ugly hack to make filter stable for forbidden values)
        harm cf q x = g * resonator (1 - g + g * cps) q x
            where cps = cf * baseCps
                  g   = gate cps  



-- | Mono version of the cool reverberation opcode reverbsc.
--
-- > reverbsc1 asig feedbackLevel cutOffFreq
reverbsc1 :: Sig -> Sig -> Sig -> Sig
reverbsc1 x k co = 0.5 * (a + b)
    where (a, b) = ar2 $ reverbsc x x k co


----------------------------------------------------------------------
-- Widgets

data AdsrBound = AdsrBound
    { attBound  :: Double
    , decBound  :: Double
    , relBound  :: Double }

data AdsrInit = AdsrInit
    { attInit   :: Double
    , decInit   :: Double
    , susInit   :: Double
    , relInit   :: Double }

expEps :: Fractional a => a
expEps = 0.00001

linAdsr :: String -> AdsrBound -> AdsrInit -> Source Sig
linAdsr = genAdsr $ \a d s r -> linsegr [0, a, 1, d, s] r 0

expAdsr :: String -> AdsrBound -> AdsrInit -> Source Sig
expAdsr = genAdsr $ \a d s r -> expsegr [double expEps, a, 1, d, s] r (double expEps)

genAdsr :: (D -> D -> D -> D -> Sig)
    -> String -> AdsrBound -> AdsrInit -> Source Sig
genAdsr mkAdsr name b inits = source $ do
    (gatt, att) <- knob "A" (linSpan expEps $ attBound b) (attInit inits)
    (gdec, dec) <- knob "D" (linSpan expEps $ decBound b) (decInit inits)
    (gsus, sus) <- knob "S" (linSpan expEps 1)       (susInit inits) 
    (grel, rel) <- knob "R" (linSpan expEps $ relBound b) (relInit inits)
    let val   = mkAdsr (ir att) (ir dec) (ir sus) (ir rel)
    gui <- setTitle name $ hor [gatt, gdec, gsus, grel]
    return (gui, val)

-- | A widget with four standard waveforms: pure tone, triangle, square and sawtooth.
-- The last parameter is a default waveform (it's set at init time).
classicWaves :: String -> Int -> Source (Sig -> Sig)
classicWaves name initVal = funnyRadio name 
    [ ("osc", osc)
    , ("tri", tri)
    , ("sqr", sqr)
    , ("saw", saw)]
    initVal



