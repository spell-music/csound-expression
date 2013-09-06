-- | The vital tools.
module Csound.Air (
    -- * Oscillators
    
    -- ** Bipolar
    osc, oscBy, saw, sq, tri, -- pulse, ramp,
    
    -- ** Unipolar
    unipolar, uosc, uoscBy, usaw, usq, utri, -- upulse, uramp,
   
    -- * Envelopes
    onIdur, lindur, expdur, linendur,
    onDur, lindurBy, expdurBy, linendurBy,
    once, onceBy, several, oscLins, oscElins, oscExps, oscEexps, oscLine, fadeIn, fadeOut, 

    -- * Filters
    -- | Arguemnts are inversed to get most out of curruing. First come parameters and the last one is the signal.
    
    -- ** Simple filters
    lp, hp, bp, br,
    
    -- ** Butterworth filters
    blp, bhp, bbp, bbr,

    -- ** Balanced filters
    -- | Applies filter and balances the output by the input signal.
    lpb, hpb, bpb, brb, blpb, bhpb, bbpb, bbrb,

    -- * Patterns
    mean, gain, vibrato, randomPitch, chorus, resons, resonsBy, modes, hase, whase, dryWet,

    -- ** List functions
    odds, evens,

    -- ** Tables
    sine, cosine, sigmoid,  
    
    -- ** Instruments
    toInstr, toMidi, toInstrD, toMidiD,

    -- ** Crossfade
    cfd, cfds, cfdSpec, cfdsSpec

) where

import Data.List(intersperse)

import Csound.Types
import Csound.Opcode(idur, oscil3, pvscross,
    linseg, expseg, linen, loopseg, looptseg, linsegr,
    atone, tone, areson, reson, mode,
    buthp, butbp, butlp, butbr, balance, randh,
    getSampleRate)
import Csound.Control(Out(..))
import Csound.Control.Midi(Msg, ampCps)

--------------------------------------------------------------------------
-- oscillators

-- | Pure tone.
osc :: Cps -> Sig
osc cps = oscil3 1 cps (sines [1])

-- | Oscillates with the given table (cubic interpolation).
oscBy :: Tab -> Cps -> Sig
oscBy tab cps = oscil3 1 cps tab

resolution :: Int
resolution = 12

-- | Sawtooth.
saw :: Cps -> Sig
saw cps = oscil3 1 cps (sines $ take resolution $ fmap (1 / ) [1 .. ])
-- vco 1 cps 1 0.5 `withInits` (sines [1])


-- | Square wave.
sq :: Cps -> Sig
sq cps = oscil3 1 cps (sines $ take resolution $ fmap f [(1::Int) .. ])
    where f :: Int -> Double
          f x
            | even x    = 0
            | otherwise = 1 / fromIntegral x
-- vco 1 cps 2 0.5 `withInits` (sines [1])

-- | Triangle wave.
tri :: Cps -> Sig
tri cps = oscil3 1 cps (sines $ take resolution $ zipWith f (cycle [1, -1]) [1 ..])
    where f :: Double -> Int -> Double
          f a x
            | even x    = 0
            | otherwise = a / fromIntegral (x ^ (2::Int))
-- vco 1 cps 3 0.5 `withInits` (sines [1])

{-
-- | Square wave with variable dty cycle.
--
-- > pulse duty cps
--
-- First argument varies between 0 and 1 (0.5 equals to square wave)
pulse :: Sig -> Sig -> Sig
pulse duty cps = vco 1 cps 2 duty `withInits` (sines [1])

-- | Triangle wave with variable ramp character.
--
-- > ramp angle cps
--
-- First argument varies between 0 and 1 (0.5 equals to triangle wave)
ramp :: Sig -> Sig -> Sig
ramp angle cps = vco 1 cps 3 angle `withInits` (sines [1])
-}

-- unipolar waves

-- | Turns a bipolar sound (ranges from -1 to 1) to unipolar (ranges from 0 to 1)
unipolar :: Sig -> Sig
unipolar a = 0.5 + 0.5 * a

-- | Unipolar pure tone.
uosc :: Cps -> Sig
uosc = unipolar . osc

-- | Unipolar 'Csound.Air.oscBy'.
uoscBy :: Tab -> Cps -> Sig
uoscBy tab = unipolar . oscBy tab

-- | Unipolar sawtooth.
usaw :: Cps -> Sig
usaw = unipolar . saw

-- | Unipolar square wave.
usq :: Cps -> Sig
usq = unipolar . sq

-- | Unipolar triangle wave.
utri :: Cps -> Sig
utri = unipolar . tri

{-
-- | Unipolar pulse.
upulse :: Sig -> Sig -> Sig
upulse a = unipolar . pulse a

uramp :: Sig -> Sig -> Sig
uramp a = unipolar . ramp a
-}

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
lindur :: [D] -> Ksig
lindur = linseg . onIdur

-- | The opcode 'Csound.Opcode.expseg' with time intervals 
-- relative to the total duration of the note.
expdur :: [D] -> Ksig
expdur = expseg . onIdur

-- | The opcode 'Csound.Opcode.linseg' with time intervals 
-- relative to the total duration of the note given by the user.
lindurBy :: D -> [D] -> Ksig
lindurBy dt = linseg . onDur dt

-- | The opcode 'Csound.Opcode.expseg' with time intervals 
-- relative to the total duration of the note given by the user.
expdurBy :: D -> [D] -> Ksig
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

-- | Table for pure sine wave.
sine :: Tab
sine = sines [1]

-- | Table for pure cosine wave.
cosine :: Tab
cosine = buzzes 1 []

-- | Table for sigmoid wave.
sigmoid :: Tab
sigmoid = sines4 [(0.5, 0.5, 270, 0.5)]

-- | Reads table once during the note length. 
once :: Tab -> Ksig
once = onceBy idur

-- | Reads table once during a given period of time. 
onceBy :: D -> Tab -> Ksig
onceBy dt tab = kr $ oscBy tab (1 / sig dt) 

-- | Reads table several times during the note length.  
several :: Tab -> Sig -> Sig
several tab rate = kr $ oscil3 1 (rate / sig idur) tab

-- | Loops over line segments with the given rate.
--
-- > oscLins [a, durA, b, durB, c, durC ..] cps
--
-- where 
--
-- * @a@, @b@, @c@ ... -- values
--
-- * durA, durB, durC -- durations of the segments relative to the current frequency.
oscLins :: [D] -> Sig -> Ksig
oscLins points cps = loopseg cps 0 0 (fmap sig points) 

-- | Loops over equally spaced line segments with the given rate.
--
-- > oscElins [a, b, c] === oscLins [a, 1, b, 1, c]
oscElins :: [D] -> Sig -> Ksig
oscElins points = oscLins (intersperse 1 points)

-- | 
--
-- > oscLine a b cps
--
-- Goes from @a@ to @b@ and back by line segments. One period is equal to @2 / cps@ so that one period is passed by @1/cps@ seconds.
oscLine :: D -> D -> Sig -> Ksig
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
oscExps :: [D] -> Sig -> Ksig
oscExps points cps = looptseg cps 0 (fmap sig points)

-- | Loops over equally spaced exponential segments with the given rate.
--
-- > oscLins [a, typeA, b, typeB, c, typeC ..] === oscLins [a, 1, typeA, b, 1, typeB, c, 1, typeC ..]
oscEexps :: [D] -> Sig -> Ksig
oscEexps points = oscExps (insertOnes points)
    where insertOnes xs = case xs of
            a:b:as  -> a:1:b:insertOnes as
            _       -> xs

-- | Mean value.
mean :: Fractional a => [a] -> a
mean xs = sum xs / (fromIntegral $ length xs)

-- | Harmonic series. Takes a function that transforms the signal by some parameter
-- and the list of parameters. It constructs the series of transformers and sums them
-- at the end with equal strength.
hase :: Out out => (p -> Sig -> out) -> [p] -> Sig -> out
hase f as x = accumOut mean $ fmap (( $ x) . f) as

-- | Harmonic series, but now you can specify the weights of the final sum.
whase :: Out out => (a -> Sig -> out) -> [(Sig, a)] -> Sig -> out
whase f as x = accumOut sum $ fmap (\(weight, param) -> mapOut (weight * ) $ f param x) as

-- | Adds vibrato to the sound unit. Sound units is a function that takes in a frequency. 
vibrato :: Sig -> Sig -> (Sig -> a) -> (Sig -> a)
vibrato vibDepth vibRate f cps = f (cps * (1 + kvib))
    where kvib = vibDepth * kr (osc vibRate) 

-- | Adds a random vibrato to the sound unit. Sound units is a function that takes in a frequency. 
randomPitch :: Sig -> Sig -> (Sig -> a) -> (Sig -> SE a)
randomPitch rndAmp rndCps f cps = fmap go $ randh (cps * rndAmp) rndCps
    where go krand = f (cps + krand)

-- | Chorus takes a list of displacments from the base frequencies and a sound unit.
-- Output is mean of signals with displacments that is applied to the base frequency. 
chorus :: Out a => [Sig] -> (Sig -> a) -> Sig -> a
chorus ks f = \cps -> accumOut mean $ fmap (f . (+ cps)) ks

-- | Applies a gain to the signals. Multiplies all signals with the given signal.
gain :: Out a => Sig -> a -> a
gain env = mapOut (env *)

-- | Applies a resonator to the signals. A resonator is
-- a list of band pass filters. A list contains the parameters for the filters:
--
-- > [(centerFrequency, bandWidth)]
resons :: Out a => [(Sig, Sig)] -> a -> a
resons = resonsBy bp

-- | A resonator with user defined band pass filter.
-- Warning: a filter takes in a center frequency, band width and the signal.
-- The signal comes last (this order is not standard in the Csound but it's more
-- convinient to use with Haskell).
resonsBy :: Out a => (cps -> bw -> Sig -> Sig) -> [(cps, bw)] -> a -> a
resonsBy filt ps asig = accumOut mean $ fmap (flip mapOut asig . uncurry filt) ps

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
dryWet k eff asig = k * asig + (1 - k) * eff asig


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
relResonsBy resonator ms baseCps pulse = gain (recip normFactor) $ sum $ fmap (\(cf, q) -> harm cf q pulse) ms
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


-- | Crossfade.
--
-- > cfd coeff sig1 sig2
--
-- If coeff equals 0 then we get the first signal and if it equals 1 we get the second signal.
cfd :: Sig -> Sig -> Sig -> Sig
cfd coeff a b = (1 - coeff) * a + coeff * b
  
genCfds :: a -> (Sig -> a -> a -> a) -> [Sig] -> [a] -> a
genCfds zero mixFun cs xs = case xs of
    []   -> zero
    a:as -> foldl (\x f -> f x) a $ zipWith mix' cs as 
    where mix' c a b = mixFun c b a
  
-- | Generic crossfade for n coefficients and n+1 signals.
--
-- > cfds coeffs sigs
cfds :: [Sig] -> [Sig] -> Sig
cfds = genCfds 0 cfd

-- | Spectral crossfade.
cfdSpec :: Sig -> Spec -> Spec -> Spec
cfdSpec coeff a b = pvscross a b (1 - coeff) coeff

-- | Generic spectral crossfade.
cfdsSpec :: [Sig] -> [Spec] -> Spec
cfdsSpec = genCfds undefined cfdSpec

-- | Instruments

-- | Creates a simple instrument from the sound unit. Instrument
-- takes in an amplitude and a frequency.
toInstr :: Out a => (Cps -> a) -> ((D, D) -> a)
toInstr f = \(amp, cps) -> mapOut ( * sig amp) $ f (sig cps)

-- | Creates a midi instrument from the sound unit. 
toMidi :: Out a => (Cps -> a) -> (Msg -> a)
toMidi f = toInstr f . ampCps
    
-- | Creates a simple instrument from the sound unit. Instrument
-- takes in an amplitude and a frequency. The frequency is a constant value.
toInstrD :: Out a => (Icps -> a) -> ((D, D) -> a)
toInstrD f = \(amp, cps) -> mapOut ( * sig amp) $ f cps

-- | Creates a midi instrument from the sound unit. The frequency is a constant value.
toMidiD :: Out a => (Icps -> a) -> (Msg -> a)
toMidiD f = toInstrD f . ampCps
    
