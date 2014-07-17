module Csound.Air (        
    -- * Basic waveforms
    -- | Basic waveforms that are used most often. A waveform function take in a time varied frequency (in Hz).

    -- ** Bipolar
    osc, oscBy, saw, isaw, pulse, sqr, tri, blosc,

    -- ** Unipolar
    unipolar, bipolar, on, uon, uosc, uoscBy, usaw, uisaw, upulse, usqr, utri, ublosc,

    -- * Noise
    rndh, urndh, rndi, urndi, white, pink,

    -- * Envelopes

    leg, xeg,

    -- ** Relative duration
    onIdur, lindur, expdur, linendur,
    onDur, lindurBy, expdurBy, linendurBy,
    once, onceBy, several, 
    -- ** Looping envelopes
    oscLins, oscElins, oscExps, oscEexps, oscLine, 
    -- ** Faders
    fadeIn, fadeOut, fades, expFadeIn, expFadeOut, expFades,

    -- * Low frequency oscillators
    Lfo, lfo,

    -- * Filters
    -- | Arguemnts are inversed to get most out of curruing. First come parameters and the last one is the signal.
    
    -- ** Simple filters
    lp, hp, bp, br, alp,
    
    -- ** Butterworth filters
    blp, bhp, bbp, bbr,

    -- ** Specific filters
    mlp,

    -- * Sound files playback

    -- ** Stereo
    readSnd, loopSnd, loopSndBy, 
    readWav, loopWav, 
    
    -- ** Mono
    readSnd1, loopSnd1, loopSndBy1, 
    readWav1, loopWav1, 
    
    -- ** Utility
    lengthSnd, segments,

    -- * Signal manipulation
    takeSnd, delaySnd, segmentSnd, repeatSnd, toMono,

    -- * Spectral functions
    toSpec, fromSpec, mapSpec, scaleSpec, addSpec, scalePitch,

    -- * Patterns
    mean, vibrate, randomPitch, chorusPitch, resons, resonsBy, modes, dryWet,    

    -- ** List functions
    odds, evens,

    -- * Widgets
    AdsrBound(..), AdsrInit(..),
    linAdsr, expAdsr, 
    classicWaves,
    masterVolume, masterVolumeKnob,

    -- Effects
    
    -- ** Reverbs
    reverbsc1, rever1, rever2, reverTime,
    smallRoom, smallHall, largeHall, magicCave,
    smallRoom2, smallHall2, largeHall2, magicCave2,

    -- ** Delays
    echo, fdelay, fvdelay, fvdelays, funDelays,

    -- ** Distortion
    distortion,

    -- ** Chorus
    chorus,

    -- ** Flanger
    flange,

    -- ** Phase
    phase1, harmPhase, powerPhase

) where

import Data.List(intersperse, isSuffixOf)
import Data.Boolean

import Csound.Typed
import Csound.Typed.Opcode hiding (display, lfo)
import Csound.Typed.Gui
import Csound.Control.Gui(funnyRadio)
import Csound.Control.Evt(metroE, eventList)
import Csound.Control.Instr(withDur, sched)

import Csound.Tab(sine, sines4)

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

-- rescaling

-- | Rescaling of the bipolar signal (-1, 1) -> (a, b)
-- 
-- > on a b biSig
on :: Sig -> Sig -> Sig -> Sig
on a b x = uon a b $ unipolar x 

-- | Rescaling of the unipolar signal (0, 1) -> (a, b)
-- 
-- > on a b uniSig
uon :: Sig -> Sig -> Sig -> Sig
uon a b x = a + (b - a) * x

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
-- envelopes

-- | Linear adsr envelope generator with release
--
-- > leg attack decay sustain release
leg :: D -> D -> D -> D -> Sig
leg = madsr

-- | Exponential adsr envelope generator with release
--
-- > xeg attack decay sustain release
xeg :: D -> D -> D -> D -> Sig
xeg a d s r = mxadsr a d (s + 0.00001) r

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
-- lfo

-- | Low frequency oscillator
type Lfo = Sig

-- | Low frequency oscillator
--
-- > lfo shape depth rate
lfo :: (Sig -> Sig) -> Sig -> Sig -> Sig
lfo shape depth rate = depth * shape rate

--------------------------------------------------------------------------
-- filters

-- | Low-pass filter.
--
-- > lp cutoff resonance sig
lp :: Sig -> Sig -> Sig -> Sig
lp cf q a = bqrez a cf q

-- | High-pass filter.
--
-- > hp cutoff resonance sig
hp :: Sig -> Sig -> Sig -> Sig
hp cf q a = bqrez a cf q `withD` 1

-- | Band-pass filter.
--
-- > bp cutoff resonance sig
bp :: Sig -> Sig -> Sig -> Sig
bp cf q a = bqrez a cf q `withD` 2

-- | Band-reject filter.
--
-- > br cutoff resonance sig
br :: Sig -> Sig -> Sig -> Sig
br cf q a = bqrez a cf q `withD` 3

-- | All-pass filter.
--
-- > alp cutoff resonance sig
alp :: Sig -> Sig -> Sig -> Sig
alp cf q a = bqrez a cf q `withD` 4

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


-- | Moog's low-pass filter.
--
-- > mlp centerFrequency qResonance signal
mlp :: Sig -> Sig -> Sig -> Sig
mlp cf q asig = moogladder asig cf q


--------------------------------------------------------------------------
-- Signal manipulation

-- | Takes only given amount (in seconds) from the signal (the rest is silence).
takeSnd :: Sigs a => D -> a -> a
takeSnd dt asig = trigs (const $ return asig) $ eventList [(0, dt, unit)]

-- | Delays signals by the given amount (in seconds).
delaySnd :: Sigs a => D -> a -> a
delaySnd dt asig = trigs (const $ return asig) $ eventList [(dt, -1, unit)]

-- | Delays a signal by the first argument and takes only second argument amount
-- of signal (everything is measured in seconds).
segmentSnd ::Sigs a => D -> D -> a -> a
segmentSnd del dur asig = trigs (const $ return asig) $ eventList [(del, dur, unit)]

-- | Repeats the signal with the given period.
repeatSnd :: Sigs a => D -> a -> a
repeatSnd dt asig = sched (const $ return asig) $ segments dt

--------------------------------------------------------------------------
-- sound files playback

isMp3 :: String -> Bool
isMp3 name = ".mp3" `isSuffixOf` name

-- | Converts stereosignal to mono with function mean.
toMono :: (Sig, Sig) -> Sig
toMono (a, b) = 0.5 * a + 0.5 * b

-- | Length in seconds of the sound file.
lengthSnd :: String -> D
lengthSnd fileName
    | isMp3 fileName	= mp3len $ text fileName
    | otherwise			= filelen $ text fileName

-- | Produces repeating segments with the given time in seconds.
segments :: D -> Evt (D, Unit)
segments dt = withDur dt $ metroE (sig $ recip dt)

-- Stereo

-- | Reads stereo signal from the sound-file (wav or mp3 or aiff).
readSnd :: String -> (Sig, Sig)
readSnd fileName
	| isMp3 fileName = mp3in (text fileName)		
	| otherwise      = diskin2 (text fileName) 1

-- | Reads stereo signal from the sound-file (wav or mp3 or aiff)
-- and loops it with the given period (in seconds).
loopSndBy :: D -> String -> (Sig, Sig)
loopSndBy dt fileName = repeatSnd dt $ readSnd fileName

-- | Reads stereo signal from the sound-file (wav or mp3 or aiff)
-- and loops it with the file length.
loopSnd :: String -> (Sig, Sig)
loopSnd fileName = loopSndBy (lengthSnd fileName) fileName

-- | Reads the wav file with the given speed (if speed is 1 it's a norma playback).
-- We can use negative speed to read file in reverse.
readWav :: Sig -> String -> (Sig, Sig)
readWav speed fileName = diskin2 (text fileName) speed

-- | Reads th wav file and loops over it.
loopWav :: Sig -> String -> (Sig, Sig)
loopWav speed fileName = flip withDs [0, 1] $ ar2 $ diskin2 (text fileName) speed

-- Mono

-- | The mono variant of the function @readSnd@.
readSnd1 :: String -> Sig
readSnd1 fileName 
    | isMp3 fileName = toMono $ readSnd fileName
    | otherwise      = diskin2 (text fileName) 1

-- | The mono variant of the function @loopSndBy@.
loopSndBy1 :: D -> String -> Sig
loopSndBy1 dt fileName = repeatSnd dt $ readSnd1 fileName

-- | The mono variant of the function @loopSnd@.
loopSnd1 :: String -> Sig
loopSnd1 fileName = loopSndBy1 (lengthSnd fileName) fileName

-- | The mono variant of the function @readWav@.
readWav1 :: Sig -> String -> Sig
readWav1 speed fileName = diskin2 (text fileName) speed

-- | The mono variant of the function @loopWav@.
loopWav1 :: Sig -> String -> Sig
loopWav1 speed fileName = flip withDs [0, 1] $ diskin2 (text fileName) speed

--------------------------------------------------------------------------
-- spectral functions

-- | Converts signal to spectrum.
toSpec :: Sig -> Spec
toSpec asig = pvsanal asig 1024 256 1024 1

-- | Converts spectrum to signal.
fromSpec :: Spec -> Sig
fromSpec = pvsynth

-- | Applies a transformation to the spectrum of the signal.
mapSpec :: (Spec -> Spec) -> Sig -> Sig
mapSpec f = fromSpec . f . toSpec

-- | Scales all frequencies. Usefull for transposition. 
-- For example, we can transpose a signal by the given amount of semitones: 
--
-- > scaleSpec (semitone 1) asig
scaleSpec :: Sig -> Sig -> Sig
scaleSpec k = mapSpec $ \x -> pvscale x k

-- | Adds given amount of Hz to all frequencies.
--
-- > addSpec hz asig
addSpec :: Sig -> Sig -> Sig
addSpec hz = mapSpec $ \x -> pvshift x hz 0

-- | Scales frequency in semitones.
scalePitch :: Sig -> Sig -> Sig
scalePitch n = scaleSpec (semitone n)

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


-- | Chorus takes a number of copies, chorus width and wave shape.
chorusPitch :: Int -> Sig -> (Sig -> Sig) -> Sig -> Sig
chorusPitch n wid = phi dts
    where
        phi :: [Sig] -> (Sig -> Sig) -> Sig -> Sig
        phi ks f = \cps -> mean $ fmap (f . (+ cps)) ks

        dts = fmap (\x -> - wid + fromIntegral x * dt) [0 .. n-1] 

        dt = 2 * wid / fromIntegral n

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

-- | Slider for master volume
masterVolume :: Source Sig
masterVolume = slider "master" uspan 0.5

-- | Knob for master volume
masterVolumeKnob :: Source Sig
masterVolumeKnob = knob "master" uspan 0.5

---------------------------------------------------------------------------
-- Reverbs

-- | Reverb with given time.
reverTime :: Sig -> Sig -> Sig
reverTime dt a =  nreverb a dt 0.3 

-- | Mono reverb (based on reverbsc)
--
-- > rever1 feedback asig
rever1 :: Sig -> Sig -> (Sig, Sig)
rever1 fbk a = reverbsc a a fbk 12000

-- | Mono reverb (based on reverbsc)
--
-- > rever2 feedback asigLeft asigRight
rever2 :: Sig -> Sig -> Sig -> (Sig, Sig)
rever2 fbk a1 a2 = (a1 + wa1, a2 + wa2)
	where (wa1, wa2) = reverbsc a1 a2 fbk 12000

-- | Mono reverb for small room.
smallRoom :: Sig -> (Sig, Sig)
smallRoom = rever1 0.6

-- | Mono reverb for small hall.
smallHall :: Sig -> (Sig, Sig)
smallHall = rever1 0.8

-- | Mono reverb for large hall.
largeHall :: Sig -> (Sig, Sig)
largeHall = rever1 0.9

-- | The magic cave reverb (mono).
magicCave :: Sig -> (Sig, Sig)
magicCave = rever1 0.99

-- | Stereo reverb for small room.
smallRoom2 :: Sig -> Sig -> (Sig, Sig)
smallRoom2 = rever2 0.6

-- | Stereo reverb for small hall.
smallHall2 :: Sig -> Sig -> (Sig, Sig)
smallHall2 = rever2 0.8

-- | Stereo reverb for large hall.
largeHall2 :: Sig -> Sig -> (Sig, Sig)
largeHall2 = rever2 0.9

-- | The magic cave reverb (stereo).
magicCave2 :: Sig -> Sig -> (Sig, Sig)
magicCave2 = rever2 0.99

-- Delays

-- | The simplest delay with feedback. Arguments are: delay length and decay ratio.
--
-- > echo delayLength ratio
echo :: D -> Sig -> Sig -> SE Sig
echo len fb = fdelay len fb 1

-- | Delay with feedback. 
--
-- > fdelay maxDelayLength delayLength decayRatio
fdelay :: D -> Sig -> Sig -> Sig -> SE Sig
fdelay len = fvdelay len (sig len)


-- | Delay with feedback. 
--
-- > fdelay maxDelayLength delayLength feedbackLevel decayRatio
fvdelay :: D -> Sig -> Sig -> Sig -> Sig -> SE Sig
fvdelay len dt fb mx a = do
	_ <- delayr len
	aDel <- deltap3 dt
	delayw $ a + fb * aDel
	return $ a + (aDel * mx)

-- | Multitap delay. Arguments are: max delay length, list of pairs @(delayLength, decayRatio)@,
-- balance of mixed signal with processed signal.
--
-- > fdelay maxDelayLength  delays balance asig
fvdelays :: D -> [(Sig, Sig)] -> Sig -> Sig -> SE Sig
fvdelays len dtArgs mx a = funDelays len (zip dts fs) mx a
	where 
		(dts, fbks) = unzip dtArgs
		fs = map (*) fbks


-- | Generic multitap delay. It's just like @fvdelays@ but instead of constant feedbackLevel 
-- it expects a function for processing a delayed signal on the tap.
--
-- > fdelay maxDelayLength  delays balance asig
funDelays :: D -> [(Sig, Sig -> Sig)] -> Sig -> Sig -> SE Sig
funDelays len dtArgs mx a = do
	_ <- delayr len
	aDels <- mapM deltap3 dts
	delayw $ a + sum (zipWith ($) fs aDels)
	return $ a + mx * sum aDels 
	where (dts, fs) = unzip dtArgs

-- Distortion

-- | Distortion. 
--
-- > distort distLevel asig
distortion :: Sig -> Sig -> Sig
distortion pre asig = distort1 asig pre 0.5 0 0 `withD` 1

-- Chorus

-- | Chorus.
--
-- > chorus depth rate balance asig
chorus :: Sig -> Sig -> Sig -> Sig -> SE Sig
chorus depth rate mx asig = do
	_ <- delayr 1.2
	adelSig <- deltap3 (0.03 * depth * oscBy fn (3 * rate) + 0.01)
	delayw asig
	return $ ntrpol asig adelSig mx
	where fn = sines4 [(0.5, 1, 180, 1)] -- U-shape parabola

-- Flanger

-- | Flanger. Lfo depth ranges in 0 to 1.
--
-- flanger lfo feedback balance asig
flange :: Lfo -> Sig -> Sig -> Sig -> Sig
flange alfo fbk mx asig = ntrpol asig (flanger asig ulfo fbk) mx
	where ulfo = 0.0001 + 0.02 * unipolar alfo

-- Phaser

-- | First order phaser.
phase1 :: Sig -> Lfo -> Sig -> Sig -> Sig -> Sig
phase1 ord alfo fbk mx asig = ntrpol asig (phaser1 asig (20 + unipolar alfo) ord fbk) mx  

-- | Second order phaser. Sweeping gaps in the timbre are placed harmonicaly
harmPhase :: Sig -> Lfo -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
harmPhase ord alfo q sep fbk mx asig = ntrpol asig (phaser2 asig (20 + unipolar alfo) q ord 1 sep fbk) mx

-- | Second order phaser. Sweeping gaps in the timbre are placed by powers of the base frequency.
powerPhase :: Sig -> Lfo -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
powerPhase ord alfo q sep fbk mx asig = ntrpol asig (phaser2 asig (20 + unipolar alfo) q ord 2 sep fbk) mx

