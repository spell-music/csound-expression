-- | Effects
module Csound.Air.Fx(    
    -- * Reverbs
    reverbsc1, rever1, rever2, reverTime,
    smallRoom, smallHall, largeHall, magicCave,
    smallRoom2, smallHall2, largeHall2, magicCave2,

    -- * Delays
    MaxDelayTime, DelayTime, Feedback, Balance,
    echo, fdelay, fvdelay, fvdelays, funDelays, tabDelay,

    -- * Distortion
    distortion,

    -- * Chorus
    DepthSig, RateSig, WidthSig, ToneSig,
    chorus,

    -- * Flanger
    flange,

    -- * Phase
    phase1, harmPhase, powerPhase,

    -- * Effects with unit parameters
    fxDistort, fxDistort2, stChorus2, fxPhaser, fxPhaser2,
    fxFlanger, fxFlanger2, analogDelay, analogDelay2, fxEcho, fxEcho2,
    fxFilter, fxFilter2,
    fxWhite, fxWhite2, fxPink, fxPink2, equalizer, equalizer2, eq4, eq7,
    fxGain, 

    -- * Misc
    trackerSplice

) where

import Data.Boolean

import Csound.Typed
import Csound.Tab(sines4, startEnds, setSize, elins, newTab, tabSizeSecondsPower2, tablewa, sec2rel)
import Csound.Typed.Opcode
import Csound.SigSpace

import Csound.Air.Wave(Lfo, unipolar, oscBy, utri, white, pink)
import Csound.Air.Filter
import Csound.Air.Misc(mean)

-- | Mono version of the cool reverberation opcode reverbsc.
--
-- > reverbsc1 asig feedbackLevel cutOffFreq
reverbsc1 :: Sig -> Feedback -> ToneSig -> Sig
reverbsc1 x k co = 0.5 * (a + b)
    where (a, b) = ar2 $ reverbsc x x k co


---------------------------------------------------------------------------
-- Reverbs

-- | Reverb with given time.
reverTime :: DelayTime -> Sig -> Sig
reverTime dt a =  nreverb a dt 0.3 

-- | Mono reverb (based on reverbsc)
--
-- > rever1 feedback asig
rever1 :: Feedback -> Sig -> (Sig, Sig)
rever1 fbk a = reverbsc a a fbk 12000

-- | Mono reverb (based on reverbsc)
--
-- > rever2 feedback (asigLeft, asigRight)
rever2 :: Feedback -> Sig2 -> Sig2
rever2 fbk (a1, a2) = (a1 + wa1, a2 + wa2)
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
smallRoom2 :: Sig2 -> Sig2
smallRoom2 = rever2 0.6

-- | Stereo reverb for small hall.
smallHall2 :: Sig2 -> Sig2
smallHall2 = rever2 0.8

-- | Stereo reverb for large hall.
largeHall2 :: Sig2 -> Sig2
largeHall2 = rever2 0.9

-- | The magic cave reverb (stereo).
magicCave2 :: Sig2 -> Sig2
magicCave2 = rever2 0.99

---------------------------------------------------------------------------------
-- Delays

-- | The maximum delay time.
type MaxDelayTime = D

-- | The delaya time
type DelayTime = Sig

-- | Feedback for delay
type Feedback = Sig

-- | Dry/Wet mix value (ranges from 0 to 1). The 0 is all dry. The 1 is all wet.
type Balance = Sig

-- | The simplest delay with feedback. Arguments are: delay length and decay ratio.
--
-- > echo delayLength ratio
echo :: MaxDelayTime -> Feedback -> Sig -> SE Sig
echo len fb = fdelay len fb 1

-- | Delay with feedback. 
--
-- > fdelay delayLength decayRatio balance
fdelay :: MaxDelayTime -> Feedback -> Balance -> Sig -> SE Sig
fdelay len = fvdelay len (sig len)

-- | Delay with feedback. 
--
-- > fdelay maxDelayLength delayLength feedback balance
fvdelay :: MaxDelayTime -> DelayTime -> Feedback -> Balance -> Sig -> SE Sig
fvdelay len dt fb mx a = do
    _ <- delayr len
    aDel <- deltap3 dt
    delayw $ a + fb * aDel
    return $ a + (aDel * mx)

-- | Multitap delay. Arguments are: max delay length, list of pairs @(delayLength, decayRatio)@,
-- balance of mixed signal with processed signal.
--
-- > fdelay maxDelayLength  delays balance asig
fvdelays :: MaxDelayTime -> [(DelayTime, Feedback)] -> Balance -> Sig -> SE Sig
fvdelays len dtArgs mx a = funDelays len (zip dts fs) mx a
    where 
        (dts, fbks) = unzip dtArgs
        fs = map (*) fbks


-- | Generic multitap delay. It's just like @fvdelays@ but instead of constant feedbackLevel 
-- it expects a function for processing a delayed signal on the tap.
--
-- > fdelay maxDelayLength  delays balance asig
funDelays :: MaxDelayTime -> [(DelayTime, Sig -> Sig)] -> Balance -> Sig -> SE Sig
funDelays len dtArgs mx a = do
    _ <- delayr len
    aDels <- mapM deltap3 dts
    delayw $ a + sum (zipWith ($) fs aDels)
    return $ a + mx * sum aDels 
    where (dts, fs) = unzip dtArgs


sec2rel :: Tab -> Sig -> Sig
sec2rel tab x = x / (sig $ ftlen tab / getSampleRate)

-- | Delay for functions that use some table (as a buffer). As granular synth or mincer.
--
-- > tabDelay fn maxDelayTime delayTime feedback balance asig
tabDelay :: (Tab -> Sig -> SE Sig) -> MaxDelayTime -> DelayTime -> Feedback -> Balance -> Sig -> SE Sig
tabDelay go maxLength delTim  kfeed kbalance asig = do
    buf <- newTab tabLen    
    ptrRef <- newSERef (0 :: Sig)
    aresRef <- newSERef (0 :: Sig)  
    ptr <- readSERef ptrRef
    when1 (ptr >=* sig tabLen) $ do
        writeSERef ptrRef 0
    ptr <- readSERef ptrRef 

    let kphs = (ptr / sig tabLen) - (delTim/(sig $ tabLen / getSampleRate))
    awet <-go buf (wrap kphs 0 1)
    writeSERef aresRef $ asig + kfeed * awet
    ares <- readSERef aresRef
    writeSERef ptrRef =<< tablewa buf ares 0
    return $ (1 - kbalance) * asig + kbalance * awet
    where
        tabLen = tabSizeSecondsPower2 maxLength

type DepthSig = Sig
type RateSig  = Sig
type WidthSig  = Sig
type ToneSig  = Sig

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
chorus :: DepthSig -> RateSig -> Balance -> Sig -> SE Sig
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
flange :: Lfo -> Feedback -> Balance -> Sig -> Sig
flange alfo fbk mx asig = ntrpol asig (flanger asig ulfo fbk) mx
    where ulfo = 0.0001 + 0.02 * unipolar alfo

-- Phaser

-- | First order phaser.
phase1 :: Sig -> Lfo -> Feedback -> Balance -> Sig -> Sig
phase1 ord alfo fbk mx asig = ntrpol asig (phaser1 asig (20 + unipolar alfo) ord fbk) mx  

-- | Second order phaser. Sweeping gaps in the timbre are placed harmonicaly
harmPhase :: Sig -> Lfo -> Sig -> Sig -> Feedback -> Balance -> Sig -> Sig
harmPhase ord alfo q sep fbk mx asig = ntrpol asig (phaser2 asig (20 + unipolar alfo) q ord 1 sep fbk) mx

-- | Second order phaser. Sweeping gaps in the timbre are placed by powers of the base frequency.
powerPhase :: Sig -> Lfo -> Sig -> Sig -> Feedback -> Balance -> Sig -> Sig
powerPhase ord alfo q sep fbk mx asig = ntrpol asig (phaser2 asig (20 + unipolar alfo) q ord 2 sep fbk) mx


-----------------------------------------------------------------
-- new effects

expScale :: Sig -> (Sig, Sig) -> Sig -> Sig
expScale steep (min, max) a = scale (expcurve a steep) max min

logScale :: Sig -> (Sig, Sig) -> Sig -> Sig
logScale steep (min, max) a = scale (logcurve a steep) max min

dryWetMix :: Sig -> (Sig, Sig)
dryWetMix kmix = (kDry, kWet) 
    where
        iWet = setSize 1024 $ elins [0, 1, 1]
        iDry = setSize 1024 $ elins [1, 1, 0]
        kWet = kr $ table kmix iWet `withD` 1
        kDry = kr $ table kmix iDry `withD` 1

fxWet :: (Num a, SigSpace a) => Sig -> a -> a -> a
fxWet mix ain aout = mul dry ain + mul wet aout
    where (dry, wet) = dryWetMix mix

-- Distortion 

-- | Distortion
--
-- > fxDistort level drive tone sigIn
fxDistort :: Feedback -> Sig -> ToneSig -> Sig -> Sig
fxDistort klevel kdrive ktone ain = aout * (scale klevel 0.8 0) * kGainComp1
    where
        aout = blp kLPF $ distort1 ain kpregain kpostgain 0 0

        drive = expScale 8 (0.01, 0.4) kdrive
        kGainComp1 = logScale 700 (5,1) ktone

        kpregain = 100 * drive
        kpostgain = 0.5 * ((1 - drive) * 0.4 + 0.6)

        kLPF = logScale 700 (200, 12000) ktone

-- | Stereo distortion.
fxDistort2 :: Feedback -> Sig -> ToneSig -> Sig2 -> Sig2
fxDistort2 klevel kdrive ktone (al, ar) = (fx al, fx ar)
    where fx = fxDistort klevel kdrive ktone

-- Stereo chorus


-- | Stereo chorus.
--
-- > stChorus2 mix rate depth width sigIn
stChorus2 :: Balance -> RateSig -> DepthSig -> WidthSig -> Sig2 -> Sig2
stChorus2 kmix krate' kdepth kwidth (al, ar) = fxWet kmix (al, ar) (aoutL, aoutR)
    where 
        krate = expScale 20 (0.001, 7) krate'
        ilfoshape = setSize 131072 $ sines4 [(1, 0.5, 0, 0.5)]
        kporttime = linseg  [0, 0.001, 0.02]
        kChoDepth = interp $ portk  (kdepth*0.01) kporttime
        amodL = osciliktp   krate ilfoshape 0
        amodR = osciliktp   krate ilfoshape (kwidth*0.5)
        vdel mod x = vdelay x (mod * kChoDepth * 1000) (1.2 * 1000)
        aChoL = vdel amodL al
        aChoR = vdel amodR ar
        aoutL = 0.6 * (aChoL + al)
        aoutR = 0.6 * (aChoR + ar)

-- Phaser

-- | Phaser
--
-- > fxPhaser mix rate depth freq feedback sigIn
fxPhaser ::Balance -> Feedback -> RateSig -> DepthSig -> Sig -> Sig -> Sig
fxPhaser kmix fb krate' kdepth kfreq ain = fxWet kmix ain aout
    where       
        krate = expScale 10 (0.01, 14) krate'
        klfo  = kdepth * utri krate
        aout  = phaser1 ain (cpsoct $ klfo + kfreq) 8 fb        

-- | Stereo phaser.
fxPhaser2 :: Balance -> Feedback -> RateSig -> DepthSig -> Sig -> Sig2 -> Sig2
fxPhaser2 kmix fb krate kdepth kfreq (al, ar) = (fx al, fx ar)
    where fx = fxPhaser kmix fb krate kdepth kfreq  

-- Flanger

-- | Flanger
--
-- > fxFlanger mix feedback rate depth delay sigIn
fxFlanger :: Balance -> Feedback -> RateSig -> DepthSig -> DelayTime -> Sig -> Sig
fxFlanger kmix kfback krate' kdepth kdelay' ain = fxWet kmix ain aout
    where
        krate = expScale 50 (0.001, 14) krate'
        kdelay = expScale 200 (0.0001, 0.1) kdelay'
        ilfoshape = setSize 131072 $ sines4 [(0.5, 1, 180, 1)]
        kporttime = linseg  [0, 0.001, 0.1]
        adlt = interp $ portk kdelay kporttime
        kdep = portk (kdepth*0.01) kporttime 
        amod = oscili kdep krate ilfoshape      
        adelsig = flanger ain (adlt + amod) kfback `withD` 1.2
        aout = mean [ain, adelsig]

-- | Stereo flanger
fxFlanger2 :: Balance -> Feedback -> RateSig -> DepthSig -> DelayTime -> Sig2 -> Sig2
fxFlanger2 kmix kfback krate kdepth kdelay  (al ,ar) = (fx al, fx ar)
    where fx = fxFlanger kmix kfback krate kdepth kdelay

-- Analog delay

-- | Analog delay.
--
-- > analogDelay mix feedback time tone sigIn
analogDelay :: Balance -> Feedback -> DelayTime -> ToneSig -> Sig -> SE Sig
analogDelay kmix kfback ktime  ktone'  ain = do
    aBuffer <- delayr 5
    atap <- deltap3 aTime
    let atap1 = tone (clip atap 0 1) kTone
    delayw $ ain + atap1*kfback
    return $ ain*kDry + atap1 * kWet
    where
        ktone = expScale 4 (100, 12000) ktone'
        (kDry, kWet) = dryWetMix kmix
        kporttime = linseg [0,0.001,0.1]
        kTime = portk   ktime  (kporttime*3)
        kTone = portk   ktone kporttime
        aTime = interp  kTime

-- | Stereo analog delay.
analogDelay2 :: Balance -> Feedback -> DelayTime -> ToneSig -> Sig2 -> SE Sig2
analogDelay2 kmix kfback ktime ktone  = bindSig fx
    where fx = analogDelay kmix kfback ktime ktone 

-- Filter

-- | Filter effect (a pair of butterworth low and high pass filters).
--
-- > fxFilter lowPassfFreq highPassFreq gain 
fxFilter :: Sig -> Sig -> Sig -> Sig -> Sig
fxFilter kLPF' kHPF' kgain' ain = mul kgain $ app (blp kLPF) $ app (bhp kHPF) $ ain 
    where 
        app f = f . f
        kLPF = scaleFreq kLPF' 
        kHPF = scaleFreq kHPF' 
        kgain = scale kgain' 20 0
        scaleFreq x = expScale 4 (20, 20000) x

-- | Stereo filter effect (a pair of butterworth low and high pass filters).
fxFilter2 :: Sig -> Sig -> Sig -> Sig2 -> Sig2
fxFilter2 kLPF kHPF kgain (al, ar) = (fx al, fx ar)
    where fx = fxFilter kLPF kHPF kgain

-- Equalizer

-- | Equalizer
--
-- > equalizer gainsAndFrequencies gain sigIn
equalizer :: [(Sig, Sig)] -> Sig -> Sig -> Sig
equalizer fs gain ain0 = case fs of
    []   -> ain
    x:[] -> g 0 x ain
    x:y:[] -> mean [g 1 x ain, g 2 y ain]
    x:xs -> mean $ (g 1 x ain : ) $ (fmap (\y -> g 0 y ain) (init xs)) ++ [g 2 (last xs) ain]
    where
        iQ = 1
        iEQcurve = skipNorm $ setSize 4096 $ startEnds [1/64,4096,7.9,64]
        iGainCurve = skipNorm $ setSize 4096 $ startEnds [0.5,4096,3,4]
        g ty (gain, freq) asig = pareq  asig freq (table gain iEQcurve `withD` 1) iQ `withD` ty
        kgain = table gain iGainCurve `withD` 1
        ain = kgain * ain0

-- | Stereo equalizer.
equalizer2 :: [(Sig, Sig)] -> Sig -> Sig2 -> Sig2
equalizer2 fs gain (al, ar) = (fx al, fx ar)
    where fx = equalizer fs gain

-- | Equalizer with frequencies: 100, 200, 400, 800, 1600, 3200, 6400
eq7 :: [Sig] -> Sig -> Sig2 -> Sig2
eq7 gs = equalizer2 (zip gs $ fmap (100 * ) [1, 2, 4, 8, 16, 32, 64])

-- | Equalizer with frequencies: 100, 400, 1600, 6400
eq4 :: [Sig] -> Sig -> Sig2 -> Sig2
eq4 gs = equalizer2 (zip gs $ fmap (100 * ) [1, 4, 16, 64])

-- | Gain
--
-- > fxGain gain sigIn
fxGain :: Sig -> Sig2 -> Sig2
fxGain = mul


-- Noise

-- | Adds filtered white noize to the signal
--
-- > fxWhite lfoFreq depth sigIn
fxWhite :: Sig -> Sig -> Sig -> SE Sig
fxWhite freq depth ain = do
    noise <- white
    return $ ain + 0.5 * depth * blp cps noise
    where cps = expScale 4 (20, 20000) freq

-- | Adds filtered white noize to the stereo signal
fxWhite2 ::Sig -> Sig -> Sig2 -> SE Sig2
fxWhite2 freq depth = bindSig fx 
    where fx = fxWhite freq depth

-- | Adds filtered pink noize to the signal
--
-- > fxWhite lfoFreq depth sigIn
fxPink :: Sig -> Sig -> Sig -> SE Sig
fxPink freq depth ain = do
    noise <- pink
    return $ ain + 0.5 * depth * blp cps noise
    where cps = expScale 4 (20, 20000) freq

-- | Adds filtered pink noize to the stereo signal
fxPink2 ::Sig -> Sig -> Sig2 -> SE Sig2
fxPink2 freq depth = bindSig fx 
    where fx = fxPink freq depth

-- Echo

-- | Simplified delay
--
-- > fxEcho maxDelayLength delTime feedback sigIn
fxEcho :: D -> Sig -> Sig -> Sig -> SE Sig
fxEcho maxLen ktime fback = fvdelay (5 * maxLen) (sig maxLen * 0.95 * kTime) fback 1  
    where
        kporttime = linseg [0,0.001,0.1]
        kTime = portk   ktime  (kporttime*3)

-- | Simplified stereo delay.
fxEcho2 :: D -> Sig -> Sig -> Sig2 -> SE Sig2
fxEcho2 maxLen ktime fback = bindSig fx
    where fx = fxEcho maxLen ktime fback




-- | Instrument plays an input signal in different modes. 
-- The segments of signal can be played back and forth. 
-- 
-- > trackerSplice maxLength segLength mode
-- 
-- * @maxLength@ -- the maximum length of the played segment (in seconds)
--
-- * @segLength@ -- the segment length in seconds
--
-- * @mode@ -- mode of the playing. If it's 1 - only a part of the sample is plyaed and
--   it's played forward. The portion of the signal starts from the current playback point.
--   It lasts for segLength. If it's 2 - the segment is played in reverse. 
--   Other values produce the normal input signal.
--
-- Original author: Rory Walsh
--
-- Example:
--
-- > main = dac $ do    
-- >    let ev ch1 ch2 dt = fmap (\x -> (x, dt)) $ mconcat [
-- >          fmap (const 1.5) $ charOn ch1 
-- >        , fmap (const 2.5) $ charOn ch2 
-- >        , fmap (const 0) $ charOff ch1 <> charOff ch2]
-- > 
-- >    (k, dt) <- stepper (0, 0.1) $ ev 'q' 'w' 0.1 <> ev 'a' 's' 0.2 <> ev 'z' 'x' 0.4
-- >    mul 1.3 $ trackerSplice 0.8 dt (int' k) $ fst $ loopWav 1 "drumLoop.wav"
trackerSplice :: D -> Sig -> Sig -> Sig -> SE Sig
trackerSplice maxLength segLengthSeconds kmode asig = do
    setksmps 1
    kindxRef <- newSERef (0 :: Sig)
    ksampRef <- newSERef (1 :: D)
    aoutRef  <- newSERef (0 :: Sig)

    buf <- newTab (tabSizeSecondsPower2 maxLength)
    let segLength = segLengthSeconds * sig getSampleRate
        andx = phasor (sig $ getSampleRate / ftlen buf)
        andx1 = delay andx 1
    tabw asig (andx * sig (ftlen buf)) buf
    ksamp <- readSERef ksampRef
    let apos = samphold (andx1 * sig (ftlen buf)) (sig ksamp)

    whens [
        (kmode >=* 1 &&* kmode <* 2, do             
                kindx <- readSERef kindxRef                             
                writeSERef kindxRef $ ifB (kindx >* segLength) 0 (kindx + 1)                
                kindx <- readSERef kindxRef
                when1 (kindx + apos >* sig (ftlen buf)) $ do
                    writeSERef kindxRef $ (-segLength)

                kindx <- readSERef kindxRef

                writeSERef aoutRef $ table (apos + kindx) buf `withDs` [0, 1]
                writeSERef ksampRef 0
        ), (kmode >=* 2 &&* kmode <* 3, do              
                kindx <- readSERef kindxRef
                writeSERef kindxRef $ ifB ((kindx+apos) <=* 0) (sig (ftlen buf) - apos) (kindx-1)
                kindx <- readSERef kindxRef
                writeSERef aoutRef $ table (apos+kindx) buf `withDs` [0, 1]
                writeSERef ksampRef 0   
        )] (do
                writeSERef ksampRef 1
                writeSERef aoutRef asig)

    aout <-readSERef aoutRef
    return aout
