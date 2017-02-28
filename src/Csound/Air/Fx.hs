{-# Language FlexibleContexts #-}
-- | Effects
module Csound.Air.Fx(    
    -- * Reverbs
    reverbsc1, rever1, rever2, reverTime,
    smallRoom, smallHall, largeHall, magicCave,
    smallRoom2, smallHall2, largeHall2, magicCave2,

    room, chamber, hall, cave,

    -- ** Impulse Responce convolution reverbs
    --
    -- | Be careful with volumes. Some IRs can require scaling with really small coefficients like 0.01.
    -- 
    monoIR, stereoIR, stereoIR2, pmonoIR, pstereoIR, pstereoIR2, 
    monoIR', stereoIR', stereoIR2',
    ZConvSpec(..), zconv, zconv',

    -- * Delays
    MaxDelayTime, DelayTime, Feedback, Balance,
    echo, fvdelay, fvdelays, funDelays, tabDelay,
    PingPongSpec(..), pingPong, pingPong', csdPingPong,

    -- * Distortion
    distortion,

    -- * Chorus
    DepthSig, RateSig, WidthSig, ToneSig,
    chorus,
    -- solinaChorus, testSolinaChorus,

    -- * Flanger
    flange,

    -- * Phase
    phase1, harmPhase, powerPhase,

    -- * Effects with unit parameters
    -- | Implemented by Iain McCurdy's Csound code.

    DriveSig, SensitivitySig, BaseCps, Resonance, TimeSig, BitsReductionSig, FoldoverSig,
    TremWaveSig, RatioSig, FftSize,

    fxDistort, stChorus2, fxPhaser, 
    fxFlanger, analogDelay, fxEcho, fxFilter, 
    fxWhite, fxPink, equalizer, eq4, eq7,
    fxGain, 

    fxAnalogDelay, fxDistortion, fxFollower, fxReverse, fxLoFi, fxChorus2, fxAutoPan, fxTrem, fxPitchShifter, fxFreqShifter, {- , 
    fxRingModulator, , -}
    fxCompress,

    -- Eq
    audaciousEq,

    -- * Misc
    trackerSplice, pitchShifterDelay

) where

import Data.Boolean
import Data.Default

import Csound.Typed
import Csound.Tab(sines4, startEnds, setSize, elins, newTab, tabSizeSecondsPower2, tablewa, sec2rel)
import Csound.Typed.Opcode
import Csound.SigSpace
import Csound.Tab

import Csound.Air.Wave(Lfo, unipolar, oscBy, utri, white, pink)
import Csound.Air.Filter
import Csound.Typed.Plugins hiding(pitchShifterDelay,
    fxAnalogDelay, fxDistortion, fxEnvelopeFollower, fxFlanger, fxFreqShifter, fxLoFi, 
    fxPanTrem, fxPhaser, fxPitchShifter, fxReverse, fxRingModulator, fxChorus2)

import qualified Csound.Typed.Plugins as P(pitchShifterDelay,
    fxAnalogDelay, fxDistortion, fxEnvelopeFollower, fxFlanger, fxFreqShifter, fxLoFi, 
    fxPanTrem, fxPhaser, fxPitchShifter, fxReverse, fxRingModulator, fxChorus2)

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

-- | An alias for 
--
-- > let room dryWet asig = mixAt dryWet smallRoom2 asig
room :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
room mx ain = mixAt mx smallRoom2 ain

-- | An alias for 
--
-- > let room dryWet asig = mixAt dryWet smallHall2 asig
chamber :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
chamber mx ain = mixAt mx smallHall2 ain

-- | An alias for 
--
-- > let room dryWet asig = mixAt dryWet largeHall2 asig
hall :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hall mx ain = mixAt mx largeHall2 ain

-- | An alias for 
--
-- > let room dryWet asig = mixAt dryWet magicCave2 asig
cave :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
cave mx ain = mixAt mx magicCave2 ain

---------------------------------------------------------------------------------
-- IR reverbs

-- | Fast zero delay convolution with impulse response that is contained in mono-audio file.
--
-- > monoIR irFile ain
monoIR :: FilePath -> Sig -> Sig
monoIR = monoIR' def

-- | Fast zero delay convolution with impulse response that is contained in mono-audio file.
-- We can specify aux parameters for convolution algorithm (see @zconv'@).
--
-- > monoIR' spec irFile ain
monoIR' :: ZConvSpec -> FilePath -> Sig -> Sig
monoIR' spec fileName ain = zconv' spec (wavLeft fileName) ain

-- | Fast zero delay convolution with impulse response that is contained in stereo-audio file.
--
-- > stereoIR irFile ain
stereoIR :: FilePath -> Sig2 -> Sig2
stereoIR = stereoIR' def

-- | Fast zero delay convolution with impulse response that is contained in stereo-audio file.
-- We can specify aux parameters for convolution algorithm (see @zconv'@).
--
-- > stereoIR' spec irFile ain
stereoIR' :: ZConvSpec -> FilePath -> Sig2 -> Sig2
stereoIR' spec fileName (ainL, ainR) = (zconv' spec (wavLeft fileName) ainL, zconv' spec (wavRight fileName) ainR)

-- | If IR is encoded in a couple of mono files.
stereoIR2 :: (FilePath, FilePath) -> Sig2 -> Sig2
stereoIR2 = stereoIR2' def

-- | If IR is encoded in a couple of mono files.
stereoIR2' :: ZConvSpec -> (FilePath, FilePath) -> Sig2 -> Sig2
stereoIR2' spec (file1, file2) (ainL, ainR) = (monoIR' spec file1 ainL, monoIR' spec file2 ainR)

-- | Precise mono IR with pconvolve (requires a lot of CPU).
pmonoIR :: FilePath -> Sig -> Sig
pmonoIR fileName ain = pconvolve ain (text fileName)

-- | Precise stereo IR with pconvolve (requires a lot of CPU).
pstereoIR :: FilePath -> Sig2 -> Sig2
pstereoIR fileName (ainL, ainR) = pconvolve ((ainL + ainR) * 0.5) (text fileName)

pstereoIR2 :: (FilePath, FilePath) -> Sig2 -> Sig2
pstereoIR2 (file1, file2) (ainL, ainR) = (pmonoIR file1 ainL, pmonoIR file2 ainR)
    

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
echo :: MaxDelayTime -> Feedback -> Sig -> Sig
echo len fb x = x + flanger x (sig len) fb `withD` (len + 0.005)

-- | Delay with feedback. 
--
-- > fdelay maxDelayLength delayLength feedback balance
fvdelay :: MaxDelayTime -> DelayTime -> Feedback -> Sig -> Sig
fvdelay len dt fb a = a + flanger a dt fb `withD` len

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

-- | Delay for functions that use some table (as a buffer). As granular synth or mincer.
--
-- > tabDelay fn maxDelayTime delayTime feedback balance asig
tabDelay :: (Tab -> Sig -> SE Sig) -> MaxDelayTime -> DelayTime -> Feedback -> Balance -> Sig -> SE Sig
tabDelay go maxLength delTim  kfeed kbalance asig = do
    buf <- newTab tabLen    
    ptrRef <- newRef (0 :: Sig)
    aresRef <- newRef (0 :: Sig)  
    ptr <- readRef ptrRef
    when1 (ptr >=* sig tabLen) $ do
        writeRef ptrRef 0
    ptr <- readRef ptrRef 

    let kphs = (ptr / sig tabLen) - (delTim/(sig $ tabLen / getSampleRate))
    awet <-go buf (wrap kphs 0 1)
    writeRef aresRef $ asig + kfeed * awet
    ares <- readRef aresRef
    writeRef ptrRef =<< tablewa buf ares 0
    return $ (1 - kbalance) * asig + kbalance * awet
    where
        tabLen = tabSizeSecondsPower2 maxLength

-- | Aux parameters for ping pong delay. 
-- They are maximum delay time, low pass filter center frequency and Pan width.
-- The defaults are @(5 sec, 3500, 0.3)@.
data PingPongSpec = PingPongSpec {
        pingPongMaxTime :: MaxDelayTime,
        pingPongDamp    :: Sig,
        pingPongWidth   :: Sig    
    }

instance Default PingPongSpec where
    def = PingPongSpec {
            pingPongMaxTime = 5,
            pingPongDamp    = 3500,
            pingPongWidth   = 0.3
        }

-- | Ping-pong delay. 
--
-- > pingPong delayTime feedback mixLevel
pingPong :: DelayTime -> Feedback -> Balance -> Sig2 -> SE Sig2
pingPong delTime feedback mixLevel (ainL, ainR) = pingPong' def delTime feedback mixLevel (ainL, ainR)

-- | Ping-pong delay with miscellaneous arguments. 
--
-- > pingPong' spec delayTime feedback mixLevel
pingPong' :: PingPongSpec -> DelayTime -> Feedback -> Balance -> Sig2 -> SE Sig2    
pingPong' (PingPongSpec maxTime damp width) delTime feedback mixLevel (ainL, ainR) = 
    csdPingPong maxTime delTime damp feedback width mixLevel (ainL, ainR)

-- | Ping-pong delay defined in csound style. All arguments are present (nothing is hidden).
-- 
-- > csdPingPong maxTime delTime damp feedback width mixLevel (ainL, ainR)
csdPingPong :: MaxDelayTime -> DelayTime -> Sig -> Feedback -> Sig -> Balance -> Sig2 -> SE Sig2
csdPingPong maxTime delTime damp feedback width mixLevel (ainL, ainR) = do
    afirst <- offsetDelay ainL   
    atapL  <- channelDelay afirst
    atapR  <- channelDelay ainR
    return $ mixControl $ widthControl afirst (atapL, atapR)
    where
        offsetDelay ain = do
            abuf <- delayr maxTime
            afirst <- deltap3 delTime
            let afirst1 = tone afirst damp
            delayw ain
            return afirst1

        channelDelay ain = do
            abuf <- delayr (2 * maxTime)
            atap <- deltap3 (2 * delTime)
            let atap1 = tone atap damp
            delayw (ain + atap1 * feedback)
            return atap1

        widthControl afirst (atapL, atapR) = (afirst + atapL + (1 - width) * atapR, atapR + (1 - width) * atapL)

        mixControl (atapL ,atapR) = (cfd mixLevel ainL atapL, cfd mixLevel ainR atapR)

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

-- Analog delay

-- | Analog delay.
--
-- > analogDelay mix feedback time tone sigIn
analogDelay :: Balance -> Feedback -> DelayTime -> ToneSig -> Sig -> Sig
analogDelay kmix kfback ktime  ktone ain = P.fxAnalogDelay kmix kfback ktime  ktone ain

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

-- | Equalizer with frequencies: 100, 200, 400, 800, 1600, 3200, 6400
eq7 :: [Sig] -> Sig -> Sig -> Sig
eq7 gs = equalizer (zip gs $ fmap (100 * ) [1, 2, 4, 8, 16, 32, 64])

-- | Equalizer with frequencies: 100, 400, 1600, 6400
eq4 :: [Sig] -> Sig -> Sig -> Sig
eq4 gs = equalizer (zip gs $ fmap (100 * ) [1, 4, 16, 64])

-- | Gain
--
-- > fxGain gain sigIn
fxGain :: SigSpace a => Sig -> a -> a
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

-- | Adds filtered pink noize to the signal
--
-- > fxWhite lfoFreq depth sigIn
fxPink :: Sig -> Sig -> Sig -> SE Sig
fxPink freq depth ain = do
    noise <- pink
    return $ ain + 0.5 * depth * blp cps noise
    where cps = expScale 4 (20, 20000) freq

-- Echo

-- | Simplified delay
--
-- > fxEcho maxDelayLength delTime feedback sigIn
fxEcho :: D -> Sig -> Sig -> Sig -> Sig
fxEcho maxLen ktime fback = fvdelay (5 * maxLen) (sig maxLen * 0.95 * kTime) fback
    where
        kporttime = linseg [0,0.001,0.1]
        kTime = portk   ktime  (kporttime*3)

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
    kindxRef <- newRef (0 :: Sig)
    ksampRef <- newRef (1 :: D)
    aoutRef  <- newRef (0 :: Sig)

    buf <- newTab (tabSizeSecondsPower2 maxLength)
    let segLength = segLengthSeconds * sig getSampleRate
        andx = phasor (sig $ getSampleRate / ftlen buf)
        andx1 = delay andx 1
    tabw asig (andx * sig (ftlen buf)) buf
    ksamp <- readRef ksampRef
    let apos = samphold (andx1 * sig (ftlen buf)) (sig ksamp)

    whens [
        (kmode >=* 1 &&* kmode `lessThan` 2, do             
                kindx <- readRef kindxRef                             
                writeRef kindxRef $ ifB (kindx >* segLength) 0 (kindx + 1)                
                kindx <- readRef kindxRef
                when1 (kindx + apos >* sig (ftlen buf)) $ do
                    writeRef kindxRef $ (-segLength)

                kindx <- readRef kindxRef

                writeRef aoutRef $ table (apos + kindx) buf `withDs` [0, 1]
                writeRef ksampRef 0
        ), (kmode >=* 2 &&* kmode `lessThan` 3, do              
                kindx <- readRef kindxRef
                writeRef kindxRef $ ifB ((kindx+apos) <=* 0) (sig (ftlen buf) - apos) (kindx-1)
                kindx <- readRef kindxRef
                writeRef aoutRef $ table (apos+kindx) buf `withDs` [0, 1]
                writeRef ksampRef 0   
        )] (do
                writeRef ksampRef 1
                writeRef aoutRef asig)

    aout <-readRef aoutRef
    return aout



-- | Mean value.
mean :: Fractional a => [a] -> a
mean xs = sum xs / (fromIntegral $ length xs)

---------------------------------------------------
-- rename the arguments and comment

-- | PitchShifterDelay
-- 
-- A pitch shifter effect that employs delay lines
-- 
-- > pitchShifterDelay maxDelayTime delayTime (feedback1, feedback2) transposeRatio ain
--
-- Arguments
--
-- * @maxDelayTime @ --  maximum delay time (kdlt should not exceed this value)
--
-- * @transposeRatio @ --  pitch transposition (in semitones)
--
-- * @delayTime      @ --  delay time employed by the pitch shifter effect (should be within the range ksmps/sr and imaxdlt) 
--
-- * @feedback1      @ --  feedback using method 1 (output from delay taps are fed back directly into their own buffers before enveloping and mixing)
--
-- * @feedback2      @ --  feedback using method 2 (enveloped and mixed output from both taps is fed back into both buffers)-- 
--
-- * @ain            @ --  input audio to be pitch shifted
pitchShifterDelay :: MaxDelayTime -> (Feedback, Feedback) -> DelayTime -> Sig -> Sig -> Sig
pitchShifterDelay maxDelayTime (fb1, fb2) dlt ratio ain = P.pitchShifterDelay maxDelayTime (fb1, fb2) dlt ratio ain

-- | Delay line with low-pass filter in the feedback chain.
-- The filter adds natural decay to the echoes.
--
-- > fxAnalogDelay mixRatio delayTime feedback toneRatio ain
--
-- Note that the center frequency of the filter is measured in normalized units (form 0  to 1).
fxAnalogDelay :: Balance -> DelayTime -> Feedback -> ToneSig -> Sig -> Sig
fxAnalogDelay kmix kdelay kfback ktone ain = P.fxAnalogDelay kmix kdelay kfback ktone ain

type DriveSig = Sig

-- | Distortion unit with low-pass filter.
--
-- > fxDistortion driveLevel toneRatio ain
--
-- Note that the center frequency of the filter is measured in normalized units (form 0  to 1).
fxDistortion :: DriveSig -> ToneSig -> Sig -> Sig
fxDistortion kdrive ktone ain = P.fxDistortion 1 kdrive ktone ain

type SensitivitySig = Sig
type BaseCps = Sig
type Resonance = Sig

-- | Envelope follower. 
--
-- > fxFollower sensitivity baseFrequencyRatio resonance ain
--
-- Arguments:
--
-- * @sensitivity        @ --  sensitivity of the envelope follower (suggested range: 0 to 1)
--
-- * @baseFrequencyRatio @ --  base frequency of the filter before modulation by the input dynamics (range: 0 to 1)
--
-- ; @resonance          @ --  resonance of the lowpass filter (suggested range: 0 to 1)
fxFollower :: SensitivitySig -> BaseCps -> Resonance -> Sig -> Sig
fxFollower ksens kbaseFreq kreson = P.fxEnvelopeFollower ksens kbaseFreq (0.99 * kreson)

type TimeSig = Sig

-- | An effect that reverses an audio stream in chunks
--
-- > fxReverse time
--
-- @time@ -- the size of the chunck in seconds.
fxReverse :: TimeSig -> Sig -> Sig
fxReverse ktime = P.fxReverse ktime

-- | A flanger effect following the typical design of a so called 'stomp box'
-- 
-- >  fxFlanger rate depth delayTime feedback ain = 
--
-- Arguments
--
-- * @rate      @ --  rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)
--
-- * @depth     @ --  depth of the lfo of the effect (range 0 to 1)
--
-- * @delayTime @ --  static delay offset of the flanging effect (range 0 to 1)
--
-- * @feedback  @ --  feedback and therefore intensity of the effect (range 0 to 1)
--
-- * @ain       @ --  input audio to which the flanging effect will be applied
fxFlanger :: RateSig -> DepthSig -> DelayTime -> Feedback -> Sig -> Sig
fxFlanger krate kdepth kdelay kfback ain = P.fxFlanger krate kdepth kdelay kfback ain

-- | Phaser
--
-- An phase shifting effect that mimics the design of a so called 'stomp box'
-- 
-- > fxPhaser rate depth freq fback ain
-- 
-- Arguments:
-- 
-- * @rate  @ --  rate of lfo of the effect (range 0 to 1)
--
-- * @depth @ --  depth of lfo of the effect (range 0 to 1)
--
-- * @freq  @ --  centre frequency of the phase shifting effect in octaves (suggested range 6 to 11)
--
-- * @fback @ --  feedback and therefore intensity of the effect (range 0 to 1)  
--
-- * @ain   @ --  input audio to be pitch shifted
fxPhaser :: RateSig -> DepthSig -> BaseCps -> Feedback -> Sig -> Sig
fxPhaser krate kdepth cps kfback ain = P.fxPhaser krate kdepth (6 + 5 * cps) kfback ain

type BitsReductionSig = Sig
type FoldoverSig = Sig

-- | LoFi
-- 
-- 'Low Fidelity' distorting effects of bit reduction and downsampling (foldover)
-- 
-- > fxLoFi  bits fold ain = ...
-- 
-- Arguments
-- 
-- * @bits  @ --  bit depth reduction (range 0 to 1)
--
-- * @fold  @ --  amount of foldover (range 0 to 1)    
--
-- * @ain   @ --  input audio to have low fidelity distortion effects applied
fxLoFi :: BitsReductionSig -> FoldoverSig -> Sig -> Sig
fxLoFi kbits kfold ain = P.fxLoFi (0.6 * kbits) kfold ain

-- | Stereo Chorus
-- 
-- A stereo chorus effect
-- 
-- > fxChorus2 rate depth width (ainLeft, ainRight)
-- 
-- Arguments
-- 
-- * @rate  @ --  rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)
--
-- * @depth @ --  depth of the lfo of the effect (range 0 to 1)
--
-- * @width @ --  width of stereo widening (range 0 to 1)
--
-- * @ainX  @ --  input stereo signal
fxChorus2 :: RateSig -> DepthSig -> WidthSig -> Sig2 -> Sig2
fxChorus2 krate kdepth kwidth ain = P.fxChorus2 krate kdepth kwidth ain

type TremWaveSig = Sig

-- | Auto pan
-- 
-- > fxAutoPan wave rate depth ain
-- 
-- ; Arguments:
-- 
-- * @wave  @ --  waveform used by the lfo (0=sine 1=triangle 2=square)
--
-- * @rate  @ --  rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)
--
-- * @depth @ --  depth of the lfo of the effect (range 0 to 1)
--
-- * @mode  @ --  mode of the effect (0=auto-panning 1=tremolo)
--
-- * @ain   @ --  input stereo audio
fxAutoPan :: TremWaveSig -> DepthSig -> RateSig -> Sig2 -> Sig2
fxAutoPan tremWave kdepth krate = P.fxPanTrem kdepth krate 0 tremWave

-- | Tremolo
-- 
-- tremolo effect
-- 
-- > fxTrem wave rate depth ain
-- 
-- ; Arguments:
-- 
-- * @wave  @ --  waveform used by the lfo (0=sine 1=triangle 2=square)
--
-- * @rate  @ --  rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)
--
-- * @depth @ --  depth of the lfo of the effect (range 0 to 1)
--
-- * @mode  @ --  mode of the effect (0=auto-panning 1=tremolo)
--
-- * @ain   @ --  input stereo audio
fxTrem :: TremWaveSig -> DepthSig -> RateSig -> Sig2 -> Sig2
fxTrem tremWave kdepth krate = P.fxPanTrem kdepth krate 1 tremWave

type RatioSig = Sig
type FftSize  = D

-- | PitchShifter
-- 
--  A pitch shifter effect based on FFT technology
-- 
-- > fxPitchShifter  fftSize mixRatio transposeRatio feedback ain
-- 
-- Arguments
-- 
-- * @fftSize  @ -- size for FFT analysis (good values 1024, 512, 256, 2048), the higher values introduce latency but lower values are less accurate.
--
-- * @mix      @ --  dry / wet mix of the output signal (range 0 to 1)
--
-- * @transpose@ -- pitch ratio
--
-- * @feedback @ --  control of the amount of output signal fed back into the input of the effect (suggested range 0 to 1) 
--
-- * @ain      @ --  input audio to be pitch shifted
fxPitchShifter :: FftSize -> Balance -> RatioSig -> Feedback -> Sig -> Sig
fxPitchShifter ifftSize kmix ratio kfback = P.fxPitchShifter ifftSize kmix ratio kfback

-- | FreqShifter
-- ; ----------------
-- ; A frequency shifter effect using the hilbert filter
-- ;
-- ; aout  FreqShifter  adry,kmix,kfreq,kmult,kfback
-- ;
-- ; Performance
-- ; -----------
-- ; adry   --  input audio to be frequency shifted
-- ; kmix   --  dry / wet mix of the output signal (range 0 to 1)
-- ; kfreq  --  frequency of frequency shifter effect (suggested range -1000 to 1000)
-- ; kmult  --  multiplier of frequency value for fine tuning control (suggested range -1 to 1)
-- ; kfback --  control of the amount of output signal fed back into the input of the effect (suggested range 0 to 1)
fxFreqShifter :: Balance -> Sig -> Sig -> Feedback -> Sig -> Sig
fxFreqShifter kmix freq kmul kfback = P.fxFreqShifter kmix freq kmul kfback


-- | Compressor. All arguments are relative (range in 0 to 1).
--
-- > fxCompress thresh (loknee, hiknee) ratio (att, rel) gain ain
fxCompress :: Sig -> (Sig, Sig) -> Sig -> (Sig, Sig) -> Sig -> Sig -> Sig
fxCompress thresh (loknee, hiknee) ratio (att, rel) gain  x = gain' * compress x x thresh' loknee' hiknee' ratio' att' rel' 0.05
    where 
        gain' = ampdb $ onLin (-36, 36) gain
        thresh' = onLin (0, 120) thresh
        att' = onExp (0, 1) att
        rel' = onExp (0, 1) rel
        ratio' = onExp (1, 30000) ratio
        loknee' = onLin (0, 120) loknee
        hiknee' = onLin (0, 120) hiknee

        onLin (min, max) val = min + val * (max - min)
        onExp (min, max) val = scale (expcurve val 4) max min