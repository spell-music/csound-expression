{-# Language FlexibleContexts #-}

-- | A friendly family of effects. These functions are kindly provided by Iain McCurdy (designed in Csound).
module Csound.Air.Fx.FxBox(
    adele, tort, fowler, revsy, flan, phasy, crusher, chory, pany, tremy, ringo, EnvelopeModSig,

    -- * Presets
    -- | For all presets we have 5 levels of strength. They are signified by numbers from 1 to 5. Also for some effects (delay and distortion)
    -- we have miscellaneous way to alter preset by suffix b (means bright) and m (means muffled). It alters the tone color of the effect.

    -- ** Analog Delay
    adele1, adele2, adele3, adele4, adele5,
    -- *** Bright
    adele1b, adele2b, adele3b, adele4b, adele5b,
    -- *** Muted
    adele1m, adele2m, adele3m, adele4m, adele5m,

    -- ** Ping Pong Delay
    pongy, pongy1, pongy2, pongy3, pongy4, pongy5,

    -- ** Distortion
    tort1, tort2, tort3, tort4, tort5,

    -- *** Bright
    tort1b, tort2b, tort3b, tort4b, tort5b,

    -- *** Muted
    tort1m, tort2m, tort3m, tort4m, tort5m,

    -- ** Envelope follower
    fowler',
    fowler1, fowler2, fowler3, fowler4, fowler5,

    -- ** Flanger
    flan',
    flan1, flan2, flan3, flan4, flan5,

    -- ** Phaser
    phasy',
    phasy1, phasy2, phasy3, phasy4, phasy5,

    -- ** Chorus
    chory',
    chory1, chory2, chory3, chory4, chory5,

    -- ** Auto Pan
    pany',
    pany1, pany2, pany3, pany4, pany5,

    -- ** Tremolo
    tremy',
    tremy1, tremy2, tremy3, tremy4, tremy5,    

    -- ** Ring modulation
    ringo',
    ringo1, ringo2, ringo3, ringo4, ringo5

    -- * Presets with UIs
    -- | If we use prefix @ui@ we can create an image of our effect that looks like guitar stompbox.
    --
    -- Let's take a distortion fr instance:
    --
    -- > type FxFun = Sig2 -> SE Sig2 
    -- >
    -- > uiTort2 :: Source FxFun
    --
    -- We can combine the effects with functions:
    --
    -- > fxHor, fxVer :: [Source FxFun] -> Source FxFun
    -- >
    -- > fxMatrix :: Int -> [Source FxFun] -> Source FxFun
    -- > fxMatrix numberOfColumns fxs = ...
    --
    -- All these functions stack the effects in the list
    -- and align visuals. The visuals can be stacked horizontally, vertically
    -- or placed on a square grid.
    -- 
    -- Let's create a chain of effects and apply it to the input signal:
    --
    -- > > let pedals ain = lift1 (\f -> f ain) $ fxHor [uiFlan1, uiAdele2 0.25 0.5, uiHall 0.2, uiGain 0.4]
    -- >
    -- > > vdac $ pedals =<< (atMidi $ dryPatch vibraphone)
    --
    -- With @uiGain@ we can change the volume of the output.    


    -- ** Reverb

    -- *** Rooms
    {-
    uiRoom, uiRoom1, uiRoom2, uiRoom3, uiRoom4, uiRoom5,

    -- ** Chambers
    uiChamber, uiChamber1, uiChamber2, uiChamber3, uiChamber4, uiChamber5,

    -- *** Halls
    uiHall, uiHall1, uiHall2, uiHall3, uiHall4, uiHall5,

    -- *** Caves
    uiCave, uiCave1, uiCave2, uiCave3, uiCave4, uiCave5,

    -- ** Delay
    uiAdele1, uiAdele2, uiAdele3, uiAdele4, uiAdele5,
    uiAdele1b, uiAdele2b, uiAdele3b, uiAdele4b, uiAdele5b,
    uiAdele1m, uiAdele2m, uiAdele3m, uiAdele4m, uiAdele5m,

    -- ** Ping Pong Delay
    uiPongy', uiPongy1, uiPongy2, uiPongy3, uiPongy4, uiPongy5,

    -- ** Distortion
    uiTort1, uiTort2, uiTort3, uiTort4, uiTort5,
    uiTort1b, uiTort2b, uiTort3b, uiTort4b, uiTort5b,
    uiTort1m, uiTort2m, uiTort3m, uiTort4m, uiTort5m,

    -- ** Envelope follower
    uiFowler',
    uiFowler1, uiFowler2, uiFowler3, uiFowler4, uiFowler5,

    -- ** Flanger
    uiFlan', uiFlan1, uiFlan2, uiFlan3, uiFlan4, uiFlan5,

    -- ** Phaser
    uiPhasy', uiPhasy1, uiPhasy2, uiPhasy3, uiPhasy4, uiPhasy5,

    -- ** Chorus
    uiChory', uiChory1, uiChory2, uiChory3, uiChory4, uiChory5,

    -- ** Auto Pan
    uiPany', uiPany1, uiPany2, uiPany3, uiPany4, uiPany5,

    -- ** Tremolo
    uiTremy', uiTremy1, uiTremy2, uiTremy3, uiTremy4, uiTremy5,

    -- ** Reverse
    uiRevsy,

    -- ** LoFi
    uiCrusher,

    -- ** Ring modulation
    uiRingo', uiRingo1, uiRingo2, uiRingo3, uiRingo4, uiRingo5
    -}

    -- ** Compressor

   
) where

import Csound.Typed
import Csound.Typed.Opcode(ampdb, scale, expcurve, compress)
import Csound.Typed.Gui

import Csound.SigSpace

import qualified Csound.Typed.Plugins as P(pitchShifterDelay,
    fxAnalogDelay, fxDistortion, fxEnvelopeFollower, fxFlanger, fxFreqShifter, fxLoFi, 
    fxPanTrem, fxPhaser, fxPitchShifter, fxReverse, fxRingModulator, fxChorus2)

import Csound.Air.Patch(Fx)
import Csound.Air.Fx(Balance, DelayTime, Feedback, ToneSig, SensitivitySig, 
    BaseCps, Resonance, DepthSig, RateSig, TremWaveSig, FoldoverSig, BitsReductionSig, 
    DriveSig, TimeSig, WidthSig, 
    rever2, pingPong)

import Csound.Air.Live(fxBox, fromMonoFx, fxColor)

import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C


-- | Analog Delay line with low-pass filter in the feedback chain.
-- The filter adds natural decay to the echoes.
--
-- > adele mixRatio delayTime feedback toneRatio ain
--
-- Note that the center frequency of the filter is measured in normalized units (form 0  to 1).
adele :: Balance -> DelayTime -> Feedback -> ToneSig -> Sig -> Sig
adele kmix kdelay kfback ktone ain = P.fxAnalogDelay kmix kdelay kfback ktone ain

size1, size2, size3, size4, size5 :: Fractional a => a

size1 = 0.1
size2 = 0.25
size3 = 0.5
size4 = 0.75
size5 = 0.95

-- | Distortion unit with low-pass filter.
--
-- > tort driveLevel toneRatio ain
--
-- Note that the center frequency of the filter is measured in normalized units (form 0  to 1).
tort :: DriveSig -> ToneSig -> Sig -> Sig
tort kdrive ktone ain = P.fxDistortion 1 kdrive ktone ain

-- | Envelope follower. 
--
-- > fowler sensitivity baseFrequencyRatio resonance ain
--
-- Arguments:
--
-- * @sensitivity        @ --  sensitivity of the envelope follower (suggested range: 0 to 1)
--
-- * @baseFrequencyRatio @ --  base frequency of the filter before modulation by the input dynamics (range: 0 to 1)
--
-- * @resonance          @ --  resonance of the lowpass filter (suggested range: 0 to 1)
fowler :: SensitivitySig -> BaseCps -> Resonance -> Sig -> Sig
fowler ksens kbaseFreq kreson = P.fxEnvelopeFollower ksens kbaseFreq (0.99 * kreson)

-- | An effect that reverses an audio stream in chunks
--
-- > revsy time
--
-- @time@ -- the size of the chunck in seconds.
revsy :: TimeSig -> Sig -> Sig
revsy ktime = P.fxReverse ktime


-- | A flanger effect following the typical design of a so called 'stomp box'
-- 
-- >  flan rate depth delayTime feedback ain = 
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
flan :: RateSig -> DepthSig -> DelayTime -> Feedback -> Sig -> Sig
flan krate kdepth kdelay kfback ain = P.fxFlanger krate kdepth kdelay kfback ain


-- | Phaser
--
-- An phase shifting effect that mimics the design of a so called 'stomp box'
-- 
-- > phasy rate depth freq fback ain
-- 
-- Arguments:
-- 
-- * @rate  @ --  rate of lfo of the effect (range 0 to 1)
--
-- * @depth @ --  depth of lfo of the effect (range 0 to 1)
--
-- * @freq  @ --  centre frequency of the phase shifting effect in octaves (suggested range 0 to 1)
--
-- * @fback @ --  feedback and therefore intensity of the effect (range 0 to 1)  
--
-- * @ain   @ --  input audio to be pitch shifted
phasy :: RateSig -> DepthSig -> BaseCps -> Feedback -> Sig -> Sig
phasy krate kdepth cps kfback ain = P.fxPhaser krate kdepth (6 + 5 * cps) kfback ain

-- | LoFi (Bit Crusher)
-- 
-- 'Low Fidelity' distorting effects of bit reduction and downsampling (foldover)
-- 
-- > crusher  bits fold ain = ...
-- 
-- Arguments
-- 
-- * @bits  @ --  bit depth reduction (range 0 to 1)
--
-- * @fold  @ --  amount of foldover (range 0 to 1)    
--
-- * @ain   @ --  input audio to have low fidelity distortion effects applied
crusher :: BitsReductionSig -> FoldoverSig -> Sig -> Sig
crusher kbits kfold ain = P.fxLoFi (0.6 * kbits) kfold ain

-- | Stereo Chorus
-- 
-- A stereo chorus effect
-- 
-- > chory rate depth width (ainLeft, ainRight)
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
chory :: RateSig -> DepthSig -> WidthSig -> Sig2 -> Sig2
chory krate kdepth kwidth ain = P.fxChorus2 krate kdepth kwidth ain


-- | Auto pan
-- 
-- > pany wave rate depth ain
-- 
-- ; Arguments:
-- 
-- * @wave  @ --  waveform used by the lfo (0=sine 1=triangle 2=square)
--
-- * @rate  @ --  rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)
--
-- * @depth @ --  depth of the lfo of the effect (range 0 to 1)
--
-- * @ain   @ --  input stereo audio
pany :: TremWaveSig -> DepthSig -> RateSig -> Sig2 -> Sig2
pany tremWave kdepth krate = P.fxPanTrem kdepth krate 0 tremWave

-- | Tremolo
-- 
-- tremolo effect
-- 
-- > tremy wave rate depth ain
-- 
-- ; Arguments:
-- 
-- * @wave  @ --  waveform used by the lfo (0=sine 1=triangle 2=square)
--
-- * @rate  @ --  rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)
--
-- * @depth @ --  depth of the lfo of the effect (range 0 to 1)
--
-- * @ain   @ --  input stereo audio
tremy :: TremWaveSig -> DepthSig -> RateSig -> Sig2 -> Sig2
tremy tremWave kdepth krate = P.fxPanTrem kdepth krate 1 tremWave

-- ringo

type EnvelopeModSig = Sig

-- | RingModulator
-- 
-- An ring modulating effect with an envelope follower
-- 
-- > ringo balance rate envelopeMod ain
-- 
-- * @balance     @  --  dry / wet mix of the output signal (range 0 to 1)
-- ; @rate        @  --  frequency of thew ring modulator *NOT IN HERTZ* (range 0 to 1)
-- ; @envelopeMod @  --  amount of dynamic envelope following modulation of frequency (range 0 to 1)
-- * @ain         @  --  input audio to be pitch shifted
ringo :: Balance -> RateSig -> EnvelopeModSig -> Sig -> Sig
ringo balance rate envelopeMod ain = P.fxRingModulator ain balance rate envelopeMod


---------------------------------------------------------------------
-- Presets


-- Analog Delay

adeleBy :: MixAt Sig Sig a => ToneSig -> Feedback -> Balance -> DelayTime -> a -> AtOut Sig Sig a
adeleBy tone size balance delTime = at (adele balance delTime size tone)

adeleBy_ :: MixAt Sig Sig a => Feedback -> Balance -> DelayTime -> a -> AtOut Sig Sig a
adeleBy_ = adeleBy 0.5

adele1, adele2, adele3, adele4, adele5 :: MixAt Sig Sig a => Balance -> DelayTime -> a -> AtOut Sig Sig a

adele1 = adeleBy_ size1
adele2 = adeleBy_ size2
adele3 = adeleBy_ size3
adele4 = adeleBy_ size4
adele5 = adeleBy_ size5

adeleByB :: MixAt Sig Sig a => Feedback -> Balance -> DelayTime -> a -> AtOut Sig Sig a
adeleByB = adeleBy 0.8

adele1b, adele2b, adele3b, adele4b, adele5b :: MixAt Sig Sig a => Balance -> DelayTime -> a -> AtOut Sig Sig a

adele1b = adeleByB size1
adele2b = adeleByB size2
adele3b = adeleByB size3
adele4b = adeleByB size4
adele5b = adeleByB size5

adeleByM :: MixAt Sig Sig a => Feedback -> Balance -> DelayTime -> a -> AtOut Sig Sig a
adeleByM = adeleBy 0.2

adele1m, adele2m, adele3m, adele4m, adele5m :: MixAt Sig Sig a => Balance -> DelayTime -> a -> AtOut Sig Sig a

adele1m = adeleByM size1
adele2m = adeleByM size2
adele3m = adeleByM size3
adele4m = adeleByM size4
adele5m = adeleByM size5

-- Ping Pong delay

-- | Ping-pong delay
--
-- > pongy feedback delayTime
pongy ::  MixAt Sig2 (SE Sig2) a => Feedback -> DelayTime -> a -> AtOut Sig2 (SE Sig2) a
pongy fbk delTime = at (pingPong delTime fbk (fbk / 2.1))

pongy1, pongy2, pongy3, pongy4, pongy5 :: MixAt Sig2 (SE Sig2) a => DelayTime -> a -> AtOut Sig2 (SE Sig2) a

pongy1 = pongy size1
pongy2 = pongy size2
pongy3 = pongy size3
pongy4 = pongy size4
pongy5 = pongy size5

-- Distortion

tortBy :: MixAt Sig Sig a => ToneSig -> DriveSig -> a -> AtOut Sig Sig a
tortBy tone drive = at (tort drive tone)

tortBy_ :: MixAt Sig Sig a => DriveSig -> a -> AtOut Sig Sig a
tortBy_ = tortBy 0.5

tort1, tort2, tort3, tort4, tort5 :: MixAt Sig Sig a => a -> AtOut Sig Sig a

tort1 = tortBy_ size1
tort2 = tortBy_ size2
tort3 = tortBy_ size3
tort4 = tortBy_ size4
tort5 = tortBy_ size5

tortByB :: MixAt Sig Sig a => DriveSig -> a -> AtOut Sig Sig a
tortByB = tortBy 0.85

tort1b, tort2b, tort3b, tort4b, tort5b :: MixAt Sig Sig a => a -> AtOut Sig Sig a

tort1b = tortByB size1
tort2b = tortByB size2
tort3b = tortByB size3
tort4b = tortByB size4
tort5b = tortByB size5

tortByM :: MixAt Sig Sig a => DriveSig -> a -> AtOut Sig Sig a
tortByM = tortBy 0.2

tort1m, tort2m, tort3m, tort4m, tort5m :: MixAt Sig Sig a => a -> AtOut Sig Sig a

tort1m = tortByM size1
tort2m = tortByM size2
tort3m = tortByM size3
tort4m = tortByM size4
tort5m = tortByM size5

-- Envelope follower

fowler' :: MixAt Sig Sig a => Sig -> a -> AtOut Sig Sig a
fowler' size = at (fowler size size size)

fowler1, fowler2, fowler3, fowler4, fowler5 :: MixAt Sig Sig a => a -> AtOut Sig Sig a

fowler1 = fowler' size1
fowler2 = fowler' size2
fowler3 = fowler' size3
fowler4 = fowler' size4
fowler5 = fowler' size5

-- Flanger

flan' :: MixAt Sig Sig a => Sig -> a -> AtOut Sig Sig a
flan' size = at (flan size size size size)

flan1, flan2, flan3, flan4, flan5 :: MixAt Sig Sig a => a -> AtOut Sig Sig a

flan1 = flan' size1
flan2 = flan' size2
flan3 = flan' size3
flan4 = flan' size4
flan5 = flan' size5

-- Phaser

-- phasy :: RateSig -> DepthSig -> BaseCps -> Feedback -> Sig -> Sig

phasy' :: MixAt Sig Sig a => Sig -> a -> AtOut Sig Sig a
phasy' size = at (phasy size size size size)

phasy1, phasy2, phasy3, phasy4, phasy5 :: MixAt Sig Sig a => a -> AtOut Sig Sig a

phasy1 = phasy' size1
phasy2 = phasy' size2
phasy3 = phasy' size3
phasy4 = phasy' size4
phasy5 = phasy' size5

-- Chorus

chory' :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
chory' size = at (chory size size size)

chory1, chory2, chory3, chory4, chory5 :: MixAt Sig2 Sig2 a => a -> AtOut Sig2 Sig2 a

chory1 = chory' size1
chory2 = chory' size2
chory3 = chory' size3
chory4 = chory' size4
chory5 = chory' size5

-- Auto Pan

-- pany :: TremWaveSig -> DepthSig -> RateSig -> Sig2 -> Sig2

pany' :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pany' size = at (pany size size size)

pany1, pany2, pany3, pany4, pany5 :: MixAt Sig2 Sig2 a => a -> AtOut Sig2 Sig2 a

pany1 = pany' size1
pany2 = pany' size2
pany3 = pany' size3
pany4 = pany' size4
pany5 = pany' size5

-- Tremolo 

tremy' :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
tremy' size = at (tremy size size size)

tremy1, tremy2, tremy3, tremy4, tremy5 :: MixAt Sig2 Sig2 a => a -> AtOut Sig2 Sig2 a

tremy1 = tremy' size1
tremy2 = tremy' size2
tremy3 = tremy' size3
tremy4 = tremy' size4
tremy5 = tremy' size5

-- Ring modulation

ringo' :: MixAt Sig Sig a => Sig -> a -> AtOut Sig Sig a
ringo' size = at (ringo size size size)

ringo1, ringo2, ringo3, ringo4, ringo5 :: MixAt Sig Sig a => a -> AtOut Sig Sig a

ringo1 = ringo' size1
ringo2 = ringo' size2
ringo3 = ringo' size3
ringo4 = ringo' size4
ringo5 = ringo' size5

----------------------------------------------------------
-- UI 

setAll :: Double -> [String] -> [(String, Double)]
setAll size names = fmap (\s -> (s, size)) ["rate", "depth", "del time", "fbk"]


-- colors

tortColor = red  
fowlerColor = maroon
adeleColor = blue
pongColor = orange
flanColor = purple
revsyColor = lime
phasyColor = yellow
crusherColor = fuchsia
choryColor = navy
panyColor = aqua
tremyColor = green
ringoColor = maroon
reverbColor = olive 

paintTo = fxColor . C.sRGB24read

red = "#FF4136"
maroon = "#85144b"
blue = "#0074D9"
aqua = "#7FDBFF"
teal = "#39CCCC"
navy = "#001f3f"
orange = "#FF851B"
lime = "#01FF70"
green = "#2ECC40"
yellow = "#FFDC00"
purple = "#B10DC9"
fuchsia = "#F012BE"
olive = "#3D9970"

-- Analog Delay


uiAdeleBy :: Sigs a => Double -> Double -> Double -> Double -> Source (Fx a)
uiAdeleBy initTone initFeedback initBalance initDelayTime = mapSource bindSig $ paintTo adeleColor $ fxBox "Delay" fx True  [("balance", initBalance), ("del time", initDelayTime), ("fbk", initFeedback), ("tone", initTone)]
    where        
        fx [balance, delayTime, feedback, tone] = return . adele balance delayTime feedback tone

uiAdeleBy_ :: Sigs a => Double -> Double -> Double -> Source (Fx a)
uiAdeleBy_ = uiAdeleBy 0.5

uiAdele1, uiAdele2, uiAdele3, uiAdele4, uiAdele5 :: Sigs a => Double -> Double -> Source (Fx a)

uiAdele1 = uiAdeleBy_ size1
uiAdele2 = uiAdeleBy_ size2
uiAdele3 = uiAdeleBy_ size3
uiAdele4 = uiAdeleBy_ size4
uiAdele5 = uiAdeleBy_ size5

{-

uiAdeleByB :: Double -> Double -> Double -> Source FxFun
uiAdeleByB = uiAdeleBy 0.8

uiAdele1b, uiAdele2b, uiAdele3b, uiAdele4b, uiAdele5b :: Double -> Double -> Source FxFun

uiAdele1b = uiAdeleByB size1
uiAdele2b = uiAdeleByB size2
uiAdele3b = uiAdeleByB size3
uiAdele4b = uiAdeleByB size4
uiAdele5b = uiAdeleByB size5

uiAdeleByM :: Double -> Double -> Double -> Source FxFun
uiAdeleByM = uiAdeleBy 0.2

uiAdele1m, uiAdele2m, uiAdele3m, uiAdele4m, uiAdele5m :: Double -> Double -> Source FxFun

uiAdele1m = uiAdeleByM size1
uiAdele2m = uiAdeleByM size2
uiAdele3m = uiAdeleByM size3
uiAdele4m = uiAdeleByM size4
uiAdele5m = uiAdeleByM size5

-- Distortion

uiTortBy :: Double -> Double -> Source FxFun
uiTortBy initTone initDrive = paintTo tortColor $ fxBox "Distort" fx True [("drive", initDrive), ("tone", initTone)]
    where
        fx :: Sig -> Sig -> FxFun
        fx drive tone = fromMonoFx $ tort drive tone

uiTortBy_ :: Double -> Source FxFun
uiTortBy_ = uiTortBy 0.5

uiTort1, uiTort2, uiTort3, uiTort4, uiTort5 :: Source FxFun

uiTort1 = uiTortBy_ size1
uiTort2 = uiTortBy_ size2
uiTort3 = uiTortBy_ size3
uiTort4 = uiTortBy_ size4
uiTort5 = uiTortBy_ size5

uiTortByB :: Double -> Source FxFun
uiTortByB = uiTortBy 0.85

uiTort1b, uiTort2b, uiTort3b, uiTort4b, uiTort5b :: Source FxFun

uiTort1b = uiTortByB size1
uiTort2b = uiTortByB size2
uiTort3b = uiTortByB size3
uiTort4b = uiTortByB size4
uiTort5b = uiTortByB size5

uiTortByM :: Double -> Source FxFun
uiTortByM = uiTortBy 0.2

uiTort1m, uiTort2m, uiTort3m, uiTort4m, uiTort5m :: Source FxFun

uiTort1m = uiTortByM size1
uiTort2m = uiTortByM size2
uiTort3m = uiTortByM size3
uiTort4m = uiTortByM size4
uiTort5m = uiTortByM size5

-- Envelope follower

uiFowler' :: Source FxFun
uiFowler' = paintTo fowlerColor $ fxBox "Follower" fx True [("size", size1)]
    where
        fx :: Sig -> FxFun
        fx size = \asig2 -> return $ fowler' size asig2 

uiFowlerBy :: Double -> Source FxFun
uiFowlerBy size = paintTo fowlerColor $ fxBox "Follower" fx True [("sense", size), ("freq", size), ("reson", size)]
    where
        fx :: Sig -> Sig -> Sig -> FxFun
        fx sense freq resonance = fromMonoFx $ fowler sense freq resonance

uiFowler1, uiFowler2, uiFowler3, uiFowler4, uiFowler5 :: Source FxFun

uiFowler1 = uiFowlerBy size1
uiFowler2 = uiFowlerBy size2
uiFowler3 = uiFowlerBy size3
uiFowler4 = uiFowlerBy size4
uiFowler5 = uiFowlerBy size5

-- Flanger

uiFlan' :: Source FxFun
uiFlan' = paintTo flanColor $ fxBox "Flanger" fx True [("size", size1)]
    where
        fx :: Sig -> FxFun
        fx size = fromMonoFx $ flan' size

uiFlanBy :: Double -> Source FxFun
uiFlanBy size = paintTo flanColor $ fxBox "Flanger" fx True $ setAll size ["rate", "depth", "del time", "fbk"]
    where
        fx :: RateSig -> DepthSig -> DelayTime -> Feedback -> FxFun
        fx rate depth delayTime fbk = fromMonoFx $ flan rate depth delayTime fbk


uiFlan1, uiFlan2, uiFlan3, uiFlan4, uiFlan5 :: Source FxFun

uiFlan1 = uiFlanBy size1
uiFlan2 = uiFlanBy size2
uiFlan3 = uiFlanBy size3
uiFlan4 = uiFlanBy size4
uiFlan5 = uiFlanBy size5

-- Phaser

-- phasy :: RateSig -> DepthSig -> BaseCps -> Feedback -> Sig -> Sig

uiPhasy' :: Source FxFun
uiPhasy' = paintTo phasyColor $ fxBox "Phaser" fx  True $ [("size", size1)]
    where
        fx :: Sig -> FxFun
        fx = fromMonoFx . phasy' 

uiPhasyBy :: Double -> Source FxFun
uiPhasyBy size = paintTo phasyColor $ fxBox "Phaser" fx True $ setAll size ["rate", "depth", "cps", "fbk"]
    where
        fx :: RateSig -> DepthSig -> BaseCps -> Feedback -> FxFun
        fx rate depth cps fbk = fromMonoFx $ phasy rate depth cps fbk

uiPhasy1, uiPhasy2, uiPhasy3, uiPhasy4, uiPhasy5 :: Source FxFun

uiPhasy1 = uiPhasyBy size1
uiPhasy2 = uiPhasyBy size2
uiPhasy3 = uiPhasyBy size3
uiPhasy4 = uiPhasyBy size4
uiPhasy5 = uiPhasyBy size5

-- Chorus

uiChory' :: Source FxFun
uiChory' = paintTo choryColor $ fxBox "Chorus" fx True [("size", size1)]
    where 
        fx :: Sig -> FxFun
        fx size = return . chory' size

uiChoryBy :: Double -> Source FxFun
uiChoryBy size = paintTo choryColor $ fxBox "Chorus" fx True $ setAll size ["rate", "depth", "width"]
    where
        fx ::  RateSig -> DepthSig -> WidthSig -> FxFun
        fx rate depth width = return . chory rate depth width

uiChory1, uiChory2, uiChory3, uiChory4, uiChory5 :: Source FxFun

uiChory1 = uiChoryBy size1
uiChory2 = uiChoryBy size2
uiChory3 = uiChoryBy size3
uiChory4 = uiChoryBy size4
uiChory5 = uiChoryBy size5

-- Auto Pan

-- pany :: TremWaveSig -> DepthSig -> RateSig -> Sig2 -> Sig2

uiPany' :: Source FxFun
uiPany' = paintTo panyColor $ fxBox "Pan" fx True [("size", size1)]
    where
        fx :: Sig -> FxFun
        fx size = return . pany' size

uiPanyBy :: Double -> Source FxFun
uiPanyBy size = paintTo panyColor $ fxBox "Pan" fx True $ setAll size ["wave", "depth", "rate"]
    where 
        fx :: TremWaveSig -> DepthSig -> RateSig -> FxFun
        fx wave depth rate = return . pany wave depth rate

uiPany1, uiPany2, uiPany3, uiPany4, uiPany5 :: Source FxFun

uiPany1 = uiPanyBy size1
uiPany2 = uiPanyBy size2
uiPany3 = uiPanyBy size3
uiPany4 = uiPanyBy size4
uiPany5 = uiPanyBy size5

-- Tremolo 

uiTremy' :: Source FxFun
uiTremy' = paintTo tremyColor $ fxBox "Tremolo" fx True [("size", size1)]
    where
        fx :: Sig -> FxFun
        fx size = return . tremy' size

uiTremyBy :: Double -> Source FxFun
uiTremyBy size = paintTo tremyColor $ fxBox "Tremolo" fx True $ setAll size ["wave", "depth", "rate"]
    where 
        fx :: TremWaveSig -> DepthSig -> RateSig -> FxFun
        fx wave depth rate = return . tremy wave depth rate


uiTremy1, uiTremy2, uiTremy3, uiTremy4, uiTremy5 :: Source FxFun

uiTremy1 = uiTremyBy size1
uiTremy2 = uiTremyBy size2
uiTremy3 = uiTremyBy size3
uiTremy4 = uiTremyBy size4
uiTremy5 = uiTremyBy size5

-- Ring modulation

uiRingo' :: Source FxFun
uiRingo' = paintTo ringoColor $ fxBox "Ringo" fx True [("size", size1)]
    where
        fx :: Sig -> FxFun
        fx size = return . ringo' size

uiRingoBy :: Double -> Source FxFun
uiRingoBy size = paintTo ringoColor $ fxBox "Ring Mod" fx True $ setAll size ["mix", "rate", "env mod"]
    where 
        fx :: Balance -> RateSig -> EnvelopeModSig -> FxFun
        fx balance rate envMod = fromMonoFx $ ringo balance rate envMod

uiRingo1, uiRingo2, uiRingo3, uiRingo4, uiRingo5 :: Source FxFun

uiRingo1 = uiRingoBy size1
uiRingo2 = uiRingoBy size2
uiRingo3 = uiRingoBy size3
uiRingo4 = uiRingoBy size4
uiRingo5 = uiRingoBy size5

-- Crusher

uiCrusher :: Double -> Double -> Source FxFun
uiCrusher initReduction initFoldover = paintTo revsyColor $ fxBox "LoFi" fx True [("redux", initReduction), ("foldover", initFoldover)]
    where
        fx :: BitsReductionSig -> FoldoverSig -> FxFun
        fx redux foldover = fromMonoFx $ crusher redux foldover

-- Reverse

uiRevsy :: Double -> Source FxFun
uiRevsy initTime = paintTo revsyColor $ fxBox "Reverse" fx True [("time", initTime)]
    where
        fx :: TimeSig -> FxFun
        fx time = fromMonoFx $ revsy time

-- Reverbs 

uiRevBy :: Double -> Double -> Source FxFun
uiRevBy initFeedback initMix = paintTo reverbColor $ fxBox "Reverb" fx True [("mix", initMix), ("fbk", initFeedback)]
    where
        fx :: Balance -> Feedback -> FxFun
        fx balance feedback = \asig2 -> return $ mixAt balance (rever2 feedback) asig2

uiRoom :: Double -> Source FxFun
uiRoom = uiRevBy 0.6 

uiRoom1, uiRoom2, uiRoom3, uiRoom4, uiRoom5 :: Source FxFun

uiRoom1 = uiRoom size1 
uiRoom2 = uiRoom size2 
uiRoom3 = uiRoom size3 
uiRoom4 = uiRoom size4 
uiRoom5 = uiRoom size5 

uiChamber :: Double -> Source FxFun
uiChamber = uiRevBy 0.8

uiChamber1, uiChamber2, uiChamber3, uiChamber4, uiChamber5 :: Source FxFun

uiChamber1 = uiChamber size1
uiChamber2 = uiChamber size2
uiChamber3 = uiChamber size3
uiChamber4 = uiChamber size4
uiChamber5 = uiChamber size5

uiHall :: Double -> Source FxFun
uiHall = uiRevBy 0.9

uiHall1, uiHall2, uiHall3, uiHall4, uiHall5 :: Source FxFun

uiHall1 = uiHall size1
uiHall2 = uiHall size2
uiHall3 = uiHall size3
uiHall4 = uiHall size4
uiHall5 = uiHall size5

uiCave :: Double -> Source FxFun
uiCave = uiRevBy 0.99

uiCave1 = uiCave size1
uiCave2 = uiCave size2
uiCave3 = uiCave size3
uiCave4 = uiCave size4
uiCave5 = uiCave size5

-- Ping Pong delay

-- | Ping-pong delay
--
-- > uiPongy feedback delayTime
uiPongy' :: Double -> Double -> Source FxFun
uiPongy' initFeedback initDelayTime = paintTo pongColor $ fxBox "Pong" fx True [("mix", initBalance), ("fbk", initFeedback), ("del time", initDelayTime)]
    where
        initBalance = initFeedback / 2.1 

        fx :: Balance -> Feedback -> DelayTime -> FxFun
        fx balance feedback delTime = pingPong delTime feedback balance
   
uiPongy1 = uiPongy' size1
uiPongy2 = uiPongy' size2
uiPongy3 = uiPongy' size3
uiPongy4 = uiPongy' size4
uiPongy5 = uiPongy' size5

-}