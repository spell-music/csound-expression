{-# Language FlexibleContexts #-}

-- | A friendly family of effects. These functions are kindly provided by Iain McCurdy (encoded in Csound).
module Csound.Air.Fx.FxBox(
    adele, tort, fowler, revsy, flan, phasy, crusher, chory, pany, tremy, ringo, EnvelopeModSig,

    -- * Presets
    -- | For all presets we have 5 levels of strength. They are signified by numbers from 1 to 5. Also for some effects (delay and distortion)
    -- we have miscellaneous way to alter preset by suffix b (means bright) and m (means muffled). It alters the one color of the effect.

    -- ** Analog Delay
    adele1, adele2, adele3, adele4, adele5,
    -- *** Bright
    adele1b, adele2b, adele3b, adele4b, adele5b,
    -- *** Muted
    adele1m, adele2m, adele3m, adele4m, adele5m,

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
) where

import Csound.Typed
import Csound.SigSpace

import qualified Csound.Typed.Plugins as P(pitchShifterDelay,
    fxAnalogDelay, fxDistortion, fxEnvelopeFollower, fxFlanger, fxFreqShifter, fxLoFi, 
    fxPanTrem, fxPhaser, fxPitchShifter, fxReverse, fxRingModulator, fxChorus2)

import Csound.Air.Fx(Balance, DelayTime, Feedback, ToneSig, SensitivitySig, 
    BaseCps, Resonance, DepthSig, RateSig, TremWaveSig, FoldoverSig, BitsReductionSig, 
    DriveSig, TimeSig, WidthSig)

-- | Analog Delay line with low-pass filter in the feedback chain.
-- The filter adds natural decay to the echoes.
--
-- > adele mixRatio delayTime feedback toneRatio ain
--
-- Note that the center frequency of the filter is measured in normalized units (form 0  to 1).
adele :: Balance -> DelayTime -> Feedback -> ToneSig -> Sig -> Sig
adele kmix kdelay kfback ktone ain = P.fxAnalogDelay kmix kdelay kfback ktone ain

size1 = 0.1
size2 = 0.3
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
-- ; @resonance          @ --  resonance of the lowpass filter (suggested range: 0 to 1)
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
-- * @freq  @ --  centre frequency of the phase shifting effect in octaves (suggested range 6 to 11)
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

-- RingModulator
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
-- s, m, l, x  b, m

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

