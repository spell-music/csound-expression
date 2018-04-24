{-# Language FlexibleContexts #-}

-- | A friendly family of effects. These functions are kindly provided by Iain McCurdy (designed in Csound).
module Csound.Air.Fx.FxBox(
    adele, pongy, tort, fowler, revsy, flan, phasy, crusher, chory, pany, oscPany, triPany, sqrPany, tremy, oscTremy, triTremy, sqrTremy, ringo, EnvelopeModSig,
    magnus,

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
    pongy1, pongy2, pongy3, pongy4, pongy5,

    -- *** Bright
    pongy1b, pongy2b, pongy3b, pongy4b, pongy5b,

    -- *** Muted
    pongy1m, pongy2m, pongy3m, pongy4m, pongy5m,

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
    oscPany',
    oscPany1, oscPany2, oscPany3, oscPany4, oscPany5,

    triPany',
    triPany1, triPany2, triPany3, triPany4, triPany5,

    sqrPany',
    sqrPany1, sqrPany2, sqrPany3, sqrPany4, sqrPany5,

    -- ** Tremolo
    oscTremy',
    oscTremy1, oscTremy2, oscTremy3, oscTremy4, oscTremy5,

    triTremy',
    triTremy1, triTremy2, triTremy3, triTremy4, triTremy5,

    sqrTremy',
    sqrTremy1, sqrTremy2, sqrTremy3, sqrTremy4, sqrTremy5,

    -- ** Ring modulation
    ringo',
    ringo1, ringo2, ringo3, ringo4, ringo5,

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
    uiRoom, uiRoom1, uiRoom2, uiRoom3, uiRoom4, uiRoom5,

    -- ** Chambers
    uiChamber, uiChamber1, uiChamber2, uiChamber3, uiChamber4, uiChamber5,

    -- *** Halls
    uiHall, uiHall1, uiHall2, uiHall3, uiHall4, uiHall5,

    -- *** Caves
    uiCave, uiCave1, uiCave2, uiCave3, uiCave4, uiCave5,

    -- ** Mono Reverb

    -- *** Rooms
    uiMonoRoom, uiRoom1m, uiRoom2m, uiRoom3m, uiRoom4m, uiRoom5m,

    -- ** Chambers
    uiMonoChamber, uiChamber1m, uiChamber2m, uiChamber3m, uiChamber4m, uiChamber5m,

    -- *** Halls
    uiMonoHall, uiHall1m, uiHall2m, uiHall3m, uiHall4m, uiHall5m,

    -- *** Caves
    uiMonoCave, uiCave1m, uiCave2m, uiCave3m, uiCave4m, uiCave5m,

    -- ** Delay
    uiAdele1, uiAdele2, uiAdele3, uiAdele4, uiAdele5,
    uiAdele1b, uiAdele2b, uiAdele3b, uiAdele4b, uiAdele5b,
    uiAdele1m, uiAdele2m, uiAdele3m, uiAdele4m, uiAdele5m,

    -- ** Tape echo
    uiMagnus,

    -- ** Ping Pong Delay
    uiPongy1, uiPongy2, uiPongy3, uiPongy4, uiPongy5,
    uiPongy1b, uiPongy2b, uiPongy3b, uiPongy4b, uiPongy5b,
    uiPongy1m, uiPongy2m, uiPongy3m, uiPongy4m, uiPongy5m,

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
    uiOscPany', uiTriPany', uiSqrPany',
    uiOscPany1, uiOscPany2, uiOscPany3, uiOscPany4, uiOscPany5,
    uiTriPany1, uiTriPany2, uiTriPany3, uiTriPany4, uiTriPany5,
    uiSqrPany1, uiSqrPany2, uiSqrPany3, uiSqrPany4, uiSqrPany5,

    -- ** Tremolo
    uiOscTremy', uiTriTremy', uiSqrTremy',
    uiOscTremy1, uiOscTremy2, uiOscTremy3, uiOscTremy4, uiOscTremy5,
    uiTriTremy1, uiTriTremy2, uiTriTremy3, uiTriTremy4, uiTriTremy5,
    uiSqrTremy1, uiSqrTremy2, uiSqrTremy3, uiSqrTremy4, uiSqrTremy5,

    -- ** Reverse
    uiRevsy,

    -- ** LoFi
    uiCrusher,

    -- ** Ring modulation
    uiRingo', uiRingo1, uiRingo2, uiRingo3, uiRingo4, uiRingo5,

    -- ** Compressor
    -- | TODO


    -- * Misc

    -- ** Ambi guitar
    ambiGuitar
) where

import Data.Default

import Csound.Typed
import Csound.Typed.Opcode(ampdb, scale, expcurve, compress)
import Csound.Typed.Gui

import Csound.SigSpace

import qualified Csound.Typed.Plugins as P(pitchShifterDelay,
    fxAnalogDelay, fxDistortion, fxEnvelopeFollower, fxFlanger, fxFreqShifter, fxLoFi,
    fxPanTrem, fxMonoTrem, fxPhaser, fxPitchShifter, fxReverse, fxRingModulator, fxChorus2)

import Csound.Air.Patch(Fx, Fx1, Fx2)
import Csound.Air.Fx(Balance, DelayTime, Feedback, ToneSig, SensitivitySig,
    BaseCps, Resonance, DepthSig, RateSig, TremWaveSig, FoldoverSig, BitsReductionSig,
    DriveSig, TimeSig, WidthSig,
    rever2, pingPong, pingPong', PingPongSpec(..),
    EchoGain, RandomSpreadSig,
    tapeEcho)

import Csound.Air.Live(fxBox, fxColor)
import Csound.Air.Wav(toMono)
import Csound.Air.Misc(fromMono, ambiEnv, saturator)

import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C


-- | Analog Delay line with low-pass filter in the feedback chain.
-- The filter adds natural decay to the echoes.
--
-- > adele mixRatio delayTime feedback toneRatio ain
--
-- Note that the center frequency of the filter is measured in normalized units (form 0  to 1).
adele :: Sigs a => Balance -> DelayTime -> Feedback -> ToneSig -> a -> a
adele kmix kdelay kfback ktone = mapSig $ P.fxAnalogDelay kmix kdelay kfback ktone

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
tort :: Sigs a => DriveSig -> ToneSig -> a -> a
tort kdrive ktone = mapSig $ P.fxDistortion 1 kdrive ktone

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
fowler :: Sigs a => SensitivitySig -> BaseCps -> Resonance -> a -> a
fowler ksens kbaseFreq kreson = mapSig $ P.fxEnvelopeFollower ksens kbaseFreq (0.99 * kreson)

-- | An effect that reverses an audio stream in chunks
--
-- > revsy time
--
-- @time@ -- the size of the chunck in seconds.
revsy :: Sigs a => TimeSig -> a -> a
revsy ktime = mapSig $ P.fxReverse ktime


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
flan :: Sigs a => RateSig -> DepthSig -> DelayTime -> Feedback -> a -> a
flan krate kdepth kdelay kfback = mapSig $ P.fxFlanger krate kdepth kdelay kfback


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
phasy :: Sigs a => RateSig -> DepthSig -> BaseCps -> Feedback -> a -> a
phasy krate kdepth cps kfback = mapSig $ P.fxPhaser krate kdepth (6 + 5 * cps) kfback

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
crusher :: Sigs a => BitsReductionSig -> FoldoverSig -> a -> a
crusher kbits kfold = mapSig $ P.fxLoFi (0.6 * kbits) kfold

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

-- | Sine auto pan
--
-- > oscPany = pany 0
oscPany ::DepthSig -> RateSig -> Sig2 -> Sig2
oscPany = pany 0

-- | Triangle auto pan
--
-- > triPany = pany 1
triPany ::DepthSig -> RateSig -> Sig2 -> Sig2
triPany = pany 1

-- | Square auto pan
--
-- > sqrPany = pany 2
sqrPany ::DepthSig -> RateSig -> Sig2 -> Sig2
sqrPany = pany 2


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
tremy :: Sigs a => TremWaveSig -> DepthSig -> RateSig -> a -> a
tremy tremWave kdepth krate = mapSig $ P.fxMonoTrem kdepth krate tremWave

-- | Sine tremolo
--
-- > oscTremy = tremy 0
oscTremy :: Sigs a => DepthSig -> RateSig -> a -> a
oscTremy = tremy 0

-- | Triangle tremolo
--
-- > triTremy = tremy 1
triTremy :: Sigs a => DepthSig -> RateSig -> a -> a
triTremy = tremy 1

-- | Square tremolo
--
-- > sqrTremy = tremy 2
sqrTremy :: Sigs a => DepthSig -> RateSig -> a -> a
sqrTremy = tremy 2

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
ringo :: Sigs a => Balance -> RateSig -> EnvelopeModSig -> a -> a
ringo balance rate envelopeMod = mapSig $ \ain -> P.fxRingModulator ain balance rate envelopeMod

---------------------------------------------------------------------
-- Presets

-- Analog Delay

adeleBy :: Sigs a => ToneSig -> Feedback -> Balance -> DelayTime -> a -> a
adeleBy tone size balance delTime = adele balance delTime size tone

adeleBy_ :: Sigs a => Feedback -> Balance -> DelayTime -> a -> a
adeleBy_ = adeleBy 0.5

adele1, adele2, adele3, adele4, adele5 :: Sigs a => Balance -> DelayTime -> a -> a

adele1 = adeleBy_ size1
adele2 = adeleBy_ size2
adele3 = adeleBy_ size3
adele4 = adeleBy_ size4
adele5 = adeleBy_ size5

adeleByB :: Sigs a => Feedback -> Balance -> DelayTime -> a -> a
adeleByB = adeleBy 0.8

adele1b, adele2b, adele3b, adele4b, adele5b :: Sigs a => Balance -> DelayTime -> a -> a

adele1b = adeleByB size1
adele2b = adeleByB size2
adele3b = adeleByB size3
adele4b = adeleByB size4
adele5b = adeleByB size5

adeleByM :: Sigs a => Feedback -> Balance -> DelayTime -> a -> a
adeleByM = adeleBy 0.2

adele1m, adele2m, adele3m, adele4m, adele5m :: Sigs a => Balance -> DelayTime -> a -> a

adele1m = adeleByM size1
adele2m = adeleByM size2
adele3m = adeleByM size3
adele4m = adeleByM size4
adele5m = adeleByM size5

-- | magnus - simulates magnetic tape echo/delay
--
-- > magnus size feedback echoGain tone randomSpread ain
--
-- * size - how many heads in the tape
-- * feedback
-- * echo gain
-- * tone - normalized center frequency of the filter (0  to 1)
-- * randomSpread - quality of the tape (the higher - the worser)
magnus :: Sigs a => D -> DelayTime -> Feedback -> EchoGain -> ToneSig -> RandomSpreadSig -> a -> a
magnus size delt fb echoGain ktone randomSpread = mapSig (tapeEcho size delt fb echoGain ktone randomSpread)

-- Ping Pong delay

-- | Ping-pong delay
--
-- > pongy kmix delayTime feedback tone ain
pongy ::  Sig2s a => Balance -> DelayTime -> Feedback -> ToneSig -> WidthSig -> a -> a
pongy balance delTime fbk tone width = mapSig2 (pingPong' (def { pingPongDamp = absTone, pingPongWidth = width }) delTime fbk balance)
    where absTone = scale (expcurve tone 4) 12000 100

pongyBy :: Sig2s a => ToneSig -> Feedback -> Balance -> DelayTime -> WidthSig -> a -> a
pongyBy tone size balance delTime width = pongy balance delTime size tone width

pongyBy_ :: Sig2s a => Feedback -> Balance -> DelayTime -> WidthSig -> a -> a
pongyBy_ = pongyBy 0.5

pongy1, pongy2, pongy3, pongy4, pongy5 :: Sig2s a => Balance -> DelayTime -> WidthSig -> a -> a

pongy1 = pongyBy_ size1
pongy2 = pongyBy_ size2
pongy3 = pongyBy_ size3
pongy4 = pongyBy_ size4
pongy5 = pongyBy_ size5

pongyByB :: Sig2s a => Feedback -> Balance -> DelayTime -> WidthSig -> a -> a
pongyByB = pongyBy 0.8

pongy1b, pongy2b, pongy3b, pongy4b, pongy5b :: Sig2s a => Balance -> DelayTime -> WidthSig -> a -> a

pongy1b = pongyByB size1
pongy2b = pongyByB size2
pongy3b = pongyByB size3
pongy4b = pongyByB size4
pongy5b = pongyByB size5

pongyByM :: Sig2s a => Feedback -> Balance -> DelayTime -> WidthSig -> a -> a
pongyByM = pongyBy 0.2

pongy1m, pongy2m, pongy3m, pongy4m, pongy5m :: Sig2s a => Balance -> DelayTime -> WidthSig -> a -> a

pongy1m = pongyByM size1
pongy2m = pongyByM size2
pongy3m = pongyByM size3
pongy4m = pongyByM size4
pongy5m = pongyByM size5

-- Distortion

tortBy :: Sigs a => ToneSig -> DriveSig -> a -> a
tortBy tone drive = tort drive tone

tortBy_ :: Sigs a => DriveSig -> a -> a
tortBy_ = tortBy 0.5

tort1, tort2, tort3, tort4, tort5 :: Sigs a => a -> a

tort1 = tortBy_ size1
tort2 = tortBy_ size2
tort3 = tortBy_ size3
tort4 = tortBy_ size4
tort5 = tortBy_ size5

tortByB :: Sigs a => DriveSig -> a -> a
tortByB = tortBy 0.85

tort1b, tort2b, tort3b, tort4b, tort5b :: Sigs a => a -> a

tort1b = tortByB size1
tort2b = tortByB size2
tort3b = tortByB size3
tort4b = tortByB size4
tort5b = tortByB size5

tortByM :: Sigs a => DriveSig -> a -> a
tortByM = tortBy 0.2

tort1m, tort2m, tort3m, tort4m, tort5m :: Sigs a => a -> a

tort1m = tortByM size1
tort2m = tortByM size2
tort3m = tortByM size3
tort4m = tortByM size4
tort5m = tortByM size5

-- Envelope follower

fowler' :: Sigs a => Sig -> a -> a
fowler' size = fowler size size size

fowler1, fowler2, fowler3, fowler4, fowler5 :: Sigs a => a -> a

fowler1 = fowler' size1
fowler2 = fowler' size2
fowler3 = fowler' size3
fowler4 = fowler' size4
fowler5 = fowler' size5

-- Flanger

flan' :: Sigs a => Sig -> a -> a
flan' size = flan size size size size

flan1, flan2, flan3, flan4, flan5 :: Sigs a => a -> a

flan1 = flan' size1
flan2 = flan' size2
flan3 = flan' size3
flan4 = flan' size4
flan5 = flan' size5

-- Phaser

-- phasy :: RateSig -> DepthSig -> BaseCps -> Feedback -> Sig -> Sig

phasy' :: Sigs a => Sig -> a -> a
phasy' size = phasy size size size size

phasy1, phasy2, phasy3, phasy4, phasy5 :: Sigs a => a -> a

phasy1 = phasy' size1
phasy2 = phasy' size2
phasy3 = phasy' size3
phasy4 = phasy' size4
phasy5 = phasy' size5

-- Chorus

chory' :: Sig2s a => Sig -> a -> a
chory' size = mapSig2 (chory size size size)

chory1, chory2, chory3, chory4, chory5 :: Sig2s a => a -> a

chory1 = chory' size1
chory2 = chory' size2
chory3 = chory' size3
chory4 = chory' size4
chory5 = chory' size5

-- Auto Pan

-- pany :: TremWaveSig -> DepthSig -> RateSig -> Sig2 -> Sig2

oscPany' :: Sig2s a => Sig -> a -> a
oscPany' size = mapSig2 (oscPany size size)

oscPany1, oscPany2, oscPany3, oscPany4, oscPany5 :: Sig2s a => a -> a

oscPany1 = oscPany' size1
oscPany2 = oscPany' size2
oscPany3 = oscPany' size3
oscPany4 = oscPany' size4
oscPany5 = oscPany' size5

triPany' :: Sig2s a => Sig -> a -> a
triPany' size = mapSig2 (triPany size size)

triPany1, triPany2, triPany3, triPany4, triPany5 :: Sig2s a => a -> a

triPany1 = triPany' size1
triPany2 = triPany' size2
triPany3 = triPany' size3
triPany4 = triPany' size4
triPany5 = triPany' size5

sqrPany' :: Sig2s a => Sig -> a -> a
sqrPany' size = mapSig2 (sqrPany size size)

sqrPany1, sqrPany2, sqrPany3, sqrPany4, sqrPany5 :: Sig2s a => a -> a

sqrPany1 = sqrPany' size1
sqrPany2 = sqrPany' size2
sqrPany3 = sqrPany' size3
sqrPany4 = sqrPany' size4
sqrPany5 = sqrPany' size5

-- Tremolo

oscTremy' :: Sigs a => Sig -> a -> a
oscTremy' size = oscTremy size size

oscTremy1, oscTremy2, oscTremy3, oscTremy4, oscTremy5 :: Sigs a => a -> a

oscTremy1 = oscTremy' size1
oscTremy2 = oscTremy' size2
oscTremy3 = oscTremy' size3
oscTremy4 = oscTremy' size4
oscTremy5 = oscTremy' size5

triTremy' :: Sigs a => Sig -> a -> a
triTremy' size = triTremy size size

triTremy1, triTremy2, triTremy3, triTremy4, triTremy5 :: Sigs a => a -> a

triTremy1 = triTremy' size1
triTremy2 = triTremy' size2
triTremy3 = triTremy' size3
triTremy4 = triTremy' size4
triTremy5 = triTremy' size5


sqrTremy' :: Sigs a => Sig -> a -> a
sqrTremy' size = sqrTremy size size

sqrTremy1, sqrTremy2, sqrTremy3, sqrTremy4, sqrTremy5 :: Sigs a => a -> a

sqrTremy1 = sqrTremy' size1
sqrTremy2 = sqrTremy' size2
sqrTremy3 = sqrTremy' size3
sqrTremy4 = sqrTremy' size4
sqrTremy5 = sqrTremy' size5

-- Ring modulation

ringo' :: Sigs a => Sig -> a -> a
ringo' size = ringo size size size

ringo1, ringo2, ringo3, ringo4, ringo5 :: Sigs a => a -> a

ringo1 = ringo' size1
ringo2 = ringo' size2
ringo3 = ringo' size3
ringo4 = ringo' size4
ringo5 = ringo' size5

----------------------------------------------------------
-- UI

setAll :: Double -> [String] -> [(String, Double)]
setAll size names = fmap (\s -> (s, size)) names


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

uiAdeleByB :: Sigs a => Double -> Double -> Double -> Source (Fx a)
uiAdeleByB = uiAdeleBy 0.8

uiAdele1b, uiAdele2b, uiAdele3b, uiAdele4b, uiAdele5b :: Sigs a => Double -> Double -> Source (Fx a)

uiAdele1b = uiAdeleByB size1
uiAdele2b = uiAdeleByB size2
uiAdele3b = uiAdeleByB size3
uiAdele4b = uiAdeleByB size4
uiAdele5b = uiAdeleByB size5

uiAdeleByM :: Sigs a => Double -> Double -> Double -> Source (Fx a)
uiAdeleByM = uiAdeleBy 0.2

uiAdele1m, uiAdele2m, uiAdele3m, uiAdele4m, uiAdele5m :: Sigs a => Double -> Double -> Source (Fx a)

uiAdele1m = uiAdeleByM size1
uiAdele2m = uiAdeleByM size2
uiAdele3m = uiAdeleByM size3
uiAdele4m = uiAdeleByM size4
uiAdele5m = uiAdeleByM size5

-- Tape echo

uiMagnus :: Sigs a => Int -> Double -> Double -> Double -> Double -> Double -> Source (Fx a)
uiMagnus size initDelayTime initFeedback initEchoGain initTone initSpread  = mapSource bindSig $ paintTo adeleColor $ fxBox "Tape echo" fx True [("del time", initDelayTime), ("fbk", initFeedback), ("echo gain", initEchoGain), ("tone", initTone), ("tape qty", initSpread)]
    where
        fx [delayTime, feedback, echoGain, tone, spread] = return . magnus (int size) delayTime feedback echoGain tone spread

-- Ping-pong delay

uiPongyBy :: Sigs a => Double -> Double -> Double -> Double -> Double -> Source (Fx a)
uiPongyBy initTone initWidth initFeedback initBalance initDelayTime = mapSource bindSig $ paintTo adeleColor $ fxBox "Ping-pong" fx True  [("balance", initBalance), ("del time", initDelayTime), ("fbk", initFeedback), ("tone", initTone), ("width", initWidth)]
    where
        fx [balance, delayTime, feedback, tone, width] = return . pongy balance delayTime feedback tone width

defWidth = 0.7

uiPongyBy_ :: Sigs a => Double -> Double -> Double -> Source (Fx a)
uiPongyBy_ = uiPongyBy 0.5 defWidth

uiPongy1, uiPongy2, uiPongy3, uiPongy4, uiPongy5 :: Sigs a => Double -> Double -> Source (Fx a)

uiPongy1 = uiPongyBy_ size1
uiPongy2 = uiPongyBy_ size2
uiPongy3 = uiPongyBy_ size3
uiPongy4 = uiPongyBy_ size4
uiPongy5 = uiPongyBy_ size5

uiPongyByB :: Sigs a => Double -> Double -> Double -> Source (Fx a)
uiPongyByB = uiPongyBy 0.8 defWidth

uiPongy1b, uiPongy2b, uiPongy3b, uiPongy4b, uiPongy5b :: Sigs a => Double -> Double -> Source (Fx a)

uiPongy1b = uiPongyByB size1
uiPongy2b = uiPongyByB size2
uiPongy3b = uiPongyByB size3
uiPongy4b = uiPongyByB size4
uiPongy5b = uiPongyByB size5

uiPongyByM :: Sigs a => Double -> Double -> Double -> Source (Fx a)
uiPongyByM = uiPongyBy 0.2 defWidth

uiPongy1m, uiPongy2m, uiPongy3m, uiPongy4m, uiPongy5m :: Sigs a => Double -> Double -> Source (Fx a)

uiPongy1m = uiPongyByM size1
uiPongy2m = uiPongyByM size2
uiPongy3m = uiPongyByM size3
uiPongy4m = uiPongyByM size4
uiPongy5m = uiPongyByM size5

-- Distortion

uiTortBy :: Sigs a => Double -> Double -> Source (Fx a)
uiTortBy initTone initDrive = mapSource bindSig $ paintTo tortColor $ fxBox "Distort" fx True [("drive", initDrive), ("tone", initTone)]
    where
        fx [drive, tone] = return . tort drive tone

uiTortBy_ :: Sigs a => Double -> Source (Fx a)
uiTortBy_ = uiTortBy 0.5

uiTort1, uiTort2, uiTort3, uiTort4, uiTort5 :: Sigs a => Source (Fx a)

uiTort1 = uiTortBy_ size1
uiTort2 = uiTortBy_ size2
uiTort3 = uiTortBy_ size3
uiTort4 = uiTortBy_ size4
uiTort5 = uiTortBy_ size5

uiTortByB :: Sigs a => Double -> Source (Fx a)
uiTortByB = uiTortBy 0.85

uiTort1b, uiTort2b, uiTort3b, uiTort4b, uiTort5b :: Sigs a => Source (Fx a)

uiTort1b = uiTortByB size1
uiTort2b = uiTortByB size2
uiTort3b = uiTortByB size3
uiTort4b = uiTortByB size4
uiTort5b = uiTortByB size5

uiTortByM :: Sigs a => Double -> Source (Fx a)
uiTortByM = uiTortBy 0.2

uiTort1m, uiTort2m, uiTort3m, uiTort4m, uiTort5m :: Sigs a => Source (Fx a)

uiTort1m = uiTortByM size1
uiTort2m = uiTortByM size2
uiTort3m = uiTortByM size3
uiTort4m = uiTortByM size4
uiTort5m = uiTortByM size5

-- Envelope follower

uiFowler' :: Sigs a => Source (Fx a)
uiFowler' = mapSource bindSig $ paintTo fowlerColor $ fxBox "Follower" fx True [("size", size1)]
    where
        fx [size] = return . fowler' size

uiFowlerBy :: Sigs a => Double -> Source (Fx a)
uiFowlerBy size = mapSource bindSig $ paintTo fowlerColor $ fxBox "Follower" fx True [("sense", size), ("freq", size), ("reson", size)]
    where
        fx [sense, freq, resonance] = return . fowler sense freq resonance

uiFowler1, uiFowler2, uiFowler3, uiFowler4, uiFowler5 :: Sigs a => Source (Fx a)

uiFowler1 = uiFowlerBy size1
uiFowler2 = uiFowlerBy size2
uiFowler3 = uiFowlerBy size3
uiFowler4 = uiFowlerBy size4
uiFowler5 = uiFowlerBy size5

-- Flanger

uiFlan' :: Sigs a => Source (Fx a)
uiFlan' = mapSource bindSig $ paintTo flanColor $ fxBox "Flanger" fx True [("size", size1)]
    where
        fx [size] = return . flan' size

uiFlanBy :: Sigs a => Double -> Source (Fx a)
uiFlanBy size = mapSource bindSig $ paintTo flanColor $ fxBox "Flanger" fx True $ setAll size ["rate", "depth", "del time", "fbk"]
    where
        fx [rate, depth, delayTime, fbk] = return . flan rate depth delayTime fbk

uiFlan1, uiFlan2, uiFlan3, uiFlan4, uiFlan5 :: Sigs a => Source (Fx a)

uiFlan1 = uiFlanBy size1
uiFlan2 = uiFlanBy size2
uiFlan3 = uiFlanBy size3
uiFlan4 = uiFlanBy size4
uiFlan5 = uiFlanBy size5

-- Phaser

-- phasy :: RateSig -> DepthSig -> BaseCps -> Feedback -> Sig -> Sig

uiPhasy' :: Sigs a => Source (Fx a)
uiPhasy' = mapSource bindSig $ paintTo phasyColor $ fxBox "Phaser" fx  True $ [("size", size1)]
    where
        fx [x] = return . phasy' x

uiPhasyBy :: Sigs a => Double -> Source (Fx a)
uiPhasyBy size = mapSource bindSig $ paintTo phasyColor $ fxBox "Phaser" fx True $ setAll size ["rate", "depth", "cps", "fbk"]
    where
        fx [rate, depth, cps, fbk] = return . phasy rate depth cps fbk

uiPhasy1, uiPhasy2, uiPhasy3, uiPhasy4, uiPhasy5 :: Sigs a => Source (Fx a)

uiPhasy1 = uiPhasyBy size1
uiPhasy2 = uiPhasyBy size2
uiPhasy3 = uiPhasyBy size3
uiPhasy4 = uiPhasyBy size4
uiPhasy5 = uiPhasyBy size5

-- Chorus

uiChory' :: Sig2s a => Source (Fx a)
uiChory' = paintTo choryColor $ fxBox "Chorus" fx True [("size", size1)]
    where
        fx [size] = return . chory' size

uiChoryBy :: Sig2s a => Double -> Source (Fx a)
uiChoryBy size = paintTo choryColor $ fxBox "Chorus" fx True $ setAll size ["rate", "depth", "width"]
    where
        fx [rate, depth, width] = return . mapSig2 (chory rate depth width)

uiChory1, uiChory2, uiChory3, uiChory4, uiChory5 :: Sig2s a => Source (Fx a)

uiChory1 = uiChoryBy size1
uiChory2 = uiChoryBy size2
uiChory3 = uiChoryBy size3
uiChory4 = uiChoryBy size4
uiChory5 = uiChoryBy size5

-- Auto Pan

-- pany :: TremWaveSig -> DepthSig -> RateSig -> Sig2 -> Sig2

genUiPany' :: (Sig -> Sig2 -> Sig2) -> Source Fx2
genUiPany' mkPany = paintTo panyColor $ fxBox "Pan" fx True [("size", size1)]
    where
        fx [size] = return . mkPany size

uiOscPany', uiTriPany', uiSqrPany' :: Source Fx2

uiOscPany' = genUiPany' oscPany'
uiTriPany' = genUiPany' triPany'
uiSqrPany' = genUiPany' sqrPany'

genUiPanyBy :: Sig -> Double -> Source Fx2
genUiPanyBy wave size = paintTo panyColor $ fxBox "Pan" fx True $ setAll size ["depth", "rate"]
    where
        fx [depth, rate] = return . pany wave depth rate

uiOscPanyBy, uiTriPanyBy, uiSqrPanyBy :: Double -> Source Fx2

uiOscPanyBy = genUiPanyBy 0
uiTriPanyBy = genUiPanyBy 1
uiSqrPanyBy = genUiPanyBy 2

uiOscPany1, uiOscPany2, uiOscPany3, uiOscPany4, uiOscPany5,
    uiTriPany1, uiTriPany2, uiTriPany3, uiTriPany4, uiTriPany5,
    uiSqrPany1, uiSqrPany2, uiSqrPany3, uiSqrPany4, uiSqrPany5 :: Source Fx2

uiOscPany1 = uiOscPanyBy size1
uiOscPany2 = uiOscPanyBy size2
uiOscPany3 = uiOscPanyBy size3
uiOscPany4 = uiOscPanyBy size4
uiOscPany5 = uiOscPanyBy size5

uiTriPany1 = uiTriPanyBy size1
uiTriPany2 = uiTriPanyBy size2
uiTriPany3 = uiTriPanyBy size3
uiTriPany4 = uiTriPanyBy size4
uiTriPany5 = uiTriPanyBy size5

uiSqrPany1 = uiSqrPanyBy size1
uiSqrPany2 = uiSqrPanyBy size2
uiSqrPany3 = uiSqrPanyBy size3
uiSqrPany4 = uiSqrPanyBy size4
uiSqrPany5 = uiSqrPanyBy size5

-- Tremolo

genUiTremy' :: Sigs a => (Sig -> Sig -> Sig) -> Source (Fx a)
genUiTremy' mkTremy = mapSource bindSig $ paintTo tremyColor $ fxBox "Tremolo" fx True [("size", size1)]
    where
        fx [size] = return . mkTremy size

uiOscTremy', uiTriTremy', uiSqrTremy' :: Sigs a => Source (Fx a)

uiOscTremy' = genUiTremy' oscTremy'
uiTriTremy' = genUiTremy' triTremy'
uiSqrTremy' = genUiTremy' sqrTremy'

genUiTremyBy :: Sigs a => Sig -> Double -> Source (Fx a)
genUiTremyBy wave size = mapSource bindSig $ paintTo tremyColor $ fxBox "Tremolo" fx True $ setAll size ["depth", "rate"]
    where
        fx [depth, rate] = return . tremy wave depth rate

uiOscTremyBy, uiTriTremyBy, uiSqrTremyBy :: Sigs a => Double -> Source (Fx a)

uiOscTremyBy = genUiTremyBy 0
uiTriTremyBy = genUiTremyBy 1
uiSqrTremyBy = genUiTremyBy 2

uiOscTremy1, uiOscTremy2, uiOscTremy3, uiOscTremy4, uiOscTremy5,
    uiTriTremy1, uiTriTremy2, uiTriTremy3, uiTriTremy4, uiTriTremy5,
    uiSqrTremy1, uiSqrTremy2, uiSqrTremy3, uiSqrTremy4, uiSqrTremy5 :: Sigs a => Source (Fx a)

uiOscTremy1 = uiOscTremyBy size1
uiOscTremy2 = uiOscTremyBy size2
uiOscTremy3 = uiOscTremyBy size3
uiOscTremy4 = uiOscTremyBy size4
uiOscTremy5 = uiOscTremyBy size5

uiTriTremy1 = uiTriTremyBy size1
uiTriTremy2 = uiTriTremyBy size2
uiTriTremy3 = uiTriTremyBy size3
uiTriTremy4 = uiTriTremyBy size4
uiTriTremy5 = uiTriTremyBy size5

uiSqrTremy1 = uiSqrTremyBy size1
uiSqrTremy2 = uiSqrTremyBy size2
uiSqrTremy3 = uiSqrTremyBy size3
uiSqrTremy4 = uiSqrTremyBy size4
uiSqrTremy5 = uiSqrTremyBy size5

-- Ring modulation

uiRingo' :: Sigs a => Source (Fx a)
uiRingo' = mapSource bindSig $ paintTo ringoColor $ fxBox "Ringo" fx True [("size", size1)]
    where
        fx [size] = return . ringo' size

uiRingoBy :: Sigs a => Double -> Source (Fx a)
uiRingoBy size = mapSource bindSig $ paintTo ringoColor $ fxBox "Ring Mod" fx True $ setAll size ["mix", "rate", "env mod"]
    where
        fx [balance, rate, envMod] = return . ringo balance rate envMod

uiRingo1, uiRingo2, uiRingo3, uiRingo4, uiRingo5 :: Sigs a => Source (Fx a)

uiRingo1 = uiRingoBy size1
uiRingo2 = uiRingoBy size2
uiRingo3 = uiRingoBy size3
uiRingo4 = uiRingoBy size4
uiRingo5 = uiRingoBy size5

-- Crusher

uiCrusher :: Sigs a => Double -> Double -> Source (Fx a)
uiCrusher initReduction initFoldover = mapSource bindSig $ paintTo revsyColor $ fxBox "LoFi" fx True [("redux", initReduction), ("foldover", initFoldover)]
    where
        fx [redux, foldover] = return . crusher redux foldover

-- Reverse

uiRevsy :: Sigs a => Double -> Source (Fx a)
uiRevsy initTime = mapSource bindSig $ paintTo revsyColor $ fxBox "Reverse" fx True [("time", initTime)]
    where
        fx [time] = return . revsy time

------------------------------------------------------------
-- Reverbs

uiRevBy :: Sig2s a => Double -> Double -> Source (Fx a)
uiRevBy initFeedback initMix = paintTo reverbColor $ fxBox "Reverb" fx True [("mix", initMix), ("fbk", initFeedback)]
    where
        fx [balance, feedback] = \asig -> return $ mapSig2 (\x -> mul (1 - balance) x + mul balance (rever2 feedback x)) asig

uiRoom :: Sig2s a => Double -> Source (Fx a)
uiRoom = uiRevBy 0.6

uiRoom1, uiRoom2, uiRoom3, uiRoom4, uiRoom5 :: Sig2s a => Source (Fx a)

uiRoom1 = uiRoom size1
uiRoom2 = uiRoom size2
uiRoom3 = uiRoom size3
uiRoom4 = uiRoom size4
uiRoom5 = uiRoom size5

uiChamber :: Sig2s a => Double -> Source (Fx a)
uiChamber = uiRevBy 0.8

uiChamber1, uiChamber2, uiChamber3, uiChamber4, uiChamber5 :: Sig2s a => Source (Fx a)

uiChamber1 = uiChamber size1
uiChamber2 = uiChamber size2
uiChamber3 = uiChamber size3
uiChamber4 = uiChamber size4
uiChamber5 = uiChamber size5

uiHall :: Sig2s a => Double -> Source (Fx a)
uiHall = uiRevBy 0.9

uiHall1, uiHall2, uiHall3, uiHall4, uiHall5 :: Sig2s a => Source (Fx a)

uiHall1 = uiHall size1
uiHall2 = uiHall size2
uiHall3 = uiHall size3
uiHall4 = uiHall size4
uiHall5 = uiHall size5

uiCave :: Sig2s a => Double -> Source (Fx a)
uiCave = uiRevBy 0.99

uiCave1, uiCave2, uiCave3, uiCave4, uiCave5 :: Sig2s a => Source (Fx a)

uiCave1 = uiCave size1
uiCave2 = uiCave size2
uiCave3 = uiCave size3
uiCave4 = uiCave size4
uiCave5 = uiCave size5

------------------------------------------------------------
-- Mono Reverbs

rever1 :: Sig -> Sig -> Sig
rever1 fbk = toMono . rever2 fbk . fromMono

uiMonoRevBy :: Double -> Double -> Source Fx1
uiMonoRevBy initFeedback initMix = paintTo reverbColor $ fxBox "Reverb" fx True [("mix", initMix), ("fbk", initFeedback)]
    where
        fx [balance, feedback] = \asig -> return $ mixAt balance (rever1 feedback) asig

uiMonoRoom :: Double -> Source Fx1
uiMonoRoom = uiMonoRevBy 0.6

uiRoom1m, uiRoom2m, uiRoom3m, uiRoom4m, uiRoom5m :: Source Fx1

uiRoom1m = uiMonoRoom size1
uiRoom2m = uiMonoRoom size2
uiRoom3m = uiMonoRoom size3
uiRoom4m = uiMonoRoom size4
uiRoom5m = uiMonoRoom size5

uiMonoChamber :: Double -> Source Fx1
uiMonoChamber = uiMonoRevBy 0.8

uiChamber1m, uiChamber2m, uiChamber3m, uiChamber4m, uiChamber5m :: Source Fx1

uiChamber1m = uiMonoChamber size1
uiChamber2m = uiMonoChamber size2
uiChamber3m = uiMonoChamber size3
uiChamber4m = uiMonoChamber size4
uiChamber5m = uiMonoChamber size5

uiMonoHall :: Double -> Source Fx1
uiMonoHall = uiMonoRevBy 0.9

uiHall1m, uiHall2m, uiHall3m, uiHall4m, uiHall5m :: Source Fx1

uiHall1m = uiMonoHall size1
uiHall2m = uiMonoHall size2
uiHall3m = uiMonoHall size3
uiHall4m = uiMonoHall size4
uiHall5m = uiMonoHall size5

uiMonoCave :: Double -> Source Fx1
uiMonoCave = uiMonoRevBy 0.99

uiCave1m, uiCave2m, uiCave3m, uiCave4m, uiCave5m :: Source Fx1

uiCave1m = uiMonoCave size1
uiCave2m = uiMonoCave size2
uiCave3m = uiMonoCave size3
uiCave4m = uiMonoCave size4
uiCave5m = uiMonoCave size5

type ThreshSig = Sig

-------------------------------------
-- ambient guitar

-- | Ambient guitar patch. It uses @ambiEnv@ with stack of delays and a bit of compression and distortion.
-- No reverb is added. The reverb is for user to add.
--
-- > ambiGuitar thresh delayTime feedback tone driveLevel sig
--
-- For thresh try out values like 0.01, 0.02
--
-- Example, we read guitar input from soundcard first input,
-- apply ambient guitar effect and a bit of reverb:
--
-- > main = dac proc
-- >
-- > proc :: Sig2 -> SE Sig2
-- > proc (x, _) = hall 0.25 $ ambiGuitar 0.02 1 0.7 0.4 0.1 x
ambiGuitar :: ThreshSig -> DelayTime -> Feedback -> ToneSig -> DriveSig -> Sig -> SE Sig
ambiGuitar thresh dt fbk toneSig drv ain = do
    envSig <- ambiEnv thresh ain
    return $ ( adele 0.35 dt fbk toneSig . adele 0.5 (dt / 4) (fbk * 1.25) toneSig .
              saturator 0.3  . tort drv 0.25) envSig
