module Csound.Typed.Plugins.Iain(
    pitchShifterDelay,
    fxAnalogDelay, fxDistortion, fxEnvelopeFollower, fxFlanger, fxFreqShifter, fxLoFi,
    fxPanTrem, fxMonoTrem, fxPhaser, fxPitchShifter, fxReverse, fxRingModulator, fxChorus2, fxPingPong
) where

import Data.Boolean
import Control.Monad.Trans.Class
import Control.Applicative

import Csound.Dynamic

import Csound.Typed.Types
import Csound.Typed.GlobalState
import qualified Csound.Typed.GlobalState.Elements as E(pitchShifterDelayPlugin,
    analogDelayPlugin, distortionPlugin, envelopeFolollowerPlugin, flangerPlugin, freqShifterPlugin,
    loFiPlugin, panTremPlugin, monoTremPlugin, phaserPlugin, pitchShifterPlugin, reversePlugin, ringModulatorPlugin, stChorusPlugin, stereoPingPongDelayPlugin)

pitchShifterDelay :: D -> (Sig, Sig) -> Sig -> Sig -> Sig -> Sig
pitchShifterDelay imaxdlt (fb1, fb2) kdel ktrans ain = csdPitchShifterDelay ain ktrans kdel fb1 fb2 imaxdlt

-- | PitchShifterDelay
-- ; ----------------
-- ; A pitch shifter effect that employs delay lines
-- ;
-- ; aout  PitchShifterDelay  ain,ktrans,kdlt,kFB1,kFB2,imaxdlt
--;
--; Initialisation
--; --------------
--; imaxdlt --  maximum delay time (kdlt should not exceed this value)
--;
--; Performance
--; -----------
--; ain     --  input audio to be pitch shifted
--; ktrans  --  pitch transposition (in semitones)
--; kdlt    --  delay time employed by the pitch shifter effect (should be within the range ksmps/sr and imaxdlt)
--; kFB1    --  feedback using method 1 (output from delay taps are fed back directly into their own buffers before enveloping and mixing)
--; kFB2    --  feedback using method 2 (enveloped and mixed output from both taps is fed back into both buffers)
--
-- opcode  PitchShifterDelay,a,akkkki
csdPitchShifterDelay :: Sig -> Sig -> Sig -> Sig -> Sig -> D -> Sig
csdPitchShifterDelay ain ktrans kdlt kFB1 kFB2 imaxdlt = fromGE $ do
    addUdoPlugin E.pitchShifterDelayPlugin
    f <$> toGE ain <*> toGE ktrans <*> toGE kdlt <*> toGE kFB1 <*> toGE kFB2 <*> toGE imaxdlt
    where f ain ktrans kdlt kFB1 kFB2 imaxdlt = opcs "PitchShifterDelay" [(Ar, [Ar, Kr, Kr, Kr, Kr, Ir])] [ain, ktrans, kdlt, kFB1, kFB2, imaxdlt]

--------------------------------------------------------
-- multi fx

fxAnalogDelay :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
fxAnalogDelay kmix ktime kfback ktone ain = csdAnalogDelay ain kmix ktime kfback ktone

-- ; AnalogDelay
-- ; ----------------
-- ; A analog style delay with signal degradation and saturation options
-- ;
-- ; aout  AnalogDelay  ain,kmix,ktime,kfback,ktone
-- ;
-- ; Performance
-- ; -----------
-- ; ain    --  input audio to which the flanging effect will be applied
-- ; kmix   --  dry / wet mix of the output signal (range 0 to 1)
-- ; ktime  --  delay time of the effect in seconds
-- ; kfback --  control of the amount of output signal fed back into the input of the effect (exceeding 1 (100%) is possible and will result in saturation clipping effects)
-- ; ktone  --  control of the amount of output signal fed back into the input of the effect (range 0 to 1)
csdAnalogDelay :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
csdAnalogDelay ain kmix ktime kfback ktone = fromGE $ do
    addUdoPlugin E.analogDelayPlugin
    f <$> toGE ain <*> toGE kmix <*> toGE ktime <*> toGE kfback <*> toGE ktone
    where f ain kmix ktime kfback ktone = opcs "AnalogDelay" [(Ar,[Ar,Kr,Kr,Kr,Kr])] [ain, kmix, ktime, kfback, ktone]

fxDistortion :: Sig -> Sig -> Sig -> Sig -> Sig
fxDistortion klevel kdrive ktone ain = csdDistortion ain klevel kdrive ktone

-- ; Distortion
-- ; ----------------
-- ; A distortion effect offering stomp-box-like controls
-- ;
-- ; aout  Distortion  ain,klevel,kdrive,ktone
-- ;
-- ; Performance
-- ; -----------
-- ; ain    --  input audio to be distorted
-- ; klevel --  output level of the effect (range: 0 to 1)
-- ; kdrive --  intensity of the distortion effect (range: 0 to 1)
-- ; ktone  --  tone of a lowpass filter (range: 0 to 1)
csdDistortion :: Sig -> Sig -> Sig -> Sig -> Sig
csdDistortion ain klevel kdrive ktone = fromGE $ do
    addUdoPlugin E.distortionPlugin
    f <$> toGE ain <*> toGE klevel <*> toGE kdrive <*> toGE ktone
    where f ain klevel kdrive ktone = opcs "Distortion" [(Ar,[Ar,Kr,Kr,Kr])] [ain, klevel, kdrive, ktone]


fxEnvelopeFollower :: Sig -> Sig -> Sig -> Sig -> Sig
fxEnvelopeFollower ksens kfreq kres ain = csdEnvelopeFollower ain ksens kfreq kres

-- ; EnvelopeFollower
-- ; ----------------
-- ; A dynamic envelope following resonant lowpass filter
-- ;
-- ; aout  EnvelopeFollower  ain,ksens,kfreq,kres
-- ;
-- ; Performance
-- ; -----------
-- ; ain    --  input audio to be filtered
-- ; ksens  --  sensitivity of the envelope follower (suggested range: 0 to 1)
-- ; kfreq  --  base frequency of the filter before modulation by the input dynamics (range: 0 to 1)
-- ; kres   --  resonance of the lowpass filter (suggested range: 0 to 0.99)
csdEnvelopeFollower :: Sig -> Sig -> Sig -> Sig -> Sig
csdEnvelopeFollower ain ksens kfreq kres = fromGE $ do
    addUdoPlugin E.envelopeFolollowerPlugin
    f <$> toGE ain <*> toGE ksens <*> toGE kfreq <*> toGE kres
    where f ain ksens kfreq kres = opcs "EnvelopeFollower" [(Ar,[Ar,Kr,Kr,Kr])] [ain, ksens, kfreq, kres]

-- ; Flanger
-- ; ----------------
-- ; A flanger effect following the typical design of a so called 'stomp box'
-- ;
-- ; aout  Flanger  ain,krate,kdepth,kdelay,kfback
-- ;
-- ; Performance
-- ; -----------
-- ; ain    --  input audio to which the flanging effect will be applied
-- ; krate  --  rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)
-- ; kdepth --  depth of the lfo of the effect (range 0 to 1)
-- ; kdelay --  static delay offset of the flanging effect (range 0 to 1)
-- ; kfback --  feedback and therefore intensity of the effect (range 0 to 1)
fxFlanger :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
fxFlanger krate kdepth kdelay kfback ain = fromGE $ do
    addUdoPlugin E.flangerPlugin
    f <$> toGE ain <*> toGE krate <*> toGE kdepth <*> toGE kdelay <*> toGE kfback
    where f ain krate kdepth kdelay kfback = opcs "Flanger" [(Ar,[Ar,Kr,Kr,Kr,Kr])] [ain, krate, kdepth, kdelay, kfback]


-- ; FreqShifter
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
fxFreqShifter :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
fxFreqShifter kmix kfreq kmult kfback adry = fromGE $ do
    addUdoPlugin E.freqShifterPlugin
    f <$> toGE adry <*> toGE kmix <*> toGE kfreq <*> toGE kmult <*> toGE kfback
    where f adry kmix kfreq kmult kfback = opcs "FreqShifter" [(Ar,[Ar,Kr,Kr,Kr,Kr])] [adry, kmix, kfreq, kmult, kfback]


-- ; LoFi
-- ; ----------------
-- ; 'Low Fidelity' distorting effects of bit reduction and downsampling (foldover)
-- ;
-- ; aout  LoFi  ain,kbits,kfold
-- ;
-- ; Performance
-- ; -----------
-- ; ain    --  input audio to have low fidelity distortion effects applied
-- ; kbits  --  bit depth reduction (suggested range 0 to 0.6)
-- ; kfold  --  amount of foldover (range 0 to 1)
fxLoFi :: Sig -> Sig -> Sig -> Sig
fxLoFi kbits kfold ain = fromGE $ do
    addUdoPlugin E.loFiPlugin
    f <$> toGE ain <*> toGE kbits <*> toGE kfold
    where f ain kbits kfold = opcs "LoFi" [(Ar,[Ar,Kr,Kr])] [ain, kbits, kfold]

-- ; PanTrem
-- ; ----------------
-- ; Auto-panning and tremolo effects
-- ;
-- ; aout1,aout2  PanTrem  ainL,ainR,,krate,kdepth,kmode,kwave
-- ;
-- ; Performance
-- ; -----------
-- ; ainL   --  first/left input audio
-- ; ainR   --  second/right input audio
-- ; krate  --  rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)
-- ; kdepth --  depth of the lfo of the effect (range 0 to 1)
-- ; kmode  --  mode of the effect (0=auto-panning 1=tremolo)
-- ; kwave  --  waveform used by the lfo (0=sine 1=triangle 2=square)
fxPanTrem :: Sig -> Sig -> Sig -> Sig -> Sig2 -> Sig2
fxPanTrem krate kdepth kmode kwave (ainL, ainR) = toTuple $ do
    addUdoPlugin E.panTremPlugin
    f <$> toGE ainL <*> toGE ainR <*> toGE krate <*> toGE kdepth <*> toGE kmode <*> toGE kwave
    where f ainL ainR krate kdepth kmode kwave = ($ 2) $ mopcs "PanTrem" ([Ar,Ar], [Ar,Ar, Kr,Kr,Kr,Kr]) [ainL, ainR, krate, kdepth, kmode, kwave]

-- ; Tremolo
-- ; ----------------
-- ; Tremolo effect
-- ;
-- ; aout MonoTrem  ain,krate,kdepth,kwave
-- ;
-- ; Performance
-- ; -----------
-- ; ain    --  input audio
-- ; krate  --  rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)
-- ; kdepth --  depth of the lfo of the effect (range 0 to 1)
-- ; kwave  --  waveform used by the lfo (0=sine 1=triangle 2=square)
fxMonoTrem :: Sig -> Sig -> Sig -> Sig -> Sig
fxMonoTrem krate kdepth kwave ain = fromGE $ do
    addUdoPlugin E.monoTremPlugin
    f <$> toGE ain <*> toGE krate <*> toGE kdepth <*> toGE kwave
    where f ain krate kdepth kwave = opcs "MonoTrem" [(Ar, [Ar,Kr,Kr,Kr])] [ain, krate, kdepth, kwave]

-- ; Phaser
-- ; ----------------
-- ; An phase shifting effect that mimics the design of a so called 'stomp box'
-- ;
-- ; aout  Phaser  ain,krate,kdepth,kfreq,kfback
-- ;
-- ; Performance
-- ; -----------
-- ; ain    --  input audio to be pitch shifted
-- ; krate  --  rate of lfo of the effect (range 0 to 1)
-- ; kdepth --  depth of lfo of the effect (range 0 to 1)
-- ; kfreq  --  centre frequency of the phase shifting effect in octaves (suggested range 6 to 11)
-- ; kfback --  feedback and therefore intensity of the effect (range 0 to 1)
fxPhaser :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
fxPhaser krate kdepth kfreq kfback ain = fromGE $ do
    addUdoPlugin E.phaserPlugin
    f <$> toGE ain <*> toGE krate <*> toGE kdepth <*> toGE kfreq <*> toGE kfback
    where f ain krate kdepth kfreq kfback = opcs "Phaser" [(Ar,[Ar,Kr,Kr,Kr,Kr])] [ain, krate, kdepth, kfreq, kfback]

-- ; PitchShifter
-- ; ------------
-- ; A pitch shifter effect based on FFT technology
-- ;
-- ; aout  PitchShifter  ain,kmix,kpitch,kfine,kfback
-- ;
-- ; Performance
-- ; -----------
-- ; ain    --  input audio to be pitch shifted
-- ; kmix   --  dry / wet mix of the output signal (range 0 to 1)
-- ; kscal  -- pitch ratio
-- #### ; kpitch --  pitch shifting interval in thousands of a semitone (suggested range -0.012 to 0.012)
-- #### ; kfine  --  fine control of pitch shifting interval in octaves (range -1/12 to 1/12)
-- ; kfback --  control of the amount of output signal fed back into the input of the effect (suggested range 0 to 1)
fxPitchShifter :: D -> Sig -> Sig -> Sig -> Sig -> Sig
fxPitchShifter ifftsize kmix kscal kfback ain = fromGE $ do
    addUdoPlugin E.pitchShifterPlugin
    f <$> toGE ain <*> toGE kmix <*> toGE kscal <*> toGE kfback <*> toGE ifftsize
    where f ain kmix kscal kfback ifftsize = opcs "PitchShifter" [(Ar,[Ar,Kr,Kr,Kr,Ir])] [ain, kmix, kscal, kfback, ifftsize]


-- ; Reverse
-- ; ----------------
-- ; An effect that reverses an audio stream in chunks
-- ;
-- ; aout  Reverse  ain,ktime
-- ;
-- ; Performance
-- ; -----------
-- ; ain    --  input audio to be reversed
-- ; ktime  --  time duration of each chunk (suggested range: 0.3 to 2)--
fxReverse :: Sig -> Sig -> Sig
fxReverse ktime ain = fromGE $ do
    addUdoPlugin E.reversePlugin
    f <$> toGE ain <*> toGE ktime
    where f ain ktime = opcs "Reverse" [(Ar,[Ar,Kr])] [ain, ktime]

-- ; RingModulator
-- ; ----------------
-- ; An ring modulating effect with an envelope follower
-- ;
-- ; aout  RingModulator  ain,kmix,kfreq,kenv
-- ;
-- ; Performance
-- ; -----------
-- ; ain    --  input audio to be pitch shifted
-- ; kmix   --  dry / wet mix of the output signal (range 0 to 1)
-- ; kfreq  --  frequency of thew ring modulator *NOT IN HERTZ* (range 0 to 1)
-- ; kenv   --  amount of dynamic envelope following modulation of frequency (range 0 to 1)
fxRingModulator :: Sig -> Sig -> Sig -> Sig -> Sig
fxRingModulator kmix kfreq kenv ain = fromGE $ do
    addUdoPlugin E.ringModulatorPlugin
    f <$> toGE ain <*> toGE kmix <*> toGE kfreq <*> toGE kenv
    where f ain kmix kfreq kenv = opcs "RingModulator" [(Ar,[Ar,Kr,Kr,Kr])] [ain, kmix, kfreq, kenv]

-- ; StChorus
-- ; ----------------
-- ; A stereo chorus effect
-- ;
-- ; aout  StChorus  ainL,ainR,krate,kdepth,kwidth
-- ;
-- ; Performance
-- ; -----------
-- ; ainL   --  first/left input audio
-- ; ainR   --  second/right input audio
-- ; krate  --  rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)
-- ; kdepth --  depth of the lfo of the effect (range 0 to 1)
-- ; kwidth --  width of stereo widening (range 0 to 1)
fxChorus2 :: Sig -> Sig -> Sig -> Sig2 -> Sig2
fxChorus2 krate kdepth kwidth (ainL, ainR) = toTuple $ do
    addUdoPlugin E.stChorusPlugin
    f <$> toGE ainL <*> toGE ainR <*> toGE krate <*> toGE kdepth <*> toGE kwidth
    where f ainL ainR krate kdepth kwidth = ($ 2) $ mopcs "StChorus" ([Ar,Ar], [Ar,Ar,Kr,Kr,Kr]) [ainL, ainR, krate, kdepth, kwidth]

-- aInL, aInR, kdelayTime, kFeedback, kMix, iMaxDelayTime xin

-- | Stereo ping-pong delay effect
--
-- > fxPingPong maxDelayTime kmix width tone time feedback (ainL, ainR)
fxPingPong :: D -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig2 -> Sig2
fxPingPong iMaxDelTime kmix kwidth ktone ktime kfeedback (ainL, ainR) = toTuple $ do
    addUdoPlugin E.stereoPingPongDelayPlugin
    f <$> toGE ainL <*> toGE ainR <*> toGE ktime <*> toGE kfeedback <*> toGE kmix <*> toGE kwidth <*> toGE ktone <*> toGE iMaxDelTime
    where f ainL ainR ktime kfeedback kmix kwidth ktone iMaxDelTime = ($ 2) $ mopcs "StereoPingPongDelay" ([Ar,Ar], [Ar,Ar,Kr,Kr,Kr,Kr,Kr,Ir]) [ainL, ainR, ktime, kfeedback, kmix, kwidth, ktone, iMaxDelTime]

