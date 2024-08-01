{- | The Csound contains a set of functions for granular synthesis.
Unfortunately they are very hard to use due to large number of arguments.
This module attempts to set most of the arguments with sensible defaults.
So that a novice could start to use it. The defaults are implemented with
the help of the class @Default@. It's a standard way to implement defaults
in the Haskell. The class @Defaults@ defines a single constnat called @def@.
With @def@ we can get the default value for the given type.

Several csound opcodes are reimplemented so that first argument contains
secondary parameters. The type for parameters always has the instance for the
class @Default@. The original csound opcodes are defined in the end of the module
with prefix @csd@.

Also many granular synth opcodes expect the sound file as input.
There are predefined versions of the opcodes that take in the file names
instead of tables with sampled sound. They have suffix @Snd@ for stereo and @Snd1@ for mono files.

For example, that's how we can use the @granule@ opcode:

> dac $ granuleSnd1 spec [1, 2, 3] grainSize "fox.wav"

No need to set all 22 parameters.
Look at the official tutorial (on github) for more examples.

The five functions are reimplemented in this way: @sndwarp@, @syncgrain@, @partikkel@, @granule@, @fof2@.

The most often used arguments are:

* Scale factors for tempo and pitch: @TempoSig@ or @speed@ and @PitchSig@. Ranges in 0 to 1

* Grain size is the size of produced grains in seconds. Good range is 0.005 to 0.01 or even 0.1.
   The higer the value the more it sounds like the original sound.

* Grain rate. It's the speed of grain production in Hz. If it's in audio range
we can no longer percieve the original pitch of the file. Then the pitch is determined
with grain rate value.

* Grain gap. It's the gap in samples between the grains. Good values are 1 to 100.

* Grain window function. For the sound to be a grain it have to be enveloped
with grain window (some sort of bell shaped envelope). We can use half-sine for this purpose
(and it's so in most of the defauts) or we can use a table in the @GEN20@ family. In the library
they implemented as window tables see the table constructors with prefix @win@.

Usual order of arguments is: @GrainRate@, @GrainSize@, @TempoSig@, @PitchSig@, file @table@ or @name@, @poniter@ to the table.
-}
module Csound.Air.Granular (
  GrainRate,
  GrainSize,
  Pointer,
  ConstPitchSig,

  -- * Grainy (simple partikkel)
  RndGrainySpec (..),
  grainy,
  grainy1,
  rndGrainy,
  rndGrainy1,
  ptrGrainy,
  rndPtrGrainy,
  ptrGrainySnd,
  ptrGrainySnd1,

  -- * Sndwarp
  SndwarpSpec (..),
  sndwarp,
  sndwarpst,
  sndwarpSnd,
  sndwarpSnd1,
  ptrSndwarp,
  ptrSndwarpst,
  ptrSndwarpSnd,
  ptrSndwarpSnd1,

  -- * Syncgrain
  SyncgrainSpec (..),
  RndSyncgrainSpec (..),
  syncgrain,
  syncgrainSnd,
  syncgrainSnd1,
  rndSyncgrain,
  rndSyncgrainSnd,
  rndSyncgrainSnd1,

  -- * Granule
  GranuleSpec (..),
  GranuleMode (..),
  granule,
  granuleSnd,
  granuleSnd1,

  -- * Partikkel
  PartikkelSpec (..),
  partikkel,

  -- * Fof2
  Fof2Spec (..),
  fof2,
  fof2Snd,
  fof2Snd1,

  -- * Granular delays

  -- | This block is for granular delay effects. To make granular delay from the granular functions
  -- it has to support reading from table with pointer (phasor).
  -- All functions have the same four parameters:
  --
  -- * @maxDelayTime@ -- maximum delay length in seсoncds.
  --
  -- * @delayTime@ -- delay time (it can vary. it's a signal).
  --
  -- * @feedback@ -- amount of feedback. How much of processed signal is mixed to
  --    the delayed signal
  --
  -- * @balance@ -- mix between dry and wet signal. 0 is dry only signal. 1 is wet only signl.
  --
  -- The rest arguments are taken from the original granular functions.
  grainyDelay,
  rndGrainyDelay,
  sndwarpDelay,
  syncgrainDelay,
  rndSyncgrainDelay,
  partikkelDelay,
  fofDelay,

  -- * Granular effets

  -- | The functions are based on the granular delays.
  -- each function is a granular delay with zero feedback and instant delay time.
  grainyFx,
  rndGrainyFx,
  sndwarpFx,
  syncgrainFx,
  rndSyncgrainFx,
  partikkelFx,
  fofFx,

  -- * Csound functions
  csdSndwarp,
  csdSndwarpst,
  csdSyncgrain,
  csdGranule,
  csdPartikkel,
) where

-- http://www.youtube.com/watch?v=tVW809gMND0

import Csound.Dynamic hiding (int, when1, whens)
import Csound.Typed
import Data.Default

import Csound.Typed.Opcode hiding (fof2, grain, granule, partikkel, pitch, sndwarp, sndwarpst, syncgrain, tab, tempo)
import Csound.Typed.Opcode qualified as C (fof2, granule, sndwarp, sndwarpst)

import Csound.Air.Fx (Balance, DelayTime, Feedback, MaxDelayTime, tabDelay)
import Csound.Air.Wav (PitchSig, TempoSig, lengthSnd)
import Csound.Tab

-- example
--
-- let q n = mul 0.1 $ grainy2 w2 1 (60 + 260 * uosc 0.25) (0.01 + 0.1 * uosc 0.1) (semitone n)
-- let w a b c = mul 0.1 $ q a + q b + q c
-- dac $ at magicCave2 $ (w 0 7 12) + delaySnd 2 (w (-12) (12 + 5) (12 + 7)) + delaySnd 1.3 (w 0 5 24)

-----------------------------------------------------------------
-- partikkel

type GrainRate = Sig
type GrainSize = Sig
type Pointer = Sig

type ConstPitchSig = D

----------------------------------------------------------------------
-- partikkel

{- | Secondary parameters for the partikkel opcode. We can use the @def@ to get the defaults.
See the official docs to know the complete description:

csound doc: <http://www.csounds.com/manual/html/partikkel.html>
-}
data PartikkelSpec = PartikkelSpec
  { partikkelDistribution :: Sig
  , partikkelDisttab :: Tab
  , partikkelSync :: Sig
  , partikkelEnv2amt :: Sig
  , partikkelEnv2tab :: Tab
  , partikkelEnv_attack :: Tab
  , partikkelEnv_decay :: Tab
  , partikkelSustain_amount :: Sig
  , partikkelA_d_ratio :: Sig
  , partikkelAmp :: Sig
  , partikkelGainmasks :: Tab
  , partikkelSweepshape :: Sig
  , partikkelWavfreqstarttab :: Tab
  , partikkelWavfreqendtab :: Tab
  , partikkelWavfm :: Sig
  , partikkelFmamptab :: Tab
  , partikkelFmenv :: Tab
  , partikkelCosine :: Tab
  , partikkelNumpartials :: Sig
  , partikkelChroma :: Sig
  , partikkelChannelmasks :: Tab
  , partikkelRandommask :: Sig
  , partikkelWaveamptab :: Tab
  , partikkelWavekeys :: [Sig]
  , partikkelMax_grains :: D
  }

instance Default PartikkelSpec where
  def =
    PartikkelSpec
      { partikkelDistribution = 1
      , partikkelDisttab = setSize 32768 $ lins [0, 1, 1]
      , partikkelSync = 0
      , partikkelEnv2amt = 1
      , partikkelEnv2tab = setSize 4096 $ winSync
      , partikkelEnv_attack = noTab
      , partikkelEnv_decay = noTab
      , partikkelSustain_amount = 0
      , partikkelA_d_ratio = 0
      , partikkelAmp = 1
      , partikkelGainmasks = noTab
      , partikkelSweepshape = 0
      , partikkelWavfreqstarttab = noTab
      , partikkelWavfreqendtab = noTab
      , partikkelWavfm = 0
      , partikkelFmamptab = noTab
      , partikkelFmenv = noTab
      , partikkelCosine = setSize 8193 $ sines3 [(1, 1, 90)]
      , partikkelNumpartials = 1
      , partikkelChroma = 1
      , partikkelChannelmasks = noTab
      , partikkelRandommask = 0
      , partikkelWaveamptab = noTab
      , partikkelWavekeys = [1]
      , partikkelMax_grains = 1000
      }

{- | Randomized parameters for function @grainy@. We can randomize pitch scaleing factor (0 to 1),
read position (in ratio: 0 to 1), and duration of the grains (in seconds, in magnitude of 0.005 to 0.5).
-}
data RndGrainySpec = RndGrainySpec
  { rndGrainyPitch :: Sig
  , rndGrainyPos :: Sig
  , rndGrainyDur :: Sig
  }

instance Default RndGrainySpec where
  def =
    RndGrainySpec
      { rndGrainyPitch = 0.25
      , rndGrainyPos = 0.1
      , rndGrainyDur = 0.2
      }

{- |
Granular synthesizer with "per grain" control
      over many of its parameters.  Has a sync input to
      sychronize its internal grain scheduler clock to an external
      clock source.

partikkel was conceived after reading Curtis Roads' book
      "Microsound", and the goal was to create an opcode that was
      capable of all time-domain varieties of granular synthesis
      described in this book. The idea being that most of the
      techniques only differ in parameter values, and by having a
      single opcode that can do all varieties of granular synthesis
      makes it possible to interpolate between techniques. Granular synthesis is sometimes dubbed particle
      synthesis, and it was thought apt to name the opcode partikkel
      to distinguish it from other granular opcodes.

> partikkel spec grainrate grainsize kpitch ifiltabs apnters

* @spec@ - secondary parameters

* @grainrate@ - rate of the grain creation

* @grainsize@ - grain size in sec (!!!not in ms as for Csound!!!).

* @kpitch@ -- pitch scaling factor.

* @apnters@ -- list of pointers (up to 4 values can be used)

* @ifiltabs@ -- list of tables (up to 4 values can be used)
-}
partikkel :: PartikkelSpec -> GrainRate -> GrainSize -> PitchSig -> [Tab] -> [Pointer] -> Sig
partikkel spec kgrainrate kgrainsize kpitch ifiltab apnter = mul 0.2 res
  where
    res =
      csdPartikkel
        kgrainrate
        (partikkelDistribution spec)
        (partikkelDisttab spec)
        (partikkelSync spec)
        (partikkelEnv2amt spec)
        (partikkelEnv2tab spec)
        (partikkelEnv_attack spec)
        (partikkelEnv_decay spec)
        (partikkelSustain_amount spec)
        (partikkelA_d_ratio spec)
        (kgrainsize * 1000)
        (partikkelAmp spec)
        (partikkelGainmasks spec)
        kwavfreq
        (partikkelSweepshape spec)
        (partikkelWavfreqstarttab spec)
        (partikkelWavfreqendtab spec)
        (partikkelWavfm spec)
        (partikkelFmamptab spec)
        (partikkelFmenv spec)
        (partikkelCosine spec)
        kgrainrate
        (partikkelNumpartials spec)
        (partikkelChroma spec)
        (partikkelChannelmasks spec)
        (partikkelRandommask spec)
        filtab1
        filtab2
        filtab3
        filtab4
        (partikkelWaveamptab spec)
        apnter1
        apnter2
        apnter3
        apnter4
        keys1
        keys2
        keys3
        keys4
        (partikkelMax_grains spec)

    iorig = 1 / (ftlen (head ifiltab) / getSampleRate)
    kwavfreq = sig iorig * kpitch

    filtab1 : filtab2 : filtab3 : filtab4 : _ = cycle ifiltab
    apnter1 : apnter2 : apnter3 : apnter4 : _ = cycle apnter
    keys1 : keys2 : keys3 : keys4 : _ = cycle (partikkelWavekeys spec)

{- | Simplified version of partikkel. The partikkel for mono sounds.

> grainy1 speed grainrate grainsize kfreqFactor file

* @speed@ - speed of the playback

* @grainrate@ - rate of the grain creation

* @grainsize@ - size of the grains

* @file@ - filename of an audio file to read the grains.
-}
grainy1 :: GrainRate -> GrainSize -> TempoSig -> PitchSig -> String -> Sig
grainy1 = grainyChn 1

{- | Simplified version of partikkel. The partikkel for stereo sounds.

> grainy1 speed grainrate grainsize kfreqFactor file

* @speed@ - speed of the playback

* @grainrate@ - rate of the grain creation

* @grainsize@ - size of the grains

* @file@ - filename of an audio file to read the grains.
-}
grainy :: GrainRate -> GrainSize -> TempoSig -> PitchSig -> String -> Sig2
grainy kgrainrate kgrainsize kspeed kfreqFactor file = (f 1, f 2)
  where
    f n = grainyChn n kgrainrate kgrainsize kspeed kfreqFactor file

grainyChn :: Int -> GrainRate -> GrainSize -> TempoSig -> PitchSig -> String -> Sig
grainyChn n kgrainrate kgrainsize kspeed kpitch file =
  ptrGrainy kgrainrate kgrainsize kpitch (grainyTab n file) (grainyPtr kspeed file)

-- | Randomized version of @grainy1@.
rndGrainy1 :: RndGrainySpec -> GrainRate -> GrainSize -> TempoSig -> PitchSig -> String -> SE Sig
rndGrainy1 = rndGrainyChn 1

-- | Randomized version of @grainy@.
rndGrainy :: RndGrainySpec -> GrainRate -> GrainSize -> TempoSig -> PitchSig -> String -> SE Sig2
rndGrainy spec kgrainrate kgrainsize kspeed kfreqFactor file = do
  asig1 <- f 1140
  asig2 <- f 2
  return (asig1, asig2)
  where
    f n = rndGrainyChn n spec kgrainrate kgrainsize kspeed kfreqFactor file

rndGrainyChn :: Int -> RndGrainySpec -> GrainRate -> GrainSize -> TempoSig -> PitchSig -> String -> SE Sig
rndGrainyChn n spec kgrainrate kgrainsize kspeed kpitch file =
  rndPtrGrainy spec kgrainrate kgrainsize kpitch (grainyTab n file) (grainyPtr kspeed file)

{- | Simplified version of partikkel with pointer access to the table. The partikkel for mono sounds.

> ptrGrainy grainrate grainsize kfreqFactor tab apnter

* @speed@ - speed of the playback

* @grainrate@ - rate of the grain creation

* @grainsize@ - size of the grains

* @tab@ - table with sampled sound.

* @apnter@ - pointer to the table. pointer is relative to total size (0 to 1).
-}
ptrGrainy :: GrainRate -> GrainSize -> PitchSig -> Tab -> Pointer -> Sig
ptrGrainy kgrainrate kgrainsize kcent ifiltab apnter =
  partikkel def kgrainrate kgrainsize kcent [ifiltab] [apnter]

{- | Simplified version of partikkel with pointer access to the table. The partikkel for mono sounds.

> ptrGrainy grainrate grainsize kfreqFactor tab apnter

* @speed@ - speed of the playback

* @grainrate@ - rate of the grain creation

* @grainsize@ - size of the grains

* @file@ - file with sampled sound.

* @apnter@ - pointer to the table in seconds
-}
ptrGrainySnd :: GrainRate -> GrainSize -> PitchSig -> String -> Pointer -> Sig2
ptrGrainySnd kgrainrate kgrainsize kcent file apnter = (f (wavs file 0 WavLeft), f (wavs file 0 WavRight))
  where
    f tab = partikkel def kgrainrate kgrainsize kcent [tab] [apnter / sig (lengthSnd file)]

{- | Simplified version of partikkel with pointer access to the table. The partikkel for mono sounds.

> ptrGrainy grainrate grainsize kfreqFactor tab apnter

* @speed@ - speed of the playback

* @grainrate@ - rate of the grain creation

* @grainsize@ - size of the grains

* @file@ - file with sampled sound.

* @apnter@ - pointer to the table in seconds
-}
ptrGrainySnd1 :: GrainRate -> GrainSize -> PitchSig -> String -> Pointer -> Sig
ptrGrainySnd1 kgrainrate kgrainsize kcent file apnter = f (wavs file 0 WavLeft)
  where
    f tab = partikkel def kgrainrate kgrainsize kcent [tab] [apnter / sig (lengthSnd file)]

-- | Randomized version of @ptrGrainy@.
rndPtrGrainy :: RndGrainySpec -> GrainRate -> GrainSize -> PitchSig -> Tab -> Pointer -> SE Sig
rndPtrGrainy rndSpec kgrainrate kgrainsize kpitch ifiltab apnter = do
  kpitchRandVal <- rand (rndGrainyPitch rndSpec)
  arndpos <- linrand (rndGrainyPos rndSpec)
  krndsize <- rand (rndGrainyDur rndSpec)
  return $ ptrGrainy kgrainrate (kgrainsize + krndsize) (kpitch + kpitchRandVal) ifiltab (apnter + arndpos)

grainyTab :: Int -> String -> Tab
grainyTab n file = wavs file 0 (if n == 1 then WavLeft else WavRight)

grainyPtr :: Sig -> String -> Sig
grainyPtr kspeed file = apnter
  where
    ifildur = filelen $ text file
    apnter = phasor (kspeed / sig ifildur)

-----------------------------------------------------------------
-- granule

-- | Granule playback mode.
data GranuleMode = GranuleForward | GranuleBackward | GranuleRandom

fromGranuleMode :: GranuleMode -> D
fromGranuleMode x = case x of
  GranuleForward -> 1
  GranuleBackward -> -1
  GranuleRandom -> 0

{- | Secondary parameters for @granule@. We can use the @def@ to get the defaults.

* @Gap@ - gap between grains in sec.

* Voice - number of voices (integer value in magnitude of 1 to 128, 64 is default)

* Ratio - ratio of the speed of the gskip pointer relative to output audio sample rate (the default is 1)

* Mode  - playback mode (see @GranuleMode@, play forward is the default)

* Skip_os - gskip pointer random offset in sec, 0 will be no offset (0.5 is default).

* Gap_os - gap random offset in ratios (0 to 1) of the gap size, 0 gives no offset (0.5 is default).

* Size_os -grain size random offset in ratios (0 to 1) of grain size, 0 gives no offset (0.5 is default).

* Seed - seed for the random number generator (0.5 is default).

* Att   -  attack of the grain envelope in ratios (0 to 1) of grain size (0.3 is default).

* Dec  - decay of the grain envelope in ratios (0 to 1) of grain size (0.3 is default).
-}
data GranuleSpec = GranuleSpec
  { granuleGap :: Sig
  , granuleVoice :: D
  , granuleRatio :: D
  , granuleMode :: GranuleMode
  , granuleSkip_os :: D
  , granuleGap_os :: D
  , granuleSize_os :: D
  , granuleSeed :: D
  , granuleAtt :: D
  , granuleDec :: D
  }

instance Default GranuleMode where
  def = GranuleForward

instance Default GranuleSpec where
  def =
    GranuleSpec
      { granuleGap = 0
      , granuleVoice = 64
      , granuleRatio = 1
      , granuleMode = def
      , granuleSkip_os = 0.5
      , granuleGap_os = 0.5
      , granuleSize_os = 0.5
      , granuleSeed = 0.5
      , granuleAtt = 0.3
      , granuleDec = 0.3
      }

toPercent :: D -> D
toPercent = (100 *)

{- | A more complex granular synthesis texture generator.

granule is a Csound unit generator which employs a wavetable as input
to produce granularly synthesized audio output. Wavetable data may be
generated by any of the GEN subroutines such as GEN01 which reads an
audio data file into a wavetable. This enable a sampled sound to be used
as the source for the grains. Up to 128 voices are implemented internally.
The maximum number of voices can be increased by redefining the variable MAXVOICE
in the grain4.h file. granule has a build-in random number generator to handle
all the random offset parameters. Thresholding is also implemented to scan the source
function table at initialization stage. This facilitates features such as skipping
silence passage between sentences.

> granule spec chord grainSize ftab

* @spec@ -- secondary parameters. We can use @def@ to get the defaults.

* @chord :: [D]@ -- the list of pitch factors to scale the original sound.
   It can be up to 4 items long. This parameters allows us to create a chords out of grains.

* @grainSize@ -- grain size in sec.

* @ftab@ - table with sampled sound.
-}
granule :: GranuleSpec -> [ConstPitchSig] -> GrainSize -> Tab -> Sig
granule spec chord kgsize ifn = granuleWithLength len spec chord kgsize ifn
  where
    len = nsamp ifn / getSampleRate

{- | @granule@ that is defined on stereo audio files. We provide the filename instead of table.
The rest is the same.
-}
granuleSnd :: GranuleSpec -> [ConstPitchSig] -> GrainSize -> String -> Sig2
granuleSnd spec chord kgsize file =
  ( granuleWithLength len spec chord kgsize (wavs file 0 WavLeft)
  , granuleWithLength len spec chord kgsize (wavs file 0 WavRight)
  )
  where
    len = filelen $ text file

{- | @granule@ that is defined on mono audio files. We provide the filename instead of table.
The rest is the same.
-}
granuleSnd1 :: GranuleSpec -> [ConstPitchSig] -> GrainSize -> String -> Sig
granuleSnd1 spec chord kgsize file = granuleWithLength len spec chord kgsize (wavs file 0 WavLeft)
  where
    len = filelen $ text file

granuleWithLength :: D -> GranuleSpec -> [ConstPitchSig] -> GrainSize -> Tab -> Sig
granuleWithLength len spec chord kgsize ifn = mul 0.2 res
  where
    kgap = granuleGap spec
    kamp = 1
    ivoice = granuleVoice spec
    iratio = granuleRatio spec
    imode = fromGranuleMode $ granuleMode spec
    ithd = 0
    ipshift = int $ min (length $ chord) 4
    igskip = 0
    igskip_os = toPercent $ granuleSkip_os spec
    ilength = len
    igap_os = toPercent $ granuleGap_os spec
    igsize_os = toPercent $ granuleSize_os spec
    iatt = toPercent $ granuleAtt spec
    idec = toPercent $ granuleDec spec
    iseed = granuleSeed spec
    ipitch1 : ipitch2 : ipitch3 : ipitch4 : _ = chord ++ [1, 1, 1, 1]

    -- create the granular synthesis textures; one for each channel
    res =
      csdGranule
        kamp
        ivoice
        iratio
        imode
        ithd
        ifn
        ipshift
        igskip
        igskip_os
        ilength
        kgap
        igap_os
        kgsize
        igsize_os
        iatt
        idec
        `withDs` [iseed, ipitch1, ipitch2, ipitch3, ipitch4]

---------------------------------------------------------
-- syncgrain

{- | Secondary parameters for syncgrain.

* @Win@ -- grain window function (half-sine is used by default)

* @Overlap@ -- grain overlap (use values in range 0 to 100, the 25 is default)
-}
data SyncgrainSpec = SyncgrainSpec
  { syncgrainWin :: Tab
  , syncgrainOverlap :: D
  }

-- | Randomized parameters for arguments (in range 0 to 1).
data RndSyncgrainSpec = RndSyncgrainSpec
  { rndSyncTimescale :: Sig
  , rndSyncgrainPitch :: Sig
  , rndSyncgrainGrainDur :: Sig
  }

instance Default SyncgrainSpec where
  def =
    SyncgrainSpec
      { syncgrainWin = setSize 16384 $ sines3 [(0.5, 1, 0)]
      , syncgrainOverlap = 25
      }

instance Default RndSyncgrainSpec where
  def =
    RndSyncgrainSpec
      { rndSyncTimescale = 0.5
      , rndSyncgrainPitch = 0.51
      , rndSyncgrainGrainDur = 0.2
      }

{- | Synchronous granular synthesis.

syncgrain implements synchronous granular synthesis.
The source sound for the grains is obtained by reading
a function table containing the samples of the source waveform.
For sampled-sound sources, GEN01 is used. syncgrain will accept
deferred allocation tables.

> syncgrain spec graidDuration timeScale PitchSig ftab

* @spec@ - secondary params (use @def@ to get the defaults)

* @graidDuration@ - duration of grains in seconds.

* @timeScale@ - tempo scaling factor.

* @PitchSig@ - pitch scaling factor.

* @ftab@ - table with sampled sound.
-}
syncgrain :: SyncgrainSpec -> GrainSize -> TempoSig -> PitchSig -> Tab -> Sig
syncgrain spec kgrdur ktimescale kpitch ftab = mul 0.2 res
  where
    kgroverlap = sig $ (syncgrainOverlap spec) / 2

    ko1 = int' (kgroverlap + 0.15)
    kfr = ko1 / kgrdur
    kps = 1 / ko1

    res = csdSyncgrain 1 kfr kpitch kgrdur (kps * ktimescale) ftab (syncgrainWin spec) (syncgrainOverlap spec)

-- | The syncgrain with randomized parameters.
rndSyncgrain :: RndSyncgrainSpec -> SyncgrainSpec -> GrainSize -> TempoSig -> PitchSig -> Tab -> SE Sig
rndSyncgrain rndSpec spec kgrdur ktimescale kpitch ftab = do
  rndSyncGrainDur <- rnd (rndSyncgrainGrainDur rndSpec)
  rndSyncGrainPitch <- birnd (rndSyncgrainPitch rndSpec)
  rndSyncGrainTimescale <- birnd (rndSyncTimescale rndSpec)
  let
    kgroverlap = sig $ (syncgrainOverlap spec) / 2
    ko1 = int' (kgroverlap + 0.15)
    kgr = kgrdur + rndSyncGrainDur
    kfr = ko1 / kgr
    kps = 1 / ko1

    res = csdSyncgrain 1 kfr (kpitch + rndSyncGrainPitch) kgr (kps * ktimescale + rndSyncGrainTimescale) ftab (syncgrainWin spec) (syncgrainOverlap spec)
  return res

{- | syncgrain that is defined on stereo audio files. We provide the filename instead of table.
The rest is the same.
-}
syncgrainSnd :: SyncgrainSpec -> GrainSize -> TempoSig -> PitchSig -> String -> Sig2
syncgrainSnd spec kgrdur ktimescale kpitch file =
  (f $ wavs file 0 WavLeft, f $ wavs file 0 WavRight)
  where
    f = syncgrain spec kgrdur ktimescale kpitch

{- | syncgrain that is defined on mono audio files. We provide the filename instead of table.
The rest is the same.
-}
syncgrainSnd1 :: SyncgrainSpec -> GrainSize -> TempoSig -> PitchSig -> String -> Sig
syncgrainSnd1 spec kgrdur ktimescale kpitch file = f $ wavs file 0 WavLeft
  where
    f = syncgrain spec kgrdur ktimescale kpitch

{- | rndSyncgrain that is defined on stereo audio files. We provide the filename instead of table.
The rest is the same.
-}
rndSyncgrainSnd :: RndSyncgrainSpec -> SyncgrainSpec -> GrainSize -> TempoSig -> PitchSig -> String -> SE Sig2
rndSyncgrainSnd rndSpec spec kgrdur ktimescale kpitch file = do
  aleft <- f $ wavs file 0 WavLeft
  aright <- f $ wavs file 0 WavRight
  return (aleft, aright)
  where
    f = rndSyncgrain rndSpec spec kgrdur ktimescale kpitch

{- | rndSyncgrain that is defined on mono audio files. We provide the filename instead of table.
The rest is the same.
-}
rndSyncgrainSnd1 :: RndSyncgrainSpec -> SyncgrainSpec -> GrainSize -> TempoSig -> PitchSig -> String -> SE Sig
rndSyncgrainSnd1 rndSpec spec kgrdur ktimescale kpitch file = f $ wavs file 0 WavLeft
  where
    f = rndSyncgrain rndSpec spec kgrdur ktimescale kpitch

-------------------------------------------------------
-- sndwarp

{- | Sndwarp secondary parameters. It's instance of @Default@, we can use the constant @def@ to get the value.

* @WinSize@ - window size in seconds (not in samples as in Csound!). The default is 0.1

* @Randw@ -  the bandwidth of a random number generator.
   The random numbers will be added to iwsize. It's measured in ratio to WinSize.
   So the 1 means the one WinSize length. The default is 0.3

* @Overlap@  - determines the density of overlapping windows. The default value is 50.
 It's in range (0 to 100)
-}
data SndwarpSpec = SndwarpSpec
  { sndwarpWinSize :: D
  , sndwarpRandw :: D
  , sndwarpOvelrap :: D
  , sndwarpWin :: Tab
  }

instance Default SndwarpSpec where
  def =
    SndwarpSpec
      { sndwarpWinSize = 0.1
      , sndwarpRandw = 0.3
      , sndwarpOvelrap = 50
      , sndwarpWin = setSize 16384 $ sines3 [(0.5, 1, 0)]
      }

{- | Simple sndwarp with scaling mode (corresponds to Csound's @initmode == 0@).

> sndwarp spec resample speed ftab

* @spec@ - secondary params (use @def@ to get the defaults)

* @resample@ -  the factor by which to change the pitch of the sound. For example, a value of 2 will produce a
    sound one octave higher than the original. The timing of the sound, however, will not be altered.

* @speed@  - the factor by which to change the tempo of the sound.

* @ftab@ -- table with the samples
-}
sndwarp :: SndwarpSpec -> TempoSig -> PitchSig -> Tab -> Sig
sndwarp spec kspeed xresample ftab =
  mul 0.2 $
    csdSndwarp
      1
      kspeed
      xresample
      ftab
      0
      (getSampleRate * sndwarpWinSize spec)
      (getSampleRate * sndwarpRandw spec)
      (sndwarpOvelrap spec)
      (sndwarpWin spec)
      0

-- | Stereo version of the @sndwarp@.
sndwarpst :: SndwarpSpec -> TempoSig -> PitchSig -> Tab -> Sig2
sndwarpst spec xspeed xresample ftab =
  mul 0.2 $
    csdSndwarpst
      1
      xspeed
      xresample
      ftab
      0
      (getSampleRate * sndwarpWinSize spec)
      (getSampleRate * sndwarpRandw spec)
      (sndwarpOvelrap spec)
      (sndwarpWin spec)
      0

{- | Sndwarp that is defined on stereo audio files. We provide the filename instead of table.
The rest is the same.
-}
sndwarpSnd :: SndwarpSpec -> TempoSig -> PitchSig -> String -> Sig2
sndwarpSnd spec kspeed xresample file = sndwarpst spec kspeed xresample (wavs file 0 WavAll)

{- | Sndwarp that is defined on mono audio files. We provide the filename instead of table.
The rest is the same.
-}
sndwarpSnd1 :: SndwarpSpec -> TempoSig -> PitchSig -> String -> Sig
sndwarpSnd1 spec kspeed xresample file = sndwarp spec kspeed xresample (wavs file 0 WavLeft)

{- | The simple sndwarp with pointer (Csound @initmode = 1@).

> sndwarp spec resample ftab ptr

* @spec@ - secondary params (use @def@ to get the defaults)

* @resample@ -  the factor by which to change the pitch of the sound. For example, a value of 2 will produce a
    sound one octave higher than the original. The timing of the sound, however, will not be altered.

* @ftab@ -- table with the samples

* @ptr@  - pointer to read the table (in seconds).
-}
ptrSndwarp :: SndwarpSpec -> PitchSig -> Tab -> Pointer -> Sig
ptrSndwarp spec xresample ftab xptr =
  mul 0.2 $
    csdSndwarp
      1
      xptr
      xresample
      ftab
      0
      (getSampleRate * sndwarpWinSize spec)
      (getSampleRate * sndwarpRandw spec)
      (sndwarpOvelrap spec)
      (sndwarpWin spec)
      1

-- | Stereo version of @ptrSndwarp@.
ptrSndwarpst :: SndwarpSpec -> PitchSig -> Tab -> Pointer -> Sig2
ptrSndwarpst spec xresample ftab xptr =
  csdSndwarpst
    1
    xptr
    xresample
    ftab
    0
    (getSampleRate * sndwarpWinSize spec)
    (getSampleRate * sndwarpRandw spec)
    (sndwarpOvelrap spec)
    (sndwarpWin spec)
    1

{- | ptrSndwarp that is defined on stereo audio files. We provide the filename instead of table.
The rest is the same.
-}
ptrSndwarpSnd :: SndwarpSpec -> PitchSig -> String -> Pointer -> Sig2
ptrSndwarpSnd spec xresample file xptr = ptrSndwarpst spec xresample (wavs file 0 WavAll) xptr

{- | ptrSndwarp that is defined on mono audio files. We provide the filename instead of table.
The rest is the same.
-}
ptrSndwarpSnd1 :: SndwarpSpec -> PitchSig -> String -> Pointer -> Sig
ptrSndwarpSnd1 spec xresample file xptr = ptrSndwarp spec xresample (wavs file 0 WavLeft) xptr

------------------------------------------------------------------------
-- fof2

-- | Defaults for @fof2@ opcode.
data Fof2Spec = Fof2Spec
  { fof2TimeMod :: Sig
  , fof2PitchMod :: Sig
  , fof2Oct :: Sig
  , fof2Band :: Sig
  , fof2Rise :: Sig
  , fof2Decay :: Sig
  , fof2Gliss :: Sig
  , fof2Win :: Tab
  }

instance Default Fof2Spec where
  def =
    Fof2Spec
      { fof2TimeMod = 0.2
      , fof2PitchMod = 0
      , fof2Oct = 0
      , fof2Band = 0
      , fof2Rise = 0.5
      , fof2Decay = 0.5
      , fof2Gliss = 0
      , fof2Win = setSize 8192 $ sines4 [(0.5, 1, 270, 1)]
      }

-- | Reimplementation of fof2 opcode for stereo  audio files.
fof2Snd :: Fof2Spec -> GrainRate -> GrainSize -> TempoSig -> String -> Sig2
fof2Snd spec kgrainrate kgrainsize kspeed file = (f 1, f 2)
  where
    f n = fof2Chn n spec kgrainrate kgrainsize kspeed file

-- | Reimplementation of fof2 opcode for mono audio files.
fof2Snd1 :: Fof2Spec -> GrainRate -> GrainSize -> TempoSig -> String -> Sig
fof2Snd1 spec kgrainrate kgrainsize kspeed file = f 1
  where
    f n = fof2Chn n spec kgrainrate kgrainsize kspeed file

fof2Chn :: Int -> Fof2Spec -> GrainRate -> GrainSize -> TempoSig -> String -> Sig
fof2Chn n spec kgrainrate kgrainsize kspeed file =
  fof2 spec kgrainrate kgrainsize (grainyTab n file) (grainyPtr kspeed file)

-- | Reimplementation of fof2 opcode.
fof2 :: Fof2Spec -> GrainRate -> GrainSize -> Tab -> Pointer -> Sig
fof2 spec grainRate grainSize buf kphs = go (ftlen buf)
  where
    kfund = grainRate
    kris = fof2Rise spec
    kdec = fof2Decay spec
    kband = fof2Band spec
    koct = fof2Oct spec
    kgliss = fof2Gliss spec

    go tabLen = do
      csdFof2
        (ampdbfs (-8))
        kfund
        kform
        koct
        kband
        (kris * kdur)
        kdur
        (kdec * kdur)
        100
        giLive
        giSigRise
        86400
        kphs
        kgliss
      where
        kdur = grainSize / kfund
        kform = (sig $ getSampleRate / tabLen)
        giSigRise = fof2Win spec
        giLive = buf

------------------------------------------------------------------------
-- granular effects

-- partikkelDelay :: PartikkelSpec -> D -> Sig -> GrainRate -> GrainSize -> Sig -> Sig -> SE Sig
-- partikkelDelay spec maxLength delTim

{- | Granular delay effect for fof2. Good values for grain rate and size are

> grainRate = 25
> grainSize = 2.5
-}
fofDelay :: MaxDelayTime -> DelayTime -> Feedback -> Balance -> Fof2Spec -> GrainRate -> GrainSize -> Sig -> SE Sig
fofDelay maxLength delTim kfeed kbalance spec grainRate grainSize asig = do
  rndTmod <- rnd31 kTmod 1
  rndFmod <- rnd31 kFmod 1
  tabDelay (go rndFmod) maxLength (delTim + rndTmod) kfeed kbalance asig
  where
    kTmod = fof2TimeMod spec
    kFmod = fof2PitchMod spec
    kfund = grainRate
    kris = fof2Rise spec
    kdec = fof2Decay spec
    kband = fof2Band spec
    koct = fof2Oct spec
    kgliss = fof2Gliss spec

    tabLen = tabSizeSecondsPower2 maxLength

    go :: Sig -> Tab -> Sig -> SE Sig
    go kFmodSig buf kphs = do
      return $
        csdFof2
          (ampdbfs (-8))
          kfund
          kform
          koct
          kband
          (kris * kdur)
          kdur
          (kdec * kdur)
          100
          giLive
          giSigRise
          86400
          kphs
          kgliss
      where
        kdur = grainSize / kfund
        kform = (1 + kFmodSig) * (sig $ getSampleRate / tabLen)

        giSigRise = fof2Win spec
        giLive = buf

-- | Granular delay effect for @grainy@.
grainyDelay :: MaxDelayTime -> DelayTime -> Feedback -> Balance -> GrainRate -> GrainSize -> PitchSig -> Sig -> SE Sig
grainyDelay maxDel delTime kfeed kbalance grainRate grainSize pitch asig = tabDelay go maxDel delTime kfeed kbalance asig
  where
    go tab ptr = return $ ptrGrainy grainRate grainSize pitch tab ptr

-- | Granular delay effect for @rndGrainy@.
rndGrainyDelay :: MaxDelayTime -> DelayTime -> Feedback -> Balance -> RndGrainySpec -> GrainRate -> GrainSize -> PitchSig -> Sig -> SE Sig
rndGrainyDelay maxDel delTime kfeed kbalance spec grainRate grainSize pitch asig = tabDelay go maxDel delTime kfeed kbalance asig
  where
    go = rndPtrGrainy spec grainRate grainSize pitch

-- | Granular delay effect for @sndwarp@.
sndwarpDelay :: MaxDelayTime -> DelayTime -> Feedback -> Balance -> SndwarpSpec -> PitchSig -> Sig -> SE Sig
sndwarpDelay maxDel delTime kfeed kbalance spec pitch asig = tabDelay go maxDel delTime kfeed kbalance asig
  where
    go tab ptr = return $ ptrSndwarp spec pitch tab (sec2rel tab ptr)

-- | Granular delay effect for @syncgrain@.
syncgrainDelay :: MaxDelayTime -> DelayTime -> Feedback -> Balance -> SyncgrainSpec -> GrainSize -> TempoSig -> PitchSig -> Sig -> SE Sig
syncgrainDelay maxDel delTime kfeed kbalance spec grainSize tempo pitch asig = tabDelay go maxDel delTime kfeed kbalance asig
  where
    go tab _ = return $ syncgrain spec grainSize tempo pitch tab

-- | Granular delay effect for @rndSyncgrain@.
rndSyncgrainDelay :: MaxDelayTime -> DelayTime -> Feedback -> Balance -> RndSyncgrainSpec -> SyncgrainSpec -> GrainSize -> TempoSig -> PitchSig -> Sig -> SE Sig
rndSyncgrainDelay maxDel delTime kfeed kbalance rndSpec spec grainSize tempo pitch asig = tabDelay go maxDel delTime kfeed kbalance asig
  where
    go tab _ = rndSyncgrain rndSpec spec grainSize tempo pitch tab

-- | Granular delay effect for @partikkel@.
partikkelDelay :: MaxDelayTime -> DelayTime -> Feedback -> Balance -> PartikkelSpec -> GrainRate -> GrainSize -> PitchSig -> Sig -> SE Sig
partikkelDelay maxDel delTime kfeed kbalance spec grainRate grainSize pitch asig = tabDelay go maxDel delTime kfeed kbalance asig
  where
    go tab ptr = return $ partikkel spec grainRate grainSize pitch [tab] [ptr]

-------------------------------------------------------------------------
-- effects

fxFeed :: Feedback
fxFeed = 0

fxBalance :: Balance
fxBalance = 1

fxMaxLength :: MaxDelayTime
fxMaxLength = 1

fxDelTime :: DelayTime
fxDelTime = 0.05

type GrainDelay a = MaxDelayTime -> DelayTime -> Feedback -> Balance -> a

toGrainFx :: GrainDelay a -> a
toGrainFx f = f fxMaxLength fxDelTime fxFeed fxBalance

-- | Granular effect for @grainy@.
grainyFx :: GrainRate -> GrainSize -> PitchSig -> Sig -> SE Sig
grainyFx = toGrainFx grainyDelay

-- | Granular effect for @rndGrainy@.
rndGrainyFx :: RndGrainySpec -> GrainRate -> GrainSize -> PitchSig -> Sig -> SE Sig
rndGrainyFx = toGrainFx rndGrainyDelay

-- | Granular effect for @sndwarp@.
sndwarpFx :: SndwarpSpec -> PitchSig -> Sig -> SE Sig
sndwarpFx = toGrainFx sndwarpDelay

-- | Granular effect for @syncgrain@.
syncgrainFx :: SyncgrainSpec -> GrainSize -> TempoSig -> PitchSig -> Sig -> SE Sig
syncgrainFx = toGrainFx syncgrainDelay

-- | Granular effect for @rndSyncgrain@.
rndSyncgrainFx :: RndSyncgrainSpec -> SyncgrainSpec -> GrainSize -> TempoSig -> PitchSig -> Sig -> SE Sig
rndSyncgrainFx = toGrainFx rndSyncgrainDelay

-- | Granular effect for @partikkel@.
partikkelFx :: PartikkelSpec -> GrainRate -> GrainSize -> PitchSig -> Sig -> SE Sig
partikkelFx = toGrainFx partikkelDelay

-- | Granular effect for @fof2@.
fofFx :: Fof2Spec -> GrainRate -> GrainSize -> Sig -> SE Sig
fofFx = toGrainFx fofDelay

------------------------------------------------------------------------
------------------------------------------------------------------------
-- csound opcodes

{- |
Granular synthesizer with "per grain" control
      over many of its parameters.  Has a sync input to
      sychronize its internal grain scheduler clock to an external
      clock source.

partikkel was conceived after reading Curtis Roads' book
      "Microsound", and the goal was to create an opcode that was
      capable of all time-domain varieties of granular synthesis
      described in this book. The idea being that most of the
      techniques only differ in parameter values, and by having a
      single opcode that can do all varieties of granular synthesis
      makes it possible to interpolate between techniques. Granular synthesis is sometimes dubbed particle
      synthesis, and it was thought apt to name the opcode partikkel
      to distinguish it from other granular opcodes.

> a1 [, a2, a3, a4, a5, a6, a7, a8]  partikkel  agrainfreq, \
>                   kdistribution, idisttab, async, kenv2amt, ienv2tab, ienv_attack, \
>                   ienv_decay, ksustain_amount, ka_d_ratio, kduration, kamp, igainmasks, \
>                   kwavfreq, ksweepshape, iwavfreqstarttab, iwavfreqendtab, awavfm, \
>                   ifmamptab, kfmenv, icosine, ktraincps, knumpartials, kchroma, \
>                   ichannelmasks, krandommask, kwaveform1, kwaveform2, kwaveform3, \
>                   kwaveform4, iwaveamptab, asamplepos1, asamplepos2, asamplepos3, \
>                   asamplepos4, kwavekey1, kwavekey2, kwavekey3, kwavekey4, imax_grains \
>                   [, iopcode_id]

csound doc: <http://www.csounds.com/manual/html/partikkel.html>
-}
csdPartikkel :: (Tuple a) => Sig -> Sig -> Tab -> Sig -> Sig -> Tab -> Tab -> Tab -> Sig -> Sig -> Sig -> Sig -> Tab -> Sig -> Sig -> Tab -> Tab -> Sig -> Tab -> Tab -> Tab -> Sig -> Sig -> Sig -> Tab -> Sig -> Tab -> Tab -> Tab -> Tab -> Tab -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> a
csdPartikkel b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31 b32 b33 b34 b35 b36 b37 b38 b39 b40 = pureTuple $ f <$> unSig b1 <*> unSig b2 <*> unTab b3 <*> unSig b4 <*> unSig b5 <*> unTab b6 <*> unTab b7 <*> unTab b8 <*> unSig b9 <*> unSig b10 <*> unSig b11 <*> unSig b12 <*> unTab b13 <*> unSig b14 <*> unSig b15 <*> unTab b16 <*> unTab b17 <*> unSig b18 <*> unTab b19 <*> unTab b20 <*> unTab b21 <*> unSig b22 <*> unSig b23 <*> unSig b24 <*> unTab b25 <*> unSig b26 <*> unTab b27 <*> unTab b28 <*> unTab b29 <*> unTab b30 <*> unTab b31 <*> unSig b32 <*> unSig b33 <*> unSig b34 <*> unSig b35 <*> unSig b36 <*> unSig b37 <*> unSig b38 <*> unSig b39 <*> unD b40
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 =
      mopcs "partikkel" ([Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar], [Ar, Kr, Ir, Ar, Kr, Ir, Ir, Ir, Kr, Kr, Kr, Kr, Ir, Kr, Kr, Ir, Ir, Ar, Ir, Kr, Ir, Kr, Kr, Kr, Ir, Kr, Kr, Kr, Kr, Kr, Ir, Ar, Ar, Ar, Ar, Kr, Kr, Kr, Kr, Ir, Ir]) [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40]

{- |
Synchronous granular synthesis.

syncgrain implements synchronous granular synthesis. The source sound for the
grains is obtained by reading a function table containing the samples of the source waveform.
For sampled-sound sources, GEN01 is used.
syncgrain will accept deferred allocation tables.

> asig  syncgrain  kamp, kfreq, kpitch, kgrsize, kprate, ifun1, \
>           ifun2, iolaps

csound doc: <http://www.csounds.com/manual/html/syncgrain.html>
-}
csdSyncgrain :: Sig -> Sig -> Sig -> Sig -> Sig -> Tab -> Tab -> D -> Sig
csdSyncgrain b1 b2 b3 b4 b5 b6 b7 b8 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5 <*> unTab b6 <*> unTab b7 <*> unD b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 = opcs "syncgrain" [(Ar, [Kr, Kr, Kr, Kr, Kr, Ir, Ir, Ir])] [a1, a2, a3, a4, a5, a6, a7, a8]

{- |
A more complex granular synthesis texture generator.

The granule unit generator is more complex than grain, but does add new possibilities.

> ares  granule  xamp, ivoice, iratio, imode, ithd, ifn, ipshift, igskip, \
>           igskip_os, ilength, kgap, igap_os, kgsize, igsize_os, iatt, idec \
>           [, iseed] [, ipitch1] [, ipitch2] [, ipitch3] [, ipitch4] [, ifnenv]

csound doc: <http://www.csounds.com/manual/html/granule.html>
-}
csdGranule :: Sig -> D -> D -> D -> D -> Tab -> D -> D -> D -> D -> Sig -> D -> Sig -> D -> D -> D -> Sig
csdGranule = C.granule

{- |
Reads a mono sound sample from a table and applies time-stretching and/or pitch modification.

sndwarp reads sound samples from a table and applies time-stretching and/or pitch modification. Time and frequency modification are independent from one another. For example, a sound can be stretched in time while raising the pitch!

> ares [, ac]  sndwarp  xamp, xtimewarp, xresample, ifn1, ibeg, iwsize, \
>           irandw, ioverlap, ifn2, itimemode

csound doc: <http://www.csounds.com/manual/html/sndwarp.html>
-}
csdSndwarp :: Sig -> Sig -> Sig -> Tab -> D -> D -> D -> D -> Tab -> D -> Sig
csdSndwarp = C.sndwarp

{- |
Reads a stereo sound sample from a table and applies time-stretching and/or pitch modification.

sndwarpst reads stereo sound samples from a table and applies time-stretching and/or pitch modification. Time and frequency modification are independent from one another. For example, a sound can be stretched in time while raising the pitch!

> ar1, ar2 [,ac1] [, ac2]  sndwarpst  xamp, xtimewarp, xresample, ifn1, \
>           ibeg, iwsize, irandw, ioverlap, ifn2, itimemode

csound doc: <http://www.csounds.com/manual/html/sndwarpst.html>
-}
csdSndwarpst :: Sig -> Sig -> Sig -> Tab -> D -> D -> D -> D -> Tab -> D -> Sig2
csdSndwarpst = C.sndwarpst

{- |
Produces sinusoid bursts including k-rate incremental indexing with each successive burst.

Audio output is a succession of sinusoid bursts initiated at frequency xfund with a spectral peak at xform. For xfund above 25 Hz these bursts produce a speech-like formant with spectral characteristics determined by the k-input parameters. For lower fundamentals this generator provides a special form of granular synthesis.

> ares  fof2  xamp, xfund, xform, koct, kband, kris, kdur, kdec, iolaps, \
>           ifna, ifnb, itotdur, kphs, kgliss [, iskip]

csound doc: <http://www.csounds.com/manual/html/fof2.html>
-}
csdFof2 :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> Tab -> Tab -> D -> Sig -> Sig -> Sig
csdFof2 = C.fof2
