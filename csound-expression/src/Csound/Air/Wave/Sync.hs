-- | Oscillators with hard and soft sync
module Csound.Air.Wave.Sync(
  -- * Hard sync
    SyncSmooth(..),

    sawSync, isawSync, pulseSync, sqrSync, triSync, bloscSync,
    sawSync', isawSync', pulseSync', sqrSync', triSync', bloscSync',

    -- ** With random phase
    rndSawSync, rndIsawSync, rndPulseSync, rndSqrSync, rndTriSync, rndBloscSync,

    -- ** With absolute slave frequencies
    sawSyncAbs, isawSyncAbs, pulseSyncAbs, sqrSyncAbs, triSyncAbs, bloscSyncAbs,
    sawSyncAbs', isawSyncAbs', pulseSyncAbs', sqrSyncAbs', triSyncAbs', bloscSyncAbs',


    -- ** With absolute custom smooth mode
    sawSyncBy, isawSyncBy, pulseSyncBy, sqrSyncBy, triSyncBy, bloscSyncBy,
    sawSyncBy', isawSyncBy', pulseSyncBy', sqrSyncBy', triSyncBy', bloscSyncBy',
    sawSyncAbsBy, isawSyncAbsBy, pulseSyncAbsBy, sqrSyncAbsBy, triSyncAbsBy, bloscSyncAbsBy,
    sawSyncAbsBy', isawSyncAbsBy', pulseSyncAbsBy', sqrSyncAbsBy', triSyncAbsBy', bloscSyncAbsBy',

    -- ** Raw (non-bandlimited) shapes

    -- *** With relative slave frequency
    rawTriSync, rawSqrSync, rawSawSync, rawPwSync,
    rawTriSyncBy, rawSqrSyncBy, rawSawSyncBy, rawPwSyncBy,

    -- *** With absolute slave frequency
  rawTriSyncAbs,  rawSqrSyncAbs, rawSawSyncAbs, rawPwSyncAbs,
    rawTriSyncAbsBy, rawSqrSyncAbsBy, rawSawSyncAbsBy, rawPwSyncAbsBy,

   -- * Soft sync

   -- *** With relative slave frequency
    softSync, rawSoftSync, softSyncBy, rawSoftSyncBy,

    -- *** With absolute slave frequency
    softSyncAbs, rawSoftSyncAbs, softSyncAbsBy, rawSoftSyncAbsBy
) where

import Data.Default

import Csound.Typed
import Csound.Typed.Opcode hiding (lfo, tab)
import Csound.Tab

import Csound.Air.Wave

--------------------------------------------------------------------------
-- hard sync random phase

rndPhsSync :: (D -> Sig -> Sig -> Sig) -> (Sig -> Sig -> SE Sig)
rndPhsSync f ratio cps = fmap (\x -> f x ratio cps) $ rnd 1

-- | Hard sync with saw waveform and randomized phase.
rndSawSync :: Sig -> Sig -> SE Sig
rndSawSync = rndPhsSync sawSync'

-- | Hard sync with integral saw waveform and randomized phase.
rndIsawSync :: Sig -> Sig -> SE Sig
rndIsawSync = rndPhsSync isawSync'

-- | Hard sync with pulse waveform and randomized phase.
rndPulseSync :: Sig -> Sig -> SE Sig
rndPulseSync = rndPhsSync pulseSync'

-- | Hard sync with square waveform and randomized phase.
rndSqrSync :: Sig -> Sig -> SE Sig
rndSqrSync = rndPhsSync sqrSync'

-- | Hard sync with triangle waveform and randomized phase.
rndTriSync :: Sig -> Sig -> SE Sig
rndTriSync = rndPhsSync triSync'

-- | Hard sync with band-limited table waveform waveform and randomized phase.
rndBloscSync :: Tab -> Sig -> Sig -> SE Sig
rndBloscSync t = rndPhsSync (bloscSync' t)

-------------------------------------------------------------------------
-- Hard-sync for simple non-bandlimited waveforms

-- | Hard-sync with non-bandlimited triangle wave.
rawTriSyncBy :: SyncSmooth -> Sig -> Sig -> Sig
rawTriSyncBy = oscSyncBy triTab

-- | Hard-sync with non-bandlimited square wave.
rawSqrSyncBy :: SyncSmooth -> Sig -> Sig -> Sig
rawSqrSyncBy = oscSyncBy sqrTab

-- | Hard-sync with non-bandlimited sawtooth wave.
rawSawSyncBy :: SyncSmooth -> Sig -> Sig -> Sig
rawSawSyncBy = oscSyncBy sawTab

-- | Hard-sync with non-bandlimited pulse-width wave.
rawPwSyncBy  :: Double -> SyncSmooth -> Sig -> Sig -> Sig
rawPwSyncBy duty = oscSyncBy (pwTab duty)

-------------------------------------------------------------------------

-- | Hard-sync with non-bandlimited triangle wave.
rawTriSync :: Sig -> Sig -> Sig
rawTriSync = rawTriSyncBy def

-- | Hard-sync with non-bandlimited square wave.
rawSqrSync :: Sig -> Sig -> Sig
rawSqrSync = rawSqrSyncBy def

-- | Hard-sync with non-bandlimited sawtooth wave.
rawSawSync :: Sig -> Sig -> Sig
rawSawSync = rawSawSyncBy def

-- | Hard-sync with non-bandlimited pulse-width wave.
rawPwSync  :: Double -> Sig -> Sig -> Sig
rawPwSync duty = rawPwSyncBy duty def

-------------------------------------------------------------------------
-- Hard-sync for simple non-bandlimited waveforms with absolute slave frequency

-- | Hard-sync with non-bandlimited triangle wave.
rawTriSyncAbsBy :: SyncSmooth -> Sig -> Sig -> Sig
rawTriSyncAbsBy = oscSyncAbsBy triTab

-- | Hard-sync with non-bandlimited square wave.
rawSqrSyncAbsBy :: SyncSmooth -> Sig -> Sig -> Sig
rawSqrSyncAbsBy = oscSyncAbsBy sqrTab

-- | Hard-sync with non-bandlimited sawtooth wave.
rawSawSyncAbsBy :: SyncSmooth -> Sig -> Sig -> Sig
rawSawSyncAbsBy = oscSyncAbsBy sawTab

-- | Hard-sync with non-bandlimited pulse-width wave.
rawPwSyncAbsBy  :: Double -> SyncSmooth -> Sig -> Sig -> Sig
rawPwSyncAbsBy duty = oscSyncAbsBy (pwTab duty)

-------------------------------------------------------------------------

-- | Hard-sync with non-bandlimited triangle wave.
rawTriSyncAbs :: Sig -> Sig -> Sig
rawTriSyncAbs = rawTriSyncAbsBy def

-- | Hard-sync with non-bandlimited square wave.
rawSqrSyncAbs :: Sig -> Sig -> Sig
rawSqrSyncAbs = rawSqrSyncAbsBy def

-- | Hard-sync with non-bandlimited sawtooth wave.
rawSawSyncAbs :: Sig -> Sig -> Sig
rawSawSyncAbs = rawSawSyncAbsBy def

-- | Hard-sync with non-bandlimited pulse-width wave.
rawPwSyncAbs  :: Double -> Sig -> Sig -> Sig
rawPwSyncAbs duty = rawPwSyncAbsBy duty def


-------------------------------------------------------------------------

oscSyncBy :: Tab -> SyncSmooth -> Sig -> Sig -> Sig
oscSyncBy tab smoothType cpsRatio cps = oscSyncAbsBy tab smoothType (cpsRatio * cps) cps

-- | Hard-sync with non-bandlimited waves.
oscSyncAbsBy :: Tab -> SyncSmooth -> Sig -> Sig -> Sig
oscSyncAbsBy tab smoothType slaveCps cps = (\smoothFun -> syncOsc smoothFun tab (ar slaveCps) (ar cps)) $ case smoothType of
    RawSync         -> (\_ _ -> 1)
    SawSync         -> (\amaster _ -> (1 - amaster))
    TriSync         -> (const $ readSync uniTriTab)
    TrapSync        -> (const $ readSync uniTrapTab)
    UserSync genTab -> (const $ readSync genTab)
    where
        readSync ft async = table3 async ft `withD` 1

uniSawTab, uniTriTab, uniTrapTab :: Tab

uniSawTab  = setSize 4097 $ elins [1, 0]
uniTriTab  = setSize 4097 $ elins [0, 1, 0]
uniTrapTab = setSize 4097 $ elins [1, 1, 0]

syncOsc :: (Sig -> Sig -> Sig) -> Tab -> Sig -> Sig -> Sig
syncOsc smoothFun ftab slaveCps cps = dcblock $ aout
    where
        (amaster, asyncMaster) = syncphasor cps 0
        (aslave,  _asyncSlave)  = syncphasor slaveCps asyncMaster
        aosc = table3 aslave ftab `withD` 1
        aout = aosc * smoothFun amaster asyncMaster

----------------------------------------------------------
-- Soft-sync

-- | Soft sync with given waveform (with band-limited square wave for switch).
-- The soft sync amount is controlled with ratio between master and slave frequencies.
--
-- > softSync slaveWave ratio masterWave
softSync :: SigSpace a => (Sig -> a) -> Sig -> (Sig -> a)
softSync = softSyncBy def

-- | Soft sync with given waveform (with raw square wave for switch). It's faster than `softSync`
-- The soft sync amount is controlled with ratio between master and slave frequencies.
--
-- > rawSoftSync slaveWave ratio masterWave
rawSoftSync :: SigSpace a => (Sig -> a) -> Sig -> (Sig -> a)
rawSoftSync = rawSoftSyncBy def

-- | Soft sync with given waveform (with band-limited square wave for switch).
-- The soft sync amount is controlled with ratio between master and slave frequencies.
-- With first argument we can specify the smoothness algorithm.
--
-- > softSyncBy spec slaveWave ratio masterWave
--
softSyncBy :: SigSpace a => SyncSmooth -> (Sig -> a) -> Sig -> (Sig -> a)
softSyncBy = genSoftSync sqr blosc

-- | Soft sync with given waveform (with raw square wave for switch). It's faster than `softSyncBy`
-- The soft sync amount is controlled with ratio between master and slave frequencies.
-- With first argument we can specify the smoothness algorithm.
--
-- > rawSoftSyncBy spec slaveWave ratio masterWave
--
rawSoftSyncBy :: SigSpace a => SyncSmooth -> (Sig -> a) -> Sig -> (Sig -> a)
rawSoftSyncBy = genSoftSync rawSqr oscBy

----------------------------------------------------------


-- | Soft sync with given waveform (with band-limited square wave for switch).
-- The soft sync amount is controlled with absolute frequency of the slave oscillator.
--
-- > softSyncAbs slaveWave ratio masterWave
softSyncAbs :: SigSpace a => (Sig -> a) -> Sig -> (Sig -> a)
softSyncAbs = softSyncAbsBy def

-- | Soft sync with given waveform (with raw square wave for switch). It's faster than `softSyncAbs`
-- The soft sync amount is controlled with absolute frequency of the slave oscillator.
--
-- > rawSoftSyncAbs slaveWave ratio masterWave
rawSoftSyncAbs :: SigSpace a => (Sig -> a) -> Sig -> (Sig -> a)
rawSoftSyncAbs = rawSoftSyncAbsBy def


-- | Soft sync with given waveform (with band-limited square wave for switch).
-- The soft sync amount is controlled with absolute frequency of the slave oscillator.
-- With first argument we can specify the smoothness algorithm.
--
-- > softSyncAbsBy spec slaveWave ratio masterWave
--
softSyncAbsBy :: SigSpace a => SyncSmooth -> (Sig -> a) -> Sig -> (Sig -> a)
softSyncAbsBy = genSoftSyncAbs sqr blosc

-- | Soft sync with given waveform (with raw square wave for switch). It's faster than `softSyncAbsBy`
-- The soft sync amount is controlled with absolute frequency of the slave oscillator.
-- With first argument we can specify the smoothness algorithm.
--
-- > rawSoftSyncBy spec slaveWave ratio masterWave
--
rawSoftSyncAbsBy :: SigSpace a => SyncSmooth -> (Sig -> a) -> Sig -> (Sig -> a)
rawSoftSyncAbsBy = genSoftSyncAbs rawSqr oscBy

----------------------------------------------------------

genSoftSync :: SigSpace a => (Sig -> Sig) -> (Tab -> Sig -> Sig) -> SyncSmooth -> (Sig -> a) -> Sig -> (Sig -> a)
genSoftSync cpsSwitchWave smoothTabWave smoothType wave ratio cps = genSoftSyncAbs cpsSwitchWave smoothTabWave smoothType wave (ratio * cps) cps

genSoftSyncAbs :: SigSpace a => (Sig -> Sig) -> (Tab -> Sig -> Sig) -> SyncSmooth -> (Sig -> a) -> Sig -> (Sig -> a)
genSoftSyncAbs cpsSwitchWave smoothTabWave smoothType wave slaveCps cps = flip mul rawSync $ case smoothType of
    RawSync  -> 1
    SawSync  -> smoothTabWave uniSawTab cps
    TriSync  -> smoothTabWave uniTriTab cps
    TrapSync -> smoothTabWave uniTrapTab cps
    UserSync t -> smoothTabWave t cps
    where
        rawSync = wave (ar $ ar slaveCps * (ar $ cpsSwitchWave cps))

------------------------------------------------------
-- PW and Ramp hard sync

-- pwSync
