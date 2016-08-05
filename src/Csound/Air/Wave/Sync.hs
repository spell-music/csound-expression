-- | Oscillators with hard and soft sync
module Csound.Air.Wave.Sync(
    -- ** With hard sync (band-limited waves)
    SyncSmooth(..),

    sawSync, isawSync, pulseSync, sqrSync, triSync, bloscSync,
    sawSync', isawSync', pulseSync', sqrSync', triSync', bloscSync',

    -- *** hard sync with random phase
    rndSawSync, rndIsawSync, rndPulseSync, rndSqrSync, rndTriSync, rndBloscSync,

    -- *** hard sync with absolute slave frequencies
    sawSyncAbs, isawSyncAbs, pulseSyncAbs, sqrSyncAbs, triSyncAbs, bloscSyncAbs,
    sawSyncAbs', isawSyncAbs', pulseSyncAbs', sqrSyncAbs', triSyncAbs', bloscSyncAbs',


    -- *** hard sync with absolute custom smooth mode
    sawSyncBy, isawSyncBy, pulseSyncBy, sqrSyncBy, triSyncBy, bloscSyncBy,
    sawSyncBy', isawSyncBy', pulseSyncBy', sqrSyncBy', triSyncBy', bloscSyncBy',
    sawSyncAbsBy, isawSyncAbsBy, pulseSyncAbsBy, sqrSyncAbsBy, triSyncAbsBy, bloscSyncAbsBy,
    sawSyncAbsBy', isawSyncAbsBy', pulseSyncAbsBy', sqrSyncAbsBy', triSyncAbsBy', bloscSyncAbsBy',

    
   -- ** With soft sync
    softSync, rawSoftSync, softSyncBy, rawSoftSyncBy,

    -- ** With hard sync (non bandlimited waves)
    rawTriSync, rawSqrSync, rawSawSync, rawPwSync, oscSyncBy

) where

import Data.Default

import Csound.Typed
import Csound.Typed.Opcode hiding (lfo)
import Csound.Tab
import Csound.SigSpace

import Csound.Air.Wave

--------------------------------------------------------------------------
-- hard sync random phase

rndPhsSync :: (D -> Sig -> Sig -> Sig) -> (Sig -> Sig -> SE Sig)
rndPhsSync f ratio cps = fmap (\x -> f x ratio cps) $ rnd 1

rndSawSync :: Sig -> Sig -> SE Sig 
rndSawSync = rndPhsSync sawSync'

rndIsawSync :: Sig -> Sig -> SE Sig
rndIsawSync = rndPhsSync isawSync'

rndPulseSync :: Sig -> Sig -> SE Sig 
rndPulseSync = rndPhsSync pulseSync'

rndSqrSync :: Sig -> Sig -> SE Sig 
rndSqrSync = rndPhsSync sqrSync'

rndTriSync :: Sig -> Sig -> SE Sig
rndTriSync = rndPhsSync triSync'

rndBloscSync :: Tab -> Sig -> Sig -> SE Sig 
rndBloscSync t = rndPhsSync (bloscSync' t)

----------------------------------------------------------
-- Hard-sync for simple non-bandlimited waveforms

-- | Hard-sync with non-bandlimited triangle wave.
rawTriSync :: SyncSmooth -> Sig -> Sig -> Sig
rawTriSync = oscSyncBy triTab

-- | Hard-sync with non-bandlimited square wave.
rawSqrSync :: SyncSmooth -> Sig -> Sig -> Sig
rawSqrSync = oscSyncBy sqrTab

-- | Hard-sync with non-bandlimited sawtooth wave.
rawSawSync :: SyncSmooth -> Sig -> Sig -> Sig
rawSawSync = oscSyncBy sawTab

-- | Hard-sync with non-bandlimited pulse-width wave.
rawPwSync  :: Double -> SyncSmooth -> Sig -> Sig -> Sig
rawPwSync duty = oscSyncBy (pwTab duty)

-- | Hard-sync with non-bandlimited waves.
oscSyncBy :: Tab -> SyncSmooth -> Sig -> Sig -> Sig
oscSyncBy tab smoothType cpsRatio cps = (\smoothFun -> syncOsc smoothFun tab (ar cpsRatio) (ar cps)) $ case smoothType of
    RawSync      -> (\_ _ -> 1)                   
    SawSync      -> (\amaster _ -> (1 - amaster)) 
    TriSync      -> (const $ readSync uniTriTab)  
    TrapSync     -> (const $ readSync uniTrapTab) 
    UserSync gen -> (const $ readSync gen)        
    where
        readSync ft async = table3 async ft `withD` 1        
        
uniSawTab  = setSize 4097 $ elins [1, 0]
uniTriTab  = setSize 4097 $ elins [0, 1, 0]
uniTrapTab = setSize 4097 $ elins [1, 1, 0]

syncOsc smoothFun ftab ratio cps = dcblock $ aout
    where
        (amaster, asyncMaster) = syncphasor cps 0
        (aslave,  asyncSlave)  = syncphasor (cps * ratio) asyncMaster
        aosc = table3 aslave ftab `withD` 1
        aout = aosc * smoothFun amaster asyncMaster


----------------------------------------------------------
-- Soft-sync

softSync :: SigSpace a => (Sig -> a) -> Sig -> (Sig -> a)
softSync = softSyncBy def

rawSoftSync :: SigSpace a => (Sig -> a) -> Sig -> (Sig -> a)
rawSoftSync = rawSoftSyncBy def

softSyncBy :: SigSpace a => SyncSmooth -> (Sig -> a) -> Sig -> (Sig -> a)
softSyncBy = genSoftSync sqr blosc

rawSoftSyncBy :: SigSpace a => SyncSmooth -> (Sig -> a) -> Sig -> (Sig -> a)
rawSoftSyncBy = genSoftSync rawSqr oscBy

genSoftSync :: SigSpace a => (Sig -> Sig) -> (Tab -> Sig -> Sig) -> SyncSmooth -> (Sig -> a) -> Sig -> (Sig -> a)
genSoftSync cpsSwitchWave smoothTabWave smoothType wave ratio cps = flip mul rawSync $ case smoothType of
    RawSync  -> 1
    SawSync  -> smoothTabWave uniSawTab cps
    TriSync  -> smoothTabWave uniTriTab cps
    TrapSync -> smoothTabWave uniTrapTab cps
    UserSync t -> smoothTabWave t cps
    where 
        rawSync = wave (ar $ (ar ratio) * (ar cps) * (ar $ cpsSwitchWave cps)) 

------------------------------------------------------
-- PW and Ramp hard sync

-- pwSync 


