-- | Band-limited oscillators
module Csound.Typed.Control.Vco(
    saw, isaw, pulse, tri, sqr, blosc,
    saw', isaw', pulse', tri', sqr', blosc',

    -- * Hard sync 
    SyncSmooth(..),

    sawSync, isawSync, pulseSync, triSync, sqrSync, bloscSync,
    sawSync', isawSync', pulseSync', triSync', sqrSync', bloscSync',

    -- ** Hard sync with absolute frequency for slave oscillator
    sawSyncAbs, isawSyncAbs, pulseSyncAbs, triSyncAbs, sqrSyncAbs, bloscSyncAbs,
    sawSyncAbs', isawSyncAbs', pulseSyncAbs', triSyncAbs', sqrSyncAbs', bloscSyncAbs',

    -- ** Hard sync with custom smoothing algorythm
    sawSyncBy, isawSyncBy, pulseSyncBy, triSyncBy, sqrSyncBy, bloscSyncBy,
    sawSyncBy', isawSyncBy', pulseSyncBy', triSyncBy', sqrSyncBy', bloscSyncBy',

    -- ** Hard sync with absolute frequency for slave oscillator
    sawSyncAbsBy, isawSyncAbsBy, pulseSyncAbsBy, triSyncAbsBy, sqrSyncAbsBy, bloscSyncAbsBy,
    sawSyncAbsBy', isawSyncAbsBy', pulseSyncAbsBy', triSyncAbsBy', sqrSyncAbsBy', bloscSyncAbsBy'


) where

import Data.Default

import Csound.Dynamic(Gen(..), GenId(..))
import Csound.Typed.GlobalState
import Csound.Typed.Types

import Csound.Typed.GlobalState

--------------------------------------------------------------
-- no phase

-- | A sawtooth.
saw :: Sig -> Sig
saw = noPhaseWave Saw

-- | Integrated sawtooth: 4 * x * (1 - x).
isaw :: Sig -> Sig
isaw = noPhaseWave IntegratedSaw

-- | A triangle wave.
tri :: Sig -> Sig
tri = noPhaseWave Triangle

-- | Pulse (not normalized).
pulse :: Sig -> Sig 
pulse = noPhaseWave Pulse

-- | A square wave.
sqr :: Sig -> Sig
sqr = noPhaseWave Square

-- | A band-limited oscillator with user defined waveform (it's stored in the table).
blosc :: Tab -> Sig -> Sig
blosc tab cps = hideGE $ do
    gen <- fromPreTab $ getPreTabUnsafe "blosc: tab should be primitive, not an expression." tab
    return $ noPhaseWave (UserGen gen) cps

--------------------------------------------------------------
-- with phase

-- | A sawtooth.
saw' :: D -> Sig -> Sig
saw' = withPhaseWave Saw

-- | Integrated sawtooth: 4 * x * (1 - x).
isaw' :: D -> Sig -> Sig
isaw' = withPhaseWave IntegratedSaw

-- | A triangle wave.
tri' :: D -> Sig -> Sig
tri' = withPhaseWave Triangle

-- | Pulse (not normalized).
pulse' :: D -> Sig -> Sig 
pulse' = withPhaseWave Pulse

-- | A square wave.
sqr' :: D -> Sig -> Sig
sqr' = withPhaseWave Square

-- | A band-limited oscillator with user defined waveform (it's stored in the table).
blosc' :: Tab -> D -> Sig -> Sig
blosc' tab phs cps = hideGE $ do
    gen <- fromPreTab $ getPreTabUnsafe "blosc: tab should be primitive, not an expression." tab
    return $ withPhaseWave (UserGen gen) phs cps

--------------------------------------------------------------

noPhaseWave :: BandLimited -> Sig -> Sig
noPhaseWave waveType cps = fromGE $ do
    expr <- toGE cps
    waveId <- saveBandLimitedWave waveType
    return $ readBandLimited Nothing waveId expr

withPhaseWave :: BandLimited -> D -> Sig -> Sig
withPhaseWave waveType phs cps = fromGE $ do
    expr <- toGE cps
    phsExpr <- toGE phs
    waveId <- saveBandLimitedWave waveType
    return $ readBandLimited (Just phsExpr) waveId expr

--------------------------------------------------------------
-- no phase relative sync

relativeSync :: (Sig -> Sig -> Sig) -> (Sig -> Sig -> Sig)
relativeSync f ratioCps masterCps = f (ratioCps * masterCps) masterCps

-- | Sawtooth oscillator with hard-sync.
-- The first argument is a ration between slave and master oscillators.
-- 
-- > sawSync ratio cps
sawSync :: Sig -> Sig -> Sig
sawSync = relativeSync sawSyncAbs

-- | Integrated sawtooth oscillator with hard-sync.
-- The first argument is a ration between slave and master oscillators.
-- 
-- > isawSync ratio cps
isawSync :: Sig -> Sig -> Sig
isawSync = relativeSync isawSyncAbs


-- | Triangle oscillator with hard-sync.
-- The first argument is a ration between slave and master oscillators.
-- 
-- > triSync ratio cps
triSync :: Sig -> Sig -> Sig
triSync = relativeSync triSyncAbs

-- | Pulse oscillator with hard-sync.
-- The first argument is a ration between slave and master oscillators.
-- 
-- > pulseSync ratio cps
pulseSync :: Sig -> Sig -> Sig
pulseSync = relativeSync pulseSyncAbs

-- | Square oscillator with hard-sync.
-- The first argument is a ration between slave and master oscillators.
-- 
-- > sqrSync ratio cps
sqrSync :: Sig -> Sig -> Sig
sqrSync = relativeSync sqrSyncAbs

-- | Band-limited oscillator with hard-sync.
-- The first argument is a ration between slave and master oscillators.
-- 
-- > bloscSync tab ratio cps
bloscSync :: Tab -> Sig -> Sig -> Sig
bloscSync t = relativeSync (bloscSyncAbs t)

--------------------------------------------------------------


relativeSync' :: (D -> Sig -> Sig -> Sig) -> (D -> Sig -> Sig -> Sig)
relativeSync' f phase ratioCps masterCps = f phase (ratioCps * masterCps) masterCps

-- | Sawtooth oscillator with hard-sync with phase.
-- The second argument is a ration between slave and master oscillators.
-- 
-- > sawSync' phase ratio cps
sawSync' :: D -> Sig -> Sig -> Sig
sawSync' = relativeSync' sawSyncAbs'

-- | Integrated sawtooth oscillator with hard-sync with phase.
-- The second argument is a ration between slave and master oscillators.
-- 
-- > isawSync' phase ratio cps
isawSync' :: D -> Sig -> Sig -> Sig
isawSync' = relativeSync' isawSyncAbs'

-- | Triangle oscillator with hard-sync with phase.
-- The second argument is a ration between slave and master oscillators.
-- 
-- > triSync' phase ratio cps
triSync' :: D -> Sig -> Sig -> Sig
triSync' = relativeSync' triSyncAbs'

-- | Pulse oscillator with hard-sync with phase.
-- The second argument is a ration between slave and master oscillators.
-- 
-- > pulseSync' phase ratio cps
pulseSync' :: D -> Sig -> Sig -> Sig
pulseSync' = relativeSync' pulseSyncAbs'

-- | Square oscillator with hard-sync with phase.
-- The second argument is a ration between slave and master oscillators.
-- 
-- > sqrSync' phase ratio cps
sqrSync' :: D -> Sig -> Sig -> Sig
sqrSync' = relativeSync' sqrSyncAbs'

-- | Band-limited oscillator with hard-sync with phase.
-- The second argument is a ration between slave and master oscillators.
-- 
-- > bloscSync' phase tab ratio cps
bloscSync' :: Tab -> D -> Sig -> Sig -> Sig
bloscSync' t = relativeSync' (bloscSyncAbs' t)

--------------------------------------------------------------
-- no phase relative sync

relativeSyncBy :: (SyncSmooth -> Sig -> Sig -> Sig) -> (SyncSmooth -> Sig -> Sig -> Sig)
relativeSyncBy f smoothType ratioCps masterCps = f smoothType (ratioCps * masterCps) masterCps

-- | Sawtooth oscillator with hard-sync. We can specify the smoothness type.
-- The @ratio@ argument is a ration between slave and master oscillators.
-- 
-- > sawSyncBy spec ratio cps
sawSyncBy :: SyncSmooth -> Sig -> Sig -> Sig
sawSyncBy = relativeSyncBy sawSyncAbsBy

-- | Integrated sawtooth oscillator with hard-sync. We can specify the smoothness type.
-- The first argument is a ration between slave and master oscillators.
-- 
-- > isawSyncB specy ratio cps
isawSyncBy :: SyncSmooth -> Sig -> Sig -> Sig
isawSyncBy = relativeSyncBy isawSyncAbsBy

-- | Triangle oscillator with hard-sync. We can specify the smoothness type.
-- The @ratio@ argument is a ration between slave and master oscillators.
-- 
-- > triSyncBy spec ratio cps
triSyncBy :: SyncSmooth -> Sig -> Sig -> Sig
triSyncBy = relativeSyncBy triSyncAbsBy

-- | Pulse oscillator with hard-sync. We can specify the smoothness type.
-- The @ratio@ argument is a ration between slave and master oscillators.
-- 
-- > pulseSyncBy spec ratio cps
pulseSyncBy :: SyncSmooth -> Sig -> Sig -> Sig
pulseSyncBy = relativeSyncBy pulseSyncAbsBy

-- | Square oscillator with hard-sync. We can specify the smoothness type.
-- The @ratio@ argument is a ration between slave and master oscillators.
-- 
-- > sawSyncBy spec ratio cps
sqrSyncBy :: SyncSmooth -> Sig -> Sig -> Sig
sqrSyncBy = relativeSyncBy sqrSyncAbsBy

-- | Bandlimited table oscillator with hard-sync. We can specify the smoothness type.
-- The @ratio@ argument is a ration between slave and master oscillators.
-- 
-- > bloscSyncBy spec tab ratio cps
bloscSyncBy :: SyncSmooth -> Tab -> Sig -> Sig -> Sig
bloscSyncBy smoothType t = relativeSyncBy (\smoothType -> bloscSyncAbsBy smoothType t) smoothType

------------------------------------------------------------
-- phase

relativeSyncBy' :: (SyncSmooth -> D -> Sig -> Sig -> Sig) -> (SyncSmooth -> D -> Sig -> Sig -> Sig)
relativeSyncBy' f smoothType phase ratioCps masterCps = f smoothType phase (ratioCps * masterCps) masterCps

-- | Sawtooth oscillator with hard-sync with phase. We can specify the smoothness type.
-- The @ratio@ argument is a ration between slave and master oscillators.
-- 
-- > sawSyncBy' spec phase ratio cps
sawSyncBy' :: SyncSmooth -> D -> Sig -> Sig -> Sig
sawSyncBy' = relativeSyncBy' sawSyncAbsBy'

-- | Integrated sawtooth oscillator with hard-sync with phase. We can specify the smoothness type.
-- The @ratio@ argument is a ration between slave and master oscillators.
-- 
-- > isawSyncBy' spec phase ratio cps
isawSyncBy' :: SyncSmooth -> D -> Sig -> Sig -> Sig
isawSyncBy' = relativeSyncBy' isawSyncAbsBy'

-- | Triangle oscillator with hard-sync with phase. We can specify the smoothness type.
-- The @ratio@ argument is a ration between slave and master oscillators.
-- 
-- > triSyncBy' spec phase ratio cps
triSyncBy' :: SyncSmooth -> D -> Sig -> Sig -> Sig
triSyncBy' = relativeSyncBy' triSyncAbsBy'

-- | Pulse oscillator with hard-sync with phase. We can specify the smoothness type.
-- The @ratio@ argument is a ration between slave and master oscillators.
-- 
-- > pulseSyncBy' spec phase ratio cps
pulseSyncBy' :: SyncSmooth -> D -> Sig -> Sig -> Sig
pulseSyncBy' = relativeSyncBy' pulseSyncAbsBy'

-- | Square oscillator with hard-sync with phase. We can specify the smoothness type.
-- The @ratio@ argument is a ration between slave and master oscillators.
-- 
-- > sawSyncBy' spec phase ratio cps
sqrSyncBy' :: SyncSmooth -> D -> Sig -> Sig -> Sig
sqrSyncBy' = relativeSyncBy' sqrSyncAbsBy'

-- | Bandlimited table oscillator with hard-sync with phase. We can specify the smoothness type.
-- The @ratio@ argument is a ration between slave and master oscillators.
-- 
-- > bloscSyncBy' spec phase tab ratio cps
bloscSyncBy' :: SyncSmooth -> Tab -> D -> Sig -> Sig -> Sig
bloscSyncBy' smoothType t = relativeSyncBy' (\smoothType -> bloscSyncAbsBy' smoothType t) smoothType

------------------------------------------------------------

-- | Sawtooth oscillator with hard-sync.
-- The @freq@ argument is an absolute frequency of a slave oscillator.
-- 
-- > sawSyncAbs freq slaveCps masterCps
sawSyncAbs :: Sig -> Sig -> Sig
sawSyncAbs = sawSyncAbsBy def

-- | Integrated sawtooth oscillator with hard-sync.
-- The @freq@ argument is an absolute frequency of a slave oscillator.
-- 
-- > isawSyncAbs freq slaveCps masterCps
isawSyncAbs :: Sig -> Sig -> Sig
isawSyncAbs = isawSyncAbsBy def

-- | Triangle oscillator with hard-sync.
-- The @freq@ argument is an absolute frequency of a slave oscillator.
-- 
-- > triSyncAbs freq slaveCps masterCps
triSyncAbs :: Sig -> Sig -> Sig
triSyncAbs = triSyncAbsBy def

-- | Pulse oscillator with hard-sync.
-- The @freq@ argument is an absolute frequency of a slave oscillator.
-- 
-- > pulseSyncAbs freq slaveCps masterCps
pulseSyncAbs :: Sig -> Sig -> Sig
pulseSyncAbs = pulseSyncAbsBy def

-- | Square oscillator with hard-sync.
-- The @freq@ argument is an absolute frequency of a slave oscillator.
-- 
-- > sqrSyncAbs freq slaveCps masterCps
sqrSyncAbs :: Sig -> Sig -> Sig
sqrSyncAbs = sqrSyncAbsBy def

-- | Bandlimited table oscillator with hard-sync.
-- The @freq@ argument is an absolute frequency of a slave oscillator.
-- 
-- > bloscSyncAbs tab freq slaveCps masterCps
bloscSyncAbs :: Tab -> Sig -> Sig -> Sig
bloscSyncAbs = bloscSyncAbsBy def

-----------------------------------------------------------

-- | Sawtooth oscillator with hard-sync with phase.
-- The @freq@ argument is an absolute frequency of a slave oscillator.
-- 
-- > sawSyncAbs' phase freq slaveCps masterCps
sawSyncAbs' :: D -> Sig -> Sig -> Sig
sawSyncAbs' = sawSyncAbsBy' def

-- | Integrated sawtooth oscillator with hard-sync with phase.
-- The @freq@ argument is an absolute frequency of a slave oscillator.
-- 
-- > isawSyncAbs' phase freq slaveCps masterCps
isawSyncAbs' :: D -> Sig -> Sig -> Sig
isawSyncAbs' = isawSyncAbsBy' def

-- | Triangle oscillator with hard-sync with phase.
-- The @freq@ argument is an absolute frequency of a slave oscillator.
-- 
-- > triSyncAbs' phase freq slaveCps masterCps
triSyncAbs' :: D -> Sig -> Sig -> Sig
triSyncAbs' = triSyncAbsBy' def

-- | Pulse oscillator with hard-sync with phase.
-- The @freq@ argument is an absolute frequency of a slave oscillator.
-- 
-- > pulseSyncAbs' phase freq slaveCps masterCps
pulseSyncAbs' :: D -> Sig -> Sig -> Sig
pulseSyncAbs' = pulseSyncAbsBy' def

-- | Square oscillator with hard-sync with phase.
-- The @freq@ argument is an absolute frequency of a slave oscillator.
-- 
-- > sqrSyncAbs' phase freq slaveCps masterCps
sqrSyncAbs' :: D -> Sig -> Sig -> Sig
sqrSyncAbs' = sqrSyncAbsBy' def

-- | Bandlimited table oscillator with hard-sync with phase.
-- The @freq@ argument is an absolute frequency of a slave oscillator.
-- 
-- > bloscSyncAbs' phase tab freq slaveCps masterCps
bloscSyncAbs' :: Tab -> D -> Sig -> Sig -> Sig
bloscSyncAbs' = bloscSyncAbsBy' def

--------------------------------------------------------------
-- no phase

-- | A hard sync for sawtooth with absolute slave frequency.
--
-- > sawSyncAbs syncType salveCps masterCps
sawSyncAbsBy :: SyncSmooth -> Sig -> Sig -> Sig
sawSyncAbsBy = noPhaseWaveHardSync Saw

-- | A hard sync for integrated sawtooth: 4 * x * (1 - x) with absolute slave frequency.
--
-- > isawSyncAbs syncType salveCps masterCps
isawSyncAbsBy :: SyncSmooth -> Sig -> Sig -> Sig
isawSyncAbsBy = noPhaseWaveHardSync IntegratedSaw

-- | A hard sync for triangle wave with absolute slave frequency.
--
-- > triSyncAbs syncType salveCps masterCps
triSyncAbsBy :: SyncSmooth -> Sig -> Sig -> Sig
triSyncAbsBy = noPhaseWaveHardSync Triangle

-- | A hard sync for pulse wave with absolute slave frequency.
--
-- > pulseSyncAbs syncType salveCps masterCps
pulseSyncAbsBy :: SyncSmooth -> Sig -> Sig -> Sig 
pulseSyncAbsBy = noPhaseWaveHardSync Pulse

-- | A hard sync for square wave with absolute slave frequency.
--
-- > sqrSyncAbs syncType salveCps masterCps
sqrSyncAbsBy :: SyncSmooth -> Sig -> Sig -> Sig
sqrSyncAbsBy = noPhaseWaveHardSync Square

-- | A hard sync for band-limited oscillator with user defined waveform (it's stored in the table) woth absolute frequency.
--
-- > bloscSyncAbs syncType ftable salveCps masterCps
bloscSyncAbsBy :: SyncSmooth -> Tab -> Sig -> Sig -> Sig
bloscSyncAbsBy smoothType tab ratioCps cps = hideGE $ do
    gen <- fromPreTab $ getPreTabUnsafe "blosc: tab should be primitive, not an expression." tab
    return $ noPhaseWaveHardSync (UserGen gen) smoothType ratioCps cps

--------------------------------------------------------------
-- with phase

-- | A sawtooth.
sawSyncAbsBy' :: SyncSmooth -> D -> Sig -> Sig -> Sig
sawSyncAbsBy' = withPhaseWaveHardSync Saw

-- | Integrated sawtooth: 4 * x * (1 - x).
isawSyncAbsBy' :: SyncSmooth -> D -> Sig -> Sig -> Sig
isawSyncAbsBy' = withPhaseWaveHardSync IntegratedSaw

-- | A triangle wave.
triSyncAbsBy' :: SyncSmooth -> D -> Sig -> Sig -> Sig
triSyncAbsBy' = withPhaseWaveHardSync Triangle

-- | Pulse (not normalized).
pulseSyncAbsBy' :: SyncSmooth -> D -> Sig -> Sig -> Sig 
pulseSyncAbsBy' = withPhaseWaveHardSync Pulse

-- | A square wave.
sqrSyncAbsBy' :: SyncSmooth -> D -> Sig -> Sig -> Sig
sqrSyncAbsBy' = withPhaseWaveHardSync Square

-- | A band-limited oscillator with user defined waveform (it's stored in the table).
bloscSyncAbsBy' :: SyncSmooth -> Tab -> D -> Sig -> Sig -> Sig
bloscSyncAbsBy' smoothType tab phs ratioCps cps = hideGE $ do
    gen <- fromPreTab $ getPreTabUnsafe "blosc: tab should be primitive, not an expression." tab
    return $ withPhaseWaveHardSync (UserGen gen) smoothType phs ratioCps cps

-----------------------------------------------

-- | Type of smooth shape to make smooth transitions on retrigger.
-- Available types are: 
--
-- * No smooth: @RawSync@
--
-- * Ramp smooth: @SawSync@
--
-- * Triangular smooth: @TriSync@
--
-- * User defined shape: @UserSync@
data SyncSmooth = RawSync | SawSync | TriSync | TrapSync | UserSync Tab

instance Default SyncSmooth where
    def = TrapSync

getSyncShape :: SyncSmooth -> GE (Maybe BandLimited)
getSyncShape x = case x of
    RawSync -> return $ Nothing
    SawSync -> gen7 4097 [1, 4097, 0]
    TriSync -> gen7 4097 [0, 2048, 1, 2049, 0]
    TrapSync -> gen7 4097 [1, 2048, 1, 2049, 0]
    UserSync tab -> do
        gen <- fromPreTab $ getPreTabUnsafe "blosc: tab should be primitive, not an expression." tab
        return $ Just $ UserGen gen
    where
        gen7 size args = return $ Just $ UserGen $ Gen { genSize = size, genId = IntGenId 7, genArgs = args, genFile = Nothing }

noPhaseWaveHardSync :: BandLimited -> SyncSmooth -> Sig -> Sig -> Sig
noPhaseWaveHardSync waveType smoothWaveType slaveCps cps = fromGE $ do
    smoothWave <- getSyncShape smoothWaveType
    exprSlaveCps <- toGE slaveCps
    exprCps <- toGE cps
    waveId <- saveBandLimitedWave waveType
    smoothWaveId <- case smoothWave of
        Nothing -> return Nothing
        Just wave -> fmap Just $ saveBandLimitedWave wave
    return $ readHardSyncBandLimited smoothWaveId Nothing waveId exprSlaveCps exprCps

withPhaseWaveHardSync :: BandLimited -> SyncSmooth -> D -> Sig -> Sig -> Sig
withPhaseWaveHardSync waveType smoothWaveType phs slaveCps cps = fromGE $ do
    smoothWave <- getSyncShape smoothWaveType
    phsExpr <- toGE phs
    exprSlaveCps <- toGE slaveCps
    exprCps <- toGE cps
    waveId <- saveBandLimitedWave waveType
    smoothWaveId <- case smoothWave of
        Nothing -> return Nothing
        Just wave -> fmap Just $ saveBandLimitedWave wave
    return $ readHardSyncBandLimited smoothWaveId (Just phsExpr) waveId exprSlaveCps exprCps
