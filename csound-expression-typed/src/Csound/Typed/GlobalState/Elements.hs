{-# Language DeriveFunctor #-}
module Csound.Typed.GlobalState.Elements(
    -- * Identifiers
    IdMap(..), saveId, newIdMapId,
    -- ** Gens
    GenMap, newGen, newGenId, nextGlobalGenCounter, newTabOfGens,
    WriteGenMap, newWriteGen, newWriteTab,
    -- Sf2
    SfFluid(..), SfSpec(..), SfMap, newSf, sfVar, renderSf,
    -- ** Band-limited waveforms
    BandLimited(..), BandLimitedMap(..), BandLimitedId(..),
    saveBandLimited, renderBandLimited,
    readBandLimited, readHardSyncBandLimited,

    -- ** String arguments
    StringMap, newString,
    -- * Midi
    MidiType(..), Channel, MidiMap, MidiKey(..), saveMidiInstr,
    -- * Global variables
    Globals(..), newPersistentGlobalVar, newClearableGlobalVar,
    newPersistentGloabalArrVar,
    renderGlobals, bpmVarName, bpmVar,
    -- * Instruments
    Instrs(..), saveInstr, getInstrIds, -- newInstrId, saveInstrById, saveInstr, CacheName, makeCacheName, saveCachedInstr, getInstrIds,
    -- * Named instruments
    NamedInstrs(..), saveNamedInstr,
    -- * Src
    InstrBody, getIn, sendOut, sendChn, sendGlobal, chnPargId,
    Event(..),
    ChnRef(..), chnRefFromParg, chnRefAlloc, readChn, writeChn, chnUpdateUdo,
    subinstr, subinstr_, event_i, event, safeOut, autoOff, changed,
    -- * OSC listen ports
    OscListenPorts, getOscPortVar,
    -- * Macros inits
    MacrosInits, MacrosInit(..), initMacros,
    -- * Udo plugins
    UdoPlugin, addUdoPlugin, getUdoPluginNames,
    tabQueuePlugin, tabQueue2Plugin,
    zdfPlugin, solinaChorusPlugin, audaciouseqPlugin, adsr140Plugin,
    diodePlugin, korg35Plugin, zeroDelayConvolutionPlugin,
    pitchShifterDelayPlugin,
    analogDelayPlugin, distortionPlugin, envelopeFolollowerPlugin, flangerPlugin, freqShifterPlugin,
    loFiPlugin, panTremPlugin, monoTremPlugin, phaserPlugin, pitchShifterPlugin, reversePlugin,
    ringModulatorPlugin, stChorusPlugin, stereoPingPongDelayPlugin,
    tapeEchoPlugin,
    delay1kPlugin,
    liveRowPlugin, liveRowsPlugin,
    ambiRowPlugin, ambiRowMp3Plugin
) where

import Data.List
import Data.Hashable

import Control.Monad.Trans.State.Strict
import Control.Monad(zipWithM_)
import Data.Default
import qualified Data.Map as M
import qualified Data.IntMap as IM

import Csound.Dynamic.Types
import Csound.Dynamic.Build
import Csound.Dynamic.Build.Numeric()

import Csound.Typed.GlobalState.Opcodes

-- tables of identifiers

data IdMap a = IdMap
    { idMapContent :: M.Map a Int
    , idMapNewId   :: Int
    } deriving (Eq, Ord)

instance Default (IdMap a) where
    def = IdMap def 1

saveId :: Ord a => a -> State (IdMap a) Int
saveId a = state $ \s ->
    case M.lookup a (idMapContent s) of
        Nothing ->
            let newId = idMapNewId s
                s1    = s{ idMapContent = M.insert a newId (idMapContent s)
                         , idMapNewId = succ newId }
            in  (newId, s1)
        Just n  -> (n, s)

newIdMapId :: State (IdMap a) Int
newIdMapId = state $ \s ->
    let newId = idMapNewId s
        s1 = s { idMapNewId = succ newId }
    in  (newId, s1)

-- gens

type GenMap = IdMap Gen

newGen :: Gen -> State GenMap Int
newGen = saveGenId

newTabOfGens :: [Gen] -> State GenMap Int
newTabOfGens = (saveGenId . intTab =<<) . mapM saveGenId
    where intTab ns = Gen (nextPowOfTwo $ length ns) (IntGenId (-2)) (fmap fromIntegral ns) Nothing

nextPowOfTwo :: Int -> Int
nextPowOfTwo n
    | frac == 0 = n
    | otherwise = 2 ^ (integ + 1)
    where
        (integ, frac) = properFraction $ logBase 2 (fromIntegral n)

saveGenId :: Ord a => a -> State (IdMap a) Int
saveGenId a = state $ \s ->
    case M.lookup a (idMapContent s) of
        Nothing ->
            let newId = nextReadOnlyTableId $ idMapNewId s
                s1    = s{ idMapContent = M.insert a newId (idMapContent s)
                         , idMapNewId = nextReadOnlyTableId newId }
            in  (newId, s1)
        Just n  -> (n, s)

newGenId :: State GenMap Int
newGenId = state $ \s ->
    let newId = idMapNewId s
        s1 = s { idMapNewId = nextReadOnlyTableId newId }
    in  (newId, s1)

-- writeable gens

type WriteGenMap = [(Int, Gen)]

newWriteGen :: Gen -> State WriteGenMap E
newWriteGen = fmap int . saveWriteGenId

newWriteTab :: Int -> State WriteGenMap E
newWriteTab = newWriteGen . fromSize
    where fromSize n = Gen n (IntGenId 2) (replicate n 0) Nothing

saveWriteGenId :: Gen -> State WriteGenMap Int
saveWriteGenId a = state $ \s -> case s of
    []         -> (initId, [(initId, a)])
    (i,_):_    ->   let newId = nextWriteTableId i
                    in (newId, (newId, a) : s)
    where
        initId = tableWriteStep

tableWriteStep :: Int
tableWriteStep = 10

nextReadOnlyTableId :: Int -> Int
nextReadOnlyTableId x
    | y `mod` tableWriteStep == 0 = y + 1
    | otherwise                   = y
    where y = x + 1

nextWriteTableId :: Int -> Int
nextWriteTableId x = tableWriteStep + x

-- strings

type StringMap = IdMap String

newString :: String -> State StringMap Prim
newString = fmap PrimInt . saveId

-- gen counter

nextGlobalGenCounter :: State Int Int
nextGlobalGenCounter = state $ \s -> (s, s + 1)

-- sf

data SfFluid = SfFluid
    { sfId   :: Int
    , sfVars :: [Var] }

data SfSpec = SfSpec
    { sfName    :: String
    , sfBank    :: Int
    , sfProgram :: Int
    } deriving (Eq, Ord, Show)

type SfMap = IdMap SfSpec

newSf :: SfSpec -> State SfMap Int
newSf = saveId

sfVar :: Int -> E
sfVar n = readOnlyVar (VarVerbatim Ir $ sfEngineName n)

sfEngineName :: Int -> String
sfEngineName n = "gi_Sf_engine_" ++ show n

sfInstrName :: Int -> String
sfInstrName n = "i_Sf_instr_" ++ show n

renderSf :: Monad m => SfSpec -> Int -> DepT m ()
renderSf (SfSpec name bank prog) n = verbatim $
    engineStr ++ "\n" ++
    loadStr   ++ "\n" ++
    selectProgStr ++ "\n"
    where
        engineStr = engineName ++ " fluidEngine"
        loadStr   = insName ++ " fluidLoad \"" ++ name ++ "\", " ++  engineName ++ ", 1"
        selectProgStr = "fluidProgramSelect " ++ engineName ++ ", 1, " ++ insName
            ++ ", " ++ show bank ++ ", " ++ show prog

        engineName = sfEngineName n
        insName    = sfInstrName n

-- band-limited waveforms (used with vco2init)

data BandLimited = Saw | Pulse | Square | Triangle | IntegratedSaw | UserGen Gen
    deriving (Eq, Ord)

data BandLimitedId = SimpleBandLimitedWave Int | UserBandLimitedWave Int
    deriving (Eq, Ord)

bandLimitedIdToExpr :: BandLimitedId -> E
bandLimitedIdToExpr x = case x of
    SimpleBandLimitedWave simpleId -> int simpleId
    UserBandLimitedWave   userId   -> noRate $ ReadVar $ bandLimitedVar userId

bandLimitedVar userId = Var GlobalVar Ir ("BandLim" ++ show userId)

data BandLimitedMap = BandLimitedMap
    { simpleBandLimitedMap :: M.Map BandLimited BandLimitedId
    , vcoInitMap     :: GenMap
    } deriving (Eq, Ord)

instance Default BandLimitedMap where
    def = BandLimitedMap def def

saveBandLimited :: BandLimited -> State BandLimitedMap BandLimitedId
saveBandLimited x = case x of
    Saw             -> simpleWave 1  0
    IntegratedSaw   -> simpleWave 2  1
    Pulse           -> simpleWave 4  2
    Square          -> simpleWave 8  3
    Triangle        -> simpleWave 16 4
    UserGen gen     -> userGen gen
    where
        simpleWave writeId readId = state $ \blMap ->
            if (M.member x (simpleBandLimitedMap blMap))
                then (SimpleBandLimitedWave readId, blMap)
                else (SimpleBandLimitedWave readId, blMap { simpleBandLimitedMap = M.insert x (SimpleBandLimitedWave writeId) (simpleBandLimitedMap blMap) })

        userGen gen = state $ \blMap ->
            let genMap = vcoInitMap blMap
                (newId, genMap1) = runState (saveId gen) genMap
                blMap1 = blMap { vcoInitMap = genMap1 }
            in  (UserBandLimitedWave newId, blMap1)


renderBandLimited :: Monad m => GenMap -> BandLimitedMap -> DepT m ()
renderBandLimited genMap blMap =
    if isEmptyBlMap blMap
        then return ()
        else render (idMapNewId genMap) (M.toList $ idMapContent $ vcoInitMap blMap) (M.toList $ simpleBandLimitedMap blMap)
    where
        isEmptyBlMap m = (M.null $ simpleBandLimitedMap m) && (M.null $ idMapContent $ vcoInitMap m)

        render lastGenId gens vcos = do
            writeVar freeVcoVar $ int (lastGenId + length gens + 100)
            mapM_ (renderGen lastGenId) gens
            mapM_ renderVco vcos

        renderGen :: Monad m => Int -> (Gen, Int) -> DepT m ()
        renderGen lastGenId (gen, genId) = do
            renderFtgen lastGenId (gen, genId)
            renderVcoGen genId
            renderVcoVarAssignment genId

        freeVcoVar = Var GlobalVar Ir "free_vco"
        ftVar n = Var GlobalVar Ir $ "vco_table_" ++ show n

        renderFtgen lastGenId (g, n) = writeVar (ftVar n) $ ftgen (int $ lastGenId + n) g

        renderVcoGen ftId  = do
            ft   <- readVar (ftVar ftId)
            free <- readVar freeVcoVar
            writeVar freeVcoVar $ vco2init [-ft, free, 1.05, -1, -1, ft]

        renderVcoVarAssignment n = writeVar (bandLimitedVar n) =<< (fmap negate $ readVar (ftVar n))

        renderVco :: Monad m => (BandLimited, BandLimitedId) -> DepT m ()
        renderVco (bandLimited, blId) = case blId of
            SimpleBandLimitedWave waveId -> do
                free <- readVar freeVcoVar
                writeVar freeVcoVar $ vco2init [int waveId, free]
            UserBandLimitedWave   _      -> return ()


{-
            renderFirstVco n (head vcos)
            mapM_ renderTailVco (tail vcos)

        getUserGens as = phi =<< as
            where phi (x, gId) = case x of
                        UserGen g   -> [(g, gId)]
                        _           -> []

        renderGen (g, n) = toDummy $ ftgen (int n) g

        renderFirstVco n x = renderVco (int n) x
        renderTailVco x = renderVco (readOnlyVar vcoVar) x

        renderVco ftId (wave, waveId) = toVcoVar $ vco2init $ case wave of
            UserGen _   -> [ int waveId, ftId, 1.05, -1, -1, int $ negate waveId ]
            _           -> [ int waveId, ftId ]

        vcoVar = dummyVar
        toVcoVar = toDummy

        dummyVar = Var LocalVar Ir "ft"

        toDummy = writeVar dummyVar
-}

readBandLimited :: Maybe E -> BandLimitedId -> E -> E
readBandLimited mphase n cps = oscilikt 1 cps (vco2ft cps (bandLimitedIdToExpr n)) mphase

readHardSyncBandLimited :: Maybe BandLimitedId -> Maybe E -> BandLimitedId -> E -> E -> E
readHardSyncBandLimited msmoothShape mphase n slaveCps masterCps = smoothWave * readShape n phasorSlave slaveCps
    where
        (phasorMaster, syncMaster) = syncphasor masterCps 0 Nothing
        (phasorSlave,  syncSlave)  = syncphasor slaveCps syncMaster mphase

        smoothWave = case msmoothShape of
            Nothing    -> 1
            Just shape -> readShape shape phasorMaster masterCps

        readShape shapeId phasor freq = tableikt phasor (vco2ft freq (bandLimitedIdToExpr shapeId))

----------------------------------------------------------
-- Midi

type Channel = Int

data MidiType = Massign | Pgmassign (Maybe Int)
    deriving (Show, Eq, Ord)

data MidiKey = MidiKey MidiType Channel
    deriving (Show, Eq, Ord)

type MidiMap m = M.Map MidiKey (DepT m ())

saveMidiInstr :: Monad m => MidiType -> Channel -> DepT m () -> MidiMap m -> MidiMap m
saveMidiInstr ty chn body = M.insertWith (flip (>>)) (MidiKey ty chn) body

-- global variables

data Globals = Globals
    { globalsNewId  :: Int
    , globalsVars   :: [AllocVar] }

data AllocVar = AllocVar
        { allocVarType     :: GlobalVarType
        , allocVar         :: Var
        , allocVarInit     :: E }
    | AllocArrVar
        { allocArrVar :: Var
        , allocArrVarSizes :: [E] }

data GlobalVarType = PersistentGlobalVar | ClearableGlobalVar
    deriving (Eq)

instance Default Globals where
    def = Globals 0 [AllocVar PersistentGlobalVar bpmVar 110]

bpmVar :: Var
bpmVar = Var GlobalVar Kr bpmVarName

bpmVarName :: String
bpmVarName = "gBpmVar"

newGlobalVar :: GlobalVarType -> Rate -> E -> State Globals Var
newGlobalVar ty rate initVal = state $ \s ->
    let newId = globalsNewId s
        var   = Var GlobalVar rate ('g' : show newId)
        s1    = s { globalsNewId = succ newId
                  , globalsVars  = AllocVar ty var initVal : globalsVars s }
    in  (var, s1)

newPersistentGlobalVar :: Rate -> E -> State Globals Var
newPersistentGlobalVar = newGlobalVar PersistentGlobalVar

newClearableGlobalVar :: Rate -> E -> State Globals Var
newClearableGlobalVar = newGlobalVar ClearableGlobalVar

newPersistentGloabalArrVar :: Rate -> [E] -> State Globals Var
newPersistentGloabalArrVar rate sizes = state $ \s ->
    let newId = globalsNewId s
        var   = Var GlobalVar rate ('g' : show newId)
        s1    = s { globalsNewId = succ newId
                  , globalsVars  = AllocArrVar var sizes : globalsVars s }
    in (var, s1)

renderGlobals :: Monad m => Globals -> (DepT m (), DepT m ())
renderGlobals a = (initAll, clear)
    where
        initAll = mapM_ initAlloc gs
        clear   = mapM_ clearAlloc clearable
        clearable = filter isClearable gs
        gs = globalsVars a

        initAlloc x = case x of
            AllocVar _  var init  -> initVar var init
            AllocArrVar var sizes -> initArr var sizes

        clearAlloc x = case x of
            AllocVar _  var init  ->  writeVar var init
            AllocArrVar _ _       -> return ()

        isClearable x = case x of
            AllocVar ty _ _ -> ty == ClearableGlobalVar
            _               -> False

-----------------------------------------------------------------
-- instrs

data Instrs = Instrs
    { instrsCache   :: IM.IntMap InstrId
    , instrsNewId   :: Int
    , instrsContent :: [(InstrId, InstrBody)]
    }

instance Default Instrs where
    def = Instrs IM.empty 18 []

getInstrIds :: Instrs -> [InstrId]
getInstrIds = fmap fst . instrsContent

-----------------------------------------------------------------
--


saveInstr :: InstrBody -> State Instrs InstrId
saveInstr body = state $ \s ->
    let h = hash body
    in  case IM.lookup h $ instrsCache s of
            Just  n -> (n, s)
            Nothing ->
                let newId = instrsNewId s
                    s1    = s { instrsCache   = IM.insert h (intInstrId newId) $ instrsCache s
                              , instrsNewId   = succ newId
                              , instrsContent = (intInstrId newId, body) : instrsContent s }
                in  (intInstrId newId, s1)

{-
saveCachedInstr :: InstrBody -> State Instrs InstrId
saveCachedInstr name body = state $ \s ->
    case IM.lookup name $ instrsCache s of
        Just n  -> (n, s)
        Nothing ->
            let newId   = instrsNewId s
                s1      = s { instrsCache   = IM.insert name (intInstrId newId) $ instrsCache s
                            , instrsNewId   = succ newId
                            , instrsContent = (intInstrId newId, body) : instrsContent s }
            in  (intInstrId newId, s1)

newInstrId :: State Instrs InstrId
newInstrId = state $ \s ->
    let newId   = instrsNewId s
        s1      = s { instrsNewId = succ newId }
    in  (intInstrId newId, s1)

saveInstrById :: InstrId -> InstrBody -> State Instrs ()
saveInstrById instrId body = state $ \s ->
    let s1 = s { instrsContent = (instrId, body) : instrsContent s }
    in  ((), s1)

saveInstr :: InstrBody -> State Instrs InstrId
saveInstr body = do
    newId <- newInstrId
    saveInstrById newId body
    return newId
-}

-----------------------------------------------------------------
-- named instrs

newtype NamedInstrs = NamedInstrs { unNamedInstrs :: [(String, InstrBody)] }

instance Default NamedInstrs where
    def = NamedInstrs []

saveNamedInstr :: String -> InstrBody -> State NamedInstrs ()
saveNamedInstr name body = state $ \(NamedInstrs xs) -> ((), NamedInstrs $ (name, body) : xs)

-----------------------------------------------------------------
-- sound sources

getIn :: Monad m => Int -> DepT m [E]
getIn arity
    | arity == 0    = return []
    | otherwise     = ($ arity ) $ mdepT $ mopcs "inch" (replicate arity Ar, replicate arity Kr) (fmap int [1 .. arity])

sendOut :: Monad m => Int -> [E] -> DepT m ()
sendOut arity sigs
    | arity == 0    = return ()
    | otherwise     = do
        vars <- newLocalVars (replicate arity Ar) (return $ replicate arity 0)
        zipWithM_ writeVar vars sigs
        vals <- mapM readVar vars
        depT_ $ opcsNoInlineArgs name [(Xr, replicate arity Ar)] vals
    where
        name
            | arity == 1 = "out"
            | arity == 2 = "outs"
            | arity == 4 = "outq"
            | arity == 6 = "outh"
            | arity == 8 = "outo"
            | arity == 16 = "outx"
            | arity == 32 = "out32"
            | otherwise = "outc"

sendGlobal :: Monad m => Int -> [E] -> State Globals ([E], DepT m ())
sendGlobal arityOuts sigs = do
    vars <- mapM (uncurry newClearableGlobalVar) $ replicate arityOuts (Ar, 0)
    return (fmap readOnlyVar vars, zipWithM_ (appendVarBy (+)) vars sigs)

sendChn :: Monad m => Int -> Int -> [E] -> DepT m ()
sendChn arityIns arityOuts sigs = writeChn (chnRefFromParg (chnPargId arityIns) arityOuts) sigs

chnPargId :: Int -> Int
chnPargId arityIns = 4 + arityIns

-- scaleVolumeFactor :: E -> E
-- scaleVolumeFactor = (setRate Ir (C.midiVolumeFactor (pn 1)) * )

-- guis

--------------------------------------------------------
-- Osc listeners

newtype OscListenPorts = OscListenPorts { unOscListenPorts :: IM.IntMap Var }

instance Default OscListenPorts where
    def = OscListenPorts IM.empty

getOscPortVar :: Int -> State (OscListenPorts, Globals) Var
getOscPortVar port = state $ \st@(OscListenPorts m, globals) -> case IM.lookup port m of
        Just a  -> (a, st)
        Nothing -> onNothing port m globals
    where
        onNothing port m globals = (var, (OscListenPorts m1, newGlobals))
            where
                (var, newGlobals) = runState (allocOscPortVar port) globals
                m1 = IM.insert port var m


allocOscPortVar :: Int -> State Globals Var
allocOscPortVar oscPort = newGlobalVar PersistentGlobalVar Ir $ oscInit (fromIntegral oscPort)

----------------------------------------------------------
-- macros arguments

type MacrosInits = M.Map String MacrosInit

data MacrosInit
    = MacrosInitDouble { macrosInitName :: String, macrosInitValueDouble :: Double }
    | MacrosInitString { macrosInitName :: String, macrosInitValueString :: String }
    | MacrosInitInt    { macrosInitName :: String, macrosInitValueInt    :: Int  }
    deriving (Show, Eq, Ord)

initMacros :: MacrosInit -> State MacrosInits ()
initMacros macrosInit = modify $ \xs -> M.insert (macrosInitName macrosInit)  macrosInit xs

--------------------------------------------------------
-- Udo plugins

newtype UdoPlugin  = UdoPlugin { unUdoPlugin :: String }

addUdoPlugin :: UdoPlugin -> State [UdoPlugin] ()
addUdoPlugin a = modify (a :)

getUdoPluginNames :: [UdoPlugin] -> [String]
getUdoPluginNames xs = nub (fmap unUdoPlugin xs)

-- tabQueue

tabQueuePlugin  = UdoPlugin "tabQueue"
tabQueue2Plugin = UdoPlugin "tabQueue2"

----------------------------------------------------------
-- Steven Yi wonderful UDOs

zdfPlugin           = UdoPlugin "zdf"               -- Zero delay filters
solinaChorusPlugin  = UdoPlugin "solina_chorus"     -- solina chorus
audaciouseqPlugin   = UdoPlugin "audaciouseq"       -- audacious 10 band EQ
adsr140Plugin       = UdoPlugin "adsr140"           -- adsr with retriggering
diodePlugin         = UdoPlugin "diode"             -- diode ladder filter
korg35Plugin        = UdoPlugin "korg35"            -- korg 35 filter
zeroDelayConvolutionPlugin = UdoPlugin "zero-delay-convolution"  -- zero delay convolutio by Victor Lazzarini
pitchShifterDelayPlugin = UdoPlugin "PitchShifterDelay" -- pitch shifter delay

analogDelayPlugin = UdoPlugin "MultiFX/AnalogDelay"
distortionPlugin = UdoPlugin "MultiFX/Distortion"
envelopeFolollowerPlugin = UdoPlugin "MultiFX/EnvelopeFollower"
flangerPlugin = UdoPlugin "MultiFX/Flanger"
freqShifterPlugin = UdoPlugin "MultiFX/FreqShifter"
loFiPlugin = UdoPlugin "MultiFX/LoFi"
panTremPlugin = UdoPlugin "MultiFX/PanTrem"
monoTremPlugin = UdoPlugin "MultiFX/MonoTrem"
phaserPlugin = UdoPlugin "MultiFX/Phaser"
pitchShifterPlugin = UdoPlugin "MultiFX/PitchShifter"
reversePlugin = UdoPlugin "MultiFX/Reverse"
ringModulatorPlugin = UdoPlugin "MultiFX/RingModulator"
stChorusPlugin = UdoPlugin "MultiFX/StChorus"
stereoPingPongDelayPlugin = UdoPlugin "MultiFX/StereoPingPongDelay"

tapeEchoPlugin = UdoPlugin "MultiFX/TapeEcho"

delay1kPlugin = UdoPlugin "Utility/Delay1k"

liveRowPlugin = UdoPlugin "LiveRow"    -- live like trigger, mono
liveRowsPlugin = UdoPlugin "LiveRows"  --                    stereo

ambiRowPlugin = UdoPlugin "AmbiRow"        -- ambi trigger, wav
ambiRowMp3Plugin = UdoPlugin "AmbiRowMp3"  --               mp3

