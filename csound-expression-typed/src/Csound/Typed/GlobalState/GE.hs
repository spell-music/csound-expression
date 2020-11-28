{-# Language TupleSections #-}
module Csound.Typed.GlobalState.GE(
    GE, Dep, History(..), withOptions, withHistory, getOptions, evalGE, execGE,
    getHistory, putHistory,
    -- * Globals
    onGlobals, bpmVar,
    -- * Midi
    MidiAssign(..), Msg(..), renderMidiAssign, saveMidi, saveToMidiInstr,
    MidiCtrl(..), saveMidiCtrl, renderMidiCtrl,
    -- * Instruments
    saveAlwaysOnInstr, onInstr, saveUserInstr0, getSysExpr,
    -- * Named instruments
    saveNamedInstr,
    -- * Total duration
    TotalDur(..), pureGetTotalDurForF0, getTotalDurForTerminator,
    setDurationForce, setDuration, setDurationToInfinite,
    -- * Notes
    addNote,
    -- * GEN routines
    GenId,
    saveGen, saveTabs, getNextGlobalGenId,
    saveWriteGen, saveWriteTab,
    -- * Sf2
    saveSf, sfTable,
    -- * Band-limited waves
    saveBandLimitedWave,
    -- * Strings
    saveStr,
    -- * Cache
    GetCache, SetCache, withCache,
    -- * Guis
    newGuiHandle, saveGuiRoot, saveDefKeybdPanel, appendToGui,
    newGuiVar, getPanels, guiHandleToVar,
    guiInstrExp,
    listenKeyEvt, Key(..), KeyEvt(..), Guis(..),
    getKeyEventListener,
    -- * OSC
    getOscPortHandle,
    -- * Macros
    MacrosInit(..), readMacrosDouble, readMacrosString, readMacrosInt,
    -- * Cabbage Guis
    cabbage,
    -- * Hrtf pan
    simpleHrtfmove, simpleHrtfstat,
    -- * Udo plugins
    addUdoPlugin, renderUdoPlugins
) where

import Paths_csound_expression_typed

import Control.Applicative
import Control.Monad
import Data.Boolean
import Data.Default
import qualified Data.IntMap as IM
import qualified Data.Map    as M

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

import Csound.Dynamic hiding (readMacrosDouble, readMacrosString, readMacrosInt)
import qualified Csound.Dynamic as D(readMacrosDouble, readMacrosString, readMacrosInt)

import Csound.Typed.GlobalState.Options
import Csound.Typed.GlobalState.Cache
import Csound.Typed.GlobalState.Elements hiding(saveNamedInstr, addUdoPlugin)
import Csound.Typed.Constants(infiniteDur)
import Csound.Typed.GlobalState.Opcodes(hrtfmove, hrtfstat, primInstrId)

import Csound.Typed.Gui.Gui(Panel(..), Win(..), GuiNode, GuiHandle(..), restoreTree, guiMap, mapGuiOnPanel, defText)
import qualified Csound.Typed.Gui.Cabbage.CabbageLang as Cabbage
import qualified Csound.Typed.Gui.Cabbage.Cabbage     as Cabbage

import qualified Csound.Typed.GlobalState.Elements as E(saveNamedInstr, addUdoPlugin)

type Dep a = DepT GE a

-- global side effects
newtype GE a = GE { unGE :: ReaderT Options (StateT History IO) a }

runGE :: GE a -> Options -> History -> IO (a, History)
runGE (GE f) opt hist = runStateT (runReaderT f opt) hist

evalGE :: Options -> GE a -> IO a
evalGE options a = fmap fst $ runGE a options def

execGE :: Options -> GE a -> IO History
execGE options a = fmap snd $ runGE a options def

instance Functor GE where
    fmap f = GE . fmap f . unGE

instance Applicative GE where
    pure = return
    (<*>) = ap

instance Monad GE where
    return = GE . return
    ma >>= mf = GE $ unGE ma >>= unGE . mf

instance MonadIO GE where
    liftIO = GE . liftIO . liftIO

data History = History
    { genMap            :: GenMap
    , writeGenMap       :: WriteGenMap
    , globalGenCounter  :: Int
    , stringMap         :: StringMap
    , sfMap             :: SfMap
    , midiMap           :: MidiMap GE
    , globals           :: Globals
    , instrs            :: Instrs
    , udoPlugins        :: [UdoPlugin]
    , namedInstrs       :: NamedInstrs
    , midis             :: [MidiAssign]
    , midiCtrls         :: [MidiCtrl]
    , totalDur          :: Maybe TotalDur
    , alwaysOnInstrs    :: [InstrId]
    , notes             :: [(InstrId, CsdEvent)]
    , userInstr0        :: Dep ()
    , bandLimitedMap    :: BandLimitedMap
    , cache             :: Cache GE
    , guis              :: Guis
    , oscListenPorts    :: OscListenPorts
    , cabbageGui        :: Maybe Cabbage.Lang
    , macrosInits       :: MacrosInits }

instance Default History where
    def = History def def def def def def def def def def def def def def def (return ()) def def def def def def

data Msg = Msg
data MidiAssign = MidiAssign MidiType Channel InstrId
data MidiCtrl   = MidiCtrl E E E

renderMidiAssign :: Monad m => MidiAssign -> DepT m ()
renderMidiAssign (MidiAssign ty chn instrId) = case ty of
    Massign         -> massign chn instrId
    Pgmassign mn    -> pgmassign chn instrId mn
    where
        massign n instr = depT_ $ opcs "massign" [(Xr, [Ir,Ir])] [int n, prim $ PrimInstrId instr]
        pgmassign pgm instr mchn = depT_ $ opcs "pgmassign" [(Xr, [Ir,Ir,Ir])] ([int pgm, prim $ PrimInstrId instr] ++ maybe [] (return . int) mchn)

renderMidiCtrl :: Monad m => MidiCtrl -> DepT m ()
renderMidiCtrl (MidiCtrl chno ctrlno val) = initc7 chno ctrlno val
    where
        initc7 :: Monad m => E -> E -> E -> DepT m ()
        initc7 a b c = depT_ $ opcs "initc7" [(Xr, [Ir, Ir, Ir])] [a, b, c]

data TotalDur = ExpDur E | InfiniteDur

getTotalDurForTerminator :: GE E
getTotalDurForTerminator = fmap (getTotalDurForTerminator' . totalDur) getHistory

pureGetTotalDurForF0 :: Maybe TotalDur -> Double
pureGetTotalDurForF0 = toDouble . maybe InfiniteDur id
    where
        toDouble x = case x of
            _           -> infiniteDur

getTotalDurForTerminator' :: Maybe TotalDur -> E
getTotalDurForTerminator' = toExpr . maybe InfiniteDur id
    where
        toExpr x = case x of
            InfiniteDur -> infiniteDur
            ExpDur e    -> e

setDurationToInfinite :: GE ()
setDurationToInfinite = setTotalDur InfiniteDur

setDuration :: E -> GE ()
setDuration = setTotalDur . ExpDur

setDurationForce :: E -> GE ()
setDurationForce = setTotalDur . ExpDur

saveStr :: String -> GE E
saveStr = fmap prim . onStringMap . newString
    where onStringMap = onHistory stringMap (\val h -> h{ stringMap = val })

getNextGlobalGenId :: GE Int
getNextGlobalGenId = onHistory globalGenCounter (\a h -> h{ globalGenCounter = a }) nextGlobalGenCounter

saveGen :: Gen -> GE Int
saveGen = onGenMap . newGen

onGenMap = onHistory genMap (\val h -> h{ genMap = val })

saveWriteGen :: Gen -> GE E
saveWriteGen = onWriteGenMap . newWriteGen

saveWriteTab :: Int -> GE E
saveWriteTab = onWriteGenMap . newWriteTab

onWriteGenMap = onHistory writeGenMap (\val h -> h{ writeGenMap = val })

saveTabs :: [Gen] -> GE E
saveTabs = onGenMap . fmap int . newTabOfGens

onSfMap :: State SfMap a -> GE a
onSfMap = onHistory sfMap (\val h -> h{ sfMap = val })

saveSf :: SfSpec -> GE Int
saveSf = onSfMap . newSf

sfTable :: History -> [(SfSpec, Int)]
sfTable = M.toList . idMapContent . sfMap

saveBandLimitedWave :: BandLimited -> GE BandLimitedId
saveBandLimitedWave = onBandLimitedMap . saveBandLimited
    where onBandLimitedMap = onHistory
                (\a -> (bandLimitedMap a))
                (\(blm) h -> h { bandLimitedMap = blm})

setTotalDur :: TotalDur -> GE ()
setTotalDur = onTotalDur . modify . const . Just
    where onTotalDur = onHistory totalDur (\a h -> h { totalDur = a })

saveMidi :: MidiAssign -> GE ()
saveMidi ma = onMidis $ modify (ma: )
    where onMidis = onHistory midis (\a h -> h { midis = a })

saveToMidiInstr :: MidiType -> Channel -> Dep () -> GE ()
saveToMidiInstr ty chn expr = onMidiMap (saveMidiInstr ty chn expr)
    where onMidiMap = modifyHistoryField midiMap (\a h -> h { midiMap = a })

saveMidiCtrl :: MidiCtrl -> GE ()
saveMidiCtrl ma = onMidis $ modify (ma: )
    where onMidis = onHistory midiCtrls (\a h -> h { midiCtrls = a })

saveUserInstr0 :: Dep () -> GE ()
saveUserInstr0 expr = onUserInstr0 $ modify ( >> expr)
    where onUserInstr0 = onHistory userInstr0 (\a h -> h { userInstr0 = a })

getSysExpr :: InstrId -> GE (Dep ())
getSysExpr terminatorInstrId = do
    e1 <- withHistory $ clearGlobals . globals
    dt <- getTotalDurForTerminator
    let e2 = event_i $ Event (primInstrId terminatorInstrId) dt 0.01 []
    return $ e1 >> e2
    where clearGlobals = snd . renderGlobals

saveAlwaysOnInstr :: InstrId -> GE ()
saveAlwaysOnInstr instrId = onAlwaysOnInstrs $ modify (instrId : )
    where onAlwaysOnInstrs = onHistory alwaysOnInstrs (\a h -> h { alwaysOnInstrs = a })

addNote :: InstrId -> CsdEvent -> GE ()
addNote instrId evt = modifyHistory $ \h -> h { notes = (instrId, evt) : notes h }

{-
setMasterInstrId :: InstrId -> GE ()
setMasterInstrId masterId = onMasterInstrId $ put masterId
    where onMasterInstrId = onHistory masterInstrId (\a h -> h { masterInstrId = a })
-}
----------------------------------------------------------------------
-- state modifiers

withOptions :: (Options -> a) -> GE a
withOptions f = GE $ asks f

getOptions :: GE Options
getOptions = withOptions id

getHistory :: GE History
getHistory = GE $ lift get

putHistory :: History -> GE ()
putHistory h = GE $ lift $ put h

withHistory :: (History -> a) -> GE a
withHistory f = GE $ lift $ fmap f get

modifyHistory :: (History -> History) -> GE ()
modifyHistory = GE . lift . modify

modifyHistoryField :: (History -> a) -> (a -> History -> History) -> (a -> a) -> GE ()
modifyHistoryField getter setter f = modifyHistory (\h -> setter (f $ getter h) h)

modifyWithHistory :: (History -> (a, History)) -> GE a
modifyWithHistory f = GE $ lift $ state f

-- update fields

onHistory :: (History -> a) -> (a -> History -> History) -> State a b -> GE b
onHistory getter setter st = GE $ ReaderT $ \_ -> StateT $ \history ->
    let (res, s1) = runState st (getter history)
    in  return (res, setter s1 history)

type UpdField a b = State a b -> GE b

onInstr :: UpdField Instrs a
onInstr = onHistory instrs (\a h -> h { instrs = a })

onGlobals :: UpdField Globals a
onGlobals = onHistory globals (\a h -> h { globals = a })

----------------------------------------------------------------------
-- named instruments

saveNamedInstr :: String -> InstrBody -> GE ()
saveNamedInstr name body = onNamedInstrs $ E.saveNamedInstr name body
    where onNamedInstrs = onHistory namedInstrs (\a h -> h { namedInstrs = a })

----------------------------------------------------------------------
-- cache

-- midi functions

type GetCache a b = a -> Cache GE -> Maybe b

fromCache :: GetCache a b -> a -> GE (Maybe b)
fromCache f key = withHistory $ f key . cache

type SetCache a b = a -> b -> Cache GE -> Cache GE

toCache :: SetCache a b -> a -> b -> GE ()
toCache f key val = modifyHistory $ \h -> h { cache = f key val (cache h) }

withCache :: TotalDur -> GetCache key val -> SetCache key val -> key -> GE val -> GE val
withCache dur lookupResult saveResult key getResult = do
    ma <- fromCache lookupResult key
    res <- case ma of
        Just a      -> return a
        Nothing     -> do
            r <- getResult
            toCache saveResult key r
            return r
    setTotalDur dur
    return res

--------------------------------------------------------
-- guis

data Guis = Guis
    { guiStateNewId     :: Int
    , guiStateInstr     :: DepT GE ()
    , guiStateToDraw    :: [GuiNode]
    , guiStateRoots     :: [Panel]
    , guiKeyEvents      :: KeyCodeMap }

-- it maps integer key codes to global variables
-- that acts like sensors.
type KeyCodeMap = IM.IntMap Var

instance Default Guis where
    def = Guis 0 (return ()) [] [] def

newGuiHandle :: GE GuiHandle
newGuiHandle = modifyWithHistory $ \h ->
    let (n, g') = bumpGuiStateId $ guis h
    in  (GuiHandle n, h{ guis = g' })

guiHandleToVar :: GuiHandle -> Var
guiHandleToVar (GuiHandle n) = Var GlobalVar Ir ('h' : show n)

newGuiVar :: GE (Var, GuiHandle)
newGuiVar = liftA2 (,) (onGlobals $ newPersistentGlobalVar Kr 0) newGuiHandle

modifyGuis :: (Guis -> Guis) -> GE ()
modifyGuis f = modifyHistory $ \h -> h{ guis = f $ guis h }

appendToGui :: GuiNode -> DepT GE () -> GE ()
appendToGui gui act = modifyGuis $ \st -> st
    { guiStateToDraw = gui : guiStateToDraw st
    , guiStateInstr  = guiStateInstr st >> act }

saveGuiRoot :: Panel -> GE ()
saveGuiRoot g = modifyGuis $ \st ->
    st { guiStateRoots = g : guiStateRoots st }

saveDefKeybdPanel :: GE ()
saveDefKeybdPanel = saveGuiRoot $ Single (Win "" Nothing g) isKeybd
    where
        g = defText "keyboard listener"
        isKeybd = True

bumpGuiStateId :: Guis -> (Int, Guis)
bumpGuiStateId s = (guiStateNewId s, s{ guiStateNewId = succ $ guiStateNewId s })

getPanels :: History -> [Panel]
getPanels h = fmap (mapGuiOnPanel (restoreTree m)) $ guiStateRoots $ guis h
    where m = guiMap $ guiStateToDraw $ guis h

-- have to be executed after all instruments
guiInstrExp :: GE (DepT GE ())
guiInstrExp = withHistory (guiStateInstr . guis)


-- key codes

-- | Keyboard events.
data KeyEvt = Press Key | Release Key
    deriving (Show, Eq)

-- | Keys.
data Key
    = CharKey Char
    | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12 | Scroll
    | CapsLook | LeftShift | RightShift | LeftCtrl | RightCtrl | Enter | LeftAlt | RightAlt | LeftWinKey | RightWinKey
    | Backspace | ArrowUp | ArrowLeft | ArrowRight | ArrowDown
    | Insert | Home | PgUp | Delete | End | PgDown
    | NumLock | NumDiv | NumMul | NumSub | NumHome | NumArrowUp
    | NumPgUp | NumArrowLeft | NumSpace | NumArrowRight | NumEnd
    | NumArrowDown | NumPgDown | NumIns | NumDel | NumEnter | NumPlus
    | Num7 | Num8 | Num9 | Num4 | Num5 | Num6 | Num1 | Num2 | Num3 | Num0 | NumDot
    deriving (Show, Eq)

keyToCode :: Key -> Int
keyToCode x = case x of
    CharKey a -> fromEnum a
    F1 -> 446
    F2 -> 447
    F3 -> 448
    F4 -> 449
    F5 -> 450
    F6 -> 451
    F7 -> 452
    F8 -> 453
    F9 -> 454
    F10 -> 456
    F11 -> 457
    F12 -> 458
    Scroll-> 276
    CapsLook -> 485
    LeftShift -> 481
    RightShift -> 482
    LeftCtrl -> 483
    RightCtrl -> 484
    Enter -> 269
    LeftAlt -> 489
    RightAlt -> 490
    LeftWinKey -> 491
    RightWinKey -> 492
    Backspace -> 264
    ArrowUp -> 338
    ArrowLeft -> 337
    ArrowRight -> 339
    ArrowDown -> 340
    Insert -> 355
    Home -> 336
    PgUp -> 341
    Delete -> 511
    End -> 343
    PgDown -> 342

    NumLock -> 383
    NumDiv -> 431
    NumMul -> 426
    NumSub -> 429
    NumHome -> 436
    NumArrowUp -> 438
    NumPgUp -> 341
    NumArrowLeft -> 337
    NumSpace -> 267
    NumArrowRight -> 339
    NumEnd -> 343
    NumArrowDown -> 340
    NumPgDown -> 342
    NumIns -> 355
    NumDel -> 511
    NumEnter -> 397
    NumPlus -> 427

    Num7 -> 439
    Num8 -> 440
    Num9 -> 441
    Num4 -> 436
    Num5 -> 437
    Num6 -> 438
    Num1 -> 433
    Num2 -> 434
    Num3 -> 435
    Num0 -> 432
    NumDot -> 430

keyEvtToCode :: KeyEvt -> Int
keyEvtToCode x = case x of
    Press k   -> keyToCode k
    Release k -> negate $ keyToCode k

listenKeyEvt :: KeyEvt -> GE Var
listenKeyEvt evt = do
    hist <- getHistory
    let g      = guis hist
        keyMap = guiKeyEvents g
        code   = keyEvtToCode evt

    case IM.lookup code keyMap of
        Just var -> return var
        Nothing  -> do
            var <- onGlobals $ newClearableGlobalVar Kr 0
            hist2 <- getHistory
            let newKeyMap = IM.insert code var keyMap
                newG      = g { guiKeyEvents = newKeyMap }
                hist3     = hist2 { guis = newG }
            putHistory hist3
            return var

-- assumes that first instrument id is 18 and 17 is free to use.
keyEventInstrId :: InstrId
keyEventInstrId = intInstrId 17

keyEventInstrBody :: KeyCodeMap -> GE InstrBody
keyEventInstrBody keyMap = execDepT $ do
    let keys     = flKeyIn
        isChange = changed keys ==* 1
    when1 Kr isChange $ do
        whens Kr (fmap (uncurry $ listenEvt keys) events) doNothing
    where
        doNothing = return ()

        listenEvt keySig keyCode var = (keySig ==* int keyCode, writeVar var 1)

        events = IM.toList keyMap

        flKeyIn :: E
        flKeyIn = opcs "FLkeyIn" [(Kr, [])] []

getKeyEventListener :: GE (Maybe Instr)
getKeyEventListener = do
    h <- getHistory
    if (IM.null $ guiKeyEvents $ guis h)
        then return Nothing
        else do
            saveAlwaysOnInstr keyEventInstrId
            body <- keyEventInstrBody $ guiKeyEvents $ guis h
            return $ Just (Instr keyEventInstrId body)

-----------------------------------------------
-- osc port listen

getOscPortHandle :: Int -> GE E
getOscPortHandle port = onOscPorts (fmap inlineVar $ getOscPortVar port)
    where
        onOscPorts = onHistory (\h -> (oscListenPorts h, globals h)) (\(ports, gs) h -> h { oscListenPorts = ports, globals = gs })

-----------------------------------------------
-- cabbage

cabbage :: Cabbage.Cab -> GE ()
cabbage cab = modifyHistory $ \h -> h { cabbageGui = Just $ Cabbage.runCab cab }

-----------------------------------------------
-- head pan

simpleHrtfmove :: E -> E -> E -> E -> E ->  E -> GE (E, E)
simpleHrtfmove a1 a2 a3 a4 a5 a6 = do
    (left, right) <- getHrtfFiles
    return $ hrtfmove a1 a2 a3 left right a4 a5 a6

simpleHrtfstat :: E -> E -> E -> E -> E -> GE (E, E)
simpleHrtfstat a1 a2 a3 a4 a5 = do
    (left, right) <- getHrtfFiles
    return $ hrtfstat a1 a2 a3 left right a4 a5

getHrtfFiles :: GE (E, E)
getHrtfFiles = do
    sr <- fmap defSampleRate getOptions
    (left, right) <- liftIO $ hrtfFileNames sr
    return (str left, str right)

hrtfFileNames :: Int -> IO (String, String)
hrtfFileNames sr = liftA2 (,) (getDataFileName (name "left" sr)) (getDataFileName (name "right" sr))
    where name dir n = concat ["data/hrtf-", show n, "-", dir, ".dat"]

-----------------------------------------------
-- read macros

readMacrosDouble :: String -> Double -> GE E
readMacrosDouble = readMacrosBy D.readMacrosDouble MacrosInitDouble

readMacrosString :: String -> String -> GE E
readMacrosString = readMacrosBy D.readMacrosString MacrosInitString

readMacrosInt :: String -> Int -> GE E
readMacrosInt    = readMacrosBy D.readMacrosInt    MacrosInitInt

readMacrosBy :: (String ->  E) -> (String -> a -> MacrosInit) -> String -> a -> GE E
readMacrosBy reader allocator name initValue = do
    onMacrosInits $ initMacros $ allocator name initValue
    return $ reader name
    where onMacrosInits = onHistory macrosInits (\val h -> h { macrosInits = val })

-----------------------------------------------
-- udo plugins

addUdoPlugin :: UdoPlugin -> GE ()
addUdoPlugin p = onUdo (E.addUdoPlugin p)
    where onUdo = onHistory udoPlugins (\val h -> h{ udoPlugins = val })

renderUdoPlugins :: History -> IO String
renderUdoPlugins h = fmap concat $ mapM getUdoPluginBody $ getUdoPluginNames $ udoPlugins h

getUdoPluginBody :: String -> IO String
getUdoPluginBody name = readFile =<< getDataFileName filename
    where filename = concat ["data/opcodes/", name, ".udo"]




