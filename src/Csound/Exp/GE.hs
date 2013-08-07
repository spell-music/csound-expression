-- | Global side effects
module Csound.Exp.GE(
    GE(..), execGE, History(..),
    getHistory, getOptions, withHistory, putHistory,

    Instrs(..),
    saveMixerInstr, saveSourceInstr, saveAlwaysOnInstr, saveSourceInstrCached,    
    saveMixerNotes,

    saveMidi,

    saveTab, saveStr,

    Scos(..),
    LowLevelSco, saveAlwaysOnNote,
    getDuration, saveDuration, setDurationToInfinite,

    Globals(..), Global(..), GlobalVarType(..), initGlobals, newGlobalVar, clearGlobals,
    
    newGuiVar, appendToGui, newGuiHandle, guiHandleToVar, saveGuiRoot, getPanels,
    guiInstrExp
) where

import qualified System.Mem.StableName.Dynamic.Map as DM(Map, empty, insert, lookup)
import qualified System.Mem.StableName.Dynamic     as DM(DynamicStableName, makeDynamicStableName)

import Control.Applicative
import Control.Monad(ap)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Data.Default
import Data.Maybe(isJust, fromJust)

import qualified Data.IntMap as IM
import qualified Data.Map as M

import Csound.Exp
import Csound.Exp.EventList(CsdEvent)
import Csound.Exp.Wrapper
import Csound.Exp.Options
import Csound.Exp.SE
import Csound.Exp.Gui(Panel, GuiNode, GuiHandle(..), restoreTree, guiMap, mapGuiOnPanel)

import Csound.Tfm.Tab

-- | GE stands for Global side Effect. It's side effect that happens 
-- on the global level. We have another type for side effects. It's 'Csound.Types.SE'.
-- They happen within an instrument. Often this type is used to represent the output sound
-- signals or GUI-widgets.
newtype GE a = GE { unGE :: ReaderT CsdOptions (StateT History IO) a }

instance Functor GE where
    fmap f = GE . fmap f . unGE

instance Applicative GE where
    pure = return
    (<*>) = ap

instance Monad GE where
    return = GE . return
    ma >>= mf = GE $ unGE ma >>= unGE . mf

data History = History 
    { tabIndex :: Index LowTab
    , strIndex :: Index String
    , midis    :: [MidiAssign]
    , instrs   :: Instrs
    , scos     :: Scos
    , guis     :: Guis 
    , globals  :: Globals
    , duration :: Maybe TotalDur }

data TotalDur = Bounded Double | Infinite

instance Default History where 
    def = History def def def def def def def def

execGE :: GE a -> CsdOptions -> IO History
execGE a opt = fmap snd $ runGE a opt

runGE :: GE a -> CsdOptions -> IO (a, History)
runGE (GE a) options = runStateT (runReaderT a options) def

ge :: (CsdOptions -> History -> IO (a, History)) -> GE a
ge phi = GE $ ReaderT $ \opt -> StateT $ \history -> phi opt history    

getHistory :: GE History
getHistory = ge $ \_ h -> return (h, h)

putHistory :: History -> GE ()
putHistory h = ge $ \_ _ -> return ((), h)

getOptions :: GE CsdOptions
getOptions = ge $ \opt h -> return (opt, h)

exec :: IO a -> GE a
exec act = ge $ \_ h -> do
    a <- act
    return (a, h)

withHistory :: (History -> (a, History)) -> GE a
withHistory phi = ge $ \_ history -> return $ phi history

modifyHistory :: (History -> History) -> GE ()
modifyHistory f = withHistory $ \h -> ((), f h)

------------------------------------------------------
-- tables

type TabId = Int
type StrId = Int

saveTab :: LowTab -> GE TabId
saveTab x = withHistory $ \history -> 
    let (n, tabs') = indexInsert x (tabIndex history)
    in  (n, history{ tabIndex = tabs' })

saveStr :: String -> GE StrId
saveStr x = withHistory $ \history -> 
    let (n, strs') = indexInsert x (strIndex history)
    in  (n, history{ strIndex = strs' })

-------------------------------------------------------
-- instruments

data Instrs = Instrs 
    { instrSources  :: [(InstrId, E)]
    , instrMixers   :: [(InstrId, E)]     
    , instrCounter  :: Int
    , instrCache    :: DM.Map Int
    , mixerNotes    :: IM.IntMap LowLevelSco }

theFirstInstrId :: InstrId
theFirstInstrId = intInstrId 1 

instance Default Instrs where
    def = Instrs def def (instrIdCeil theFirstInstrId) DM.empty def

saveSourceInstrCached :: a -> (a -> GE E) -> GE InstrId
saveSourceInstrCached instr render = do
    h <- getHistory
    let cache = instrCache $ instrs h
    name <- exec $ DM.makeDynamicStableName instr
    case DM.lookup name cache of
        Just n  -> return $ intInstrId n
        Nothing -> do
            instrId <- saveSourceInstr =<< render instr
            saveToCache name (instrIdCeil instrId)
            return instrId

saveToCache :: DM.DynamicStableName -> Int -> GE ()
saveToCache name counter = modifyHistory $ \h ->
    let x = instrs h
    in  h { instrs = x { instrCache = DM.insert name counter (instrCache x) }}

saveAlwaysOnInstr :: E -> GE ()
saveAlwaysOnInstr expr 
    | isEmptyExp expr = return ()
    | otherwise       = do
        instrId <- saveSourceInstr expr
        saveAlwaysOnNote instrId

saveSourceInstr :: E -> GE InstrId
saveSourceInstr = saveInstr $ \a s -> s{ instrSources = a : instrSources s }

saveMixerInstr :: E -> GE InstrId
saveMixerInstr = saveInstr $ \a s -> s{ instrMixers = a : instrMixers s }

-- have to be executed after all instruments
guiInstrExp :: GE E
guiInstrExp = fmap (execSE . guiStateInstr . guis) getHistory

saveInstr :: ((InstrId, E) -> Instrs -> Instrs) -> E -> GE InstrId
saveInstr save exprWithTabs = do
    expr <- substTabs exprWithTabs
    withHistory $ \h ->
        let ins = instrs h
            counter' = succ $ instrCounter ins
            instrId  = intInstrId $ instrCounter ins
        in  (instrId, h{ instrs = save (instrId, expr) $ ins { instrCounter = counter' }})

saveMixerNotes :: IM.IntMap LowLevelSco -> GE ()
saveMixerNotes sco = modifyHistory $ \h -> 
    let x = instrs h
    in  h { instrs = x{ mixerNotes = sco }}

substTabs :: E -> GE E
substTabs expr = do
    opt <- getOptions
    let expr' = defineInstrTabs (tabFi opt) expr
        tabs  = getInstrTabs expr'
    ids <- mapM saveTab tabs
    let tabMap = M.fromList $ zip tabs ids
    return $ substInstrTabs tabMap expr'
   

--------------------------------------------------------
-- midi

saveMidi :: MidiAssign -> GE ()
saveMidi ma = modifyHistory $ \h -> h{ midis = ma : midis h }

--------------------------------------------------------
-- scores

data Scos = Scos 
    { alwaysOnInstrs :: [InstrId] }

type LowLevelSco = [(InstrId, CsdEvent Note)]

instance Default Scos where
    def = Scos def

saveAlwaysOnNote :: InstrId -> GE ()
saveAlwaysOnNote instrId = modifyHistory $ \h -> 
    let x = scos h
    in  h { scos = x{ alwaysOnInstrs = instrId : alwaysOnInstrs x } }

getDuration :: History -> Double
getDuration = maybe dayAndNight toDouble . duration
    where dayAndNight = 24 * 60 * 60
          toDouble x = case x of
            Bounded d   -> d
            Infinite    -> dayAndNight

saveDuration :: Double -> GE ()
saveDuration d = modifyHistory $ \h ->
    h { duration = upd $ duration h }
    where upd x = case x of
            Nothing             -> Just $ Bounded d
            Just (Bounded a)    -> Just $ Bounded $ a `max` d
            Just Infinite       -> Just Infinite

setDurationToInfinite :: GE ()
setDurationToInfinite = modifyHistory $ \h ->
    h { duration = Just Infinite }

--------------------------------------------------------
-- guis

data Guis = Guis
    { guiStateNewId     :: Int
    , guiStateInstr     :: SE ()
    , guiStateToDraw    :: [GuiNode] 
    , guiStateRoots     :: [Panel] }

instance Default Guis where 
    def = Guis 0 (return ()) [] []

newGuiHandle :: GE GuiHandle 
newGuiHandle = withHistory $ \h -> 
    let (n, g') = bumpGuiStateId $ guis h
    in  (GuiHandle n, h{ guis = g' })

guiHandleToVar :: GuiHandle -> Var
guiHandleToVar (GuiHandle n) = Var GlobalVar Ir ('h' : show n)

newGuiVar :: GE (Var, GuiHandle)
newGuiVar = liftA2 (,) (mkNewGlobalVar Kr (GlobalVarType WriteOnly PersistentVar Nothing)) newGuiHandle

modifyGuis :: (Guis -> Guis) -> GE ()
modifyGuis f = modifyHistory $ \h -> h{ guis = f $ guis h }

appendToGui :: GuiNode -> SE () -> GE ()
appendToGui gui act = modifyGuis $ \st -> st
    { guiStateToDraw = gui : guiStateToDraw st
    , guiStateInstr  = guiStateInstr st >> act }

saveGuiRoot :: Panel -> GE ()
saveGuiRoot g = modifyGuis $ \st -> 
    st { guiStateRoots = g : guiStateRoots st }

bumpGuiStateId :: Guis -> (Int, Guis)
bumpGuiStateId s = (guiStateNewId s, s{ guiStateNewId = succ $ guiStateNewId s })

getPanels :: History -> [Panel]
getPanels h = fmap (mapGuiOnPanel (restoreTree m)) $ guiStateRoots $ guis h 
    where m = guiMap $ guiStateToDraw $ guis h

--------------------------------------------------------
-- globals

data Globals = Globals
    { newGlobalVarId :: Int
    , globalsSoFar   :: [Global] }

instance Default Globals where 
    def = Globals 0 []

data Global = Global 
    { globalVar     :: Var
    , globalVarType :: GlobalVarType }

data GlobalVarType = GlobalVarType
    { writeMethod   :: WriteMethod
    , presence      :: Presence
    , initVal       :: Maybe E }

data WriteMethod = WriteOnly | AppendOnly
data Presence    = PersistentVar | ClearableVar 
    deriving (Eq)

newGlobalVarOnGlobals :: Rate -> GlobalVarType -> Globals -> (Var, Globals)
newGlobalVarOnGlobals rate a s = 
    (v, s{ newGlobalVarId = succ n, globalsSoFar = g : globalsSoFar s })
    where n = newGlobalVarId s
          v = Var GlobalVar rate (show n)
          g = Global v a

newGlobalVar :: Val a => Rate -> a -> GE Var
newGlobalVar rate v0 = mkNewGlobalVar rate ty
    where ty = GlobalVarType AppendOnly ClearableVar (Just $ toE v0)

mkNewGlobalVar :: Rate -> GlobalVarType -> GE Var
mkNewGlobalVar rate gVarType = withHistory $ \h -> 
    let (v, globals') = newGlobalVarOnGlobals rate gVarType (globals h)
    in  (v, h{ globals = globals' })

initGlobals :: [Global] -> SE ()
initGlobals = mapM_ initMe . filter hasInits
    where initMe   g = initVar (globalVar g) (fromJust $ getInit g)
          hasInits g = isJust $ getInit g  
          getInit  g = initVal $ globalVarType g 

clearGlobals :: GE (SE ())
clearGlobals = do 
    gs <- fmap (filter isClearable . globalsSoFar . globals) $ getHistory
    return $ mapM_ (\g -> writeVar (globalVar g) (double 0)) gs 
    where isClearable g = presence (globalVarType g) == ClearableVar

