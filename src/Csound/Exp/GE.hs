-- | Global side effects
module Csound.Exp.GE(
    GE(..), runGE, History(..),
    history, options, withHistory, putHistory,
    saveMixerInstr, saveSourceInstr, saveAlwaysOnInstr, saveSourceInstrCached,    

    saveTab, saveStr,

    saveSco, LowLevelSco,    
    -- * globals
 --   newGlobalVar,
    -- * instruments

    -- ** sound sources
 --   Instr(..), Arity(..), 
    -- ** event driven
 --   TrigInstr(..), TrigInstrMap(..),
    -- * guis    
    appendToGui, newGuiId,
    newGlobalVar
) where

import qualified System.Mem.StableName.Dynamic.Map as DM(Map, empty, insert, member, lookup)
import qualified System.Mem.StableName.Dynamic     as DM(DynamicStableName, makeDynamicStableName)

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad(ap)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Data.Default

import qualified Data.IntMap as IM

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.SE
import Csound.Exp.Gui(Gui)

import Csound.Tfm.Tab

import Csound.Render.Pretty(Doc)

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
    , instrs   :: Instrs
    , scos     :: Scos
    , guis     :: Guis 
    , globals  :: Globals }

instance Default History where 
    def = History def def def def def def

runGE :: GE a -> CsdOptions -> IO (a, History)
runGE (GE a) options = runStateT (runReaderT a options) def

ge :: (CsdOptions -> History -> IO (a, History)) -> GE a
ge phi = GE $ ReaderT $ \opt -> StateT $ \history -> phi opt history    

history :: GE History
history = ge $ \opt h -> return (h, h)

putHistory :: History -> GE ()
putHistory h = ge $ \_ _ -> return ((), h)

options :: GE CsdOptions
options = ge $ \opt h -> return (opt, h)

exec :: IO a -> GE a
exec act = ge $ \opt h -> do
    a <- act
    return (a, h)

withHistory :: (History -> (a, History)) -> GE a
withHistory phi = ge $ \opt history -> return $ phi history

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
    , instrCache    :: DM.Map Int }

instance Default Instrs where
    def = Instrs def def 1 DM.empty

saveSourceInstrCached :: a -> (a -> GE E) -> GE InstrId
saveSourceInstrCached instr render = do
    h <- history
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

saveAlwaysOnInstr :: E -> GE InstrId
saveAlwaysOnInstr exp = do
    instrId <- saveSourceInstr exp
    saveAlwaysOnNote instrId
    return instrId

saveSourceInstr :: E -> GE InstrId
saveSourceInstr = saveInstr $ \a s -> s{ instrSources = a : instrSources s }

saveMixerInstr :: E -> GE InstrId
saveMixerInstr = saveInstr $ \a s -> s{ instrMixers = a : instrMixers s }

saveInstr :: ((InstrId, E) -> Instrs -> Instrs) -> E -> GE InstrId
saveInstr save exp = withHistory $ \h ->
    let ins = instrs h
        counter' = succ $ instrCounter ins
        instrId  = intInstrId counter'
    in  (instrId, h{ instrs = save (instrId, exp) $ ins { instrCounter = counter' }})

--------------------------------------------------------
-- scores

data Scos = Scos 
    { alwaysOnInstrs :: [InstrId]
    , mixerNotes     :: IM.IntMap LowLevelSco }

type LowLevelSco = [(InstrId, Note)]

instance Default Scos where
    def = Scos def def

saveAlwaysOnNote :: InstrId -> GE ()
saveAlwaysOnNote instrId = modifyHistory $ \h -> 
    let x = scos h
    in  h { scos = x{ alwaysOnInstrs = instrId : alwaysOnInstrs x } }

saveSco :: IM.IntMap LowLevelSco -> GE ()
saveSco sco = modifyHistory $ \h -> 
    let x = scos h
    in  h { scos = x{ mixerNotes = sco }}

--------------------------------------------------------
-- guis

data Guis = Guis
    { guiStateNewId     :: Int
    , guiStateInstr     :: SE ()
    , guiStateToDraw    :: [Gui] }

instance Default Guis where 
    def = Guis 0 (return ()) []

newGuiId :: GE Int 
newGuiId = withHistory $ \h -> 
    let (n, g') = bumpGuiStateId $ guis h
    in  (n, h{ guis = g' })

appendToGui :: Gui -> SE () -> GE ()
appendToGui gui act = withHistory $ \h ->
    ((), h{ guis = appendToGuiState gui act $ guis h })

bumpGuiStateId :: Guis -> (Int, Guis)
bumpGuiStateId s = (guiStateNewId s, s{ guiStateNewId = succ $ guiStateNewId s })

appendToGuiState :: Gui -> SE () -> Guis -> Guis
appendToGuiState gui act s = s
    { guiStateToDraw = gui : guiStateToDraw s
    , guiStateInstr  = guiStateInstr s >> act }

--------------------------------------------------------
-- globals

data Globals = Globals
    { newGlobalVarId :: Int
    , globalsSoFar   :: [Global] }

instance Default Globals where 
    def = Globals 0 []

data Global = Global 
    { globalVar     :: Var
    , globalInit    :: E }


newGlobalVarOnGlobals :: Val a => Rate -> a -> Globals -> (Var, Globals)
newGlobalVarOnGlobals rate a s = 
    (v, s{ newGlobalVarId = succ n, globalsSoFar = g : globalsSoFar s })
    where n = newGlobalVarId s
          v = Var GlobalVar rate (show n)
          g = Global v (toE a)  

newGlobalVar :: Val a => Rate -> a -> GE Var
newGlobalVar rate initVal = withHistory $ \h -> 
    let (v, globals') = newGlobalVarOnGlobals rate initVal (globals h)
    in  (v, h{ globals = globals' })

{-
runGE :: GE a -> IO (a, History)
runGE a = runStateT (unGE a) def

-- global state of the program
data History = History
    -- making new global variables
    { globals           :: Globals
    -- instruments
    , instrSet          :: DM.IndexMap
    , instrMap          :: [(InstrId, E)]
    , trigMap           :: TrigInstrMap 
    , midiInstrs        :: [MidiAssign]
    -- guis
    , guiState          :: GuiState }

instance Default History where
    def = History def (DM.empty 1) def def def def

------------------------------------------------------
-- instruments

-- sound sources

data Instr = Instr 
    { instrArity    :: Arity 
    , instrBody     :: SE [Sig] }

data Arity = Arity
    { arityIns  :: Int
    , arityOuts :: Int }

-- event driven

data TrigInstr = TrigInstr 
    { instrToTrig     :: DM.InstrName
    , trigInstrExp    :: Int -> E }

newtype TrigInstrMap = TrigInstrMap { unTrigInstrMap :: [TrigInstr] }

instance Default TrigInstrMap where
    def = TrigInstrMap []

-- midis


---------------------------------------------------------
-- globals

data Globals = Globals
    { newGlobalVarId :: Int
    , globalsSoFar   :: [Global] }

instance Default Globals where 
    def = Globals 0 []

data Global = Global 
    { globalVar     :: Var
    , globalInit    :: E }

newGlobalId :: GE Int
newGlobalId = GE $ do
    s <- get
    put $ s{ globals = globals s }
    return $ newGlobalVarId $ globals s 
    
insertGlobal :: Rate -> E -> Globals -> (Global, Globals)
insertGlobal rate init s = (g, s{ newGlobalVarId = succ n, globalsSoFar = g:gs })
    where g = Global (Var GlobalVar rate (show $ newGlobalVarId s)) init
          n  = newGlobalVarId s
          gs = globalsSoFar s
            
newGlobalVar :: Rate -> E -> GE Var
newGlobalVar rate init = GE $ do
    s <- get
    let (g, gs) = insertGlobal rate init (globals s)
    put $ s{ globals = gs }    
    return $ globalVar g

---------------------------------------------------------------
-- gui

data GuiState = GuiState 
    { guiStateNewId     :: Int
    , guiStateInstr     :: SE ()
    , guiStateToDraw    :: [Gui] }

instance Default GuiState where 
    def = GuiState 0 (return ()) []

bumpGuiStateId :: GuiState -> (Int, GuiState)
bumpGuiStateId s = (guiStateNewId s, s{ guiStateNewId = succ $ guiStateNewId s })

appendToGuiState :: Gui -> SE () -> GuiState -> GuiState
appendToGuiState gui act s = s
    { guiStateToDraw = gui : guiStateToDraw s
    , guiStateInstr  = guiStateInstr s >> act }

newGuiId :: GE Int 
newGuiId = GE $ do 
    s <- get
    let (n, guiState')  = bumpGuiStateId $ guiState s
    put $ s{ guiState = guiState' } 
    return n

appendToGui :: Gui -> SE () -> GE ()
appendToGui gui act = GE $ modify $ \s -> s{ guiState = appendToGuiState gui act $ guiState s }
-}
