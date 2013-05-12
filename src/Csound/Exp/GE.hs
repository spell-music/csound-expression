-- | Global side effects
module Csound.Exp.GE(
    GE(..), runGE, History(..),
    -- * globals
    newGlobalVar,
    -- * instruments

    -- ** sound sources
    Instr(..), Arity(..), 
    -- ** event driven
    TrigInstr(..), TrigInstrMap(..),
    -- ** miids
    MidiAssign(..), 
    -- * guis    
    appendToGui, newGuiId
) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad(ap)
import Control.Monad.Trans.State.Strict
import Data.Default

import qualified Csound.Render.IndexMap as DM

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.SE
import Csound.Exp.Gui(Gui)


newtype GE a = GE { unGE :: StateT History IO a }

instance Functor GE where
    fmap f = GE . fmap f . unGE

instance Applicative GE where
    pure = return
    (<*>) = ap

instance Monad GE where
    return = GE . return
    ma >>= mf = GE $ unGE ma >>= unGE . mf

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

data MidiAssign = MidiAssign 
    { midiAssignType    :: MidiType
    , midiAssignChannel :: Channel
    , midiAssignInstr   :: Int }

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

