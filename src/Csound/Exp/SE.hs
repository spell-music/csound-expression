-- | side effects
module Csound.Exp.SE(
    Outs,
    SE(..), History(..), Instr(..), Arity(..), 
    TrigInstr(..), TrigInstrMap(..),
    MidiAssign(..), 
    se, se_, runSE, execSE, historySE, newVar, newGlobalVar,
    ifBegin, ifEnd, elseIfBegin, elseBegin,
    writeVar, readVar
) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad(ap)
import Control.Monad.Trans.State.Strict
import Data.Default
import Data.Maybe(fromJust)
import Data.Fix(Fix(..))

import Csound.Exp
import Csound.Exp.Wrapper

import qualified Csound.Render.IndexMap as DM

type Outs = SE [Sig]

-- | Csound's synonym for 'IO'-monad. 'SE' means Side Effect. 
-- You will bump into 'SE' trying to read and write to delay lines,
-- making random signals or trying to save your audio to file. 
-- Instrument is expected to return a value of @SE [Sig]@. 
-- So it's okay to do some side effects when playing a note.
newtype SE a = SE { unSE :: StateT History IO a }

data History = History
    { expDependency     :: Maybe E
    , newVarId          :: Int
    , globals           :: Globals
    , instrSet          :: DM.IndexMap
    , instrMap          :: [(InstrId, E)]
    , trigMap           :: TrigInstrMap 
    , midiInstrs        :: [MidiAssign] }

data Instr = Instr 
    { instrArity    :: Arity 
    , instrBody     :: SE [Sig] }

data Arity = Arity
    { arityIns  :: Int
    , arityOuts :: Int }


data Globals = Globals
    { newGlobalVarId :: Int
    , globalsSoFar   :: [Global] }

instance Default Globals where 
    def = Globals 0 []

data Global = Global 
    { globalVar     :: Var
    , globalInit    :: E }

data TrigInstr = TrigInstr 
    { instrToTrig     :: DM.InstrName
    , trigInstrExp    :: Int -> E }

newtype TrigInstrMap = TrigInstrMap { unTrigInstrMap :: [TrigInstr] }

instance Default TrigInstrMap where
    def = TrigInstrMap []

data MidiAssign = MidiAssign 
    { midiAssignType    :: MidiType
    , midiAssignChannel :: Channel
    , midiAssignInstr   :: Int }

instance Default History where
    def = History def def def (DM.empty 1) def def def

instance Functor SE where
    fmap f = SE . fmap f . unSE

instance Applicative SE where
    pure = return
    (<*>) = ap

instance Monad SE where
    return = SE . return
    ma >>= mf = SE $ unSE ma >>= unSE . mf

runSE :: SE a -> IO (a, History)
runSE a = runStateT (unSE a) def

historySE :: SE a -> IO History
historySE = fmap snd . runSE

execSE :: SE a -> IO E
execSE = fmap (fromJust . expDependency . snd) . runSE

se :: (Val a) => E -> SE a
se a = SE $ state $ \s -> 
    let x = Fix $ (unFix a) { ratedExpDepends = expDependency s }
    in  (fromE x, s{ expDependency = Just x } )

se_ :: E -> SE ()
se_ = fmap (const ()) . (se :: E -> SE E)

newVar :: Rate -> SE Var
newVar rate = SE $ state $ \s -> 
    (Var LocalVar rate ("var" ++ show (newVarId s)), s{ newVarId = succ (newVarId s) })

newGlobalId :: SE Int
newGlobalId = SE $ do
    s <- get
    put $ s{ globals = globals s }
    return $ newGlobalVarId $ globals s 

insertGlobal :: Rate -> E -> Globals -> (Global, Globals)
insertGlobal rate init s = (g, s{ newGlobalVarId = succ n, globalsSoFar = g:gs })
    where g = Global (Var GlobalVar rate (show $ newGlobalVarId s)) init
          n  = newGlobalVarId s
          gs = globalsSoFar s
            
newGlobalVar :: Rate -> E -> SE Var
newGlobalVar rate init = SE $ do
    s <- get
    let (g, gs) = insertGlobal rate init (globals s)
    put $ s{ globals = gs }    
    return $ globalVar g

ifBegin :: Val a => a -> SE ()
ifBegin = withCond IfBegin

elseIfBegin :: Val a => a -> SE ()
elseIfBegin = withCond ElseIfBegin

elseBegin :: SE ()
elseBegin = stmtOnly ElseBegin

ifEnd :: SE ()
ifEnd = stmtOnly IfEnd

stmtOnly stmt = se_ $ fromE $ noRate stmt

withCond :: Val a => (E -> MainExp E) -> a -> SE ()
withCond stmt cond = se_ $ fromE $ noRate $ fmap (PrimOr . Right) $ stmt (toE cond)

--------------------------------------------------

writeVar :: (Val a) => Var -> a -> SE ()
writeVar v x = se_ $ noRate $ WriteVar v $ toPrimOr $ toE x 

readVar :: (Val a) => Var -> a
readVar v = noRate $ ReadVar v

---------------------------------------------------

