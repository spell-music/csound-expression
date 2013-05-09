-- | side effects
module Csound.Exp.SE(
    Outs,
    SE, History(..), Instr(..), Arity(..), 
    PureMidiAssign, DirtyMidiAssign, MidiAssign(..), pureMidiAssign,
    InstrIndexMap, se, se_, runSE, execSE, historySE, newVar,
    ifBegin, ifEnd, elseIfBegin, elseBegin,
    writeVar, readVar,
    saveInstr
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
    , newGlobalVarId    :: Int
    , instrMap          :: InstrIndexMap
    , midiInstrs        :: [DirtyMidiAssign] }

-- Instruments that are referenced with stable names.
type InstrIndexMap = DM.IndexMap Instr

data Instr = Instr 
    { instrArity    :: Arity 
    , instrBody     :: SE [Sig] }

data Arity = Arity
    { arityIns  :: Int
    , arityOuts :: Int }

type PureMidiAssign  = MidiAssign Int
type DirtyMidiAssign = MidiAssign DM.InstrName

pureMidiAssign :: InstrIndexMap -> DirtyMidiAssign -> IO PureMidiAssign
pureMidiAssign m x = do
    instrId <- fmap fromJust $ DM.lookup (midiAssignInstr x) m
    return $ x{ midiAssignInstr = instrId }

data MidiAssign a = MidiAssign 
    { midiAssignType    :: MidiType
    , midiAssignChannel :: Channel
    , midiAssignInstr   :: a }

instance Default History where
    def = History Nothing 0 0 (DM.empty 1) []

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

saveInstr :: DM.InstrName -> Instr -> SE ()
saveInstr name instr = SE $ do
    s <- get
    im <- lift $ DM.insert name instr (instrMap s)
    put $ s { instrMap = im }

