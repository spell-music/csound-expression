-- | Converts to low-level instruments
module Csound.Typed.Control.Instr(
    Arity(..), InsExp, EffExp,
    funArity, constArity,
    insExp, effExp, masterExp, midiExp, unitExp,
    apInstr, apInstr0
) where

import Data.Default
import Csound.Dynamic(InstrId(..))
import qualified Csound.Typed.GlobalState.Elements as C

import Csound.Typed.Types
import Csound.Typed.GlobalState
import Csound.Typed.GlobalState.Opcodes(primInstrId)

funProxy :: (a -> f b) -> (a, b)
funProxy = const (msg, msg)
    where msg = error "I'm a Csound.Typed.Control.Instr.funProxy"

funArity :: (Tuple a, Tuple b) => (a -> SE b) -> Arity
funArity instr = Arity (tupleArity a) (tupleArity b)
    where (a, b) = funProxy instr

constArity :: (Tuple a) => SE a -> Arity
constArity a = Arity 0 (outArity a)

insExp :: (Arg a, Tuple b) => (a -> SE b) -> InsExp
insExp instr = execGEinSE $ fmap fromTuple $ instr toArg

effExp :: (Tuple a, Tuple b) => (a -> SE b) -> EffExp
effExp instr = execGEinSE . fmap fromTuple . instr . toTuple . return

masterExp :: (Tuple a) => SE a -> InsExp
masterExp = execGEinSE . fmap fromTuple

midiExp :: (Tuple a) => (Msg -> SE a) -> InsExp
midiExp instr = execGEinSE $ fmap fromTuple $ instr Msg

unitExp :: SE Unit -> UnitExp
unitExp = execGEinSE . fmap unUnit

apInstr :: (Arg a, Sigs b) => GE InstrId -> a -> b
apInstr instrIdGE args = res
    where
        res = toTuple $ do
            instrId <- instrIdGE
            argList <- fromTuple args
            return $ C.subinstr (tupleArity res) instrId argList

apInstr0 :: (Sigs b) => GE InstrId -> b
apInstr0 instrId = apInstr instrId unit
