{-# Language ScopedTypeVariables #-}
-- | Converts to low-level instruments
module Csound.Typed.Control.Instr(
    Arity(..), InsExp, EffExp,
    funArity, constArity,
    insExp, effExp, masterExp, midiExp, unitExp,
    apInstr, apInstr0
) where

import Data.Proxy
import Csound.Dynamic(InstrId(..))
import qualified Csound.Typed.GlobalState.Elements as C

import Csound.Typed.Types
import Csound.Typed.GlobalState

funArity :: forall a b. (Tuple a, Tuple b) => (a -> SE b) -> Arity
funArity _instr = Arity (tupleArity (Proxy :: Proxy a)) (tupleArity (Proxy :: Proxy b))

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

apInstr :: forall a b. (Arg a, Sigs b) => GE InstrId -> a -> b
apInstr instrIdGE args =
  toTuple $ do
    instrId <- instrIdGE
    argList <- fromTuple args
    return $ C.subinstr (tupleArity (Proxy :: Proxy b)) instrId argList

apInstr0 :: (Sigs b) => GE InstrId -> b
apInstr0 instrId = apInstr instrId unit
