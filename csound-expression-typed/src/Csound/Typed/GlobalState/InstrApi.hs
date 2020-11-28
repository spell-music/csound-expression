{-# Language ScopedTypeVariables #-}
module Csound.Typed.GlobalState.InstrApi(
    InstrId, event, eventi,
    newInstr, newInstrLinked,
    turnoff, turnoff2
) where

import Control.Monad
import Control.Monad.Trans.Class

import Csound.Dynamic hiding (InstrId, when1)
import Csound.Typed.GlobalState.Instr
import Csound.Typed.GlobalState.GE
import Csound.Typed.GlobalState.SE
import Csound.Typed.Types.Tuple
import Csound.Typed.Types.Prim

import Csound.Typed.GlobalState.Port


import qualified Csound.Typed.GlobalState.Opcodes as Opcodes(Event(..), event, eventi, turnoff2, turnoff, initSig, activeKr)

data InstrId a
    = InstrId { unInstrId :: GE E }
    | InstrLinkedId { instrLivenessPort :: PortCtrl Sig, unInstrId :: GE E }

newInstr :: Arg a => (a -> SE ()) -> InstrId a
newInstr instr = InstrId $ fmap instrIdE $ saveInstr (instr toArg)

event :: Arg a => InstrId a -> (Sig,Sig,a) -> SE ()
event idx note = do
    e <- getEvent idx note
    SE $ Opcodes.event e

eventi :: Arg a => InstrId a -> (D,D,a) -> SE ()
eventi idx note = do
    e <- getEventi idx note
    SE $ Opcodes.eventi e

getEvent :: Tuple a => InstrId a -> (Sig, Sig, a) -> SE Opcodes.Event
getEvent (InstrId idx) (start, dur, args) = SE $ lift $ do
            i <- idx
            s <- toGE start
            d <- toGE dur
            as <- fromTuple args
            return $ Opcodes.Event i s d as
getEvent (InstrLinkedId port idx) (start, dur, arg) = do
    getEvent (InstrId idx) (start, dur, (arg, port))


getEventi :: Tuple a => InstrId a -> (D, D, a) -> SE Opcodes.Event
getEventi (InstrId idx) (start, dur, args) = SE $ lift $ do
            i <- idx
            s <- toGE start
            d <- toGE dur
            as <- fromTuple args
            return $ Opcodes.Event i s d as
getEventi (InstrLinkedId port idx) (start, dur, arg) = do
    getEventi (InstrId idx) (start, dur, (arg, port))


turnoff2 :: InstrId a -> SE ()
turnoff2 (InstrId expr) = SE $ Opcodes.turnoff2 =<< lift expr

turnoff :: SE ()
turnoff = SE $ Opcodes.turnoff

newInstrLinked :: forall a. Arg a => (a -> SE ()) -> SE (InstrId a)
newInstrLinked instr = do
    p <- freePortCtrl
    writePort p 10
    let instrId = fmap instrIdE $ saveInstr (instr' toArg)
    let resInstrId = InstrLinkedId p instrId
    writePort p $ (fromGE $ fmap Opcodes.activeKr instrId) + 1
    return resInstrId
    where
        instr' :: (a, PortCtrl Sig) -> SE ()
        instr' (arg, port) = do
            instr arg
            testLiveness port

testLiveness :: PortCtrl Sig -> SE ()
testLiveness p = do
    isAlive <- readPort p
    when1 (isAlive `lessThan` 0) $ turnoff
    modifyPort p (\x -> x - 1)
