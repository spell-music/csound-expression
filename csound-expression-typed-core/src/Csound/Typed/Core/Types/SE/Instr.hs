-- | Define instruments
module Csound.Typed.Core.Types.SE.Instr
  ( newProc
  , Id (..), InstrPort (..)
  , newInstr
  , EffId (..), EffPort (..)
  , newEff
  , IsInstrId
  , Note (..)
  , play
  ) where

import Data.Kind (Type)

import Csound.Dynamic (E, Rate (..))
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.State qualified as State
import Csound.Typed.Core.Types.Prim
import Csound.Typed.Core.Types.Tuple
import Csound.Typed.Core.Types.SE
import Csound.Typed.Core.Types.SE.Logic
import Csound.Typed.Core.Types.SE.Port
import Control.Monad.Trans.Class (lift)

------------------------------------------------------------
-- procedures

-- | Procedure is a regular Csound-instrument
newProc :: forall a . Arg a
  => (a -> SE ())
  -> SE (InstrId D a)
newProc instr = SE $ lift $ State.localy $ do
  expr <- renderBody instr
  toInstrId <$> State.insertInstr expr
  where
    renderBody :: (a -> SE ()) -> Run E
    renderBody instrBody = Dynamic.execDepT $ unSE $
      instrBody (toTuple $ pure $ take (tupleArity @a) $ zipWith Dynamic.pn (tupleRates @a) [4..])

    toInstrId = InstrId . fromE . pure . Dynamic.prim . Dynamic.PrimInstrId

data Id out arg = Id
  { idInstr :: InstrId D (Port (InstrPort out), arg)
  , idPort  :: Port (InstrPort out)
  }

data InstrPort a = InstrPort
  { instrAlive :: Sig
  , instrOuts  :: a
  }

instance (Tuple out, Arg arg) => Tuple (Id out arg) where
  tupleMethods = makeTupleMethods (uncurry Id) (\(Id inId port) -> (inId, port))

instance Tuple a => Tuple (InstrPort a) where
  tupleMethods = makeTupleMethods (uncurry InstrPort) (\(InstrPort isAlive outs) -> (isAlive, outs))

-- | Instrument can produce output that is read from another instrument
newInstr :: forall a b . (Arg a, Tuple b)
  => (a -> SE b)
  -> SE (Id b a, b)
newInstr body = do
  port :: Port (InstrPort b) <- newPort (InstrPort 1 defTuple)
  instrId <- Id <$> newProc body' <*> pure port
  outs <- instrOuts <$> readRef port
  pure (instrId, outs)
  where
    body' :: (Port (InstrPort b), a) -> SE ()
    body' (port, arg) = do
      checkLive port
      out <- body arg
      modifyRef port $ \s -> s { instrOuts = out }

    -- we turn off the child instrument if parent instrument is no longer alive
    checkLive :: Port (InstrPort b) -> SE ()
    checkLive port = do
      isAlive <- instrAlive <$> readRef port
      when1 (isAlive `lessThan` 1) $ turnoff

---------------------------------------------------------------
-- Effects

data EffId ins outs arg = EffId
  { idEff   :: InstrId D (Port (EffPort ins outs), arg)
  , effPort :: Port (EffPort ins outs)
  }

data EffPort ins outs = EffPort
  { effAlive :: Sig
  , effIns   :: ins
  , effOuts  :: outs
  }

instance (Tuple out, Tuple ins, Arg arg) => Tuple (EffId ins out arg) where
  tupleMethods = makeTupleMethods (uncurry EffId) (\(EffId inId port) -> (inId, port))

instance (Tuple ins, Tuple outs) => Tuple (EffPort ins outs) where
  tupleMethods =
    makeTupleMethods
      (\(isAlive, ins, outs) -> EffPort isAlive ins outs)
      (\(EffPort isAlive ins outs) -> (isAlive, ins, outs))

-- | Eff can produce output that is read from another instrument
-- and can consume input from the parent instrument
newEff :: forall a ins outs . (Arg a, Tuple ins, Tuple outs)
  => (a -> ins -> SE outs)
  -> ins -> SE (EffId ins outs a, outs)
newEff body ins = do
  port <- newPort (EffPort 1 defTuple defTuple)
  modifyRef port $ \s -> s { effIns = ins }
  instrId <- EffId <$> newProc body' <*> pure port
  outs <- effOuts <$> readRef port
  pure (instrId, outs)
  where
    body' :: (Port (EffPort ins outs), a) -> SE ()
    body' (port, arg) = do
      checkLive port
      parentIns <- effIns <$> readRef port
      out <- body arg parentIns
      modifyRef port $ \s -> s { effOuts = out }

    -- we turn off the child instrument if parent instrument is no longer alive
    checkLive :: Port (EffPort ins outs) -> SE ()
    checkLive port = do
      isAlive <- effAlive <$> readRef port
      when1 (isAlive `lessThan` 1) $ turnoff

------------------------------------------------------------------------------
-- trigger instr with note

data Note a = Note
  { noteStart :: D
  , noteDur   :: D
  , noteArgs  :: a
  }

instance Arg a => Arg (Note a)

instance Tuple a => Tuple (Note a) where
  tupleMethods =
    makeTupleMethods
      (\(start, dur, args) -> Note start dur args)
      (\(Note start dur args) -> (start, dur, args))

class IsInstrId (instr :: (Type -> Type)) where

instance Val ty => IsInstrId (InstrId ty) where
instance (Tuple outs) => IsInstrId (Id outs) where
instance (Tuple ins, Tuple outs) => IsInstrId (EffId ins outs) where

play :: IsInstrId instrId => instrId a -> [Note a] -> SE ()
play = undefined

------------------------------------------------------------------------------
-- utils

-- |
-- Enables an instrument to turn itself off or to turn an instance of another instrument off.
--
-- >  turnoff
-- >  turnoff  inst
-- >  turnoff  knst
--
-- csound doc: <http://csound.com/docs/manual/turnoff.html>
turnoff ::   SE ()
turnoff  = SE $ (Dynamic.depT_ =<<) $ lift $ pure f
    where f  = Dynamic.opcs "turnoff" [(Xr,[])] []
