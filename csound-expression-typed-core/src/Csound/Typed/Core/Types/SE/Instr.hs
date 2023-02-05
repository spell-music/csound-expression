-- | Define instruments
module Csound.Typed.Core.Types.SE.Instr
  ( newProc
  , InstrId (..), InstrPort (..)
  , newInstr
  , EffId (..), EffPort (..)
  , newEff
  , IsInstrId
  , Note (..)
  , play
  ) where

import Data.Kind (Type)
import Data.Maybe

import Csound.Dynamic (E, Rate (..), IfRate (..))
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.State qualified as State
import Csound.Typed.Core.Types.Prim
import Csound.Typed.Core.Types.Tuple
import Csound.Typed.Core.Types.Rate
import Csound.Typed.Core.Types.SE
import Csound.Typed.Core.Types.SE.Logic
import Csound.Typed.Core.Types.SE.Port
import Control.Monad.Trans.Class (lift)

------------------------------------------------------------
-- procedures

-- | Procedure is a regular Csound-instrument
newProc :: forall a . Arg a
  => (a -> SE ())
  -> SE (ProcId D a)
newProc instr = SE $ lift $ State.localy $ do
  expr <- renderBody instr
  toInstrId <$> State.insertInstr expr
  where
    renderBody :: (a -> SE ()) -> Run E
    renderBody instrBody = Dynamic.execDepT $ unSE $
      instrBody (toTuple $ pure $ take (tupleArity @a) $ zipWith Dynamic.pn (tupleRates @a) [4..])

    toInstrId = ProcId . fromE . pure . Dynamic.prim . Dynamic.PrimInstrId

------------------------------------------------------------
-- procedures

data InstrId out arg = InstrId
  { instrIdInternal :: ProcId D (Port (InstrPort out), arg)
  , instrIdPort     :: Port (InstrPort out)
  }

data InstrPort a = InstrPort
  { instrAlive :: Sig
  , instrOuts  :: a
  }

instance (Tuple out, Arg arg) => Tuple (InstrId out arg) where
  tupleMethods = makeTupleMethods (uncurry InstrId) (\(InstrId inId port) -> (inId, port))

instance Tuple a => Tuple (InstrPort a) where
  tupleMethods = makeTupleMethods (uncurry InstrPort) (\(InstrPort isAlive outs) -> (isAlive, outs))

-- | Instrument can produce output that is read from another instrument
newInstr :: forall a b . (Arg a, Sigs b)
  => (a -> SE b)
  -> SE (InstrId b a, b)
newInstr body = do
  port :: Port (InstrPort b) <- newPort (InstrPort 1 defTuple)
  instrId <- InstrId <$> newProc body' <*> pure port
  outs <- instrOuts <$> readRef port
  writeRef port $ InstrPort
    { instrAlive = linsegr [1] 0.25 0
    , instrOuts  = 0
    }
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
  { effIdInternal :: ProcId D (Port (EffPort ins outs), arg)
  , effIdPort     :: Port (EffPort ins outs)
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
newEff :: forall a ins outs . (Arg a, Sigs ins, Sigs outs)
  => (a -> ins -> SE outs)
  -> ins -> SE (EffId ins outs a, outs)
newEff body ins = do
  port <- newPort (EffPort 1 defTuple defTuple)
  instrId <- EffId <$> newProc body' <*> pure port
  outs <- effOuts <$> readRef port
  writeRef port $ EffPort
    { effAlive = linsegr [1] 0.25 0
    , effIns   = ins
    , effOuts  = 0
    }
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

class (Val (InternalType instrId), Tuple (InternalPort instrId)) => IsInstrId instrId where
  type InternalPort instrId :: Type
  type InternalType instrId :: Type

  getInternalId   :: instrId a -> ProcId (InternalType instrId) (Port (InternalPort instrId), a)
  getInternalPort :: instrId a -> Port (InternalPort instrId)

instance Val ty => IsInstrId (ProcId ty) where
  type InternalPort (ProcId ty) = ()
  type InternalType (ProcId ty) = ty

  getInternalId (ProcId procId) = ProcId procId
  getInternalPort = const $ defTuple

instance Sigs outs => IsInstrId (InstrId outs) where
  type InternalPort (InstrId outs) = InstrPort outs
  type InternalType (InstrId outs) = D

  getInternalId = instrIdInternal
  getInternalPort = instrIdPort

instance (Sigs outs, Sigs ins) => IsInstrId (EffId ins outs) where
  type InternalPort (EffId ins outs) = EffPort ins outs
  type InternalType (EffId ins outs) = D

  getInternalId = effIdInternal
  getInternalPort = effIdPort

play :: (IsInstrId instrId, Arg a) => instrId a -> [Note a] -> SE ()
play instrId notes = do
  currentRate <- fromMaybe IfIr <$> getCurrentRate
  mapM_ (\(Note start dur args) -> schedule currentRate (getInternalId instrId) start dur (getInternalPort instrId, args)) notes

------------------------------------------------------------------------------
-- utils

-- TODO: use turnoff2 with release time as param

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

schedule :: forall args ty . (Val ty, Arg args) => IfRate -> ProcId ty args -> D -> D -> args -> SE ()
schedule ifRate instrId start dur args = SE $ (Dynamic.depT_ =<<) $ lift $ f <$> fromTuple (instrId, start, dur, args)
    where
      f as = case ifRate of
        IfIr -> Dynamic.opcs "schedule"  [(Xr, tupleRates @(ProcId ty args, D, D, args))] as
        IfKr -> Dynamic.opcs "schedulek" [(Xr, tupleRates @(K (ProcId ty args, D, D, args)))] as

{-
schedulek :: forall a ty . (Val ty, Arg a) => ProcId ty a -> D -> D -> a -> SE ()
schedulek instrId start dur args = SE $ (depT_ =<<) $ lift $ f <$> fromTuple (instrId, start, dur, args)
    where f as = opcs "schedulek" [(Xr, tupleRates @(K (ProcId ty a, D, D, a)))] as
-}

-- |
-- Trace a series of line segments between specified points including a release segment.
--
-- > ares  linsegr  ia, idur1, ib [, idur2] [, ic] [...], irel, iz
-- > kres  linsegr  ia, idur1, ib [, idur2] [, ic] [...], irel, iz
--
-- csound doc: <http://csound.com/docs/manual/linsegr.html>
linsegr ::  [D] -> D -> D -> Sig
linsegr b1 b2 b3 = Sig $ f <$> mapM unD b1 <*> unD b2 <*> unD b3
    where f a1 a2 a3 = Dynamic.setRate Kr $ Dynamic.opcs "linsegr" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, last a1, a2, a3])

