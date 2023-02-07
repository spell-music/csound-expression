-- | Define instruments
module Csound.Typed.Core.Types.SE.Instr
  ( newProc
  , InstrId (..)
  , MixMode (..)
  , newInstr
  , EffId (..)
  , newEff
  , IsInstrId
  , Note (..)
  , play
  ) where

import Data.Maybe
import Data.Boolean (maxB)

import Csound.Dynamic (E, Rate (..), IfRate (..))
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.State (Run, Dep)
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
-- instruments

data InstrId out arg = InstrId
  { instrIdInternal   :: ProcId D (Port (K Sig), Port out, arg)
  , instrIdPortAlive  :: Port (K Sig)
  , instrIdPortOuts   :: Port out
  }

data MixMode = PolyMix | MonoMix

-- | Instrument can produce output that is read from another instrument
newInstr :: forall a b . (Arg a, Sigs b)
  => MixMode -> D -> (a -> SE b)
  -> SE (InstrId b a, b)
newInstr mixMode userRelease body = do
  portAlive <- newPort 1
  portOuts  <- newPort defTuple
  instrId <- InstrId <$> newProc body' <*> pure portAlive <*> pure portOuts
  outs <- readRef portOuts
  writeRef portAlive $ K (linsegr [0] release 1)
  clearRef portOuts
  pure (instrId, outs)
  where
    release = maxB 0.01 userRelease

    body' :: (Port (K Sig), Port b, a) -> SE ()
    body' (portAlive, portOuts, arg) = do
      checkLive portAlive
      let env = linsegr [1] release 0
      setRef portOuts . mulSigs env =<< body arg

    setRef = case mixMode of
      PolyMix -> mixRef
      MonoMix -> writeRef

    -- we turn off the child instrument if parent instrument is no longer alive
    checkLive :: Port (K Sig) -> SE ()
    checkLive port = do
      isAlive <- unK <$> readRef port
      when1 (isAlive `greaterThan` 0) $ turnoffSelf release

mulSigs :: Sigs a => Sig -> a -> a
mulSigs k sigs = toTuple $ do
  es <- fromTuple sigs
  kE <- toE k
  pure $ fmap (kE * ) es

---------------------------------------------------------------
-- Effects

data EffId ins outs arg = EffId
  { effIdInternal :: ProcId D (Port (K Sig), Port ins, Port outs, arg)
  , effIdAlive    :: Port (K Sig)
  , effIdOuts     :: Port ins
  , effIdIns      :: Port outs
  }

-- | Eff can produce output that is read from another instrument
-- and can consume input from the parent instrument
newEff :: forall a ins outs . (Arg a, Sigs ins, Sigs outs)
  => MixMode -> D -> (a -> ins -> SE outs)
  -> ins -> SE (EffId ins outs a, outs)
newEff mixMode userRelease body ins = do
  portAlive <- newPort (K 0)
  portOuts <- newPort 0
  portIns <- newPort ins
  instrId <- EffId <$> newProc body' <*> pure portAlive <*> pure portIns <*> pure portOuts
  outs <- readRef portOuts

  writeRef portAlive (K $ linsegr [0] 0.25 1)
  writeRef portIns ins
  clearRef portOuts
  pure (instrId, outs)
  where
    release = maxB 0.01 userRelease

    body' :: (Port (K Sig), Port ins, Port outs, a) -> SE ()
    body' (portAlive, portIns, portOuts, arg) = do
      checkLive portAlive
      parentIns <- readRef portIns
      out <- body arg parentIns
      let env = linsegr [1] release 0
      setRef portOuts (mulSigs env out)

    setRef = case mixMode of
      PolyMix -> mixRef
      MonoMix -> writeRef

    -- we turn off the child instrument if parent instrument is no longer alive
    checkLive :: Port (K Sig) -> SE ()
    checkLive port = do
      isAlive <- unK <$> readRef port
      when1 (isAlive `greaterThan` 0) $ turnoffSelf release

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

class IsInstrId instrId where
  play :: (Arg a) => instrId a -> [Note a] -> SE ()

instance Val ty => IsInstrId (ProcId ty) where
  play instrId notes = do
    currentRate <- fromMaybe IfIr <$> getCurrentRate
    mapM_ (\(Note start dur args) -> schedule currentRate instrId start dur args) notes

instance Sigs outs => IsInstrId (InstrId outs) where
  play (InstrId instrId portAlive portOuts) notes = do
    currentRate <- fromMaybe IfIr <$> getCurrentRate
    mapM_ (\(Note start dur args) -> schedule currentRate instrId start dur (portAlive, portOuts, args)) notes

instance (Sigs outs, Sigs ins) => IsInstrId (EffId ins outs) where
  play = undefined

------------------------------------------------------------------------------
-- utils

-- TODO: use turnoff2 with release time as param

turnoffSelf :: D -> SE ()
turnoffSelf rel = SE $ do
  relE <- lift $ toE rel
  let self = Dynamic.pn Ir 1
  csdTurnoff2 self 0 relE

csdTurnoff2 :: E -> E -> E -> Dep ()
csdTurnoff2 instrId mode release =
  Dynamic.depT_ $ Dynamic.opcs "turnoff2" [(Xr, [Kr, Kr, Kr])] [instrId, mode, release]

schedule :: forall args ty . (Val ty, Arg args) => IfRate -> ProcId ty args -> D -> D -> args -> SE ()
schedule ifRate instrId start dur args = SE $ (Dynamic.depT_ =<<) $ lift $ f <$> fromTuple (instrId, start, dur, args)
    where
      f as = case ifRate of
        IfIr -> Dynamic.opcs "schedule"  [(Xr, tupleRates @(ProcId ty args, D, D, args))] as
        IfKr -> Dynamic.opcs "schedulek" [(Xr, tupleRates @(K (ProcId ty args, D, D, args)))] as

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

