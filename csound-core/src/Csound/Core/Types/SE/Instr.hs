-- | Define instruments
module Csound.Core.Types.SE.Instr
  ( iself
  , InstrRef
  , getInstrRefId
  , MixMode (..)
  , newProc
  , newNamedProc
  , newInstr
  , newEff
  , Note (..)
  , play
  ) where

import Data.Maybe
import Data.Boolean (maxB)

import Csound.Dynamic (E, Rate (..), IfRate (..))
import Csound.Dynamic qualified as Dynamic
import Csound.Core.State (Run, Dep)
import Csound.Core.State qualified as State
import Csound.Core.Types.Prim
import Csound.Core.Types.Tuple
import Csound.Core.Types.Rate
import Csound.Core.Types.SE
import Csound.Core.Types.SE.Logic
import Csound.Core.Types.SE.Port
import Control.Monad.Trans.Class (lift)

data InstrRef a
  = ProcRef (ProcId D a)
  | StrRef (ProcId Str a)
  | InstrRef (InstrId a)
  | EffRef (EffId a)

getInstrRefId :: Arg a => InstrRef a -> Either Str D
getInstrRefId = \case
  ProcRef pid -> Right $ fromE $ toE pid
  StrRef pid -> Left $ fromE $ toE pid
  InstrRef (InstrId pid _ _) -> Right $ fromE $ toE pid
  EffRef (EffId pid _ _ _) -> Right $ fromE $ toE pid

------------------------------------------------------------
-- procedures

-- | New procedure with a use given name. Procedure is a regular Csound instrument
newNamedProc :: Arg a => Str -> (a -> SE ()) -> SE (InstrRef a)
newNamedProc name instr = SE $ lift $ State.localy $ do
  expr <- renderBody instr
  instrName <- getName
  StrRef . toInstrId <$> State.insertNamedInstr instrName expr
  where
    getName = do
      nameE <- toE name
      case Dynamic.getPrimUnsafe nameE of
        Dynamic.PrimString str -> pure str
        _ -> error "newNamedInstr: instr name type is not a primitive string"

-- | Procedure is a regular Csound instrument
newProc :: Arg a => (a -> SE ()) -> SE (InstrRef a)
newProc instr = ProcRef <$> newProcId instr

-- | Procedure is a regular Csound-instrument
newProcId :: Arg a => (a -> SE ()) -> SE (ProcId D a)
newProcId instr = SE $ lift $ State.localy $ do
  expr <- renderBody instr
  toInstrId <$> State.insertInstr expr

-- | Querries a self name of the instrument
iself :: InstrRef a
iself = ProcRef $ ProcId $ fromE $ pure $ Dynamic.pn Ir 1

toInstrId :: Val ty => Dynamic.InstrId -> ProcId ty a
toInstrId = ProcId . fromE . pure . Dynamic.prim . Dynamic.PrimInstrId

renderBody :: forall a . Arg a => (a -> SE ()) -> Run E
renderBody instrBody = Dynamic.execDepT $ unSE $
  instrBody (toTuple $ pure $ take (tupleArity @a) $ zipWith toInstrArg (tupleRates @a) [4..])
  where
    toInstrArg :: Rate -> Int -> E
    toInstrArg = \case
      Sr   -> strcpy . Dynamic.pn Sr
      rate -> Dynamic.pn rate

    strcpy :: E -> E
    strcpy arg = Dynamic.opcs "strcpy" [(Sr, [Sr])] [arg]

------------------------------------------------------------
-- instruments

data InstrId arg = forall out . Sigs out => InstrId
  { _instrIdInternal   :: ProcId D (Port (K Sig), Port out, arg)
  , _instrIdPortAlive  :: Port (K Sig)
  , _instrIdPortOuts   :: Port out
  }

data MixMode = PolyMix | MonoMix

-- | Instrument can produce output that is read from another instrument
newInstr :: forall arg outs . (Arg arg, Sigs outs)
  => MixMode -> D -> (arg -> SE outs)
  -> SE (InstrRef arg, outs)
newInstr mixMode userRelease body = do
  portAlive <- newPort @(K Sig)
  portOuts  <- newPort @outs
  instrId <- InstrId <$> newProcId body' <*> pure portAlive <*> pure portOuts
  outs <- readRef portOuts
  writeRef portAlive $ K (linsegr [0] release 1)
  clearRef portOuts
  pure (InstrRef instrId, outs)
  where
    release = maxB 0.01 userRelease

    body' :: (Port (K Sig), Port outs, arg) -> SE ()
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
      when1 (isAlive `greater` 0) $ turnoffSelf release

mulSigs :: Sigs a => Sig -> a -> a
mulSigs k sigs = toTuple $ do
  es <- fromTuple sigs
  kE <- toE k
  pure $ fmap (kE * ) es

---------------------------------------------------------------
-- Effects

data EffId arg = forall ins outs . (Sigs ins, Sigs outs) => EffId
  { _effIdInternal :: ProcId D (Port (K Sig), Port ins, Port outs, arg)
  , _effIdAlive    :: Port (K Sig)
  , _effIdOuts     :: Port ins
  , _effIdIns      :: Port outs
  }

-- | Eff can produce output that is read from another instrument
-- and can consume input from the parent instrument
newEff :: forall arg ins outs . (Arg arg, Sigs ins, Sigs outs)
  => MixMode -> D -> (arg -> ins -> SE outs)
  -> ins -> SE (InstrRef arg, outs)
newEff mixMode userRelease body ins = do
  portAlive <- newPort @(K Sig)
  portOuts <- newPort @outs
  portIns <- newPort @ins
  instrId <- EffId <$> newProcId body' <*> pure portAlive <*> pure portIns <*> pure portOuts
  outs <- readRef portOuts

  writeRef portAlive (K $ linsegr [0] 0.25 1)
  writeRef portIns ins
  clearRef portOuts
  pure (EffRef instrId, outs)
  where
    release = maxB 0.01 userRelease

    body' :: (Port (K Sig), Port ins, Port outs, arg) -> SE ()
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
      when1 (isAlive `greater` 0) $ turnoffSelf release

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

play :: (Arg a) => InstrRef a -> [Note a] -> SE ()
play = \case
  ProcRef instrId  -> playProc instrId
  StrRef instrId   -> playProc instrId
  InstrRef instrId -> playInstr instrId
  EffRef instrId   -> playEff instrId
  where
    playProc instrId notes = do
      currentRate <- fromMaybe IfIr <$> getCurrentRate
      mapM_ (\(Note start dur args) -> schedule currentRate instrId start dur args) notes

    playInstr (InstrId instrId portAlive portOuts) notes = do
      currentRate <- fromMaybe IfIr <$> getCurrentRate
      mapM_ (\(Note start dur args) -> schedule currentRate instrId start dur (portAlive, portOuts, args)) notes

    playEff (EffId instrId portAlive portOuts portIns) notes = do
      currentRate <- fromMaybe IfIr <$> getCurrentRate
      mapM_ (\(Note start dur args) -> schedule currentRate instrId start dur (portAlive, portOuts, portIns, args)) notes

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

