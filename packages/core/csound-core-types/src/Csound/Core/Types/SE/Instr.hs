-- | Define instruments
module Csound.Core.Types.SE.Instr
  ( iself
  , InstrRef
  , getInstrRefId
  , getInstrRefIdNum
  , instrRefFromNum
  , MixMode (..)
  , newProc
  , newNamedProc
  , newInstr
  , newEff
  , Note (..)
  , play
  , setFraction
  , negateInstrRef
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
import Csound.Core.Types.SE.Core
import Csound.Core.Types.SE.Logic
import Csound.Core.Types.SE.Port
import Control.Monad.Trans.Class (lift)

data InstrRef a
  = ProcRef (ProcId D a)
  | StrRef (ProcId Str a)
  | InstrRef (InstrId a)
  | EffRef (EffId a)

instance Arg a => FromTuple (InstrRef a) where { fromTuple = fmap pure . toE }
instance Arg a => Tuple (InstrRef a) where { toTuple = fromE . fmap head; tupleRates = [valRate @Sig]; tupleArity = 1; defTuple = instrRefFromNum (-1) }

instance Arg a => Arg (InstrRef a) where

instance Arg a => Val (InstrRef a) where
  fromE = instrRefFromNum  . fromE
  toE = toE . nstrnum
  valRate = valRate @D

-- | nstrnum — Returns the number of a named instrument
nstrnum :: Arg a => InstrRef a -> D
nstrnum instrRef = case getInstrRefId instrRef of
  Left strId -> liftOpc "nstrnum" [(Ir,[Sr])] strId
  Right intId -> intId

getInstrRefId :: Arg a => InstrRef a -> Either Str D
getInstrRefId = \case
  ProcRef pid -> Right $ fromE $ toE pid
  StrRef pid -> Left $ fromE $ toE pid
  InstrRef (InstrId pid _ _) -> Right $ fromE $ toE pid
  EffRef (EffId pid _ _ _) -> Right $ fromE $ toE pid

getInstrRefIdNum :: Arg a => InstrRef a -> D
getInstrRefIdNum ref = case getInstrRefId ref of
  Right n -> n
  Left _ -> error "Instr ref is not a number"

instrRefFromNum :: Arg a => D -> InstrRef a
instrRefFromNum n = ProcRef (ProcId n)

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

instance FromTuple a => FromTuple (Note a) where
  fromTuple (Note start dur args) = fromTuple (start, dur, args)

instance Tuple a => Tuple (Note a) where
  tupleArity = tupleArity @(D, D, a)
  tupleRates = tupleRates @(D, D, a)
  defTuple = Note 0 0 defTuple
  toTuple = (\(start, dur, args) -> Note start dur args) . toTuple

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
      mapM_ (\(Note start dur args) -> schedule currentRate (makeNoteUnique portOuts instrId) start dur (portAlive, portOuts, args)) notes

    playEff (EffId instrId portAlive portOuts portIns) notes = do
      currentRate <- fromMaybe IfIr <$> getCurrentRate
      mapM_ (\(Note start dur args) -> schedule currentRate (makeNoteUnique portOuts instrId) start dur (portAlive, portOuts, portIns, args)) notes

    -- | Adds port identifier as fraction to instrument id.
    -- It makes turn off of the self note safe.
    makeNoteUnique :: Val q => Port x -> q -> q
    makeNoteUnique (Port port) instrId = setFractionD maxNotes port instrId

    maxNotes = 10000

------------------------------------------------------------------------------
-- utils

-- TODO: use turnoff2 with release time as param

turnoffSelf :: D -> SE ()
turnoffSelf rel = SE $ do
  relE <- lift $ toE rel
  let self = Dynamic.pn Ir 1
  csdTurnoff2 self 4 relE

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

-- | Adds fractional part to the instrument reference. This trick is used in Csound to identify the notes (or specific instrument invokation).
setFraction :: Arg a => D -> D -> InstrRef a -> InstrRef a
setFraction maxSize value = \case
  ProcRef pid -> ProcRef $ addFrac pid
  StrRef pid -> ProcRef $ fromE $ toE (addFrac $ csdNstrnum pid)
  InstrRef (InstrId pid a b) -> InstrRef (InstrId (addFrac pid) a b)
  EffRef (EffId pid a b c) -> EffRef (EffId (addFrac pid) a b c)
  where
    addFrac :: Val a => a -> a
    addFrac = setFractionD maxSize value

-- | nstrnum — Returns the number of a named instrument
csdNstrnum :: Arg a => ProcId Str a -> D
csdNstrnum strId = liftOpc "nstrnum" [(Ir,[Sr])] strId

setFractionD :: Val a => D -> D -> a -> a
setFractionD maxSize value a = fromE $ do
  maxSizeE <- toE maxSize
  valueE <- toE (value `mod'` maxSize)
  aE <- toE a
  pure $ aE + (valueE / maxSizeE)

-- | Negates instrument id.
-- Negation is used in Csound to update initialisation parameters
-- of indefinate instrument
negateInstrRef ::  Arg a => InstrRef a -> InstrRef a
negateInstrRef = \case
  ProcRef pid -> ProcRef $ neg pid
  StrRef pid -> ProcRef $ fromE $ toE $ csdNstrnum pid
  InstrRef (InstrId pid a b) -> InstrRef (InstrId (neg pid) a b)
  EffRef (EffId pid a b c) -> EffRef (EffId (neg pid) a b c)
  where
    neg :: Val a => a -> a
    neg = liftE negate
