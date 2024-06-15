-- | Instrument control
module Csound.Core.Base.Instr
  ( active
  , maxalloc
  , nstrnum
  , turnoff2
  , turnoff2_i
  , turnoffSelf
  , turnoffSelf_i
  , stopSelf
  , stopInstr
  , stopInstr_i
  , schedule
  , scheduleEvent
  ) where

import Csound.Core.Types

-- | active — Returns the number of active instances of an instrument.
active :: (Arg a, SigOrD b) => InstrRef a -> SE b
active instrRef = case getInstrRefId instrRef  of
  Left strId  -> liftOpcDep "active" strRates strId
  Right intId -> liftOpcDep "active" intRates intId
  where
    intRates = [(Ir, [Ir,Ir,Ir]), (Kr, [Kr,Ir,Ir])]
    strRates = [(Ir, [Sr,Ir,Ir])]

-- | maxalloc — Limits the number of allocations of an instrument.
-- It's often used with @global@
maxalloc :: (Arg a) => InstrRef a -> D -> SE ()
maxalloc instrRef val = case getInstrRefId instrRef of
  Left strId -> liftOpcDep_ "maxalloc" strRates (strId, val)
  Right intId -> liftOpcDep_ "maxalloc" intRates (intId, val)
  where
    strRates = [(Xr, [Sr,Ir])]
    intRates = [(Xr, [Ir,Ir])]

-- | nstrnum — Returns the number of a named instrument
nstrnum :: Arg a => InstrRef a -> D
nstrnum instrRef = case getInstrRefId instrRef of
  Left strId -> liftOpc "nstrnum" [(Ir,[Sr])] strId
  Right intId -> intId

-- | turnoff2 — Turn off instance(s) of other instruments at performance time.
turnoff2 :: Arg a => InstrRef a -> Sig -> Sig -> SE ()
turnoff2 instrRef kmode krelease = do
  case getInstrRefId instrRef of
    Left strId  -> liftOpcDep_ "turnoff2" strRates (strId, kmode, krelease)
    Right intId -> liftOpcDep_ "turnoff2" intRates (intId, kmode, krelease)
  where
    strRates = [(Xr, [Sr,Kr,Kr])]
    intRates = [(Xr, [Kr,Kr,Kr])]

-- | turnoff2 — Turn off instance(s) of other instruments at performance time.
turnoff2_i :: Arg a => InstrRef a -> D -> D -> SE ()
turnoff2_i instrRef imode irelease = do
  case getInstrRefId instrRef of
    Left strId  -> liftOpcDep_ "turnoff2_i" strRates_i (strId, imode, irelease)
    Right intId -> liftOpcDep_ "turnoff2_i" intRates_i (intId, imode, irelease)
  where
    strRates_i = [(Xr, [Sr,Ir,Ir])]
    intRates_i = [(Xr, [Ir,Ir,Ir])]

-- | turnoff self instrument
turnoffSelf :: Sig -> Sig -> SE ()
turnoffSelf kmode krelease = turnoff2 (iself @()) kmode krelease

-- | turnoff self instrument
turnoffSelf_i :: D -> D -> SE ()
turnoffSelf_i kmode krelease = turnoff2_i (iself @()) kmode krelease

stopInstr :: Arg a => InstrRef a -> SE ()
stopInstr instrId = turnoff2 instrId 0 0

stopInstr_i :: Arg a => InstrRef a -> SE ()
stopInstr_i instrId = turnoff2_i instrId 0 0

stopSelf :: SE ()
stopSelf = turnoffSelf 0 0

schedule :: (Arg a) => InstrRef a -> D -> D -> a -> SE ()
schedule instrId start dur args = play instrId [Note start dur args]

-- | Schedules an event for the instrument.
--
-- > scheduleEvent instrRef delay duration args
--
-- The arguments for time values are set in seconds.
scheduleEvent :: (Arg a) => InstrRef a -> D -> D -> a -> SE ()
scheduleEvent instrRef start end args =
  case getInstrRefId instrRef of
    Left strId  -> liftOpcDep_ "event" strRates ("i" :: Str, strId, start, end, args)
    Right intId -> liftOpcDep_ "event" intRates ("i" :: Str, intId, start, end, args)
  where
    intRates = [(Xr, Sr : repeat Kr)]
    strRates = [(Xr, Sr : Sr : repeat Kr)]
