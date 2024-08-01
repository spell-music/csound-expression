module Csound.Core.Base.Midi.Internal (
  Msg,
  MidiChannel,
  midi,
  midin,
  pgmidi,
  midi_,
  midin_,
  pgmidi_,
  initMidiCtrl,
) where

import Control.Monad
import Csound.Core.Opcode
import Csound.Core.Types
import Data.Boolean
import Data.Maybe

data Msg = Msg

type MidiChannel = Int

data MidiType = Massign | Pgmassign (Maybe Int)
  deriving (Show, Eq, Ord)

data MidiKey = MidiKey MidiType MidiChannel
  deriving (Show, Eq, Ord)

{- | Triggers a midi-instrument (aka Csound's massign) for all channels.
It's useful to test a single instrument.
-}
midi :: (Num a, Sigs a) => (Msg -> SE a) -> SE a
midi = fromProcMidi midiWithInstrId_

-- | Triggers a midi-instrument (aka Csound's massign) on the specified channel.
midin :: (Num a, Sigs a) => MidiChannel -> (Msg -> SE a) -> SE a
midin n = fromProcMidi (midinWithInstrId_ n)

-- | Triggers a midi-instrument (aka Csound's pgmassign) on the specified programm bank.
pgmidi :: (Num a, Sigs a) => Maybe Int -> MidiChannel -> (Msg -> SE a) -> SE a
pgmidi mchn n = fromProcMidi (pgmidiWithInstrId_ mchn n)

fromProcMidi :: (Num a, Sigs a) => ((Msg -> SE ()) -> SE ()) -> (Msg -> SE a) -> SE a
fromProcMidi procMidi f = do
  ref <- newRef 0
  procMidi (mixRef ref . scaleMidiVolumeFactor <=< f)
  res <- readRef ref
  writeRef ref 0
  return res

-----------------------------------------------------------------

-- | Triggers a midi-procedure (aka Csound's massign) for all channels.
midiWithInstrId_ :: (Msg -> SE ()) -> SE ()
midiWithInstrId_ = midinWithInstrId_ 0

-- | Triggers a midi-procedure (aka Csound's pgmassign) on the given MidiChannel.
midinWithInstrId_ :: MidiChannel -> (Msg -> SE ()) -> SE ()
midinWithInstrId_ chn instr = genMidi_ Massign chn instr

-- | Triggers a midi-procedure (aka Csound's pgmassign) on the given programm bank.
pgmidiWithInstrId_ :: Maybe Int -> MidiChannel -> (Msg -> SE ()) -> SE ()
pgmidiWithInstrId_ mchn chn instr = genMidi_ (Pgmassign mchn) chn instr

-----------------------------------------------------------------

-- | Triggers a midi-procedure (aka Csound's massign) for all channels.
midi_ :: (Msg -> SE ()) -> SE ()
midi_ = midin_ 0

-- | Triggers a midi-procedure (aka Csound's pgmassign) on the given channel.
midin_ :: MidiChannel -> (Msg -> SE ()) -> SE ()
midin_ chn instr = genMidi_ Massign chn instr

-- | Triggers a midi-procedure (aka Csound's pgmassign) on the given programm bank.
pgmidi_ :: Maybe Int -> MidiChannel -> (Msg -> SE ()) -> SE ()
pgmidi_ mchn chn instr = genMidi_ (Pgmassign mchn) chn instr

genMidi_ :: MidiType -> MidiChannel -> (Msg -> SE ()) -> SE ()
genMidi_ midiType chn instr = do
  instrRef <- newProc (\() -> instr Msg)
  case midiType of
    Massign -> global $ massign (int chn) instrRef
    Pgmassign mNum -> global $ pgmassign (int $ fromMaybe 0 mNum) instrRef (int chn)

-----------------------------------------------------------------
-- midi ctrls

initMidiCtrl :: D -> D -> D -> SE ()
initMidiCtrl chno ctrlno val =
  global $ initc7 (CtrlInit chno ctrlno val)

-----------------------------------------------------------------
-- midi volume factor

scaleMidiVolumeFactor :: (Sigs a) => a -> a
scaleMidiVolumeFactor = mul (toSig midiVolumeFactor)

-- if we use the scaling at I-rate we don't need to use portamento.
-- If we want to scale with signal the portamento is must
midiVolumeFactor :: D
midiVolumeFactor = ifB (n `less` 2) 1 (recip sqrtN)
  where
    sqrtN = sqrt n
    n = activeIr (getInstrRefIdNum $ iself @())

activeIr :: D -> D
activeIr instrId = liftOpc "active" [(Ir, [Ir])] instrId
