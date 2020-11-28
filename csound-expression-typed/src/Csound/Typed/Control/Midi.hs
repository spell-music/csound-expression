{-# Language FlexibleContexts #-}
module Csound.Typed.Control.Midi(
    Msg, Channel,
    midi, midin, pgmidi, 
    midi_, midin_, pgmidi_,
    initMidiCtrl
) where

import System.Mem.StableName

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Csound.Dynamic

import Csound.Typed.Types
import Csound.Typed.GlobalState
import Csound.Typed.Control.Instr
import Csound.Typed.Control.Ref

import qualified Csound.Typed.GlobalState.Opcodes as C(midiVolumeFactor)

-- | Triggers a midi-instrument (aka Csound's massign) for all channels. 
-- It's useful to test a single instrument.
midi :: (Num a, Sigs a) => (Msg -> SE a) -> SE a
midi = fromProcMidi midiWithInstrId_

-- | Triggers a midi-instrument (aka Csound's massign) on the specified channel. 
midin :: (Num a, Sigs a) => Channel -> (Msg -> SE a) -> SE a
midin n = fromProcMidi (midinWithInstrId_ n)

-- | Triggers a midi-instrument (aka Csound's pgmassign) on the specified programm bank. 
pgmidi :: (Num a, Sigs a) => Maybe Int -> Channel -> (Msg -> SE a) -> SE a
pgmidi mchn n = fromProcMidi (pgmidiWithInstrId_ mchn n)

fromProcMidi :: (Num a, Sigs a) => ((Msg -> SE ()) -> SE ()) -> (Msg -> SE a) -> SE a
fromProcMidi procMidi f = do
    ref <- newGlobalRef 0
    procMidi (mixRef ref . scaleMidiVolumeFactor <=< f)
    res <- readRef ref
    writeRef ref 0
    return res

-----------------------------------------------------------------

-- | Triggers a midi-procedure (aka Csound's massign) for all channels. 
midiWithInstrId_ :: (Msg -> SE ()) -> SE ()
midiWithInstrId_ = midinWithInstrId_ 0

-- | Triggers a midi-procedure (aka Csound's pgmassign) on the given channel. 
midinWithInstrId_ :: Channel -> (Msg -> SE ()) -> SE ()
midinWithInstrId_ chn instr = genMidi_ Massign chn instr

-- | Triggers a midi-procedure (aka Csound's pgmassign) on the given programm bank. 
pgmidiWithInstrId_ :: Maybe Int -> Channel -> (Msg -> SE ()) -> SE ()
pgmidiWithInstrId_ mchn chn instr = genMidi_ (Pgmassign mchn) chn instr

-----------------------------------------------------------------

-- | Triggers a midi-procedure (aka Csound's massign) for all channels. 
midi_ :: (Msg -> SE ()) -> SE ()
midi_ = midin_ 0

-- | Triggers a midi-procedure (aka Csound's pgmassign) on the given channel. 
midin_ :: Channel -> (Msg -> SE ()) -> SE ()
midin_ chn instr = genMidi_ Massign chn instr

-- | Triggers a midi-procedure (aka Csound's pgmassign) on the given programm bank. 
pgmidi_ :: Maybe Int -> Channel -> (Msg -> SE ()) -> SE ()
pgmidi_ mchn chn instr = genMidi_ (Pgmassign mchn) chn instr

genMidi_ :: MidiType -> Channel -> (Msg -> SE ()) -> SE ()
genMidi_ midiType chn instr = geToSe $ saveToMidiInstr midiType chn (unSE $ instr Msg)

-----------------------------------------------------------------
-- midi ctrls

initMidiCtrl :: D -> D -> D -> SE ()
initMidiCtrl chno ctrlno val = geToSe $ 
    saveMidiCtrl =<< (MidiCtrl <$> toGE chno <*> toGE ctrlno <*> toGE val)


-----------------------------------------------------------------
-- midi volume factor

scaleMidiVolumeFactor :: Sigs a => a -> a
scaleMidiVolumeFactor = mapTuple (setRate Ir (C.midiVolumeFactor (pn 1)) * )