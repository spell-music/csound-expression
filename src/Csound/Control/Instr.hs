{-# Language TypeFamilies, FlexibleContexts, FlexibleInstances #-}
module Csound.Control.Instr(
    -- * Mix
    CsdSco(..), Mix, sco, mix, eff, CsdEventList(..), CsdEvent, 
    sco_, mix_,

    -- * Midi
    Msg, Channel, midi, midin, pgmidi, ampCps,
    -- ** Reading midi note parameters
    cpsmidi, ampmidi,

    -- * Evt            
    trig, sched, schedHarp, autoOff,
) where

import Csound.Typed hiding (midi, pgmidi)
import qualified Csound.Typed as T(midi, pgmidi)
import Csound.Typed.Opcode

--------------------------------------------------------------------------
-- midi

midi :: Sigs a => (Msg -> SE a) -> a
midi = midin 0

midin :: Sigs a => Channel -> (Msg -> SE a) -> a
midin chn instr = T.midi chn instr

pgmidi :: Sigs a => Maybe Int -> Channel -> (Msg -> SE a) -> a
pgmidi mn chn instr = T.pgmidi mn chn instr

ampCps :: Msg -> (D, D)
ampCps msg = (ampmidi msg 1, cpsmidi msg)

