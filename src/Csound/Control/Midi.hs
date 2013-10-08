module Csound.Control.Midi(
    Msg, midi, pgmidi, 
    -- * Reading midi note parameters
    cpsmidi, ampmidi, ampCps
) where

import Csound.Typed
import Csound.Typed.Opcode

ampCps :: Msg -> (D, D)
ampCps msg = (ampmidi msg, cpsmidi msg)

