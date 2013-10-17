{-# Language TypeFamilies, FlexibleContexts, FlexibleInstances #-}
module Csound.Control.Instr(
    -- * Mix
    CsdSco(..), Mix, sco, mix, eff, CsdEventList(..), CsdEvent, 
    sco_, mix_, mixBy, 

    -- * Midi
    Msg, Channel, midi, midin, pgmidi, ampCps,
    midi_, midin_, pgmidi_,
    -- ** Reading midi note parameters
    cpsmidi, ampmidi,

    -- * Evt            
    trig, sched, schedHarp, autoOff,
    trig_, sched_,
    trigBy, schedBy, schedHarpBy
) where

import Csound.Typed
import Csound.Typed.Opcode

--------------------------------------------------------------------------
-- midi

ampCps :: Msg -> (D, D)
ampCps msg = (ampmidi msg 1, cpsmidi msg)

