-- | The module contains the modules that are responsible
-- for converting events to signals 
module Csound.Control(
    -- * Side effects
    -- | A Csound's equivallent for IO monad.
    module Csound.Control.SE, 
    -- * Events
    -- | Handy functions to arrange the event streams (periodic and random).
    module Csound.Control.Evt,
    -- * Instruments
    -- | Invoking the instruments.
    module Csound.Control.Instr,
    -- * Gui
    -- | Interactive controllers.
    module Csound.Control.Gui,
    -- * Midi
    -- | Interface with Midi.
    module Csound.Control.Midi,
    -- * Osc
    -- | Interface with Osc.
    module Csound.Control.Osc,
    -- * Channels
    -- | Interface with named channels.
    module Csound.Control.Channel
) where

import Csound.Control.SE        
import Csound.Control.Evt
import Csound.Control.Instr
import Csound.Control.Gui

import Csound.Control.Midi
import Csound.Control.Osc
import Csound.Control.Channel

