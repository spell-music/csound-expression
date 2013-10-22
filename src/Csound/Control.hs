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
    -- | Invoking the instruments
    module Csound.Control.Instr
) where

import Csound.Control.SE        
import Csound.Control.Evt
import Csound.Control.Instr


