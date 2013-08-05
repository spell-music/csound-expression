module Csound.Control(    
    -- * Instrument

    -- | Let's make some noise. Sound is build from special container of values from the class 'Csound.Base.Out'.
    -- ** Output
    Out,
    
    -- *** Handy short-cuts
    Sig2, Sig3, Sig4, Ksig, Amp, Cps, Iamp, Icps,
    
    -- ** Input
    Arg(..), ArgMethods, makeArgMethods,

    -- * Invocation
    GE, runMix, schedule, scheduleUntil,

    -- * Sound sources
    module Csound.Control.Mix,
    module Csound.Control.Evt,
    module Csound.Control.Midi
) where

import Csound.Exp.Tuple(Out)
import Csound.Exp.Arg
import Csound.Exp.Wrapper(Sig2, Sig3, Sig4, Ksig, Amp, Cps, Iamp, Icps)
import Csound.Exp.GE(GE)
import Csound.Exp.Mix(runMix)
import Csound.Exp.Event(schedule, scheduleUntil)

-- re-exports
import Csound.Control.Mix
import Csound.Control.Evt
import Csound.Control.Midi

