module Csound.Air (
    -- * Basic waveforms
    osc, saw, isaw, pulse, sqr, tri, blosc
) where

import Csound.Typed
import Csound.Typed.Opcode

import Csound.Tab(sine)

osc :: Sig -> Sig
osc cps = oscil 1 cps sine

