-- | Open Sound Control.
module Csound.Control.Osc(
    initOsc, listenOsc, sendOsc, listenOscVal, ListenOsc
) where

import Csound.Typed

type ListenOsc a = OscAddress -> a -> SE a