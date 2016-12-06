-- | Open Sound Control.
module Csound.Control.Osc(
    OscHost, OscPort, OscAddress, OscType,
    -- * Server
    initOsc, listenOsc, ListenOsc, OscVal, listenOscVal,
    -- * Client
    sendOsc
) where

import Csound.Typed

type ListenOsc a = OscAddress -> a -> SE a