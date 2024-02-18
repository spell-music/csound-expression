-- | Open Sound Control.
module Csound.Control.Osc(
    OscHost, OscPort, OscAddress, OscType, OscRef,
    -- * Server
    initOsc, listenOsc, ListenOsc, OscVal,
    listenOscVal, listenOscSig, listenOscSig2,
    -- * Client
    sendOsc
) where

import Csound.Typed

type ListenOsc a = OscAddress -> a -> SE a

-- | Listens for continuous signal from OSC-channel
--
-- > listenOscSig host address initValue
listenOscSig :: OscRef -> OscAddress -> Sig -> SE Sig
listenOscSig = listenOscVal

-- | Listens for pair of continuous signals from OSC-channel
--
-- > listenOscSig2 host address initValue
listenOscSig2 :: OscRef -> OscAddress -> Sig2 -> SE Sig2
listenOscSig2 = listenOscVal