module Csound.Air.Padsynth (
    padsynthOsc
) where

import Csound.Typed
import Csound.Tab
import Csound.Air.Wave

padsynthOsc :: PadsynthSpec -> Sig -> SE Sig
padsynthOsc spec freq = ares
    where
        tab = padsynth spec
        len = ftlen tab
        baseFreq = double $ padsynthFundamental spec

        ares = rndOscBy tab (freq * (sig $ (getSampleRate / len) / baseFreq))

-- harms = [ 1,  1, 0.7600046992, 0.6199994683, 0.9399998784, 0.4400023818, 0.0600003302, 0.8499968648, 0.0899999291, 0.8199964762, 0.3199984133, 0.9400014281, 0.3000001907, 0.120003365, 0.1799997687, 0.5200006366]
-- spec = defPadsynthSpec 261.625565 82.2 harms
-- dac $ mul 0.4 $ at (bhp 30) $ mixAt 0.35 largeHall2 $ mixAt 0.45 (echo 0.25 0.75) $ midi $ onMsg $ (\cps -> (at fromMono . at (mlp (200 + (cps + 3000)) 0.15) . mul (fades 0.5 0.7) . padsynthOsc spec) cps)