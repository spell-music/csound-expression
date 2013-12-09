-- | An ADSR-envelope.
module Envelope where

import Csound.Base

main = vdac $ do
    (g, env) <- linAdsr "" (AdsrBound 1 1 3) (AdsrInit 0.1 0.5 0.5 0.1)
    (gv, vol) <- slider "volume" uspan 0.5 
    let instr = onMsg $ \cps -> vol * env * tri cps
    panel $ ver [g, sca 0.25 gv]
    return $ midi instr

