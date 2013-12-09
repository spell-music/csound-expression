module Main where

import Csound.Base

main = dac $ do     
    keyPanel =<< box "hi"
    let [on, off, ons, onLow] = map keyIn [Press (CharKey 'a'), Release (CharKey 'a'), Press (CharKey 's'), Press F1]
        a4 = sched (const $ return $ osc 110) $ withDur 1 onLow
        a3 = sched (const $ return $ osc 440) $ withDur 1 on
        a2 = sched (const $ return $ osc 330) $ withDur 1 off
        a1 = sched (const $ return $ osc 220) $ withDur 1 ons
    return $ mean [a1, a2, a3, a4]


