-- | Development of the example Osc.hs (see it first). 
-- Now oscillators are equipped with filter.
module Filter where

import Csound.Base

import Osc(OscInit(..), moogOscs)

data FilterInit = FilterInit 
    { filterInitResonance :: Double     -- the Q
    , filterInitFreq      :: Double     -- the minimum of the center frequency
    , filterInitRange     :: Double }   -- center frequency range

-- | A moog like filter with ADSR-envelope for center frequency.
moogFilter :: String -> Sig -> FilterInit -> Source (Sig -> Sig)
moogFilter name env (FilterInit initQ initCfq initRange) = source $ do
    (gq, q) <- knob "resonance" uspan initQ 
    (gcfq, cfq) <- knob "freq" (linSpan 0 1000) initCfq  
    (grange, range) <- knob "range" (linSpan 0 10000) initRange
    let cfqSig = cfq + env * range
    gui <- setTitle name $ hor [gq, gcfq, grange]
    return (gui, mlp cfqSig q)

adsrBound = AdsrBound 3 3 5
adsrInit  = AdsrInit  0.1 0.1 0.5 0.2

main = vdac $ do
    (gampEnv, ampEnv) <- linAdsr "amplitude" adsrBound adsrInit
    (gfiltEnv, filtEnv) <- linAdsr "filter" adsrBound adsrInit
    (goscs, oscs) <- moogOscs [OscInit 1 1 1, OscInit 0.5 2 2, OscInit 0.125 3 3]
    (gfilt, filt) <- moogFilter "" filtEnv (FilterInit 0.6 100 3000)
    (gvol, vol) <- masterVolume 

    let instr cps = vol * ampEnv * (filt $ oscs cps)
    let gui = hor [goscs, ver [gampEnv, gfiltEnv, gfilt], sca 0.1 gvol]
    panelBy "" (Just $ Rect 50 50 900 800) gui
    return $ midi $ onMsg instr

