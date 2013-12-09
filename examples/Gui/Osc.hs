module Osc where

import Csound.Base
import Control.Monad

data OscInit = OscInit 
    { oscInitVol   :: Double
    , oscInitRange :: Int
    , oscInitWave  :: Int }

moogOsc1 :: String -> OscInit -> Source (Sig -> Sig)
moogOsc1 name (OscInit initVol initRange initWave) = source $ do
    (gf, f) <- classicWaves "" initWave
    (gvol, vol) <- knob "vol" uspan initVol 
    (grange, range) <- knob "range" (linSpan 1 10) (fromIntegral initRange)
    gui <- setTitle name $ hor [setMaterial NoPlastic $ gf, gvol, grange]
    let instr cps = vol * f (floor' range * cps) 
    return (gui, instr)

moogOscs :: [OscInit] -> Source (Sig -> Sig)
moogOscs xs = source $ do
    (gfs, fs) <- fmap unzip $ zipWithM 
        (\n initVal -> moogOsc1 ("osc" ++ show n) initVal) [1 ..] xs 
    let instr cps = sum $ fmap ($ cps) fs 
    let gui = ver gfs
    return (gui, instr)

main = vdac $ do
    (g, f) <- moogOscs [(OscInit 1 1 0), (OscInit 0.2 1 1), (OscInit 0 1 2)]
    (gv, vol) <- masterVolume 
    (genv, env) <- linAdsr "amplitude envelope" (AdsrBound 3 3 5) (AdsrInit 0.1 0.5 0.5 1) 
    
    panelBy "" (Just $ Rect 50 50 600 600) (hor [ver [sca 0.25 genv, g], sca 0.1 gv])
    return $ midi $ onMsg (mul (vol * env) . f)

