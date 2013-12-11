-- | Simulates a moog-like oscillators.
-- We create a waveform from the sum of the three oscillators.
module Osc where

import Csound.Base
import Control.Monad

-- An oscillator
data OscInit = OscInit 
    { oscInitVol   :: Double    -- volume
    , oscInitRange :: Int       -- integer scale for the frequency
    , oscInitWave  :: Int }     -- the waveform identifier
                                -- (0 - osc, 1 - triangle, 2 - square, 3 - saw)

-- A single moog oscillator.
-- It takes a title and initial values. Produces the waveform function.
moogOsc1 :: String -> OscInit -> Source (Sig -> Sig)
moogOsc1 name (OscInit initVol initRange initWave) = source $ do
    -- Creates a widget to chose between four classic waveforms
    (gf, f) <- classicWaves "" initWave
    
    -- A knob for a volume of the oscillator
    (gvol, vol) <- knob "vol" uspan initVol 

    -- A knob for a range of the oscillator (scaling of the frequency)
    -- It's the number of the harmonic.
    (grange, range) <- knob "range" (linSpan 1 10) (fromIntegral initRange)

    -- Creates a gui element with the given title and horizontal placement.
    gui <- setTitle name $ hor [setMaterial NoPlastic $ gf, gvol, grange]

    -- Creates a waveform
    let instr cps = vol * f (floor' range * cps) 
    return (gui, instr)

-- A list of moog oscillators.
moogOscs :: [OscInit] -> Source (Sig -> Sig)
moogOscs xs = source $ do
    -- Creates a list of single moog oscillators
    (gfs, fs) <- fmap unzip $ zipWithM 
        (\n initVal -> moogOsc1 ("osc" ++ show n) initVal) [1 ..] xs 

    -- The final waveform is the sum of all oscillators
    let instr cps = sum $ fmap ($ cps) fs 
    -- Alligns all elements vertically
    let gui = ver gfs

    -- Creates a source widget.
    return (gui, instr)

main = vdac $ do
    -- Let's create a stack of three oscillators:
    (g, f) <- moogOscs [(OscInit 1 1 0), (OscInit 0.2 1 1), (OscInit 0 1 2)]
    -- Creates a master volume slider.
    (gvol, vol) <- masterVolume 
    -- Creates an amplitude envelope (see Envelope.hs).
    (genv, env) <- linAdsr "amplitude envelope" (AdsrBound 3 3 5) (AdsrInit 0.1 0.5 0.5 0.5) 
    
    -- Places all elements on window with given title and sizes.
    panelBy "" (Just $ Rect 50 50 600 600) (hor [ver [sca 0.25 genv, g], sca 0.1 gvol])

    -- Triggers the instrument with midi controller.
    return $ midi $ onMsg (mul (vol * env) . f)

