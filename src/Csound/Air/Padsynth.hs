-- | Padsynth algorithm.
--
-- An example:
--
-- > harms = [ 
-- >     1,  1, 0.7600046992, 0.6199994683, 0.9399998784, 0.4400023818, 0.0600003302, 
-- >     0.8499968648, 0.0899999291, 0.8199964762, 0.3199984133, 
-- >     0.9400014281, 0.3000001907, 0.120003365, 0.1799997687, 0.5200006366]
-- > 
-- > spec = defPadsynthSpec 42.2 harms
-- > 
-- > main = dac $ mul 0.4 $ mixAt 0.35 largeHall2 $ mixAt 0.45 (echo 0.25 0.75) $ 
-- >             midi $ onMsg $ mul (fades 0.5 0.7) . padsynthOsc2 spec

module Csound.Air.Padsynth (
    -- * Generic padsynth oscillators
    padsynthOsc, padsynthOsc2,
    -- * Simple padsynth oscillators
    bwOscBy, bwOddOscBy, bwOscBy2, bwOddOscBy2,
    bwOsc, bwTri, bwSqr, bwSaw, bwOsc2, bwTri2, bwSqr2, bwSaw2,
    -- * Layered padsynth
    padsynthOscMultiCps, padsynthOscMultiCps2,
    padsynthOscMultiVol, padsynthOscMultiVol2,
    padsynthOscMultiVolCps, padsynthOscMultiVolCps2    
) where

import Data.List
import Control.Arrow

import Csound.Typed
import Csound.Tab
import Csound.Air.Wave
import Csound.Typed.Opcode(poscil)

-- | Padsynth oscillator. 
--
-- padsynthOsc spec frequency
--
-- It makes it easy to create padsynth sound waves (see Tab.padsynth). 
-- It creates a padsynth table and reads it with poscil at the right speed.
padsynthOsc :: PadsynthSpec -> Sig -> SE Sig
padsynthOsc spec freq = padsynthOscByTab (double $ padsynthFundamental spec) (padsynth spec) freq

padsynthOscByTab :: D -> Tab -> Sig -> SE Sig
padsynthOscByTab baseFreq tab freq = ares
    where
        len = ftlen tab
        wave = rndPhs (\phs freq -> poscil 1 freq tab `withD` phs)
        ares = wave (freq * (sig $ (getSampleRate / len) / baseFreq))

toStereoOsc :: (a -> SE Sig) -> (a -> SE Sig2)
toStereoOsc f x = do
    left  <- f x
    right <- f x
    return (left, right)

-- | Stereo padsynth oscillatro. It creates two padsynth ftables for left and right channels.
padsynthOsc2 :: PadsynthSpec -> Sig -> SE Sig2
padsynthOsc2 spec freq = toStereoOsc (padsynthOsc spec) freq

layeredPadsynthSpec :: D -> [(D, PadsynthSpec)] -> SE (D, Tab)
layeredPadsynthSpec val specs = do
    refTab      <- newCtrlRef lastTab
    refBaseFreq <- newCtrlRef lastBaseFreq

    compareWhenD val (fmap (second $ toCase refTab refBaseFreq) specs)

    tab <- readRef refTab
    baseFreq <- readRef refBaseFreq

    return (baseFreq, tab)
    where      
        toCase refTab refBaseFreq spec = do
            writeRef refTab (padsynth spec)
            writeRef refBaseFreq (double $ padsynthFundamental spec)

        lastTab      = padsynth $ snd $ last specs
        lastBaseFreq = double $ padsynthFundamental $ snd $ last specs
   
toThreshholdCond :: D -> (Double, PadsynthSpec) -> (BoolD, PadsynthSpec)
toThreshholdCond val (thresh, spec) = (val `lessThanEquals` double thresh, spec)

-- | It uses several padsynth tables. Each table is responsible for specific interval of frequencies.
-- The list of pairs specifies the threshhold value and padsynth specification.
-- The padsynth table is active for all frequencies that lie below the given threshold.
--
-- > padsynthOscMultiCps thresholdSpecPairs frequency = ...
padsynthOscMultiCps :: [(Double, PadsynthSpec)] -> D -> SE Sig
padsynthOscMultiCps specs freq = do
    (baseFreq, tab) <- layeredPadsynthSpec freq (fmap (first double) specs)
    padsynthOscByTab baseFreq tab (sig freq)

-- | Stereo version of @padsynthOscMultiCps@.
padsynthOscMultiCps2 :: [(Double, PadsynthSpec)] -> D -> SE Sig2
padsynthOscMultiCps2 specs freq = do
    (baseFreq, tab) <- layeredPadsynthSpec freq (fmap (first double) specs)
    toStereoOsc (padsynthOscByTab baseFreq tab) (sig freq)

-- | It behaves just like @padsynthOscMultiCps@ but it spreads the padsynth tables among amplitude values.
-- So the last input argument is a pair of amplitude and frequency:
--
-- > padsynthOscMultiVol thresholdSpecPairs (amplitude, frequency) = ...
padsynthOscMultiVol :: [(Double, PadsynthSpec)] -> (D, Sig) -> SE Sig
padsynthOscMultiVol specs (amp, freq) = do
    (baseFreq, tab) <- layeredPadsynthSpec amp (fmap (first double) specs)
    fmap (sig amp * ) $ padsynthOscByTab baseFreq tab freq

-- | Stereo version of @padsynthOscMultiVol@.
padsynthOscMultiVol2 :: [(Double, PadsynthSpec)] -> (D, Sig) -> SE Sig2
padsynthOscMultiVol2 specs (amp, freq) = do
    (baseFreq, tab) <- layeredPadsynthSpec amp (fmap (first double) specs)
    toStereoOsc (fmap (sig amp * ) . padsynthOscByTab baseFreq tab) freq

-- | TODO (undefined function)
--
-- With this function we can create square zones in the domain of @(amplitude, frequency)@.
-- We can assign a separate padsynth table for each zone.
-- The list of pairs contains a pair of two threshold values @(amplitude, frequency)@ and dedicated padsynth specification.
--
-- > padsynthOscMultiVolCps thresholdSpecPairs (amplitude, frequency) = ...
padsynthOscMultiVolCps :: [((Double, Double), PadsynthSpec)] -> (D, D) -> SE Sig
padsynthOscMultiVolCps specs (amp, freq) = undefined

-- | TODO (undefined function)
--
-- Stereo version of @padsynthOscMultiVolCps@.
padsynthOscMultiVolCps2 :: [((Double, Double), PadsynthSpec)] -> (D, D) -> SE Sig2
padsynthOscMultiVolCps2 specs x = toStereoOsc (padsynthOscMultiVolCps specs) x

----------------------------------------------------
-- 

whenElseD :: BoolD -> SE () -> SE () -> SE ()
whenElseD cond ifDo elseDo = whenDs [(cond, ifDo)] elseDo

compareWhenD :: D -> [(D, SE ())] -> SE ()
compareWhenD val conds = case conds of
    [] -> return ()
    [(cond, ifDo)] -> ifDo 
    (cond1, do1):(cond2, do2): [] -> whenElseD (val `lessThan` cond1) do1 do2
    _ -> whenElseD (val `lessThan` rootCond) (compareWhenD val less) (compareWhenD val more)
    where
        (less, more) = splitAt (length conds `div` 2) conds
        rootCond = fst $ last less

----------------------------------------------------
-- waves

-- | Creates padsynth oscillator with given harmonics.
--
-- > bwOscBy harmonics bandwidth frequency
bwOscBy :: [Double] -> Double -> Sig -> SE Sig
bwOscBy harmonics bandwidth = padsynthOsc (defPadsynthSpec bandwidth harmonics)

-- | Stereo version of @bwOscBy@.
bwOscBy2 :: [Double] -> Double -> Sig -> SE Sig2
bwOscBy2 harmonics bandwidth = toStereoOsc (bwOscBy harmonics bandwidth)

-- | Creates padsynth oscillator with given odd harmonics.
--
-- > bwOddOscBy harmonics bandwidth frequency
bwOddOscBy :: [Double] -> Double -> Sig -> SE Sig
bwOddOscBy harmonics bandwidth = padsynthOsc ((defPadsynthSpec bandwidth harmonics) { padsynthHarmonicStretch = 2 })

-- | Stereo version of @bwOddOscBy@.
bwOddOscBy2 :: [Double] -> Double -> Sig -> SE Sig2
bwOddOscBy2 harmonics bandwidth = toStereoOsc (bwOddOscBy harmonics bandwidth)

limit = 15

triCoeff = intersperse 0 $ zipWith (*) (iterate (* (-1)) (1)) $ fmap (\x -> 1 / (x * x)) $ [1, 3 ..]
sqrCoeff = intersperse 0 $ zipWith (*) (iterate (* (-1)) (1)) $ fmap (\x -> 1 / (x))     $ [1, 3 ..]
sawCoeff = zipWith (*) (iterate (* (-1)) (1)) $ fmap (\x -> 1 / (x)) $ [1, 2 ..]

-- | Pure sine wave with padsynth wave table:
--
-- > bwOsc bandwidth frequency
bwOsc :: Double -> Sig -> SE Sig
bwOsc = bwOscBy [1]

-- | Triangle wave with padsynth wave table:
--
-- > bwTri bandwidth frequency
bwTri :: Double -> Sig -> SE Sig
bwTri = bwOscBy (take limit triCoeff)

-- | Square wave with padsynth wave table:
--
-- > bwSqr bandwidth frequency
bwSqr :: Double -> Sig -> SE Sig
bwSqr = bwOscBy (take limit sqrCoeff)

-- | Saw-tooth wave with padsynth wave table:
--
-- > bwSaw bandwidth frequency
bwSaw :: Double -> Sig -> SE Sig
bwSaw = bwOscBy (take limit sawCoeff)


-- | Stereo version of @bwOsc@.
bwOsc2 :: Double -> Sig -> SE Sig2
bwOsc2 bandwidth = toStereoOsc (bwOsc bandwidth)

-- | Stereo version of @bwTri@.
bwTri2 :: Double -> Sig -> SE Sig2
bwTri2 bandwidth = toStereoOsc (bwTri bandwidth)

-- | Stereo version of @bwSqr@.
bwSqr2 :: Double -> Sig -> SE Sig2
bwSqr2 bandwidth = toStereoOsc (bwSqr bandwidth)

-- | Stereo version of @bwSaw@.
bwSaw2 :: Double -> Sig -> SE Sig2
bwSaw2 bandwidth = toStereoOsc (bwSaw bandwidth)


-- harms = [ 1,  1, 0.7600046992, 0.6199994683, 0.9399998784, 0.4400023818, 0.0600003302, 0.8499968648, 0.0899999291, 0.8199964762, 0.3199984133, 0.9400014281, 0.3000001907, 0.120003365, 0.1799997687, 0.5200006366]
-- spec = defPadsynthSpec 82.2 harms
-- dac $ mul 0.4 $ at (bhp 30) $ mixAt 0.35 largeHall2 $ mixAt 0.45 (echo 0.25 0.75) $ midi $ onMsg $ (\cps -> (at (mlp (200 + (cps + 3000)) 0.15) . mul (fades 0.5 0.7) . padsynthOsc2 spec) cps)


-- noisy
-- dac $ mul 0.24 $ at (bhp 30) $ mixAt 0.35 largeHall2 $ mixAt 0.5 (echo 0.25 0.85) $ midi $ onMsg $ (\cps -> (bat (lp (200 + (cps + 3000)) 45) . mul (fades 0.5 0.7) . (\x -> (at (mul 0.3 . fromMono . lp (300 + 2500 * linseg [0, 0.73, 0, 8, 3]) 14) pink) +  padsynthOsc2 spec x + mul 0.5 (padsynthOsc2 spec (x / 2)))) cps)
-- dac $ mul 0.24 $ at (bhp 30) $ mixAt 0.35 largeHall2 $ mixAt 0.5 (echo 0.25 0.85) $ midi $ onMsg $ (\cps -> (bat (lp (200 + (cps + 3000)) 45) . mul (fades 0.5 0.7) . (\x -> (at (mul 0.3 . fromMono . bat (bp (x * 5) 23) . lp (300 + 2500 * linseg [0, 0.73, 0, 8, 3]) 14) white) +  padsynthOsc2 spec x + mul 0.15 (padsynthOsc2 spec (x * 5)) + mul 0.5 (padsynthOsc2 spec (x / 2)))) cps)
-- dac $ mul 0.24 $ at (bhp 30) $ mixAt 0.15 magicCave2 $ mixAt 0.43 (echo 0.35 0.85) $ midi $ onMsg $ (\cps -> (bat (lp (200 + (cps + 3000)) 45) . mul (fades 0.5 0.7) . (\x -> (at (mul 0.3 . fromMono . bat (bp (x * 11) 23) . lp (300 + 2500 * linseg [0, 0.73, 0, 8, 3] * uosc (expseg [0.25, 5, 8])) 14) white) +  padsynthOsc2 spec x + mul 0.15 (padsynthOsc2 spec (x * 5)) + mul 0.5 (padsynthOsc2 spec (x / 2)))) cps)

-- an idea ^ to crossfade between noises 4 knobs and to crossfade between harmonics other 4 knobs
-- for a synth

