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
    padsynthOscMultiVolCps, padsynthOscMultiVolCps2,
    layeredPadsynthOsc
) where

import Data.List

import Csound.Typed
import Csound.Tab
import Csound.Air.Wave
import Csound.Typed.Opcode(poscil)

padsynthOsc :: PadsynthSpec -> Sig -> SE Sig
padsynthOsc spec freq = padsybthOscByTab (double $ padsynthFundamental spec) (padsynth spec) freq

padsybthOscByTab :: D -> Tab -> Sig -> SE Sig
padsybthOscByTab baseFreq tab freq = ares
    where
        len = ftlen tab
        wave = rndPhs (\phs freq -> poscil 1 freq tab `withD` phs)
        ares = wave (freq * (sig $ (getSampleRate / len) / baseFreq))

toStereoOsc :: (a -> SE Sig) -> (a -> SE Sig2)
toStereoOsc f x = do
    left  <- f x
    right <- f x
    return (left, right)

padsynthOsc2 :: PadsynthSpec -> Sig -> SE Sig2
padsynthOsc2 spec freq = toStereoOsc (padsynthOsc spec) freq

layeredPadsynthOsc :: [(BoolD, PadsynthSpec)] -> Sig -> SE Sig
layeredPadsynthOsc specs freq = do
    refTab      <- newCtrlRef lastTab
    refBaseFreq <- newCtrlRef lastBaseFreq

    whenDs (fmap (toTabCaseExpr refTab) specs) doNothing
    whenDs (fmap (toBaseFreqCaseExpr refBaseFreq) specs)   doNothing

    tab <- readRef refTab
    baseFreq <- readRef refBaseFreq

    padsybthOscByTab baseFreq tab freq
    where      
        toTabCaseExpr = caseExpr padsynth
        toBaseFreqCaseExpr = caseExpr (double . padsynthFundamental)

        caseExpr extract ref (cond, spec) = (cond, writeRef ref $ extract spec)

        lastTab = padsynth $ snd $ last specs
        lastBaseFreq = double $ padsynthFundamental $ snd $ last specs
        doNothing = return ()
   
toThreshholdCond :: D -> (Double, PadsynthSpec) -> (BoolD, PadsynthSpec)
toThreshholdCond val (thresh, spec) = (val `lessThanEquals` double thresh, spec)

padsynthOscMultiCps :: [(Double, PadsynthSpec)] -> D -> SE Sig
padsynthOscMultiCps specs freq = layeredPadsynthOsc (fmap (toThreshholdCond freq) specs) (sig freq)
           
padsynthOscMultiCps2 :: [(Double, PadsynthSpec)] -> D -> SE Sig2
padsynthOscMultiCps2 specs freq = toStereoOsc (padsynthOscMultiCps specs) freq

padsynthOscMultiVol :: [(Double, PadsynthSpec)] -> (D, Sig) -> SE Sig
padsynthOscMultiVol specs (amp, freq) = fmap (sig amp * ) $ layeredPadsynthOsc (fmap (toThreshholdCond amp) specs) freq

padsynthOscMultiVol2 :: [(Double, PadsynthSpec)] -> (D, Sig) -> SE Sig2
padsynthOscMultiVol2 specs freq = toStereoOsc (padsynthOscMultiVol specs) freq

padsynthOscMultiVolCps :: [((Double, Double), PadsynthSpec)] -> (D, D) -> SE Sig
padsynthOscMultiVolCps specs (amp, freq) = fmap (sig amp * ) $ layeredPadsynthOsc (fmap (toCond amp freq) specs) (sig freq)
    where 
        toCond = undefined

padsynthOscMultiVolCps2 :: [((Double, Double), PadsynthSpec)] -> (D, D) -> SE Sig2
padsynthOscMultiVolCps2 specs x = toStereoOsc (padsynthOscMultiVolCps specs) x

----------------------------------------------------
-- waves


bwOscBy :: [Double] -> Double -> Sig -> SE Sig
bwOscBy harmonics bandwidth = padsynthOsc (defPadsynthSpec bandwidth harmonics)

bwOscBy2 :: [Double] -> Double -> Sig -> SE Sig2
bwOscBy2 harmonics bandwidth = toStereoOsc (bwOscBy harmonics bandwidth)

bwOddOscBy :: [Double] -> Double -> Sig -> SE Sig
bwOddOscBy harmonics bandwidth = padsynthOsc ((defPadsynthSpec bandwidth harmonics) { padsynthHarmonicStretch = 2 })

bwOddOscBy2 :: [Double] -> Double -> Sig -> SE Sig2
bwOddOscBy2 harmonics bandwidth = toStereoOsc (bwOddOscBy harmonics bandwidth)

limit = 15

triCoeff = intersperse 0 $ zipWith (*) (iterate (* (-1)) (1)) $ fmap (\x -> 1 / (x * x)) $ [1, 3 ..]
sqrCoeff = intersperse 0 $ zipWith (*) (iterate (* (-1)) (1)) $ fmap (\x -> 1 / (x))     $ [1, 3 ..]
sawCoeff = zipWith (*) (iterate (* (-1)) (1)) $ fmap (\x -> 1 / (x)) $ [1, 2 ..]

bwOsc :: Double -> Sig -> SE Sig
bwOsc = bwOscBy [1]

bwTri :: Double -> Sig -> SE Sig
bwTri = bwOscBy (take limit triCoeff)

bwSqr :: Double -> Sig -> SE Sig
bwSqr = bwOscBy (take limit sqrCoeff)

bwSaw :: Double -> Sig -> SE Sig
bwSaw = bwOscBy (take limit sawCoeff)

bwOsc2 :: Double -> Sig -> SE Sig2
bwOsc2 bandwidth = toStereoOsc (bwOsc bandwidth)

bwTri2 :: Double -> Sig -> SE Sig2
bwTri2 bandwidth = toStereoOsc (bwTri bandwidth)

bwSqr2 :: Double -> Sig -> SE Sig2
bwSqr2 bandwidth = toStereoOsc (bwSqr bandwidth)

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

