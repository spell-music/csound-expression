-- | Effects
module Csound.Air.Pan(    
    HeadPanSpec(..),
    headPan, headPan', staticHeadPan,
    headPan2, headPan2', staticHeadPan2,
    headPanNet, headPanNet2
) where

import Csound.Typed
import Csound.Air.Wav(toMono)

-- | The same as headPan but for stereo signals.
headPan2 :: (Sig, Sig) -> Sig2 -> Sig2
headPan2 point = headPan point . toMono

-- | The same as headPan' but for stereo signals.
headPan2' :: HeadPanSpec -> (Sig, Sig) -> Sig2 -> Sig2
headPan2' spec point = headPan' spec point . toMono

-- | The same as staticHeadPan but for stereo signals.
staticHeadPan2 :: (D, D) -> Sig2 -> Sig2
staticHeadPan2 point = staticHeadPan point . toMono

-- | Net of sounds evenly distributed oround the head.
-- First argument is a pair of numbers (column, rows) in the matrix.
-- The second argument is a matrix written in a single list.
-- The rows are for elevation and the columns are for azimuth.
--
-- A ghci session example:
--
-- > let f t x = mul 0.4 $ sched (\_ -> return $ fades 0.07 0.1 * tri x) $ withDur 0.2 $ metro t
-- > dac $ headPanNet (3, 2) [f 1 220, f 0.75 330, f 0.5 440, f 0.2 660, delaySnd 0.75 $ f 2 (220 * 5/4),delaySnd 0.4 $  f 1 (220 * 9/8)]
headPanNet :: (Int, Int) -> [Sig] -> Sig2
headPanNet (m, n) sigs = sum $ zipWith staticHeadPan [(double a, double b) | a <- xs m, b <- xs n] sigs
    where xs t = fmap (( / fromIntegral t) . fromIntegral) [0 .. (t - 1)]
  
-- | The same as headPanNet but for stereo signals.
headPanNet2 :: (Int, Int) -> [Sig2] -> Sig2
headPanNet2 (m, n) sigs = headPanNet (m, n) (fmap toMono sigs)



