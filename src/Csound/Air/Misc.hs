-- | Patterns
module Csound.Air.Misc(
    mean, vibrate, randomPitch, chorusPitch, resons, resonsBy, modes, dryWet, 
    once, onceBy, several, fromMono,
    -- * List functions
    odds, evens,
    -- * Random functions
    rndPan, rndPan2, rndVol, gaussVol, 
    -- * Saving to file
    writeHifi
) where

import Data.Boolean

import Csound.Typed
import Csound.Typed.Opcode
import Csound.Air.Wave
import Csound.Air.Filter
import Csound.SigSpace
import Csound.IO(writeSndBy)
import Csound.Options(setRates)
import Csound.Types(Sig2)

--------------------------------------------------------------------------
-- patterns

-- | Selects odd elements from the list.
odds :: [a] -> [a]
odds as = fmap snd $ filter fst $ zip (cycle [True, False]) as 

-- | Selects even elements from the list.
evens :: [a] -> [a]
evens as 
    | null as   = []
    | otherwise = odds $ tail as

-- | Reads table once during the note length. 
once :: Tab -> Sig
once = onceBy idur

-- | Reads table once during a given period of time. 
onceBy :: D -> Tab -> Sig
onceBy dt tb = kr $ oscBy tb (1 / sig dt) 

-- | Reads table several times during the note length.  
several :: Tab -> Sig -> Sig
several tb rate = kr $ oscil3 1 (rate / sig idur) tb

-- | Mean value.
mean :: Fractional a => [a] -> a
mean xs = sum xs / (fromIntegral $ length xs)

-- | Adds vibrato to the sound unit. Sound units is a function that takes in a frequency. 
vibrate :: Sig -> Sig -> (Sig -> a) -> (Sig -> a)
vibrate vibDepth vibRate f cps = f (cps * (1 + kvib))
    where kvib = vibDepth * kr (osc vibRate) 

-- | Adds a random vibrato to the sound unit. Sound units is a function that takes in a frequency. 
randomPitch :: Sig -> Sig -> (Sig -> a) -> (Sig -> SE a)
randomPitch rndAmp rndCps f cps = fmap go $ randh (cps * rndAmp) rndCps
    where go krand = f (cps + krand)

-- | Chorus takes a number of copies, chorus width and wave shape.
chorusPitch :: Int -> Sig -> (Sig -> Sig) -> Sig -> Sig
chorusPitch n wid = phi dts
    where
        phi :: [Sig] -> (Sig -> Sig) -> Sig -> Sig
        phi ks f = \cps -> mean $ fmap (f . (+ cps)) ks

        dts = fmap (\x -> - wid + fromIntegral x * dt) [0 .. n-1] 

        dt = 2 * wid / fromIntegral n


-- | Applies a resonator to the signals. A resonator is
-- a list of band pass filters. A list contains the parameters for the filters:
--
-- > [(centerFrequency, bandWidth)]
resons :: [(Sig, Sig)] -> Sig -> Sig
resons = resonsBy bp

-- | A resonator with user defined band pass filter.
-- Warning: a filter takes in a center frequency, band width and the signal.
-- The signal comes last (this order is not standard in the Csound but it's more
-- convinient to use with Haskell).
resonsBy :: (cps -> bw -> Sig -> Sig) -> [(cps, bw)] -> Sig -> Sig
resonsBy filt ps asig = mean $ fmap (( $ asig) . uncurry filt) ps

-- | Mixes dry and wet signals. 
--
-- > dryWet ratio effect asig
--
-- * @ratio@ - of dry signal to wet
--
-- * @effect@ - means to wet the signal
--
-- * @asig@ -- processed signal
dryWet :: Sig -> (Sig -> Sig) -> Sig -> Sig
dryWet k ef asig = k * asig + (1 - k) * ef asig


-- | Chain of mass-spring-damping filters.
--
-- > modes params baseCps exciter 
--
-- * params - a list of pairs @(resonantFrequencyRatio, filterQuality)@
--
-- * @baseCps@ - base frequency of the resonator
--
-- * exciter - an impulse that starts a resonator.
modes :: [(Sig, Sig)] -> Sig -> Sig -> Sig
modes = relResonsBy (\cf q asig -> mode asig cf q)

relResonsBy :: (Sig -> a -> Sig -> Sig) -> [(Sig, a)] -> Sig -> Sig -> Sig
relResonsBy resonator ms baseCps apulse = (recip normFactor * ) $ sum $ fmap (\(cf, q) -> harm cf q apulse) ms
    where 
        -- limit modal frequency to prevent explosions by 
        -- skipping if the maximum value is exceeded (with a little headroom)
        gate :: Sig -> Sig
        gate cps = ifB (sig getSampleRate >* pi * cps) 1 0        

        normFactor = sum $ fmap (gate . (* baseCps) . fst) ms

                                    -- an ugly hack to make filter stable for forbidden values)
        harm cf q x = g * resonator (1 - g + g * cps) q x
            where cps = cf * baseCps
                  g   = gate cps

-- | Doubles the mono signal to get the stereo signal.
fromMono :: Sig -> (Sig, Sig)
fromMono a = (a, a)


-- | Random panning
rndPan2 :: Sig2 -> SE Sig2
rndPan2 (a, b) = rndPan $ mean [a, b]

-- | Random panning
rndPan :: Sig -> SE Sig2
rndPan a = do   
    return $ pan2 a (sig $ rnd (1 :: D))

-- | Random volume (with gauss distribution)
-- 
-- > gaussVol radiusOfDistribution
gaussVol :: SigSpace a => D -> a -> SE a
gaussVol k a = do
    level <- fmap ir $ gauss (sig k)
    return $ mul (sig $ level + 1) a

-- | Random volume
-- 
-- > gaussVol (minVolume, maxVolume)
rndVol :: SigSpace a => (D, D) -> a -> SE a
rndVol (kMin, kMax) a = do
    let level = rnd (1 :: D)
    return $ mul (sig $ kMin + (kMax - kMin) * level) a


-- | Hi-fi output for stereo signals. Saves the stereo signal to file.
-- The length of the file is defined in seconds.
--
-- > writeHifi fileLength fileName asig
writeHifi :: D -> String -> SE Sig2 -> IO ()
writeHifi n fileName a = writeSndBy (setRates 48000 10) fileName $ fmap (setDur $ n) a

