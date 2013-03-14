-- | The vital tools.
module Csound.Air (
    -- * Oscillators
    
    -- ** Bipolar
    osc, saw, sq, tri,
    
    -- ** Unipolar
    unipolar, uosc, usaw, usq, utri,
    
    -- * Filters
    -- | Arguemnts are inversed to get most out of curruing. First come parameters and the last one is the signal.
    
    -- ** Simple filters
    lp, hp, bp, br,
    
    -- ** Butterworth filters
    blp, bhp, bbp, bbr,
    
    -- * Patterns
    once, mean,
    
    -- ** Series
    hase, whase,
    haseS, whaseS,
    
    -- ** Crossfade
    cfd, cfds, cfdSpec, cfdsSpec
) where

import Csound.Exp(Tab)
import Csound.Exp.Wrapper(Sig, Spec, SE, kr)
import Csound.Exp.Numeric
import Csound.Opcode(idur, oscil3, pvscross, 
    atone, tone, areson, reson,
    buthp, butbp, butlp, butbr)
import Csound.Tab(hifi, sines, guardPoint)

--------------------------------------------------------------------------
-- oscillators

-- | Pure tone.
osc :: Sig -> Sig
osc cps = oscil3 1 cps (hifi $ sines [1])

-- | Sawtooth.
saw :: Sig -> Sig
saw cps = oscil3 1 cps (hifi $ sines [1, 0.5, 0.3, 0.25, 0.2, 0.167, 0.14, 0.111])

-- | Square wave.
sq :: Sig -> Sig
sq cps = oscil3 1 cps (hifi $ sines [1, 0, 0.3, 0, 0.2, 0, 0.14, 0, 0.111])

-- | Triangle wave.
tri :: Sig -> Sig
tri cps = oscil3 1 cps (hifi $ sines [1, 0, -1/9, 0, 1/25, 0, -1/49, 0, 1/81, 0, -1/121])

-- unipolar waves

-- | Turns a bipolar sound (ranges from -1 to 1) to unipolar (ranges from 0 to 1)
unipolar :: Sig -> Sig
unipolar a = 0.5 + 0.5 * a

-- | Unipolar pure tone.
uosc :: Sig -> Sig
uosc = unipolar . osc

-- | Unipolar sawtooth.
usaw :: Sig -> Sig
usaw = unipolar . saw

-- | Unipolar square wave.
usq :: Sig -> Sig
usq = unipolar . sq

-- | Unipolar triangle wave.
utri :: Sig -> Sig
utri = unipolar . tri

--------------------------------------------------------------------------
-- filters

-- | High-pass filter.
--
-- > hp cutoff sig
hp :: Sig -> Sig -> Sig
hp = flip atone

-- | Low-pass filter.
--
-- > lp cutoff sig
lp :: Sig -> Sig -> Sig
lp = flip tone

-- | Band-pass filter.
--
-- > bp cutoff bandwidth sig
bp :: Sig -> Sig -> Sig -> Sig
bp freq band a = reson a freq band

-- | Band-regect filter.
--
-- > br cutoff bandwidth sig
br :: Sig -> Sig -> Sig -> Sig 
br freq band a = areson a freq band

-- Butterworth filters

-- | High-pass filter.
--
-- > bhp cutoff sig
bhp :: Sig -> Sig -> Sig
bhp = flip buthp

-- | Low-pass filter.
--
-- > blp cutoff sig
blp :: Sig -> Sig -> Sig
blp = flip butlp

-- | Band-pass filter.
--
-- > bbp cutoff bandwidth sig
bbp :: Sig -> Sig -> Sig -> Sig
bbp freq band a = butbp a freq band

-- | Band-regect filter.
--
-- > bbr cutoff bandwidth sig
bbr :: Sig -> Sig -> Sig -> Sig 
bbr freq band a = butbr a freq band


--------------------------------------------------------------------------
-- patterns

-- | Reads table once during the note length. Guard point is inserted inside the function.
-- No need to specify it explicitly.
once :: Tab -> Sig
once a = oscil3 1 (1 / kr idur) (guardPoint a)

-- | Mean value.
mean :: Fractional a => [a] -> a
mean xs = sum xs / (fromIntegral $ length xs)

-- | Harmonic series. Takes a function that transforms the signal by some parameter
-- and the list of parameters. It constructs the series of transformers and sums them
-- at the end with equal strength.
hase :: (a -> Sig -> Sig) -> [a] -> Sig -> Sig
hase f as x = mean $ fmap (( $ x) . f) as

-- | Harmonic series, but now you can specify the weights of the final sum.
whase :: (a -> Sig -> Sig) -> [(Sig, a)] -> Sig -> Sig
whase f as x = sum $ fmap (\(weight, param) -> weight * f param x) as

-- | Harmonic series for functions with side effects.
haseS :: (a -> Sig -> SE Sig) -> [a] -> Sig -> SE Sig
haseS mf as x = fmap mean $ mapM (\param -> mf param x) as

-- | Weighted harmonic series for functions with side effects.
whaseS :: (a -> Sig -> SE Sig) -> [(Sig, a)] -> Sig -> SE Sig
whaseS mf as x = fmap sum $ mapM (\(weight, param) -> fmap (weight * ) (mf param x)) as


-- | Crossfade.
--
-- > cfd coeff sig1 sig2
--
-- If coeff equals 0 then we get the first signal and if it equals 1 we get the second signal.
cfd :: Sig -> Sig -> Sig -> Sig
cfd coeff a b = (1 - coeff) * a + coeff * b
  
genCfds :: a -> (Sig -> a -> a -> a) -> [Sig] -> [a] -> a
genCfds zero mixFun cs xs = case xs of
    []   -> zero
    a:as -> foldl (\x f -> f x) a $ zipWith mix' cs as 
    where mix' c a b = mixFun c b a
  
-- | Generic crossfade for n coefficients and n+1 signals.
--
-- > cfds coeffs sigs
cfds :: [Sig] -> [Sig] -> Sig
cfds = genCfds 0 cfd

-- | Spectral crossfade.
cfdSpec :: Sig -> Spec -> Spec -> Spec
cfdSpec coeff a b = pvscross a b (1 - coeff) coeff

-- | Generic spectral crossfade.
cfdsSpec :: [Sig] -> [Spec] -> Spec
cfdsSpec = genCfds undefined cfdSpec

    
