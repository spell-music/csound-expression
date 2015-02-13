-- | Filters
module Csound.Air.Filter(
    -- | Arguemnts are inversed to get most out of curruing. First come parameters and the last one is the signal.
    
    -- * Simple filters
    lp, hp, bp, br, alp,
    
    -- * Butterworth filters
    blp, bhp, bbp, bbr,

    -- * Specific filters
    mlp,

    -- * Making the smooth lines
    slide
) where

import Csound.Typed
import Csound.Typed.Opcode

-- | Low-pass filter.
--
-- > lp cutoff resonance sig
lp :: Sig -> Sig -> Sig -> Sig
lp cf q a = bqrez a cf q

-- | High-pass filter.
--
-- > hp cutoff resonance sig
hp :: Sig -> Sig -> Sig -> Sig
hp cf q a = bqrez a cf q `withD` 1

-- | Band-pass filter.
--
-- > bp cutoff resonance sig
bp :: Sig -> Sig -> Sig -> Sig
bp cf q a = bqrez a cf q `withD` 2

-- | Band-reject filter.
--
-- > br cutoff resonance sig
br :: Sig -> Sig -> Sig -> Sig
br cf q a = bqrez a cf q `withD` 3

-- | All-pass filter.
--
-- > alp cutoff resonance sig
alp :: Sig -> Sig -> Sig -> Sig
alp cf q a = bqrez a cf q `withD` 4

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


-- | Moog's low-pass filter.
--
-- > mlp centerFrequency qResonance signal
mlp :: Sig -> Sig -> Sig -> Sig
mlp cf q asig = moogladder asig cf q


-- | Produces smooth transitions between values in the signals.
-- The first value defines a duration in seconds for a transition from one
-- value to another in piecewise constant signals.
slide :: Sig -> Sig -> Sig
slide = flip portk
