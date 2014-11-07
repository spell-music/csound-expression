 -- | Spectral functions
 module Csound.Air.Spec( 	
    toSpec, fromSpec, mapSpec, scaleSpec, addSpec, scalePitch
) where

import Csound.Typed
import Csound.Typed.Opcode

--------------------------------------------------------------------------
-- spectral functions

-- | Converts signal to spectrum.
toSpec :: Sig -> Spec
toSpec asig = pvsanal asig 1024 256 1024 1

-- | Converts spectrum to signal.
fromSpec :: Spec -> Sig
fromSpec = pvsynth

-- | Applies a transformation to the spectrum of the signal.
mapSpec :: (Spec -> Spec) -> Sig -> Sig
mapSpec f = fromSpec . f . toSpec

-- | Scales all frequencies. Usefull for transposition. 
-- For example, we can transpose a signal by the given amount of semitones: 
--
-- > scaleSpec (semitone 1) asig
scaleSpec :: Sig -> Sig -> Sig
scaleSpec k = mapSpec $ \x -> pvscale x k

-- | Adds given amount of Hz to all frequencies.
--
-- > addSpec hz asig
addSpec :: Sig -> Sig -> Sig
addSpec hz = mapSpec $ \x -> pvshift x hz 0

-- | Scales frequency in semitones.
scalePitch :: Sig -> Sig -> Sig
scalePitch n = scaleSpec (semitone n)

