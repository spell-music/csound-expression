 -- | Spectral functions
 module Csound.Air.Spec( 	
    toSpec, fromSpec, mapSpec, scaleSpec, addSpec, scalePitch,
    crossSpecFilter, crossSpecVocoder, crossSpecFilter1, crossSpecVocoder1
) where

import Csound.Typed
import Csound.Typed.Opcode
import Csound.Tab(sine)

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

--------------------------------------------------------------------------

at2 :: (Sig -> Sig -> Sig) -> Sig2 -> Sig2 -> Sig2
at2 f (left1, right1) (left2, right2) = (f left1 left2, f right1 right2)

-- | Filters the partials of the first signal with partials of the second signal.
crossSpecFilter :: Sig2 -> Sig2 -> Sig2
crossSpecFilter = at2 crossSpecFilter1

-- | Substitutes the partials of the first signal with partials of the second signal.
crossSpecVocoder :: Sig2 -> Sig2 -> Sig2
crossSpecVocoder = at2 crossSpecVocoder1

-- | @crossSpecFilter@ for mono signals.
crossSpecFilter1 :: Sig -> Sig -> Sig
crossSpecFilter1 = crossSpecBy 0

-- | @crossSpecVocoder@ for mono signals.
crossSpecVocoder1 :: Sig -> Sig -> Sig
crossSpecVocoder1 = crossSpecBy 1

crossSpecBy :: D -> Sig -> Sig -> Sig
crossSpecBy imode ain1 ain2 = 
	tradsyn (trcross (getPartials ain1) (getPartials ain2) 1.05 1 `withD` imode) 1 1 500 sine
	where
		getPartials asig = partials fs1 fsi2 0.01 1 3 500
			where (fs1, fsi2) = pvsifd asig 2048 512 1
