 -- | Spectral functions
 module Csound.Air.Spec( 	
    toSpec, fromSpec, mapSpec, scaleSpec, addSpec, scalePitch,
    CrossSpec(..),
    crossSpecFilter, crossSpecVocoder, crossSpecFilter1, crossSpecVocoder1
) where

import Data.Default

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

-- | Settings for cross filtering algorithm.
--
-- They are the defaults for opvodes: @pvsifd@, @tradsyn@, @trcross@ and @partials@.
--
-- * Fft size degree --  it's the power of 2. The default is 12.
--
-- * Hop size degree -- it's the power of 2. The default is 9
--
-- * scale --amplitude scaling factor. default is 1
--
-- * pitch -- the pitch scaling factor. default is 1 
--
-- * @maxTracks@ -- max number of tracks in resynthesis (tradsyn) and analysis (partials).
--
-- * @winType@ -- O: Hamming, 1: Hanning (default)
--
-- * @Search@ -- search interval length. The default is 1.05
--
-- * @Depth@ -- depth of the effect
--
-- * @Thresh@ -- analysis threshold. Tracks below ktresh*max_magnitude will be discarded (1 > ktresh >= 0).The default is 0.01
--
-- * @MinPoints@ -- minimum number of time points for a detected peak to make a track (1 is the minimum).
--
-- * @MaxGap@ -- maximum gap between time-points for track continuation (> 0). Tracks that have no continuation after kmaxgap will be discarded.
data CrossSpec = CrossSpec 
	{ crossFft 		:: D
	, crossHopSize 	:: D
	, crossScale    :: Sig
	, crossPitch    :: Sig
	, crossMaxTracks :: D
	, crossWinType  :: D
	, crossSearch   :: Sig
	, crossDepth    :: Sig
	, crossThresh   :: Sig
	, crossMinPoints :: Sig
	, crossMaxGap    :: Sig
	}

instance Default CrossSpec where
	def = CrossSpec 
		{ crossFft 		= 12
		, crossHopSize 	= 9
		, crossScale    = 1
		, crossPitch    = 1
		, crossMaxTracks = 500
		, crossWinType  = 1
		, crossSearch   = 1.05
		, crossDepth    = 1
		, crossThresh   = 0.01
		, crossMinPoints = 1
		, crossMaxGap    = 3
		}


-- | Filters the partials of the second signal with partials of the first signal.
crossSpecFilter :: CrossSpec -> Sig2 -> Sig2 -> Sig2
crossSpecFilter spec = at2 (crossSpecFilter1 spec)

-- | Substitutes the partials of the second signal with partials of the first signal.
crossSpecVocoder :: CrossSpec -> Sig2 -> Sig2 -> Sig2
crossSpecVocoder spec = at2 (crossSpecVocoder1 spec)

-- | @crossSpecFilter@ for mono signals.
crossSpecFilter1 :: CrossSpec -> Sig -> Sig -> Sig
crossSpecFilter1 = crossSpecBy 0

-- | @crossSpecVocoder@ for mono signals.
crossSpecVocoder1 :: CrossSpec -> Sig -> Sig -> Sig
crossSpecVocoder1 = crossSpecBy 1

crossSpecBy :: D -> CrossSpec -> Sig -> Sig -> Sig
crossSpecBy imode spec ain1 ain2 = 
	tradsyn (trcross (getPartials ain2) (getPartials ain1) (crossSearch spec) (crossDepth spec) `withD` imode) (crossScale spec) (crossPitch spec) (sig $ crossMaxTracks spec) sine
	where
		getPartials asig = partials fs1 fsi2 (crossThresh spec) (crossMinPoints spec) (crossMaxGap spec) (crossMaxTracks spec)
			where (fs1, fsi2) = pvsifd asig (2 ** (crossFft spec)) (2 ** (crossHopSize spec)) (crossWinType spec) 
