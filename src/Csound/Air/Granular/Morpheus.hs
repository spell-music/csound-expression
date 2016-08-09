-- | Wonderful echoes from morpheus.
-- Granular synthesis for morphing between waveforms.
-- It's a simplification of partikkel opcode for the case of morphing.
module Csound.Air.Granular.Morpheus(
	WaveAmp, WaveKey, MorphWave, 
	MorphSpec(..), GrainDensity(..), GrainEnv(..),

	morpheus,

	-- *  Sound files
	morphSnd1, morphSnd,

	-- * Amplitude modes
	pairToSquare,

	-- * Oscillators
	morpheusOsc, morpheusOsc2
) where

import Control.Arrow
import Data.Default

import Csound.Typed
import Csound.Typed.Opcode
import Csound.Tab
import Csound.SigSpace

import Csound.Air.Granular(Pointer, csdPartikkel)
import Csound.Air.Wav
import Csound.Air.Wave
import Csound.Types(compareWhenD)

type WaveAmp = Sig
type WaveKey = Sig

type MorphWave = (Tab, WaveAmp, WaveKey, Pointer)

-- | Density of the grain stream.  
-- 
-- * @rate@ is how many grains per second is generated
--
-- * @size@ is the size of each grain in milliseconds (it's good to set it relative to grain rate)
--
-- * @skip@ skip is a skip ratio (0 to 1). It's the probability of grain skip. Zero means no skip and 1 means every grain is left out.
--
-- see docs for Csound partikkel opcode for more detailed information <http://www.csounds.com/manual/html/partikkel.html>
data GrainDensity = GrainDensity 
	{ grainRate :: Sig
	, grainSize :: Sig
	, grainSkip :: Sig }

instance Default GrainDensity where
	def = GrainDensity
			{ grainRate = kGrainRate
			, grainSize = kduration
			, grainSkip = 0 }
		where 
			kGrainDur	= 2.5							-- length of each grain relative to grain rate 
			kduration	= (kGrainDur*1000)/kGrainRate	-- grain dur in milliseconds, relative to grain rate
			kGrainRate  = 12

-- | Parameters for grain envelope.
--
-- * attShape -- table that contains shape of the attack.
--
-- * decShape -- table that contains shape of the decay
--
-- * sustRatio -- how big is sustain phase relative to attack and decay
--
-- * attack to decay ration -- relative amount of attack decay ration. 0.5 means attack equals decay.
--
-- see docs for Csound partikkel opcode for more detailed information <http://www.csounds.com/manual/html/partikkel.html>
data GrainEnv = GrainEnv 
	{ grainAttShape :: Tab
	, grainDecShape :: Tab
	, grainSustRatio :: Sig
	, grainAttDecRatio :: Sig }

instance Default GrainEnv where
	def = GrainEnv 
			{ grainAttShape = sigmoidRise
			, grainDecShape = sigmoidFall
			, grainSustRatio = 0.25
			, grainAttDecRatio = 0.5 }

-- sigmoidRise = guardPoint $ sines4 [(0.5, 1, 270, 1)]
-- sigmoidFall = guardPoint $ sines4 [(0.5, 1, 90, 1)]

-- | Specification of morphing synth. It has the default instance 
-- and the values in its records has default instances too
data MorphSpec = MorphSpec 
	{ morphGrainDensity :: GrainDensity
	, morphGrainEnv     :: GrainEnv	
	}

instance Default MorphSpec where
	def = MorphSpec 
		{ morphGrainDensity = def
		, morphGrainEnv     = def
		}

-- | Synth that is based on partikkel. It allows easy morphing between up to four waves.
-- Many parameters of partikel were simplified to get the good defaults for sound morphing behaviour.
--
-- > morpheus spec waves frequencyScale
--
-- * spec -- contains many misc parameters
--
-- * waves list can contain up to four wave tables to read grains from.
--
-- * frequencyScale -- scaling factor for frequency. 1 means playing at the original frequency, 2 rises the pitch by octave. 
--     We can use negative values to play the grains in reverse.
morpheus :: MorphSpec -> [MorphWave] -> Sig -> SE Sig2
morpheus spec pwaves cps = do
	iwaveamptab <- makeMorphTable amp1 amp2 amp3 amp4
	return $ csdPartikkel agrainrate kdistribution idisttab async kenv2amt ienv2tab
					ienv_attack ienv_decay ksustain_amount ka_d_ratio kduration kamp igainmasks
               	  	kwavfreq ksweepshape iwavfreqstarttab iwavfreqendtab awavfm
               	  	ifmamptab ifmenv icosine kTrainCps knumpartials
               	  	kchroma ichannelmasks krandommask kwaveform1 kwaveform2 kwaveform3 kwaveform4
               	  	iwaveamptab asamplepos1 asamplepos2 asamplepos3 asamplepos4
               	  	kwavekey1 kwavekey2 kwavekey3 kwavekey4 imax_grains
    where
    	wave1 : wave2 : wave3 : wave4 : _ = cycle pwaves

    	async = 0
    	kamp = 1	
   	
    	ichannelmasks = skipNorm $ doubles [0, 0,  0.5]
    			    	
    	kdistribution = 1
    	idisttab = setSize 16 $ startEnds [1, 16, -10, 0]

    	-- grain shape settings
    	grainEnv = morphGrainEnv spec
    	ienv_attack = grainAttShape grainEnv
    	ienv_decay  = grainDecShape grainEnv
    	ksustain_amount = grainSustRatio grainEnv
    	ka_d_ratio = grainAttDecRatio grainEnv
    	kenv2amt = 0    
    	ienv2tab = eexps [1, 0.0001]	

    	-- grain density
    	grainDensity = morphGrainDensity spec
    	kGrainRate = grainRate grainDensity
    	kduration = grainSize grainDensity

    	kwavfreq = cps

    	krandommask = grainSkip grainDensity

    	-- waves

    	kwavekey1 = getWaveKey wave1
    	kwavekey2 = getWaveKey wave2
    	kwavekey3 = getWaveKey wave3
    	kwavekey4 = getWaveKey wave4

    	asamplepos1 = getSamplePos wave1
    	asamplepos2 = getSamplePos wave2
    	asamplepos3 = getSamplePos wave3
    	asamplepos4 = getSamplePos wave4

    	kwaveform1 = getWaveForm wave1
    	kwaveform2 = getWaveForm wave2
    	kwaveform3 = getWaveForm wave3
    	kwaveform4 = getWaveForm wave4

    	amp1 = getAmp wave1
    	amp2 = getAmp wave2
    	amp3 = getAmp wave3
    	amp4 = getAmp wave4

    	imax_grains = 100   	

    	getWaveKey (tab1, amp1, key1, ptr1) = key1 / sig (getTabLen tab1)

    	getSamplePos (_, _, _, ptr) = ptr
    	getWaveForm (form, _, _, _) = form
    	getAmp (_, amp, _, _) = kr amp

    	-- no trainlets
    	icosine = cosine
    	kTrainCps = kGrainRate
    	knumpartials = 7
    	kchroma = 3

    	-- no FM
    	kGrFmFreq = kGrainRate / 4
    	kGrFmIndex = 0    	
    	aGrFmSig = kGrFmIndex * osc kGrFmFreq
    	agrainrate = kGrainRate + aGrFmSig * kGrainRate
    	ifmenv = elins [0, 1, 0]
    	ifmamptab = skipNorm $ doubles [0, 0, 1]    	
    	awavfm = 0

    	-- other params
    	igainmasks = skipNorm $ doubles [0, 0,   1]
    	ksweepshape = 0.5
    	iwavfreqstarttab = skipNorm $ doubles [0, 0, 1]
    	iwavfreqendtab = skipNorm $ doubles [0, 0, 1]

    	makeMorphTable a1 a2 a3 a4 = do
    		t <- newTab 64
    		mapM_  (\(i, amp) -> tablew amp  (2 + sig (int i)) t ) (zip [0 .. ] [a1, a2, a3, a4])
    		return t

getTabLen t = ftlen t / getSampleRate

pairToSquare :: (Sig, Sig) -> (Sig, Sig, Sig, Sig)
pairToSquare (x, y) = ((1 - x) * (1 - y), x * (1 - y) , x * y, (1 - x) * y)

morphSnd1 :: MorphSpec -> [(String, WaveAmp, WaveKey)] -> Sig -> SE Sig2
morphSnd1 spec waves cps = morpheus spec (fmap fromSnd waves) cps
	where
		fromSnd (file, amp, key) = (wavl file, amp, key, phasor (1 / sig (lengthSnd file)))

morphSnd :: MorphSpec -> [(String, WaveAmp, WaveKey)] -> Sig -> SE Sig2
morphSnd spec waves cps = morphSndByTab wavl spec waves cps + morphSndByTab wavr spec waves cps

morphSndByTab :: (String -> Tab) -> MorphSpec -> [(String, WaveAmp, WaveKey)] -> Sig -> SE Sig2
morphSndByTab getTab spec waves cps = morpheus spec (fmap fromSnd waves) cps
	where
		fromSnd (file, amp, key) = (getTab file, amp, key, phasor (1 / sig (lengthSnd file)))

-- | Morpheus oscillator.
morpheusOsc :: MorphSpec -> (D, Tab) -> Sig -> SE Sig2
morpheusOsc spec (baseFreq, t) cps = morpheus spec waves ratio
	where
		ratio = cps / sig baseFreq
		aptr = cycleTab t
		waves = [(t, 1, 1, aptr)]

cycleTab t = phasor $ sig $ recip $ getTabLen t

-- | Morpheus oscillator.
morpheusOsc2 :: MorphSpec -> D -> [(Sig, Tab)] -> (Sig, Sig) -> Sig -> SE Sig2
morpheusOsc2 spec baseFreq ts (x, y) cps = morpheus spec waves ratio
	where
		(a1, a2, a3, a4) = pairToSquare (x, y)
		ratio = cps / sig baseFreq		
		waves = zipWith (\amp (key, t) -> (t, amp, key, cycleTab t)) [a1, a2, a3, a4] (cycle $ ts)		


{- examples

main' = dac $ mul 0.2 $ morphSnd1 def [("floss/ClassGuit.wav", linseg [1, 3, 1, 3, 0], linseg [1, 3, 1, 3, 0]), ("floss/ClassGuit.wav", linseg [0, 3, 0, 3, 1], (-1))] 1

main = dac $ lift1 (\p -> mixAt 0.25 largeHall2 $ mixAt 0.6 (pingPong 0.124 0.5 0.7) $
	at (filt 2 (\cfq res x -> moogladder x cfq res) (env * 12000) 0.1) $ mul (0.2 * env) $ 
	morpheus (def { morphGrainDensity = def { grainRate = linseg [36, 18, 4], grainSize = linseg [ 1200, 6, 5700, 12, 750 ], grainSkip = 0.45 * uosc 0.17 }}) 
		(tabs p) (negate $ semitone (5))) (ujoy (0.5, 0.5)) 
		where
			tabs (x, y) = [file a1 1, file a2 0.5, file2 a3 1, file3 a4 1]
				where (a1, a2, a3, a4) = pairToSquare (x, y)

			file a x = (wavl "floss/ClassGuit.wav", a, x, linseg [2.5, 18, 3.5])
			file2 a x = (wavl "floss/hd.wav", a, x, linseg [0.2, 18, 0.6])
			file3 a x = (wavl "floss/hd.wav", a, x, linseg [0.02, 18, 0.5])

			env = linseg [0, 1, 1, 3, 1] -- 10, 0]

			amp1 = linseg [1, 8, 1, 4, 0]
			amp2 = linseg [0, 6, 0, 6, 1]

-}


{-
-- todo
-- playing samples in chain

pyramidWeights

partWaveChain :: [Double] -> Sig -> (Sig, Sig, Sig, Sig)
partWaveChain xs pointer = case xs of
	[a, da] -> 
		let (amp1, amp2) = go1 a da pointer
		in  (amp1, amp2, 0, 0)
	[a, da, b, db] -> 		
		let (amp1, amp2, amp3) = go2 a da b db pointer
		in  (amp1, amp2, amp3, 0)
	[a, da, b, db, c, dc] -> 
		let (amp1, amp2, amp3, amp4) = go3 a da b db c dc pointer	
		in  (amp1, amp2, amp3, amp4)
	_ -> error "partWaveChain: wrong number of elements in the list. Should be [a, da], [a, da, b, db] or [a, da, b, db, c, dc]."		
	where
		go1 a da ptr = (readTab t1 ptr, readTab t2 ptr)
			where
				d = da / 2
				t1 = leftTab (a - d) (a + d)
				t2 = rightTab (a - d) (a + d)

		go2 a da b db = (readTab t1 ptr, readTab t2 ptr, readTab t3 ptr)
			where
				da2 = da / 2
				db2 = db / 2
				t1 = leftTab (a - da2) (a + da2)
				t2 = centerTab (a - da2) (a + da2) (b - db2) (b + db2)
				t3 = rightTab (b - db2) (b + db2)

		go3 = undefined

		readTab t ptr = table ptr t1 `withD` 1
		leftTab a b c  = lins [1, a, 1, b, 0, c, 0] 
		rightTab a b c = lins [0, a, 0, b, 1, c, 1] 
		centerTab a b c d e = lins [0, a, 0, b, 1, c, 1, d, 0, e, 0]

partWaveChain2 :: Sig -> (Sig, Sig, Sig, Sig)
partWaveChain2 = partWaveChain [0.5, 0.25]

partWaveChain3 :: Sig -> (Sig, Sig, Sig, Sig) 
partWaveChain3 = partWaveChain [1/3, 0.25, 1/3, 0.25]

partWaveChain4 :: Sig -> (Sig, Sig, Sig, Sig) 
partWaveChain4 = partWaveChain [0.25, 0.2, 0.25, 0.2, 0.25, 0.2]

cfdChainWeights :: [Double] -> Sig -> [Sig]
cfdChainWeights xs ptr = getWeights ptr (getPairs xs)
	where
		getPairs xs = case xs of
			a:b:rest -> (a, b) : getPairs rest
			_        -> []

		getPairs ptr xs = case xs of
			[] -> [1]
			[(a, rada)] -> go1 a rada ptr
			a : as -> goN a (init as) (zip lengs $ makeAdjacentPairs xs) (last as)
		where
			go1 a da ptr = [readTab t1 ptr, readTab t2 ptr]
				where
					d = da / 2
					t1 = leftTab (a - d) (a + d)
					t2 = rightTab (a - d) (a + d)

			goN (start, startRad) center (end, endRad) = 
				startTab ++ centerTabs ++ [endTab]
				where
					startTab = leftTab (start - startRad) (2 * startRad) (1 - (start + startRad))
					endTab   = rightTab (1 - (end - endRad)) (2 * endRad) (end + endRad) 
					centerTabs = fmap toCenterTab center

					toCenterTab (leng, (a, rada), (b, radb)) = centerTab (leng - rada) (2 * rada)

			readTab t ptr = table ptr t1 `withD` 1
			leftTab a b c  = lins [1, a, 1, b, 0, c, 0] 
			rightTab a b c = lins [0, a, 0, b, 1, c, 1] 
			centerTab a b c d e = lins [0, a, 0, b, 1, c, 1, d, 0, e, 0]

			makeAdjacentPairs xs = case xs of
				[] -> []
				x:xs -> tail $ scanl (\(a, b) c -> (b, c)) (x, x) xs 

			lengs xs = tail $ scanl (\res (a, _) -> res + a) 0 xs 
-}			