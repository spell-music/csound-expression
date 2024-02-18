{-# Language DeriveFunctor #-}
module Sampler (
	Sample, Sam, Bpm, runSam, 
	-- * Lifters
	mapBpm, bindSam, bindBpm, liftSam,
	-- * Constructors
	sig1, sig2, infSig1, infSig2, rest,
	-- ** Stereo
	wav, wavr, seg, segr, rndWav, rndWavr, rndSeg, rndSegr, ramWav,
	-- ** Mono
	wav1, wavr1, seg1, segr1, rndWav1, rndWavr1, rndSeg1, rndSegr1, ramWav1,	
	-- * Envelopes
	linEnv, expEnv, hatEnv, decEnv, riseEnv, edecEnv, eriseEnv,
	-- * Arrange
	del, flow, pick, pickBy, lim, atPch, atCps,	
	-- * Loops
	loop, rep1, rep, pat1, pat, wall,	
	-- * Arpeggio
	arpUp, arpDown, arpOneOf, arpFreqOf,
	arpUp1, arpDown1, arpOneOf1, arpFreqOf1,
	-- * Chords
	chTrans, chRot, chMin, chMaj, chLead, chMaj7, chMin7, ch7, chLead7, atMaj, atMin, atMaj7, atMin7
) where

import Data.List(foldr1)

import Control.Monad.Trans.Class
import Control.Applicative
import Control.Monad.Trans.Reader

import Csound.Base

type Sam = Sample Sig2

data Dur = Dur D | InfDur

type Bpm = D

newtype Sample a = Sam { unSam :: ReaderT Bpm SE (S a) 
	} deriving (Functor)

instance Applicative Sample where
	pure = Sam . pure . pure
	(Sam rf) <*> (Sam ra) = Sam $ liftA2 (<*>) rf ra

data S a = S
	{ samSig :: a
	, samDur :: Dur 
	} deriving (Functor)

instance Applicative S where
	pure a = S a InfDur
	(S f df) <*> (S a da) = S (f a) $ case (df, da) of
		(Dur df, Dur da) -> Dur $ maxB df da
		_			     -> InfDur

data EnvType = TrapLin D D | TrapExp D D | CosEnv

instance Num a => Num (Sample a) where
	(+) = liftA2 (+)
	(*) = liftA2 (*)
	(-) = liftA2 (-)
	negate = fmap negate
	abs = fmap abs
	signum = fmap signum
	fromInteger = pure . fromInteger

instance Fractional a => Fractional (Sample a) where
	recip = fmap recip
	fromRational = pure . fromRational

instance SigSpace a => SigSpace (Sample a) where
	mapSig f = fmap (mapSig f)

	
-- | Hides the effects inside sample.
liftSam :: Sample (SE a) -> Sample a
liftSam (Sam ra) = Sam $ do
	a <- ra
	lift $ fmap (\x -> a{ samSig = x}) $ samSig a

-- | Transforms the sample with BPM.
mapBpm :: (Bpm -> Sig2 -> Sig2) -> Sam -> Sam
mapBpm f (Sam ra) = Sam $ do
	bpm <- ask
	a <- ra
	return $ a { samSig = f bpm $ samSig a }

-- | Lifts bind on stereo signals to samples.
bindSam :: (Sig2 -> SE Sig2) -> Sam -> Sam
bindSam f = liftSam . fmap f

-- | Lifts bind on stereo signals to samples with BPM.
bindBpm :: (Bpm -> Sig2 -> SE Sig2) -> Sam -> Sam
bindBpm f (Sam ra) = Sam $ do
	bpm <- ask
	a <- ra
	lift $ fmap (\x -> a{ samSig = x}) $ f bpm $ samSig a


-- | Constructs sample from mono signal
infSig1 :: Sig -> Sam
infSig1 x = pure (x, x)

-- | Constructs sample from stereo signal
infSig2 :: Sig2 -> Sam
infSig2 = pure

-- | Constructs sample from limited mono signal (duration is in BPMs)
sig1 :: D -> Sig -> Sam
sig1 dt a = Sam $ reader $ \bpm -> S (a, a) (Dur $ toSec bpm dt)

-- | Constructs sample from limited stereo signal (duration is in BPMs)
sig2 :: D -> Sig2 -> Sam
sig2 dt a = Sam $ reader $ \bpm -> S a (Dur $ toSec bpm dt)

-- | Constructs silence. The first argument is length in BPMs.
rest :: D -> Sam
rest dt = Sam $ reader $ \bpm -> S 0 (Dur $ toSec bpm dt)

-- | Constructs sample from wav or aiff files.
wav :: String -> Sam
wav fileName = Sam $ return $ S (readSnd fileName) (Dur $ lengthSnd fileName)

-- | Constructs sample from wav that is played in reverse.
wavr :: String -> Sam
wavr fileName = Sam $ return $ S (takeSnd len $ loopWav (-1) fileName) (Dur len)
	where len = lengthSnd fileName

-- | Constructs sample from the segment of a wav file. The start and end times are measured in seconds.
--
-- > seg begin end fileName
seg :: D -> D -> String -> Sam
seg start end fileName = Sam $ return $ S (readSegWav start end 1 fileName) (Dur len)
	where len = end - start

--- | Constructs reversed sample from segment of an audio file.
segr :: D -> D -> String -> Sam
segr start end fileName = Sam $ return $ S (readSegWav start end (-1) fileName) (Dur len)
	where len = end - start

-- | Picks segments from the wav file at random. The first argument is the length of the segment.
rndWav :: D -> String -> Sam
rndWav dt fileName = rndSeg dt 0 (lengthSnd fileName) fileName

-- | Picks segments from the wav file at random. The first argument is the length of the segment.
rndWavr :: D -> String -> Sam
rndWavr dt fileName = rndSegr dt 0 (lengthSnd fileName) fileName

-- | Constructs random segments of the given length from an interval.
rndSeg :: D -> D -> D -> String -> Sam
rndSeg = genRndSeg 1

-- | Constructs reversed random segments of the given length from an interval.
rndSegr :: D -> D -> D -> String -> Sam
rndSegr = genRndSeg (-1)

genRndSeg :: Sig -> D -> D -> D -> String -> Sam
genRndSeg speed len start end fileName = Sam $ lift $ do
	x <- random 0 1
	let a = start + dl * x
	let b = a + len
	return $ S (readSegWav a b speed fileName) (Dur len)
	where dl = end - len

-- | Reads a sample from the file in RAM.
--
-- > ramWav loopMode speed fileName
ramWav :: LoopMode -> Sig -> String -> Sam
ramWav mode speed fileName = Sam $ return $ S (ramSnd mode speed fileName) (Dur $ lengthSnd fileName)

-- | Reads a sample from the mono file in RAM.
--
-- > ramWav1 loopMode speed fileName
ramWav1 :: LoopMode -> Sig -> String -> Sam
ramWav1 mode speed fileName = Sam $ return $ S (let x = ramSnd1 mode speed fileName in (x, x)) (Dur $ lengthSnd fileName)

-- | Constructs sample from mono wav or aiff files.
wav1 :: String -> Sam
wav1 fileName = Sam $ return $ S (let x = readSnd1 fileName in (x, x)) (Dur $ lengthSnd fileName)

-- | Constructs sample from mono wav that is played in reverse.
wavr1 :: String -> Sam
wavr1 fileName = Sam $ return $ S (let x = takeSnd len $ loopWav1 (-1) fileName in (x, x)) (Dur len)
	where len = lengthSnd fileName

-- | Constructs sample from the segment of a mono wav file. The start and end times are measured in seconds.
--
-- > seg begin end fileName
seg1 :: D -> D -> String -> Sam
seg1 start end fileName = Sam $ return $ S (let x = readSegWav1 start end 1 fileName in (x, x)) (Dur len)
	where len = end - start

--- | Constructs reversed sample from segment of a mono audio file.
segr1 :: D -> D -> String -> Sam
segr1 start end fileName = Sam $ return $ S (let x = readSegWav1 start end (-1) fileName in (x, x)) (Dur len)
	where len = end - start

-- | Picks segments from the mono wav file at random. The first argument is the length of the segment.
rndWav1 :: D -> String -> Sam
rndWav1 dt fileName = rndSeg1 dt 0 (lengthSnd fileName) fileName

-- | Picks segments from the mono wav file at random. The first argument is the length of the segment.
rndWavr1 :: D -> String -> Sam
rndWavr1 dt fileName = rndSegr1 dt 0 (lengthSnd fileName) fileName

-- | Constructs random segments of the given length from an interval.
rndSeg1 :: D -> D -> D -> String -> Sam
rndSeg1 = genRndSeg1 1

-- | Constructs reversed random segments of the given length from an interval.
rndSegr1 :: D -> D -> D -> String -> Sam
rndSegr1 = genRndSeg1 (-1)

genRndSeg1 :: Sig -> D -> D -> D -> String -> Sam
genRndSeg1 speed len start end fileName = Sam $ lift $ do
	x <- random 0 1
	let a = start + dl * x
	let b = a + len
	return $ S (let x = readSegWav1 a b speed fileName in (x, x)) (Dur len)
	where dl = end - len


toSec :: Bpm -> D -> D
toSec bpm a = a * 60 / bpm

toSecSig :: Bpm -> Sig -> Sig
toSecSig bpm a = a * 60 / sig bpm

runSam :: Bpm -> Sam -> SE Sig2
runSam bpm x = fmap samSig $ runReaderT (unSam x) bpm

addDur :: D -> Dur -> Dur
addDur d x = case x of
	Dur a  -> Dur $ d + a
	InfDur -> InfDur

-- | Scales sample by pitch in tones.
atPch :: Sig -> Sam -> Sam
atPch k = mapSig (scalePitch k)

-- | Scales sample by pitch in factor of frequency.
atCps :: Sig -> Sam -> Sam
atCps k = mapSig (scaleSpec k)


tfmBy :: (S Sig2 -> Sig2) -> Sam -> Sam
tfmBy f = Sam . fmap (\x -> x { samSig = f x }) . unSam

tfmR :: (Bpm -> Sig2 -> Sig2) -> Sam -> Sam
tfmR f ra = Sam $ do
	bpm <- ask
	a <- unSam ra
	return $ a { samSig = f bpm $ samSig a }

tfmS :: (Bpm -> S Sig2 -> S Sig2) -> Sam -> Sam
tfmS f ra = Sam $ do
	bpm <- ask
	a <- unSam ra
	return $ f bpm a	

setInfDur :: Sam -> Sam
setInfDur = Sam . fmap (\a -> a { samDur = InfDur }) . unSam


-- | Delays a sample by the given amount of BPMs.
del :: D -> Sam -> Sam
del dt = tfmS phi
	where phi bpm x = x { samSig = sig, samDur = dur }
			where 
				absDt = toSec bpm dt
				sig   = delaySnd absDt $ samSig x
				dur   = addDur absDt $ samDur x

-- | Plays a list of samples one after another.
flow :: [Sam] -> Sam
flow [] = 0
flow as = foldr1 flow2 as 

flow2 :: Sam -> Sam -> Sam
flow2 (Sam ra) (Sam rb) = Sam $ do
	a <- ra
	b <- rb
	let sa = samSig a
	let sb = samSig b
	return $ case (samDur a, samDur b) of
		(Dur da, Dur db) -> S (sa + delaySnd da sb) (Dur $ da + db)
		(InfDur, _)      -> a
		(Dur da, InfDur) -> S (sa + delaySnd da sb) InfDur

type PickFun = [(D, D)] -> Evt Unit -> Evt (D, D)

genPick :: PickFun -> Sig -> [Sam] -> Sam
genPick pickFun dt as = Sam $ do
	bpm <- ask
	xs <- sequence $ fmap unSam as
	let ds = fmap (getDur . samDur)  xs
	let sigs = fmap samSig xs
	return $ S (sched (\n -> return $ atTuple sigs $ sig n) $ pickFun (zip ds (fmap int [0..])) $ metroS bpm dt) InfDur 
	where getDur x = case x of
			InfDur -> -1
			Dur d  -> d

-- | Picks samples at random. The first argument is the period ofmetronome in BPMs.
-- The tick of metronome produces new random sample from the list.
pick :: Sig -> [Sam] -> Sam
pick = genPick oneOf

-- | Picks samples at random. We can specify a frequency of the occurernce.
-- The sum of all frequencies should be equal to 1.
pickBy :: Sig -> [(D, Sam)] -> Sam
pickBy dt as = genPick (\ds -> freqOf $ zip (fmap fst as) ds) dt (fmap snd as)

-- | Limits the length of the sample. The length is expressed in BPMs.
lim :: D -> Sam -> Sam
lim d = tfmS $ \bpm x -> 
	let absD = toSec bpm d 
	in  x { samSig = takeSnd absD $ samSig x
		  , samDur = Dur absD }

type EnvFun = (Dur -> D -> D -> Sig)

genEnv :: EnvFun -> D -> D -> Sam -> Sam
genEnv env start end = tfmS f
	where f bpm a = a { samSig = mul (env (samDur a) absStart absEnd) $ samSig a }
			where 
				absStart = toSec bpm start
				absEnd   = toSec bpm end

-- | A linear rise-decay envelope. Times a given in BPMs.
--
-- > linEnv rise dec sample
linEnv :: D -> D -> Sam -> Sam
linEnv = genEnv f
	where f dur start end = case dur of
			InfDur -> linseg [0, start, 1]
			Dur d  -> linseg [0, start, 1, maxB 0 (d - start - end), 1, end , 0]

-- | An exponential rise-decay envelope. Times a given in BPMs.
--
-- > expEnv rise dec sample
expEnv :: D -> D -> Sam -> Sam
expEnv = genEnv f
	where 
		f dur start end = case dur of
			InfDur -> expseg [zero, start, 1]
			Dur d  -> expseg [zero, start, 1, maxB 0 (d - start - end), 1, end , zero]
		zero = 0.00001

genEnv1 :: (D -> Sig) -> Sam -> Sam
genEnv1 envFun = tfmBy f
	where 
		f a = flip mul (samSig a) $ case samDur a of
			InfDur -> 1
			Dur d  -> envFun d


-- | Parabolic envelope that starts and ends at zero and reaches maximum at the center.
hatEnv :: Sam -> Sam
hatEnv = genEnv1 $ \d -> oscBy (polys 0 1 [0, 1, -1]) (1 / sig d)

-- | Fade in linear envelope.
riseEnv :: Sam -> Sam
riseEnv = genEnv1 $ \d -> linseg [0, d, 1]

-- | Fade out linear envelope.
decEnv :: Sam -> Sam
decEnv = genEnv1 $ \d -> linseg [1, d, 0]

-- | Fade in exponential envelope.
eriseEnv :: Sam -> Sam
eriseEnv = genEnv1 $ \d -> expseg [0.0001, d, 1]

-- | Fade out exponential envelope.
edecEnv :: Sam -> Sam
edecEnv = genEnv1 $ \d -> expseg [1, d, 0.0001]

type LoopFun = D -> D -> Sig2 -> Sig2

genLoop :: LoopFun -> Sam -> Sam 
genLoop g = setInfDur . tfmS f
	where 
		f bpm a = a { samSig = case samDur a of
			InfDur -> samSig a
			Dur d  -> g bpm d (samSig a)
		}

-- | Plays sample in the loop.
loop :: Sam -> Sam
loop = genLoop $ \_ d asig -> repeatSnd d asig

-- | Plays the sample at the given period (in BPMs). The samples don't overlap.
rep1 :: D -> Sam -> Sam
rep1 dt = genLoop $ \bpm d asig -> repeatSnd (toSec bpm dt) asig

-- | Plays the sample at the given period (in BPMs). The overlapped samples are mixed together.
pat1 :: Sig -> Sam -> Sam
pat1 dt = genLoop $ \bpm d asig -> sched (const $ return asig) $ withDur d $ metroS bpm dt

-- | Plays the sample at the given pattern of periods (in BPMs). The samples don't overlap.
rep :: [D] -> Sam -> Sam 
rep dts = genLoop $ \bpm d asig -> trigs (const $ return asig) $ fmap (const $ notes bpm d) $ metroS bpm (sig $ sum dts)
	where 
		notes bpm d = zipWith (\t dt-> (toSec bpm t, toSec bpm dt, unit)) (patDurs dts)	dts	 
		
-- | Plays the sample at the given pattern of periods (in BPMs). The overlapped samples are mixed together.
pat :: [D] -> Sam -> Sam 
pat dts = genLoop $ \bpm d asig -> trigs (const $ return asig) $ fmap (const $ notes bpm d) $ metroS bpm (sig $ sum dts)
	where notes bpm d = fmap (\t -> (toSec bpm t, d, unit)) $ patDurs dts		

-- | Constructs the wall of sound from the initial segment of the sample.
-- The segment length is given in BPMs.
--
-- > wall segLength
wall :: D -> Sam -> Sam 
wall dt a = mean [b, del hdt b]
	where 
		hdt = 0.5 * dt
		f = pat1 (sig hdt) . hatEnv . lim dt
		b = f a

-- | The tones of the chord.
type Chord = [D]

type Arp1Fun = Evt Unit -> Evt D

arpInstr :: Sig2 -> D -> SE Sig2
arpInstr asig k = return $ mapSig (scalePitch (sig k)) asig 

patDurs :: [D] -> [D]
patDurs dts = reverse $ snd $ foldl (\(count, res) a -> (a + count, count:res)) (0, []) dts

genArp1 :: Arp1Fun -> Sig -> Sam -> Sam
genArp1 arpFun dt = genLoop $ \bpm d asig -> 
	sched (arpInstr asig) $ withDur d $ arpFun $ metroS bpm dt

-- | Plays ascending arpeggio of samples.
arpUp1 :: Chord -> Sig -> Sam -> Sam
arpUp1 = genArp1 . cycleE

-- | Plays descending arpeggio of samples.
arpDown1 :: Chord -> Sig -> Sam -> Sam
arpDown1 ch = arpUp1 (reverse ch)

-- | Plays arpeggio of samles with random notes from the chord.
arpOneOf1 :: Chord -> Sig -> Sam -> Sam
arpOneOf1 = genArp1 . oneOf

-- | Plays arpeggio of samles with random notes from the chord.
-- We can assign the frequencies of the notes.
arpFreqOf1 :: [D] -> Chord -> Sig -> Sam -> Sam
arpFreqOf1 freqs ch = genArp1 (freqOf (zip freqs ch))

genArp :: Arp1Fun -> [D] -> Sam -> Sam
genArp arpFun dts = genLoop $ \bpm d asig -> trigs (arpInstr asig) $ fmap (notes bpm d) $ arpFun $ metroS bpm (sig $ sum dts)
	where notes bpm d pchScale = fmap (\t -> (toSec bpm t, d, pchScale)) $ patDurs dts		

-- | Plays ascending arpeggio of samples.
arpUp :: Chord -> [D] -> Sam -> Sam
arpUp = genArp . cycleE

-- | Plays descending arpeggio of samples.
arpDown :: Chord -> [D] -> Sam -> Sam
arpDown ch = arpUp (reverse ch)

-- | Plays arpeggio of samles with random notes from the chord.
arpOneOf :: Chord -> [D] -> Sam -> Sam 
arpOneOf = genArp . oneOf

-- | Plays arpeggio of samles with random notes from the chord.
-- We can assign the frequencies of the notes.
arpFreqOf :: [D] -> Chord -> [D] -> Sam -> Sam
arpFreqOf freqs ch = genArp (freqOf $ zip freqs ch)

metroS bpm dt = metroE (recip $ toSecSig bpm dt)

-- | A major chord.
chMaj :: Chord
chMaj = [0, 4, 7]

-- | A minor chord
chMin :: Chord
chMin = [0, 3, 7]

-- | A lead tone triad.
chLead :: Chord
chLead = [0, 3, 6]

-- |  A dominant seventh chord.
ch7 :: Chord
ch7 = [0, 4, 7, 10]

-- | A major seventh chord.
chMaj7 :: Chord
chMaj7 = [0, 4, 7, 11]

-- | A minor seventh chord.
chMin7 :: Chord
chMin7 = [0, 3, 7, 10]

-- | A lead tone seventh chord.
chLead7 :: Chord
chLead7 = [0, 3, 6, 10]

chTrans :: D -> Chord -> Chord
chTrans k = fmap (k + )

-- | Rotates the chord.
chRot :: Int -> Chord -> Chord
chRot n = 
	| n == 0    = id
	| n < 0     = rotPos n
	| otherwise = rotNeg (abs n)
	where		
		rotPos 1 xs = tail xs ++ [head xs + 12]
		rotPos n xs = rotPos (n - 1) (rotPos 1 xs)

		rotNeg 1 xs = (last xs - 12) : init xs
		rotNeg n xs = rotNeg (n - 1) (rotNeg 1 xs)

-- | Chord in major scale at the given note (if there are seven notes)
atMaj :: Int -> Chord
atMaj n = chTrans (int $ 12 * oct + inMaj tone) $ case tone of
	0 -> chMaj
	1 -> chMin
	2 -> chMin
	3 -> chMaj
	4 -> chMaj
	5 -> chMin
	6 -> chLead
	where (oct, tone) = octTone n

-- | Chord in minor scale at the given note (if there are seven notes)
atMin :: Int -> Chord
atMin n = chTrans (int $ 12 * oct + inMin tone) $ case tone of
	0 -> chMin
	1 -> chLead
	2 -> chMaj
	3 -> chMin
	4 -> chMin
	5 -> chMaj
	6 -> chMaj
	where (oct, tone) = octTone n

-- | Seventh chord in major scale at the given note (if there are seven notes)
atMaj7 :: Int -> Chord
atMaj7 n = chTrans (int $ 12 * oct + inMaj tone) $ case tone of
	0 -> chMaj7
	1 -> chMin7
	2 -> chMin7
	3 -> chMaj7
	4 -> ch7
	5 -> chMin7
	6 -> chLead7
	where (oct, tone) = octTone n

-- | Seventh chord in minor scale at the given note (if there are seven notes)
atMin7 :: Int -> Chord
atMin7 n = chTrans (int $ 12 * oct + inMin tone) $ case tone of
	0 -> chMin7
	1 -> chLead7
	2 -> chMaj7
	3 -> chMin7
	4 -> chMin7
	5 -> chMaj7
	6 -> ch7
	where (oct, tone) = octTone n

inMaj :: Int -> Int
inMaj x = case x of
	0 -> 0
	1 -> 2
	2 -> 4
	3 -> 5
	4 -> 7
	5 -> 9
	6 -> 11

inMin :: Int -> Int
inMin x = case x of
	0 -> 0
	1 -> 2
	2 -> 3
	3 -> 5
	4 -> 7
	5 -> 9
	6 -> 10

octTone :: Int -> (Int, Int)
octTone n 
	| n < 0     = (oct - 1, tone + 7)
	| otherwise = (oct, tone)
	where (oct, tone) = quotRem n 7
