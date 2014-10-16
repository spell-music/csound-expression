{-# Language DeriveFunctor #-}
module Sam
{-
(
	Sample, Sam, Bpm, runSam, 
	-- * Lifters
	mapBpm, bindSam, bindBpm, liftSam,
	-- * Constructors
	fromSig, sam, rev, seg, revSeg,	
	-- * Arrange
	del, rest, flow, pick, pickBy, lim, atPch, 
	-- * Arpeggio
	arpUp, arpDown, arpOneOf, arpFreqOf,
	-- * Envelopes
	linEnv, expEnv, hatEnv, 
	-- * Loops
	rep, repBy, reps, pat, wall	

) 
-}
where

import Data.List(foldr1)

import Control.Monad.Trans.Class
import Control.Applicative
import Control.Monad.Trans.Reader

import Csound.Base

todo = undefined

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

atPch :: Sig -> Sam -> Sam
atPch k = mapSig (scalePitch k)

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

fromSig :: D -> Sig -> Sam
fromSig dt a = Sam $ reader $ \bpm -> S (a, a) (Dur $ toSec bpm dt)

sam :: String -> Sam
sam fileName = Sam $ return $ S (readSnd fileName) (Dur $ lengthSnd fileName)

sam1 :: String -> Sam
sam1 fileName = Sam $ return $ S (let x = readSnd1 fileName in (x, x)) (Dur $ lengthSnd fileName)


rev :: String -> Sam
rev fileName = Sam $ return $ S (takeSnd len $ loopWav (-1) fileName) (Dur len)
	where len = lengthSnd fileName

seg :: D -> D -> String -> Sam
seg start end fileName = Sam $ return $ S (readSegWav start end 1 fileName) (Dur len)
	where len = end - start

revSeg :: D -> D -> String -> Sam
revSeg start end fileName = Sam $ return $ S (readSegWav start end (-1) fileName) (Dur len)
	where len = end - start

rndSeg :: D -> D -> D -> String -> Sam
rndSeg = genRndSeg 1

revRndSeg :: D -> D -> D -> String -> Sam
revRndSeg = genRndSeg (-1)

genRndSeg :: Sig -> D -> D -> D -> String -> Sam
genRndSeg speed len start end fileName = Sam $ lift $ do
	x <- random 0 1
	let a = start + dl * x
	let b = a + len
	return $ S (readSegWav a b speed fileName) (Dur len)
	where dl = end - len

rest :: D -> Sam
rest dt = Sam $ reader $ \bpm -> S 0 (Dur $ toSec bpm dt)

del :: D -> Sam -> Sam
del dt = tfmS phi
	where phi bpm x = x { samSig = sig, samDur = dur }
			where 
				absDt = toSec bpm dt
				sig   = delaySnd absDt $ samSig x
				dur   = addDur absDt $ samDur x

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

pick :: Sig -> [Sam] -> Sam
pick = genPick oneOf

pickBy :: Sig -> [(D, Sam)] -> Sam
pickBy dt as = genPick (\ds -> freqOf $ zip (fmap fst as) ds) dt (fmap snd as)

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

linEnv :: D -> D -> Sam -> Sam
linEnv = genEnv f
	where f dur start end = case dur of
			InfDur -> linseg [0, start, 1]
			Dur d  -> linseg [0, start, 1, maxB 0 (d - start - end), 1, end , 0]

expEnv :: D -> D -> Sam -> Sam
expEnv = genEnv f
	where 
		f dur start end = case dur of
			InfDur -> expseg [zero, start, 1]
			Dur d  -> expseg [zero, start, 1, maxB 0 (d - start - end), 1, end , zero]
		zero = 0.00001

hatEnv :: Sam -> Sam
hatEnv = tfmBy f
	where 
		f a = flip mul (samSig a) $ case samDur a of
			InfDur -> 1
			Dur d  -> oscBy (polys 0 1 [0, 1, -1]) (1 / sig d)

type LoopFun = D -> D -> Sig2 -> Sig2

genLoop :: LoopFun -> Sam -> Sam 
genLoop g = setInfDur . tfmS f
	where 
		f bpm a = a { samSig = case samDur a of
			InfDur -> samSig a
			Dur d  -> g bpm d (samSig a)
		}

rep :: Sam -> Sam
rep = genLoop $ \_ d asig -> repeatSnd d asig

repBy :: D -> Sam -> Sam
repBy dt = genLoop $ \bpm d asig -> repeatSnd (toSec bpm dt) asig

reps :: Sig -> Sam -> Sam
reps dt = genLoop $ \bpm d asig -> sched (const $ return asig) $ withDur d $ metroS bpm dt

pat :: [D] -> Sam -> Sam 
pat dts = genLoop $ \bpm d asig -> trigs (const $ return asig) $ fmap (const $ notes bpm d) $ metroS bpm (sig $ sum dts)
	where 
		notes bpm d = fmap (\t -> (toSec bpm t, d, unit)) $ durs		
		durs = reverse $ snd $ foldl (\(count, res) a -> (a + count, count:res)) (0, []) dts

wall :: D -> Sam -> Sam 
wall dt a = mean [b, del hdt b]
	where 
		hdt = 0.5 * dt
		f = reps (sig hdt) . hatEnv . lim dt
		b = f a

type Chord = [D]

arpUp, arpDown, arpOneOf :: Chord -> Sig -> Sam -> Sam

arpFreqOf :: [D] -> Chord -> Sig -> Sam -> Sam

arpUp ch dt = genLoop $ \bpm d asig -> 
	sched (\k -> return $ mapSig (scalePitch (sig k)) asig) $ withDur d $ cycleE ch $ metroS bpm dt

arpDown ch = arpUp (reverse ch)

arpOneOf ch dt = genLoop $ \bpm d asig -> 
	sched (\k -> return $ mapSig (scalePitch (sig k)) asig) $ withDur d $ oneOf ch $ metroS bpm dt

arpFreqOf freqs ch dt = genLoop $ \bpm d asig -> 
	sched (\k -> return $ mapSig (scalePitch (sig k)) asig) $ withDur d $ freqOf (zip freqs ch) $ metroS bpm dt


metroS bpm dt = metroE (recip $ toSecSig bpm dt)

-------------------------------------
-- test

ra1 = rev "/home/anton/music/rr/samples/Abstract Twinkle Chime Loop.wav"
ra2 = rev "/home/anton/music/rr/samples/Abstract Pod Loop 01.wav" 

a1 = sam "/home/anton/music/rr/samples/Abstract Twinkle Chime Loop.wav"
a2 = sam "/home/anton/music/rr/samples/Abstract Pod Loop 01.wav" 
x = sam "/home/anton/music/rr/samples/Piano Blur 01.wav"
rx = rev "/home/anton/music/rr/samples/Piano Blur 01.wav"
y = repBy 4 $ linEnv 0.9 0.1 $ lim 1 x

piano = "/home/anton/music/rr/samples/Piano Blur 01.wav"
pod = "/home/anton/music/rr/samples/Abstract Pod Loop 01.wav" 
beat = "/home/anton/music/rr/samples/Java Gourd Fast 01.wav"
mallet = "/home/anton/music/rr/samples/Shimmer Mallet 04.wav"
beat2 = "/home/anton/x/sync/loop.wav"

z = mean [mul 0.3 $ rep x, y]

run = dac . runSam (110 * 4)


res = rep $ flow $ fmap (lim $ 16 * 4) [res1, res2]

res1 = mean [pat [3, 3, 2] q1, pat [4] q3, rep $ del 5 $ pat [1,1,3,3] q2, mul 0.7 $ pat [5] q4, del 3 $ pat [13] q5]
	where
		q x = fromSig 0.5 (mul (fades 0.01 0.1) $ osc x)
		q1 = q 110
		q2 = q 220
		q3 = q 330
		q4 = q 440
		q5 = q 660

res2 = mean [pat [3, 3, 2] q1, pat [4] q3, rep $ del 5 $ lim 3 $ pat [1] q2, mul 0.7 $ pat [5] q4, del 3 $ pat [13] q5]
	where
		q x = fromSig 0.5 (mul (fades 0.01 0.1) $ osc (x * 9 /8))
		q1 = q 110
		q2 = q 220
		q3 = q 330
		q4 = q 440
		q5 = q 660

	
liftSam :: Sample (SE a) -> Sample a
liftSam (Sam ra) = Sam $ do
	a <- ra
	lift $ fmap (\x -> a{ samSig = x}) $ samSig a

mapBpm :: (Bpm -> Sig2 -> Sig2) -> Sam -> Sam
mapBpm f (Sam ra) = Sam $ do
	bpm <- ask
	a <- ra
	return $ a { samSig = f bpm $ samSig a }

bindSam :: (Sig2 -> SE Sig2) -> Sam -> Sam
bindSam f = liftSam . fmap f

bindBpm :: (Bpm -> Sig2 -> SE Sig2) -> Sam -> Sam
bindBpm f (Sam ra) = Sam $ do
	bpm <- ask
	a <- ra
	lift $ fmap (\x -> a{ samSig = x}) $ f bpm $ samSig a
