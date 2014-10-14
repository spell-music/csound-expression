module Try where

import Data.List(foldr1)

import Control.Monad.Trans.Class
import Control.Applicative
import Control.Monad.Trans.Reader

import Csound.Base

todo = undefined

data Dur = Dur D | InfDur

type Bpm = D

newtype Sam = Sam { unSam :: ReaderT Bpm SE S }

data S = S
	{ samSig :: Sig2
	, samDur :: Dur } 

data EnvType = TrapLin D D | TrapExp D D | CosEnv

instance Num Sam where
	(+) = tfm2 (+)
	(*) = tfm2 (*)
	(-) = tfm2 (-)
	negate = tfm negate
	abs = tfm abs
	signum = tfm signum
	fromInteger n = Sam $ return $ S (fromInteger n) InfDur

instance Fractional Sam where
	recip = tfm recip
	fromRational n = Sam $ return $ S (fromRational n) InfDur


instance SigSpace Sam where
	mapSig f = tfm (mapSig f)
	bindSig f (Sam a) = undefined

onSig1 :: (Sig2 -> Sig2) -> S -> S
onSig1 f a = a { samSig = f $ samSig a }

onSig2 :: (Sig2 -> Sig2 -> Sig2) -> S -> S -> S
onSig2 f a b = S asig dur
	where 
		asig = f (samSig a) (samSig b)
		dur  = case (samDur a, samDur b) of
					(Dur da, Dur db) -> Dur $ maxB da db
					_                -> InfDur

toSec :: Bpm -> D -> D
toSec bpm a = a * 60 / bpm

toSecSig :: Bpm -> Sig -> Sig
toSecSig bpm a = a * 60 / sig bpm

toSig :: Bpm -> Sam -> SE Sig2
toSig bpm x = fmap samSig $ runReaderT (unSam x) bpm

addDur :: D -> Dur -> Dur
addDur d x = case x of
	Dur a  -> Dur $ d + a
	InfDur -> InfDur

tfm :: (Sig2 -> Sig2) -> Sam -> Sam
tfm f = Sam . fmap (\x -> x { samSig = f $ samSig x }) . unSam

tfm2 :: (Sig2 -> Sig2 -> Sig2) -> Sam -> Sam -> Sam
tfm2 f a b = Sam $ liftA2 (onSig2 f) (unSam a) (unSam b)

tfmBy :: (S -> Sig2) -> Sam -> Sam
tfmBy f = Sam . fmap (\x -> x { samSig = f x }) . unSam

tfmR :: (Bpm -> Sig2 -> Sig2) -> Sam -> Sam
tfmR f ra = Sam $ do
	bpm <- ask
	a <- unSam ra
	return $ a { samSig = f bpm $ samSig a }

tfmS :: (Bpm -> S -> S) -> Sam -> Sam
tfmS f ra = Sam $ do
	bpm <- ask
	a <- unSam ra
	return $ f bpm a	

setInfDur :: Sam -> Sam
setInfDur = Sam . fmap (\a -> a { samDur = InfDur }) . unSam

sam :: String -> Sam
sam fileName = Sam $ return $ S (readSnd fileName) (Dur $ lengthSnd fileName)

rev :: String -> Sam
rev fileName = Sam $ return $ S (takeSnd len $ loopWav (-1) fileName) (Dur len)
	where len = lengthSnd fileName

seg :: D -> D -> String -> Sam
seg start end fileName = Sam $ return $ S (readSegWav start end 1 fileName) (Dur len)
	where len = end - start

revSeg :: D -> D -> String -> Sam
revSeg start end fileName = Sam $ return $ S (readSegWav start end (-1) fileName) (Dur len)
	where len = end - start

readSegWav :: D -> D -> Sig -> String -> (Sig, Sig)
readSegWav start end speed fileName = takeSnd (end - start) $ diskin2 (text fileName) speed `withDs` [start, 1]

-- grainy :: 

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

atTuple :: (Tuple a) => [a] -> Sig -> a
atTuple as ind = guardedTuple (zip (fmap (\x -> sig (int x) ==* ind) [0 .. ]) as) (head as)

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

wall :: D -> Sam -> Sam 
wall dt = reps (sig hdt) . linEnv hdt hdt . lim dt
	where hdt = 0.5 * dt

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

ra1 = rev "samples/Abstract Twinkle Chime Loop.wav"

a1 = sam "samples/Abstract Twinkle Chime Loop.wav"
a2 = sam "samples/Abstract Pod Loop 01.wav" 
x = sam "samples/Piano Blur 01.wav"
rx = rev "samples/Piano Blur 01.wav"
y = repBy 4 $ linEnv 0.9 0.1 $ lim 1 x

z = mean [mul 0.3 $ rep x, y]

run = dac . toSig 120
