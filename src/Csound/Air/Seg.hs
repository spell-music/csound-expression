{-# Language TypeFamilies #-}
module Csound.Air.Seg (
	Seg, toSeg, runSeg,	
	constLim, constDel, constRest, limSnd
) where

import Data.Maybe
import Data.Monoid
import Data.Boolean

import Temporal.Class

import Csound.Typed
import Csound.SigSpace
import Csound.Control

import Csound.Air.Wav hiding (Loop)

-- | A segment of the signal. 
-- The signal segment is a limited span of signal in time.
-- The time can be measured in seconds or in events!
-- The time span which is measured in events is the first
-- occurence of the event in the event stream. 
--
-- There are handy functions for scheduling the signal segments.
-- we can delay the segment or loop over it or limit it with tme interval
-- or play a sequence of segments. The main feature of the segments is the
-- ability to schedule the signals with event streams (like button clicks or midi-events). 
data Seg a 
	= Unlim a
	| Lim Tick (Seg a)
	| ConstLim D (Seg a)
	| Seq [Seg a]
	| Par [Seg a]
	| Loop (Seg a)

instance Functor Seg where
	fmap f x = case x of
		Unlim a -> Unlim $ f a
		Lim dt a -> Lim dt $ fmap f a
		ConstLim dt a -> ConstLim dt $ fmap f a
		Seq as  -> Seq $ fmap (fmap f) as
		Par as  -> Par $ fmap (fmap f) as
		Loop a  -> Loop $ fmap f a

instance SigSpace a => SigSpace (Seg a) where
	mapSig f x = fmap (mapSig f) x

type instance DurOf (Seg a) = Tick

instance Sigs a => Compose (Seg a) where
	mel = sflow
	har = spar

instance Sigs a => Delay (Seg a) where
	del = sdel

instance Sigs a => Loop (Seg a) where
	loop = sloop

instance (Sigs a, Num a) => Rest (Seg a) where
	rest = srest

instance Sigs a => Limit (Seg a) where
	lim = slim

seq1 :: Tick -> a -> Seg a
seq1 dt a = Lim dt (Unlim a)

-- | Converts signals to segments.
-- The segment is not limited in length.
toSeg :: a -> Seg a
toSeg a = Unlim a

-- | Limits the length of the segment with event stream.
slim :: Tick -> Seg a -> Seg a
slim da x = case x of
	Par as   -> Par (fmap (slim da) as)
	_        -> Lim da x

-- | Limits the length of the segment with constant length in seconds.
constLim :: D -> Seg a -> Seg a
constLim da x = case x of
	Par as   -> Par (fmap (constLim da) as)
	_        -> ConstLim da x

-- | Plays the sequence of segments one ofter another.
sflow :: [Seg a] -> Seg a
sflow as = Seq $ flatten =<< as
	where 
		flatten x = case x of
			Seq as -> as
			_      -> [x]

-- | Plays a list of segments at the same time.
-- the total length equals to the biggest length of all segments.
spar :: [Seg a] -> Seg a
spar as = Par $ flatten =<< as
	where 
		flatten x = case x of
			Par as -> as
			_      -> [x]

-- | Loops over a segment. The segment should be limited for loop to take effect.
sloop :: Seg a -> Seg a
sloop x = case x of
	Unlim a -> Unlim a
	Loop a  -> Loop a
	Par as  -> Par (fmap sloop as)
	_       -> Loop x


-- | Limits a signal with an event stream and retriggers it after stop.
limSnd :: Sigs a => Tick -> a -> a
limSnd dt = runSeg . sloop . slim dt . toSeg

------------------------------------------------

-- | Converts segments to signals.
runSeg :: (Sigs a) => Seg a -> a
runSeg x = case x of
	Unlim a -> a

	Lim dt (Unlim a) -> elim dt a
	Lim dt (Seq as)  -> uncurry (evtLoopOnce (Just dt)) (getEvtAndSig $ rmTailAfterUnlim as)	
	Lim dt (Loop (Seq as)) -> uncurry (evtLoop (Just dt)) (getEvtAndSig $ rmTailAfterUnlim as)
	Lim dt (Loop a) -> elim dt (runSeg (Loop a))
	Lim dt a -> elim dt (runSeg a)


	ConstLim dt (Unlim a) -> takeSnd dt a
	ConstLim dt (Seq as)  -> uncurry (evtLoopOnce (Just $ impulseE dt)) (getEvtAndSig $ rmTailAfterUnlim as)	
	ConstLim dt (Loop (Seq as)) -> uncurry (evtLoop (Just $ impulseE dt)) (getEvtAndSig $ rmTailAfterUnlim as)
	ConstLim dt (Loop a) -> takeSnd dt (runSeg (Loop a))
	ConstLim dt a -> takeSnd dt (runSeg a)

	Seq as -> uncurry (evtLoopOnce Nothing) (getEvtAndSig $ rmTailAfterUnlim as)

	Loop (ConstLim dt a) -> repeatSnd dt $ runSeg a
	Loop (Lim dt a)      -> evtLoop Nothing [return $ runSeg a] [dt]
	Loop (Seq as)            -> uncurry (evtLoop Nothing) (getEvtAndSig as)

	Par as -> maybeElim (getDur x) $ sum $ fmap (\a -> maybeElim (getDur a) $ runSeg a) as

getDur :: Seg a -> Maybe (Either D Tick)
getDur x = case x of
	Unlim _ -> Nothing
	Loop  _ -> Nothing 
	Lim dt _ -> Just $ Right dt
	ConstLim dt _ -> Just $ Left dt
	Seq as -> fromListT sum aftT' as
	Par as -> fromListT (foldl1 maxB) simT' as
	where 
		fromListT g f as 
			| all isJust ds = Just $ phi g f $ fmap fromJust ds
			| otherwise     = Nothing 
			where ds = fmap getDur as

		phi g f xs
			| all isJust as = Left  $ g $ fmap fromJust as
			| otherwise     = Right $ f $ fmap toEvt xs
			where as = fmap getConstT xs

		getConstT x = case x of
			Left d -> Just d
			_      -> Nothing

		toEvt = either impulseE id

getEvtAndSig :: (Num a, Sigs a) => [Seg a] -> ([SE a], [Tick])
getEvtAndSig as = unzip $ fmap (\x -> (return (runSeg x), getTick $ getDur x)) as
	where getTick = maybe mempty (either impulseE id)


rmTailAfterUnlim :: [Seg a] -> [Seg a]
rmTailAfterUnlim = takeByIncludeLast isUnlim 
	where 
		isUnlim x = case x of
			Unlim _ -> True
			Loop  _ -> True
			Par  as -> any isUnlim as
			_       -> False 

takeByIncludeLast :: (a -> Bool) -> [a] -> [a]
takeByIncludeLast f xs = case xs of
	[] -> []
	a:as -> if f a then [a] else a : takeByIncludeLast f as

-------------------------------------------------
-- aux

-- | A pause. Plays nothing until something happens on the event stream.
srest :: (Num a) => Tick -> Seg a
srest dt = seq1 dt 0

-- | Delays a segment until something happens on the event stream.
sdel :: (Sigs a, Num a) => Tick -> Seg a -> Seg a
sdel dt a = sflow [srest dt, a]

-- | A pause. Plays nothing for the given time interval in seconds.
constRest :: Num a => D -> Seg a
constRest dt = constLim dt $ toSeg 0

-- | Delays a segment by a given time interval in seconds.
constDel :: Num a => D -> Seg a -> Seg a
constDel dt a = sflow [constRest dt, a]

-----------------------------------------------------------

elim :: Sigs a => Tick -> a -> a
elim dt asig = schedUntil (const $ return $ asig) (impulseE 0) dt

maybeElim :: (Num a, Sigs a) => Maybe (Either D Tick) -> a -> a
maybeElim mdt a = case mdt of
	Nothing -> a
	Just x  -> case x of 
		Left d  -> takeSnd d a
		Right t -> elim t a

-- | Takes the first event from the event stream and ignores the rest of the stream.
take1 :: Evt a -> Evt a
take1 = fmap fst . filterE ((==* 0) . snd) . accumE (0 :: D) (\a s -> ((a, s), s + 1) )

-----------------------------------------------------------
-- tick funs with less instrs

aftT' :: [Tick] -> Tick
aftT' evts = take1 $ sigToEvt $ evtLoop Nothing asigs evts
	where 
		asigs :: [SE Sig]
		asigs = fmap (return . sig) $ (replicate (length evts - 1) 0) ++ [1]

simT' :: [Tick] -> Tick
simT' as = Evt $ \bam -> do
	isAwaitingRef <- newSERef (1 :: D)
	countDownRef  <- newSERef (int (length as) :: D)

	mapM_ (mkEvt countDownRef) as

	countDown <- readSERef countDownRef
	isAwaiting <- readSERef isAwaitingRef
	when1 (sig isAwaiting ==* 1 &&* sig countDown ==* 0) $ do
		bam unit
		writeSERef isAwaitingRef 0
	where 
		mkEvt ref e = do
			notFiredRef <- newSERef (1 :: D)
			notFired <- readSERef notFiredRef
			runEvt e $ \_ -> do
				when1 (sig notFired ==* 1) $ do
					writeSERef notFiredRef 0
					modifySERef ref (\x -> x - 1)

