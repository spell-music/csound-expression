-- | The core types/ They are not imported by default.
{-# Language DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
module Csound.Sam.Core (
	Sam, runSam, Sample(..), S(..), Dur(..), Bpm,
	liftSam, mapBpm, mapBpm2, bindSam, bindBpm, bindBpm2, withBpm
) where

import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

import Csound.Base

-- | The main type. A stereo sample.
type Sam = Sample Sig2

instance RenderCsd Sam where
    renderCsdBy opt sample = renderCsdBy opt (runSam (getBpm * 4) sample)

instance RenderCsd (Source Sam) where
	renderCsdBy opt sample = renderCsdBy opt (lift1 (runSam (getBpm * 4)) sample)

instance RenderCsd (SE Sam) where
    renderCsdBy opt sample = renderCsdBy opt (runSam (getBpm * 4) =<< sample)

instance RenderCsd (SE (Source Sam)) where
    renderCsdBy opt sample = renderCsdBy opt $ do
    	sample' <- sample
    	lift1 (runSam (getBpm * 4)) sample'

runSam :: Bpm -> Sam -> SE Sig2
runSam bpm x = fmap samSig $ runReaderT (unSam x) bpm

data Dur = Dur Sig | InfDur

-- | The Beats Per Minute measure (BPM). Almost all values are measured in BPMs.
type Bpm = Sig

-- | The generic type for samples.
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
		(Dur durF, Dur durA) -> Dur $ maxB durF durA
		_			     -> InfDur

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

instance SigSpace2 a => SigSpace2 (Sample a) where
	mapSig2 f = fmap (mapSig2 f)

instance BindSig2 a => BindSig2 (Sample a) where
	bindSig2 f = return . bindSam (bindSig2 f)

-- Lifters

-- | Hides the effects inside sample.
liftSam :: Sample (SE a) -> Sample a
liftSam (Sam ra) = Sam $ do
	a <- ra
	lift $ fmap (\x -> a{ samSig = x}) $ samSig a

-- | Transforms the sample with BPM.
mapBpm :: (Bpm -> a -> b) -> Sample a -> Sample b
mapBpm f a = Sam $ do
	bpm <- ask
	unSam $ fmap (f bpm) a

-- | Transforms the sample with BPM.
mapBpm2 :: (Bpm -> a -> b -> c) -> Sample a -> Sample b -> Sample c
mapBpm2 f a b = Sam $ do
	bpm <- ask
	unSam $ liftA2 (f bpm) a b

-- | Lifts bind on stereo signals to samples.
bindSam :: (a -> SE b) -> Sample a -> Sample b
bindSam f = liftSam . fmap f

-- | Lifts bind on stereo signals to samples with BPM.
bindBpm :: (Bpm -> a -> SE b) -> Sample a -> Sample b
bindBpm f a = liftSam $ mapBpm f a

-- | Lifts bind on stereo signals to samples with BPM.
bindBpm2 :: (Bpm -> a -> b -> SE c) -> Sample a -> Sample b -> Sample c
bindBpm2 f a b = liftSam $ mapBpm2 f a b


withBpm :: (Bpm -> Sample a) -> Sample a
withBpm x = Sam $ do
	bpm <- ask
	unSam $ x bpm
