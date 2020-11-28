{-# Language FlexibleContexts #-}
-- | Graphical widgets for playing samples
module Csound.Sam.Ui(
	freeSim, hfreeSim, freeSimWith, hfreeSimWith,
	freeTog, hfreeTog,
	sim, hsim, simWith, hsimWith,
	tog, htog,
	live, liveEf,
	mixSam, uiSam, addGain
) where

import Data.List(transpose)
import Control.Monad
import Control.Monad.Trans.Reader

import Csound.Base
import Csound.Sam.Core

groupToggles :: ([Sig2] -> Sig2) -> [Sam] -> [Evt D] -> Sam
groupToggles group sams ts = Sam $ reader $ \r ->
	S (group $ zipWith (\sam t -> schedToggle (runSam r sam) t) sams ts) InfDur

-- | A widget for playing several samples at the same time (aka `sim`ultaneously).
-- The prefix `free` means no syncronization. the samples start to play when the button is pressed.
freeSim :: [(String, Sam)] -> Source Sam
freeSim = genFreeSim ver

-- | It's just like the function @freeSim@ but the visual representation is horizontal.
-- That's why there is a prefix @h@.
hfreeSim :: [(String, Sam)] -> Source Sam
hfreeSim = genFreeSim hor

-- | It's just like the function `freeSim` but the user can
-- activate some samples right in the code. If the third
-- element is @True@ the sample is played.
freeSimWith :: [(String, Sam, Bool)] -> Source Sam
freeSimWith = genFreeSimInits ver

-- | It's just like the function `freeSimWith` but the visual representation is horizontal.
-- That's why there is a prefix @h@.
hfreeSimWith :: [(String, Sam, Bool)] -> Source Sam
hfreeSimWith = genFreeSimInits hor

genFreeSim :: ([Gui] -> Gui) -> [(String, Sam)] -> Source Sam
genFreeSim gcat as = genFreeSimInits gcat $ fmap (\(a, b) -> (a, b, False)) as

genFreeSimInits :: ([Gui] -> Gui) -> [(String, Sam, Bool)] -> Source Sam
genFreeSimInits gcat as = source $ do
	(guis, ts) <- fmap unzip $ zipWithM (\a b -> toggle a b) names initVals
	let res = groupToggles mean sams ts
	return (gcat guis, res)
	where
		(names, sams, initVals) = unzip3 as

-- | The widget to toggle between several samples (aka `tog`gle).
-- The prefix `free` means no syncronization. the samples start to play when the button is pressed.
freeTog :: [(String, Sam)] -> Source Sam
freeTog = genFreeTog ver

-- | It's just like the function @freeTog@ but the visual representation is horizontal.
hfreeTog :: [(String, Sam)] -> Source Sam
hfreeTog = genFreeTog hor

genFreeTog :: ([Gui] -> Gui) -> [(String, Sam)] -> Source Sam
genFreeTog gcat as = source $ do
	(guis, writes, reads) <- fmap unzip3 $ mapM (flip setToggleSig False) names
	curRef <- newGlobalRef (0 :: Sig)
	current <- readRef curRef
	zipWithM_ (\w i -> w $ ifB (current ==* i) 1 0) writes ids
	zipWithM_ (\r i -> runEvt (snaps r) $ \x -> do
		when1 (sig x ==* 0 &&* current ==* i) $ do
			writeRef curRef 0
		when1 (sig x ==* 1) $ do
			writeRef curRef i
		) reads ids

	let res = groupToggles sum sams $ fmap (snaps . (\i -> ifB (current ==* i) 1 0)) ids
	return (gcat guis, res)
	where
		(names, sams) = unzip as
		ids = fmap (sig . int) [1 .. length as]


genSim :: ([Gui] -> Gui) -> Int -> [(String, Sam)] -> Source Sam
genSim gcat numBeats as = genSimInits gcat numBeats $ fmap (\(a, b) -> (a, b, False)) as

genSimInits :: ([Gui] -> Gui) -> Int -> [(String, Sam, Bool)] -> Source Sam
genSimInits gcat numBeats as = source $ do
	(guis, writes, reads) <- fmap unzip3 $ zipWithM (\a b -> setToggleSig a b) names initVals
	curRefs <- mapM (const $ newGlobalRef (0 :: Sig)) ids
	currents <- mapM readRef curRefs
	zipWithM_ (\w val -> w val) writes currents
	let mkReaders bpm = zipWithM_ (\r ref -> runEvt (syncBpm (bpm / sig (int numBeats)) $ snaps r) $ \x -> do
			writeRef ref (sig x)
		) reads curRefs
	let res = bindBpm (\bpm x -> mkReaders bpm >> return x) $ groupToggles mean sams $ fmap snaps currents
	return (gcat guis, res)
	where
		(names, sams, initVals) = unzip3 as
		ids = fmap (sig . int) [1 .. length as]

-- | A widget for playing several samples at the same time (aka `sim`ultaneously).
-- The first argument is about syncronization.
--
-- > sim n nameAndSamples
--
-- The samples are started only on every n'th beat.
-- The tempo is specified with rendering the sample (see the function @runSam@).
sim :: Int -> [(String, Sam)] -> Source Sam
sim = genSim ver

-- | It's just like the function @sim@ but the visual representation is horizontal.
-- That's why there is a prefix @h@.
hsim :: Int -> [(String, Sam)] -> Source Sam
hsim = genSim hor


-- | It's just like the function `sim` but the user can
-- activate some samples right in the code. If the third
-- element is @True@ the sample is played.
simWith :: Int -> [(String, Sam, Bool)] -> Source Sam
simWith = genSimInits ver

-- | It's just like the function `hsimWith` but the visual representation is horizontal.
-- That's why there is a prefix @h@.
hsimWith :: Int -> [(String, Sam, Bool)] -> Source Sam
hsimWith = genSimInits hor


genTog :: ([Gui] -> Gui) -> Int -> [(String, Sam)] -> Source Sam
genTog gcat numBeats as = fmap (\(g, x) -> (g, fst x)) $ genTogWithRef gcat numBeats as

genTogWithRef :: ([Gui] -> Gui) -> Int -> [(String, Sam)] -> Source (Sam, Ref Sig)
genTogWithRef gcat numBeats as = source $ do
	(guis, writes, reads) <- fmap unzip3 $ mapM (flip setToggleSig False) names
	curRef <- newGlobalRef (0 :: Sig)
	current <- readRef curRef
	zipWithM_ (\w i -> w $ ifB (current ==* i) 1 0) writes ids
	let mkReaders bpm = zipWithM_ (\r i -> runEvt (syncBpm (bpm / (sig $ int numBeats)) $ snaps r) $ \x -> do
		when1 (sig x ==* 0 &&* current ==* i) $ do
			writeRef curRef 0
		when1 (sig x ==* 1) $ do
			writeRef curRef i
		) reads ids

	let res = bindBpm (\bpm x -> mkReaders bpm >> return x) $ groupToggles sum sams $ fmap (snaps . (\i -> ifB (current ==* i) 1 0)) ids
	return (gcat guis, (res, curRef))
	where
		(names, sams) = unzip as
		ids = fmap (sig . int) [1 .. length as]

-- | A widget to toggle playing of several samples. The switch
-- of the playing is synchronized with each n'th beat where
-- n is the first argument of the function.
tog :: Int -> [(String, Sam)] -> Source Sam
tog = genTog ver

-- | It's just like the function @tog@ but the visual representation is horizontal.
-- That's why there is a prefix @h@.
htog :: Int -> [(String, Sam)] -> Source Sam
htog = genTog hor

-- | The widget resembles the Ableton Live session view.
-- We create a matrix of samples. we can toggle the samples in
-- each row and we can start playing the whole row of samples.
--
-- > live n groupNames samples
--
-- The first argument is for synchroization. we can start samples
-- only on every n'th beat. The second argument gives names to the columns.
-- the length of the list is the number of columns.
-- the column represents samples that belong to the same group.
-- The third argument is a list of samples. It represents the matrix of samples
-- in row-wise fashion.
live :: Int -> [String] -> [Sam] -> Source Sam
live numBeats names sams = source $ do
	(gVols, vols) <- fmap unzip $  mapM  defSlider $ replicate n "vol"
	(gs, xs) <- fmap unzip $ zipWithM (\a b -> mkLiveRow numBeats a b) (zip names gVols) rows
	let (sigs, refs) = unzip xs
	(gMaster, masterVol) <- defSlider "master"
	(g, proc) <- mkLiveSceneRow numBeats gMaster ids refs
	return $ (hor $ g : gs, bindBpm (\bpm asig -> proc bpm >> return asig) $ mul masterVol $ mean $ zipWith mul vols sigs)
	where
		rows = transpose $ splitRows n sams
		ids = fmap (sig . int) [1 .. length (head rows)]
		n = length names

mkLiveRow :: Int -> (String, Gui) -> [Sam] -> Source (Sam, Ref Sig)
mkLiveRow numBeats (name, gVol) xs = genTogWithRef (\xs -> ver $ xs ++ [gVol]) numBeats (zip (name : repeat "") xs)

mkLiveSceneRow :: Int -> Gui -> [Sig] -> [Ref Sig] -> SE (Gui, Sig -> SE ())
mkLiveSceneRow numBeats gMaster ids refs = do
	(guis, writes, reads) <- fmap unzip3 $ mapM (flip setToggleSig False) names
	curRef <- newGlobalRef (0 :: Sig)
	current <- readRef curRef
	zipWithM_ (\w i -> w $ ifB (current ==* i) 1 0) writes ids
	let mkReaders bpm = zipWithM_ (\r i -> runEvt (syncBpm (bpm / sig (int numBeats)) $ snaps r) $ \x -> do
		when1 (sig x ==* 0 &&* current ==* i) $ do
			writeRef curRef 0
			mapM_ (flip writeRef 0) refs
		when1 (sig x ==* 1) $ do
			writeRef curRef i
			mapM_ (flip writeRef i) refs
		) reads ids

	return (ver $ guis ++ [gMaster], mkReaders)
	where
		names = take len $ fmap show [1 ..]
		len = length ids

splitRows :: Int -> [a] -> [[a]]
splitRows n as
	| length as < n = []
	| otherwise     = take n as : splitRows n (drop n as)

defSlider :: String -> Source Sig
defSlider tag = slider tag (linSpan 0 1) 0.5

-- | It's just like the function @live@ but we can provide the list
-- of effects for each column. The double value specifies the mix
-- between dry and wet signals.
liveEf :: Int -> [String] -> [Sam] -> (Double, Fx2) -> [(Double, Fx2)] -> Source Sam
liveEf numBeats names sams masterEff effs = source $ do
	(gVols, vols) <- fmap unzip $  mapM defSlider $ replicate n "vol"
	(gEffs, effCtrls) <- fmap unzip $
		mapM (\(tag, initVal) -> slider tag (linSpan 0 1) initVal) $ zip (replicate n "eff") (fmap fst effs)
	let gCtrls = zipWith ctrlGui gEffs gVols
	(gs, xs) <- fmap unzip $ zipWithM (\a b -> mkLiveRow numBeats a b) (zip names gCtrls) rows
	let (sigs, refs) = unzip xs
	(gMaster, masterVol) <- defSlider "master"
	(gMasterEff, masterEffCtrl) <- slider "eff" (linSpan 0 1) (fst masterEff)
	(g, proc) <- mkLiveSceneRow numBeats (ctrlGui gMasterEff gMaster) ids refs
	return $ (hor $ g : gs, bindBpm (\bpm asig -> proc bpm >> return asig) $
		mul masterVol $ appEff (snd  masterEff) masterEffCtrl $
		mean $ zipWith mul vols $ zipWith (uncurry appEff) (zip (fmap snd effs) effCtrls) sigs)
	where
		rows = transpose $ splitRows n sams
		ids = fmap (sig . int) [1 .. length (head rows)]
		n = length names

		appEff f depth a = bindSam (\x -> fmap (\y -> y + (mul (1 - depth) x)) $ mul depth $ f x) a

		ctrlGui eff vol = sca 2.5 $ ver [eff, vol]

-- | It's useful to convert samples to signals an insert
-- them in the widget @mixer@.
mixSam :: String -> Bpm -> Sam -> (String, SE Sig2)
mixSam name bpm sam = (name, runSam bpm sam)

-- | Creates fx-unit from sampler widget.
--
-- > uisam name isOn bpm samWidget
uiSam :: String -> Bool -> Sig -> Source Sam -> Source Fx2
uiSam name onOff bpm sam = uiSig name onOff (joinSource $ mapSource (runSam bpm) sam)
	where
		joinSource :: Source (SE Sig2) -> Source Sig2
		joinSource a = source $ do
			(g, mres) <- a
			res <- mres
			return (g, res)

-- | Adds gain slider on top of the widget.
addGain :: SigSpace a => Source a -> Source a
addGain x = source $ do
	(g, asig) <- x
	(gainGui, gain) <- slider "gain" (linSpan 0 1) 0.5
	return (ver [sca 0.15 gainGui, g], mul gain asig)
