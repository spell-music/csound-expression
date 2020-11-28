{-# Language FlexibleContexts #-}
module Csound.Catalog.Wave.Sean(
	RissetBellSpec(..), rissetBell, timpani, timpaniSpec, noiseBell, noiseBellSpec,
	snowCrackle,
	fmDrone, fmDrones,
	tenorOsc, sopranoOsc
) where

import Data.List
import Control.Monad

import Csound.Base hiding (formant)

data RissetBellSpec = RissetBellSpec
	{ rissetBellRands 		:: [D]
	, rissetBellRandShifts	:: [D]
	, rissetBellDurs 		:: [D]
	, rissetBellAmps		:: [Sig]
	, rissetBellFreqs       :: [Sig]
	, rissetBellFreqShifts	:: [Sig]
	}

rissetBell :: RissetBellSpec -> (D, D) -> D -> Sig -> Sig -> SE Sig
rissetBell spec (from, to) dur amp cps = ares
	where
		idurs  = fmap (dur * ) (rissetBellDurs spec)
		ifreqs = fmap (cps * ) (rissetBellFreqs spec)
		ifreqDt = (rissetBellFreqShifts spec)
		iamps  = fmap (amp * ) (rissetBellAmps spec)
		irands = (rissetBellRands spec)
		irandDt = (rissetBellRandShifts spec)

		partial iamp ifreq ifreqDt idur irand irandDt = do
			amod <- randi iamp (linseg [from * irand + irandDt, idur, to * irand + irandDt])
			return $ mul amod $ osc (ifreq + ifreqDt)

		env = expsegr [1, dur, 0.001] dur 0.001
		ares = mul 0.75 $ fmap sum $ zipWithM (\(iamp, ifreq, ifreqDt) (idur, irand, irandDt) -> partial iamp ifreq ifreqDt idur irand irandDt) (zip3 iamps ifreqs ifreqDt) (zip3 idurs irands irandDt)

timpaniSpec = RissetBellSpec
	{	rissetBellDurs       = [0.087, 0.5, 0.804, 0.065, 0.325, 0.54, 1,    0.195, 0.108, 0.89, 0.075]
	,	rissetBellFreqs      = [0.8,  1.00,  1.5,  1.65,  1.97,  2,    2.44, 2.86,  2.71,  2.91,  3.27]
	,	rissetBellFreqShifts = [0,    0,    0,     0,     0,     0,    0,    0,    0,      0,     0]
	,	rissetBellAmps       = [1,    2.52,  1.83, 0.55,  1.47,  1.67, 0.62, 0.5,  0.52,   0.55,  0.33]
	,	rissetBellRands      = [0.56, 0.56, 0.92,  0.92,  1.19,  1.7,  2,    2.74, 3,      3.75,  4.07]
	,	rissetBellRandShifts = [0,    1,    0,     1.7,   0,     0,    0,    0,    0,      0,     0] }

timpani :: (D, D) -> D -> Sig -> Sig -> SE Sig
timpani (from, to) dur amp cps = mul env $ rissetBell timpaniSpec (from, to) dur amp cps
	where env = expsegr [1, dur, 0.001] dur 0.001


noiseBellSpec = RissetBellSpec
	{ rissetBellDurs  			=[1,    0.9,  0.65, 0.55, 0.325, 0.35, 0.25, 0.2,  0.15,  0.1, 0.075]
	, rissetBellFreqs 			= [0.56, 0.56, 0.92, 0.92, 1.19,  1.7,  3,    2.74, 3,     3.75, 4.07]
	, rissetBellFreqShifts 		= [0,    1,    0,     1.7,    0,   0,   0,    0,    0,      0,      0]
	, rissetBellAmps  			= [1,    0.67, 1.35, 1.8,  2.67, 1.67, 1.46,  1.33, 1.33,  0.75, 1.33]
	, rissetBellRands      		= [0.56, 0.56, 0.92,  0.92,  1.19,  1.7,  2,    2.74, 3,      3.75,  4.07]
	, rissetBellRandShifts 		= [0,    1,    0,     1.7,   0,     0,    0,    0,    0,      0,     0] }

-- | > dac $ noiseBell (31, 125) 2.3 0.2 2900
noiseBell :: (D, D) -> D -> Sig -> Sig -> SE Sig
noiseBell (from, to) dur amp cps = mul env $ rissetBell noiseBellSpec (from, to) dur amp cps
	where env = expsegr [1, dur, 0.001] dur 0.001

------------------------------------------------------------------------

-- | speed ~ 10 - 20
--
-- > snowCrackle speed
snowCrackle :: Sig -> Sig
snowCrackle speed = mlp 1200 0.1 $ mouseDrum speed (3 + 2 * uosc 0.1)  (160 + 100 * uosc 0.13)
	where
		mouseDrum :: Sig -> Sig -> Sig -> Sig
		mouseDrum freq index cps =
			sched instr $ withDur (sig dur) $ fmap (\[a, b] -> (a, b)) $ randList 2 $ dust freq
			where
				dur = 0.049
				instr (rndCps, rndIndex) = return $
					mouseDrumGrain dur
						(cps + 10 * sig (2 * rndCps - 1))
						(index + 0.01 * sig (2 * rndIndex - 1))

		mouseDrumGrain dur icarfreq index = aosc
			where
				iratio = 1.416
				idev = imodfreq * index
				imodfreq = icarfreq * iratio
				amod = mul (idev * imodfreq) $ osc imodfreq
				kenv = expsegr [1, dur, 0.001] dur 0.001
				aosc = mul kenv $ osc (icarfreq + amod)

------------------------------------------------------------------------

fmDronePartial amod' index idev kamp1 ifreq1 (a1, a2, a3, a4) = ares
	where
		aosc1 = mul (idev * kamp1) $ osc (ifreq1 * a1)
		aosc2 = osc (ifreq1 * a2 + aosc1 + amod')
		aosc3 = osc (ifreq1 * a3 + aosc1 + amod')
		aosc4 = osc (a4 + aosc1 + amod')
		ares  = 0.5 * kamp1 * sum [aosc2, aosc3, aosc4]

scDrone = fmDrone 3
scDrones = fmDrones 3

pulseIndex ns speed = 1 + 7 * seqSqr [seqDesc ns] speed

fmPulse ns speed = fmDrone (pulseIndex ns speed) (0.05, 0.5)
fmPulses amps harms ns speed = fmDrones (pulseIndex ns speed) amps harms (0.05, 0.5)

-- | > dac $ fmDrone 3 (20, 5) 110
fmDrone index (iatt, irel) cps = (aout1, aout2)
 	where
 		ifreq1 = cps
 		iamp = 0.39
 		idev = index * ifreq1
 		kamp1 = leg iatt 0 1 irel

 		f a1 a2 a3 a4 = iamp * fmDronePartial 0 index idev kamp1 ifreq1 (a1, a2, a3, a4)

 		aout1 = f 1    0.998 1.5007 0.1
 		aout2 = f 0.99 0.987 1.498 0.13

fmDrones index amps harms (iatt, irel) cps = aout
	where
		iamp = 0.39
	 	kamp1 = leg iatt 0 1 irel

		f amp h = do
			let ifreq1 = h * cps
	 		    idev = index * ifreq1

			a1 <- randomSig 1     0.03
			a2 <- randomSig 0.998 0.025
			a3 <- randomSig 1.5   0.004
			a4 <- randomSig 0.1   0.03
			return $ amp * fmDronePartial 0 index idev kamp1 ifreq1 (a1, a2, a3, a4)

		ares = fmap sum $ zipWithM f amps harms
		aout = liftA2 (,) ares ares

randomD :: D -> D -> SE D
randomD val dev = fmap ir $ random (sig $ val - dev) (sig $ val + dev)

randomSig :: Sig -> Sig -> SE Sig
randomSig val dev = random (val - dev) (val + dev)

gaussD :: D -> D -> SE D
gaussD val dev = fmap ((+ val) . ir) $ gauss (sig val)

gaussSig :: Sig -> Sig -> SE Sig
gaussSig val dev = fmap ((+ val)) $ gauss val

randiDev :: Sig -> Sig -> Sig -> SE Sig
randiDev val dev cps = fmap (+ val) $ randi dev cps

randhDev :: Sig -> Sig -> Sig -> SE Sig
randhDev val dev cps = fmap (+ val) $ randh dev cps

------------------------------------------------------------------------
-- choir

tenorOsc   = voiceOsc 0.9
sopranoOsc = voiceOsc 0.8

linVibr2 (v1, v2) (vtime1, vtime2) = linseg [v1, vtime1, v1, vtime2, v2]

voiceOsc :: Sig -> (Sig -> Sig) -> Sig -> Sig -> SE Sig
voiceOsc mulHarm formantFilter kvib cps = at formantFilter $ voiceAnimator (RndDev 0.05 0.75)  kvib $ asig * kenv
	where
		iharms = sig getSampleRate * 0.4 / cps
		asig = gbuzz 1 cps iharms 1 mulHarm (sines3 [(1, 1, 0.25)])
		kenv = leg 0.1 0 1 0.1

data RndDev = RndDev
	{ rndDevRatio :: Sig
	, rndDevSpeed :: Sig
	}

voiceAnimator :: RndDev -> Sig -> Sig -> SE Sig
voiceAnimator rndDev kvib ain = aout
	where
		ktimes = zipWithM (\amp cps -> mul (amp * osc cps) $ addRnd rndDev kvib) [0.0012, 0.0009, 0.00087, 0.0011] [4, 5, 6.3, 4.4]
		-- ktimes = zipWith (\amp cps -> kvib * amp * osc cps) [0.0012, 0.0009, 0.00087, 0.0011, 0.00093, 0.00081, 0.0071] [4, 5, 6.3, 4.4, 5.2, 4.2, 5.5]
		aout = fmap (mean . fmap (\t -> vdelay ain t 0.015)) ktimes

addRnd :: RndDev -> Sig -> SE Sig
addRnd spec ain = do
	xDt <- randi (rndDevRatio spec) (rndDevSpeed spec)
	return $ ain * (1 + xDt)


data Formant = Formant
	{ formantWeight :: Sig
	, formantCenter :: Sig
	, formantWidth  :: Sig
	}

------------------------------------------------------------------------
--


