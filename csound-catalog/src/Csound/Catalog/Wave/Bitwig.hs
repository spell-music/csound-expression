module Csound.Catalog.Wave.Bitwig(
	pwPad, triPad, triPadFx, triPadBy, pwPadBy,
	Accordeon(..), accordeon, accordeonFx
) where

import Csound.Base

---------------------------------------------------
-- wind pads

triPadFx :: Sig2 -> SE Sig2
triPadFx a = mixAt 0.5 smallHall2 $ at (chorus 0.2 0.3 0.25) (return a :: SE Sig2)

triPad = triPadBy mlp

triPadBy :: ResonFilter -> Sig -> SE Sig
triPadBy filter x = mul (1.5 * fades 0.3 0.5) $ at (filter (x * 5) 0.15) $ do
	lfo <- rand 1.2
	mul 0.5 $ rndTri (x + 1.5 * lfo) + rndTri (x * cent 8)

pwPad :: Sig -> SE Sig
pwPad = pwPadBy mlp

pwPadBy :: ResonFilter -> Sig -> SE Sig
pwPadBy filter x = mul (fades 0.3 0.95) $ at (filter (x * 5) 0.15) $ do
	let lfo = uosc 4
	return $ mul 0.5 $ pw (0.2 + 0.4 * lfo) x + tri (x * cent 8)

pwPadMidi = mul 0.5 $ mixAt 0.5 smallHall2 $ at (chorus 0.2 0.3 0.25) $ at fromMono $ midi $ onMsg pwPad

---------------------------------------------------
-- accordeon

osc4 freq1 freq2 freq3 freq4 a b cps = cfd4 a b (saw (cps * freq1)) (sqr (cps * freq2)) (saw (cps * freq3)) (sqr (cps * freq4))
mlpTrack cps center q = mlp (cps + 6500 * center) q

data Accordeon = Accordeon 
	{ accordeonFreq1 :: Sig
	, accordeonFreq2 :: Sig
	, accordeonFreq3 :: Sig
	, accordeonFreq4 :: Sig
	}

instance Default Accordeon where
	def = Accordeon 1 0.5 2.01 2

accordeon :: Accordeon -> Sig -> SE Sig2
accordeon spec cps = fmap fromMono $ liftA2 (\a b -> mul vcaEg $ mlpTrack (cps * 2) (0.5 * vcfEg) 0.1 $ f a b cps) (rndPointer 6 (0.3, 0.2)) (rndPointer 10 (0.4, 0.1))
	where
		vcaEg = leg 0.01 0.3 0.5 0.3
		vcfEg = leg 0.05 0.3 0.2 0.2
		rndPointer' a dt b cps (x, y) = fmap (\r -> x + y * linseg [0, 0.01, a, dt, b] * r) (randi 1 cps)
		rndPointer = rndPointer' 1 5 0.35
		f = osc4 (accordeonFreq1 spec) (accordeonFreq2 spec) (accordeonFreq3 spec) (accordeonFreq4 spec)

accordeonFx :: Sig2 -> SE Sig2
accordeonFx a = at smallHall2 $ mixAt 0.35 (echo 0.25 0.55) (return a :: SE Sig2)
