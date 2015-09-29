-- | Patches.
module Csound.Air.Patch(
	CsdNote, Instr, Fx, Fx1, Fx2, FxSpec(..), DryWetRatio,
	Patch1, Patch2, Patch(..),	
	getPatchFx, dryPatch, atMix, atMixes,

	-- * Midi
	atMidi,

	-- * Events
	atSched,
	atSchedUntil,

	-- * Sco
	atSco,

	-- * Single note
	atNote,

	-- * Fx
	addInstrFx, addPreFx, addPostFx,

	-- * Pads
	harmonPatch, deepPad,

	-- * Misc
	patchWhen, mixInstr
) where

import Control.Monad
import Control.Applicative

import Csound.Typed
import Csound.SigSpace
import Csound.Control.Midi
import Csound.Control.Instr

-- | A simple csound note (good for playing with midi-keyboard).
-- It's a pair of amplitude (0 to 1) and freuqncy (Hz).
type CsdNote = (D, D)

-- | An instrument transforms a note to a signal.
type Instr a = CsdNote -> SE a

-- | An effect processes the input signal.
type Fx a = a  -> SE a
type DryWetRatio = Sig

-- | Mono effect.
type Fx1 = Fx Sig

-- | Stereo effect.
type Fx2 = Fx Sig2

-- | Mono patches.
type Patch1 = Patch Sig

-- | Stereo patches.
type Patch2 = Patch Sig2

data FxSpec a = FxSpec
	{ fxMix :: DryWetRatio
	, fxFun :: Fx a	
	}

-- | A patch. It's an instrument, an effect and default dry/wet ratio.
data Patch a = Patch
	{ patchInstr :: Instr a
	, patchFx	 :: [FxSpec a]
	}

dryPatch :: Patch a -> Patch a
dryPatch p = p { patchFx = [] }

-- | Sets the mix of the last effect.
atMix :: Sig -> Patch a -> Patch a
atMix k p = p { patchFx = mapHead (\x -> x { fxMix = k }) (patchFx p) }
	where 
		mapHead f xs = case xs of
			[]   -> []
			a:as -> f a : as

-- | Sets the mix of the effects from last to first.
atMixes :: [Sig] -> Patch a -> Patch a
atMixes ks p = p { patchFx = zipFirst (\k x -> x { fxMix = k }) ks (patchFx p) }
	where
		zipFirst f xs ys = case (xs, ys) of
			(_,    [])   -> []
			([],   bs)   -> bs
			(a:as, b:bs) -> f a b : zipFirst f as bs


wet :: (SigSpace a, Sigs a) => FxSpec a -> Fx a
wet (FxSpec k fx) asig = fmap ((mul (1 - k) asig + ) . mul k) $ fx asig

-- | Transforms all the effects for the given patch into a single function. 
getPatchFx :: (SigSpace a, Sigs a) => Patch a -> Fx a
getPatchFx p = foldr (<=<) return $ fmap wet $ patchFx p

--------------------------------------------------------------

instance SigSpace a => SigSpace (Patch a) where
	mapSig f p = p { patchInstr = fmap (mapSig f) . patchInstr p }

--------------------------------------------------------------
-- note

-- | Plays a patch at the given note.
atNote :: (SigSpace a, Sigs a) => Patch a -> CsdNote -> SE a
atNote p note = getPatchFx p =<< patchInstr p note

--------------------------------------------------------------
-- midi

-- | Plays a patch with midi. Supplies a custom value for mixing effects (dry/wet).
-- The 0 is a dry signal, the 1 is a wet signal.
atMidi :: (SigSpace a, Sigs a) => Patch a -> SE a
atMidi a = getPatchFx a =<< midi (patchInstr a . ampCps)	

--------------------------------------------------------------
-- sched

-- | Plays a patch with event stream. Supplies a custom value for mixing effects (dry/wet).
-- The 0 is a dry signal, the 1 is a wet signal.
atSched :: (SigSpace a, Sigs a) => Patch a -> Evt (Sco CsdNote) -> SE a
atSched p evt = getPatchFx p $ sched (patchInstr p) evt

atSchedUntil :: (SigSpace a, Sigs a) => Patch a -> Evt CsdNote -> Evt b -> SE a
atSchedUntil p evt stop = getPatchFx p $ schedUntil (patchInstr p) evt stop

--------------------------------------------------------------
-- sco
 
-- | Plays a patch with scores. Supplies a custom value for mixing effects (dry/wet).
-- The 0 is a dry signal, the 1 is a wet signal.
atSco :: (SigSpace a, Sigs a) => Patch a -> Sco CsdNote -> Sco (Mix a)
atSco p sc = eff (getPatchFx p) $ sco (patchInstr p) sc	

--------------------------------------------------------------

-- | Adds an effect to the patch's instrument.
addInstrFx :: Fx a -> Patch a -> Patch a
addInstrFx f p = p { patchInstr = f <=< patchInstr p }

-- | Appends an effect before patch's effect.
addPreFx :: DryWetRatio -> Fx a -> Patch a -> Patch a
addPreFx dw f p = p { patchFx = patchFx p ++ [FxSpec dw f] }

-- | Appends an effect after patch's effect.
addPostFx :: DryWetRatio -> Fx a -> Patch a -> Patch a
addPostFx dw f p = p { patchFx = FxSpec dw f : patchFx p }

----------------------------------------------------------------

-- | Plays the patch when confition is true otherwise it produces silence.
patchWhen :: Sigs a => BoolSig -> Patch a -> Patch a
patchWhen cond p = p 
	{ patchInstr = playWhen cond (patchInstr p)
	, patchFx    = fmap (mapFun $ playWhen cond) (patchFx p) }
	where mapFun f x = x { fxFun = f $ fxFun x }


mixInstr :: (SigSpace a, Num a) => Sig -> Patch a -> Patch a -> Patch a
mixInstr k f p = p { patchInstr = \x -> liftA2 (+) (patchInstr p x) (fmap (mul k) (patchInstr f x)) }

------------------------------------------------
-- pads

harmonPatch :: (SigSpace a, Sigs a) => [Sig] -> [D] -> Patch a -> Patch a
harmonPatch amps freqs p = p { 
		patchInstr = \(amp, cps) -> fmap sum $ zipWithM (\a f -> fmap (mul a) $ patchInstr p (amp, cps * f)) amps freqs	
	}

deepPad :: (SigSpace a, Sigs a) => Patch a -> Patch a
deepPad = harmonPatch (fmap (* 0.75) [1, 0.5]) [1, 0.5]
