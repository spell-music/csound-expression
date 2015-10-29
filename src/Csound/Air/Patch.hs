-- | Patches.
module Csound.Air.Patch(
	CsdNote, Instr, Fx, Fx1, Fx2, FxSpec(..), DryWetRatio,
	Patch1, Patch2, Patch(..),	
	PatchSig1, PatchSig2,
	getPatchFx, dryPatch, atMix, atMixes,

	-- * Midi
	atMidi, atMono, atMono', atMonoSharp, atHoldMidi,

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
	patchWhen, mixInstr,

	-- * Rever
	withSmallRoom, withSmallRoom', 
	withSmallHall, withSmallHall',
	withLargeHall, withLargeHall',
	withMagicCave, withMagicCave'
) where

import Control.Monad
import Control.Applicative

import Csound.Typed
import Csound.SigSpace
import Csound.Control.Midi
import Csound.Control.Instr
import Csound.Air.Fx

-- | A simple csound note (good for playing with midi-keyboard).
-- It's a pair of amplitude (0 to 1) and freuqncy (Hz).
type CsdNote a = (a, a)

-- | An instrument transforms a note to a signal.
type Instr a b = CsdNote a -> SE b

-- | An effect processes the input signal.
type Fx a = a  -> SE a
type DryWetRatio = Sig

-- | Mono effect.
type Fx1 = Fx Sig

-- | Stereo effect.
type Fx2 = Fx Sig2

-- | Mono patches.
type Patch1 = Patch D Sig

-- | Stereo patches.
type Patch2 = Patch D Sig2

-- | Mono continuous patches.
type PatchSig1 = Patch Sig Sig

-- | Stereo continuous patches.
type PatchSig2 = Patch Sig Sig2

data FxSpec a = FxSpec
	{ fxMix :: DryWetRatio
	, fxFun :: Fx a	
	}

-- | A patch. It's an instrument, an effect and default dry/wet ratio.
data Patch a b = Patch
	{ patchInstr :: Instr a b
	, patchFx	 :: [FxSpec b]
	}

dryPatch :: Patch a b -> Patch a b
dryPatch p = p { patchFx = [] }

-- | Sets the mix of the last effect.
atMix :: Sig -> Patch a b -> Patch a b
atMix k p = p { patchFx = mapHead (\x -> x { fxMix = k }) (patchFx p) }
	where 
		mapHead f xs = case xs of
			[]   -> []
			a:as -> f a : as

-- | Sets the mix of the effects from last to first.
atMixes :: [Sig] -> Patch a b -> Patch a b
atMixes ks p = p { patchFx = zipFirst (\k x -> x { fxMix = k }) ks (patchFx p) }
	where
		zipFirst f xs ys = case (xs, ys) of
			(_,    [])   -> []
			([],   bs)   -> bs
			(a:as, b:bs) -> f a b : zipFirst f as bs


wet :: (SigSpace a, Sigs a) => FxSpec a -> Fx a
wet (FxSpec k fx) asig = fmap ((mul (1 - k) asig + ) . mul k) $ fx asig

-- | Transforms all the effects for the given patch into a single function. 
getPatchFx :: (SigSpace a, Sigs a) => Patch b a -> Fx a
getPatchFx p = foldr (<=<) return $ fmap wet $ patchFx p

--------------------------------------------------------------

instance SigSpace a => SigSpace (Patch b a) where
	mapSig f p = p { patchInstr = fmap (mapSig f) . patchInstr p }

--------------------------------------------------------------
-- note

-- | Plays a patch at the given note.
atNote :: (SigSpace b, Sigs b) => Patch a b -> CsdNote a -> SE b
atNote p note = getPatchFx p =<< patchInstr p note

--------------------------------------------------------------
-- midi

-- | Plays a patch with midi. Supplies a custom value for mixing effects (dry/wet).
-- The 0 is a dry signal, the 1 is a wet signal.
atMidi :: (SigSpace a, Sigs a) => Patch D a -> SE a
atMidi a = getPatchFx a =<< midi (patchInstr a . ampCps)	

-- | Simplified monosynth patch
atMono :: (SigSpace a, Sigs a) => Patch Sig a -> SE a
atMono = atMono' ChnAll 0.01 0.1

-- | Simplified monosynth patch (sharp attack and transitions)
atMonoSharp :: (SigSpace a, Sigs a) => Patch Sig a -> SE a
atMonoSharp = atMono' ChnAll 0.005 0.05

-- | Monosynth patch. Plays the patch with function @monoMsg@
--
-- > atMonoMidi midiChn portamentotime releaseTime patch
atMono' :: (SigSpace a, Sigs a) => MidiChn -> D -> D -> Patch Sig a -> SE a
atMono' chn port rel a = getPatchFx a =<< patchInstr a =<< monoMsg chn port rel

-- | Monosynth patch. Plays the patch with function @holdMsg@
--
-- > atMonoMidi midiChn portamentotime patch
atHoldMidi :: (SigSpace a, Sigs a) => MidiChn -> D -> Patch Sig a -> SE a
atHoldMidi chn port a = getPatchFx a =<< patchInstr a =<< holdMsg chn port

--------------------------------------------------------------
-- sched

-- | Plays a patch with event stream. Supplies a custom value for mixing effects (dry/wet).
-- The 0 is a dry signal, the 1 is a wet signal.
atSched :: (SigSpace a, Sigs a) => Patch D a -> Evt (Sco (CsdNote D)) -> SE a
atSched p evt = getPatchFx p $ sched (patchInstr p) evt

atSchedUntil :: (SigSpace a, Sigs a) => Patch D a -> Evt (CsdNote D) -> Evt b -> SE a
atSchedUntil p evt stop = getPatchFx p $ schedUntil (patchInstr p) evt stop

--------------------------------------------------------------
-- sco
 
-- | Plays a patch with scores. Supplies a custom value for mixing effects (dry/wet).
-- The 0 is a dry signal, the 1 is a wet signal.
atSco :: (SigSpace a, Sigs a) => Patch D a -> Sco (CsdNote D) -> Sco (Mix a)
atSco p sc = eff (getPatchFx p) $ sco (patchInstr p) sc	

--------------------------------------------------------------

-- | Adds an effect to the patch's instrument.
addInstrFx :: Fx b -> Patch a b -> Patch a b
addInstrFx f p = p { patchInstr = f <=< patchInstr p }

-- | Appends an effect before patch's effect.
addPreFx :: DryWetRatio -> Fx b -> Patch a b -> Patch a b
addPreFx dw f p = p { patchFx = patchFx p ++ [FxSpec dw f] }

-- | Appends an effect after patch's effect.
addPostFx :: DryWetRatio -> Fx b -> Patch a b -> Patch a b
addPostFx dw f p = p { patchFx = FxSpec dw f : patchFx p }

----------------------------------------------------------------

-- | Plays the patch when confition is true otherwise it produces silence.
patchWhen :: Sigs b => BoolSig -> Patch a b -> Patch a b
patchWhen cond p = p 
	{ patchInstr = playWhen cond (patchInstr p)
	, patchFx    = fmap (mapFun $ playWhen cond) (patchFx p) }
	where mapFun f x = x { fxFun = f $ fxFun x }


mixInstr :: (SigSpace b, Num b) => Sig -> Patch a b -> Patch a b -> Patch a b
mixInstr k f p = p { patchInstr = \x -> liftA2 (+) (patchInstr p x) (fmap (mul k) (patchInstr f x)) }

------------------------------------------------
-- pads

harmonPatch :: (Fractional a, SigSpace b, Sigs b) => [Sig] -> [a] -> Patch a b -> Patch a b
harmonPatch amps freqs p = p { 
		patchInstr = \(amp, cps) -> fmap sum $ zipWithM (\a f -> fmap (mul a) $ patchInstr p (amp, cps * f)) amps freqs	
	}

deepPad :: (Fractional a, SigSpace b, Sigs b) => Patch a b -> Patch a b
deepPad = harmonPatch (fmap (* 0.75) [1, 0.5]) [1, 0.5]

------------------------------------------------
-- revers

withSmallRoom :: Patch2 -> Patch2
withSmallRoom = withSmallRoom' 0.25

withSmallRoom' :: DryWetRatio -> Patch2 -> Patch2
withSmallRoom' = withRever smallRoom2

withSmallHall :: Patch2 -> Patch2
withSmallHall = withSmallHall' 0.25

withSmallHall' :: DryWetRatio -> Patch2 -> Patch2
withSmallHall' = withRever smallHall2

withLargeHall :: Patch2 -> Patch2
withLargeHall = withLargeHall' 0.25

withLargeHall' :: DryWetRatio -> Patch2 -> Patch2
withLargeHall' = withRever largeHall2

withMagicCave :: Patch2 -> Patch2
withMagicCave = withMagicCave' 0.25

withMagicCave' :: DryWetRatio -> Patch2 -> Patch2
withMagicCave' = withRever magicCave2

withRever :: (Sig2 -> Sig2) -> DryWetRatio -> Patch2 -> Patch2
withRever fx ratio p = addPostFx ratio (return . fx) p

------------------------------------------------
-- 
