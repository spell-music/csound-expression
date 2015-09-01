-- | Patches.
module Csound.Air.Patch(
	CsdNote, Instr, Fx, Fx1, Fx2,
	Patch1, Patch2, Patch(..),
	getPatchFx,

	-- * Midi
	atMidi, atMidi', dryMidi,

	-- * Events
	atSched, atSched', drySched,
	atSchedUntil, atSchedUntil', drySchedUntil,

	-- * Sco
	atSco, atSco', drySco,

	-- * Single note
	atNote, atNote', dryNote,

	-- * Fx
	addInstrFx, addPreFx, addPostFx
) where

import Control.Monad

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

-- | Mono effect.
type Fx1 = Fx Sig

-- | Stereo effect.
type Fx2 = Fx Sig2

-- | Mono patches.
type Patch1 = Patch Sig

-- | Stereo patches.
type Patch2 = Patch Sig2

-- | A patch. It's an instrument, an effect and default dry/wet ratio.
data Patch a = Patch
	{ patchInstr :: Instr a
	, patchFx	 :: Fx a
	, patchMix   :: Sig	
	}

wet :: (SigSpace a, Sigs a) => Sig -> Fx a -> a -> SE a
wet k fx asig = fmap ((mul (1 - k) asig + ) . mul k) $ fx asig

getPatchFx :: (SigSpace a, Sigs a) => Patch a -> a -> SE a
getPatchFx p = wet (patchMix p) (patchFx p)

--------------------------------------------------------------

instance SigSpace a => SigSpace (Patch a) where
	mapSig f p = p { patchInstr = fmap (mapSig f) . patchInstr p }

--------------------------------------------------------------
-- 

dryNote :: Patch a -> CsdNote -> SE a
dryNote p note = patchInstr p note

atNote' :: (SigSpace a, Sigs a) => Sig -> Patch a -> CsdNote -> SE a
atNote' k p note = wet k (patchFx p) =<< patchInstr p note

atNote :: (SigSpace a, Sigs a) => Patch a -> CsdNote -> SE a
atNote p = atNote' (patchMix p) p

--------------------------------------------------------------
-- midi

-- | Plays dry patch with midi.
dryMidi :: (Sigs a) => Patch a -> SE a
dryMidi a = midi (patchInstr a . ampCps)

-- | Plays a patch with midi. Supplies a custom value for mixing effects (dry/wet).
-- The 0 is a dry signal, the 1 is a wet signal.
atMidi' :: (SigSpace a, Sigs a) => Sig -> Patch a -> SE a
atMidi' k a = wet k (patchFx a) =<< midi (patchInstr a . ampCps)	

-- | Plays a patch with midi
atMidi :: (SigSpace a, Sigs a) => Patch a -> SE a
atMidi a = atMidi' (patchMix a) a

--------------------------------------------------------------
-- sched

-- | Plays dry patch with event stream.
drySched :: (Sigs a) => Patch a -> Evt (Sco CsdNote) -> SE a
drySched p evt = return $ sched (patchInstr p) evt

-- | Plays a patch with event stream. Supplies a custom value for mixing effects (dry/wet).
-- The 0 is a dry signal, the 1 is a wet signal.
atSched' :: (SigSpace a, Sigs a) => Sig -> Patch a -> Evt (Sco CsdNote) -> SE a
atSched' k p evt = wet k (patchFx p) $ sched (patchInstr p) evt

-- | Plays a patch with event stream.
atSched :: (SigSpace a, Sigs a) => Patch a -> Evt (Sco CsdNote) -> SE a
atSched p = atSched' (patchMix p) p

drySchedUntil :: (Sigs a) => Patch a -> Evt CsdNote -> Evt b -> SE a
drySchedUntil p evt stop = return $ schedUntil (patchInstr p) evt stop

atSchedUntil' :: (SigSpace a, Sigs a) => Sig -> Patch a -> Evt CsdNote -> Evt b -> SE a
atSchedUntil' k p evt stop = wet k (patchFx p) $ schedUntil (patchInstr p) evt stop

atSchedUntil :: (SigSpace a, Sigs a) => Patch a -> Evt CsdNote -> Evt b -> SE a
atSchedUntil p = atSchedUntil' (patchMix p) p

--------------------------------------------------------------
-- sco

-- | Plays dry patch with scores.
drySco :: (Sigs a) => Patch a -> Sco CsdNote -> Sco (Mix a)
drySco p sc = sco (patchInstr p) sc
 
 -- | Plays a patch with scores. Supplies a custom value for mixing effects (dry/wet).
-- The 0 is a dry signal, the 1 is a wet signal.
atSco' :: (SigSpace a, Sigs a) => Sig -> Patch a -> Sco CsdNote -> Sco (Mix a)
atSco' k p sc = eff (wet k (patchFx p)) $ sco (patchInstr p) sc	

-- | Plays a patch with event scores.
atSco :: (SigSpace a, Sigs a) => Patch a -> Sco CsdNote -> Sco (Mix a)
atSco p = atSco' (patchMix p) p

--------------------------------------------------------------

-- | Adds an effect to the patch's instrument.
addInstrFx :: Fx a -> Patch a -> Patch a
addInstrFx f p = p { patchInstr = f <=< patchInstr p }

-- | Appends an effect before patch's effect.
addPreFx :: Fx a -> Patch a -> Patch a
addPreFx f p = p { patchFx = patchFx p <=< f }

-- | Appends an effect after patch's effect.
addPostFx :: Fx a -> Patch a -> Patch a
addPostFx f p = p { patchFx = f <=< patchFx p }
