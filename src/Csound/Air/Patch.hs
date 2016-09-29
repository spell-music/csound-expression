{-# Language ScopedTypeVariables #-}
-- | Patches.
module Csound.Air.Patch(
  
	CsdNote, Instr, Fx, Fx1, Fx2, FxSpec(..), DryWetRatio,
	Patch1, Patch2, Patch(..)
{-      
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
	withMagicCave, withMagicCave',

	-- * Sound font patches
	sfPatch, sfPatchHall,

	-- * Csound API
	patchByNameMidi, monoPatchByNameMidi, monoSharpPatchByNameMidi, monoPatchByNameMidi',

	-- * Custom temperament
	-- ** Midi
	atMidiTemp, atMonoTemp, atMonoSharpTemp, atMonoTemp', atHoldMidiTemp, 
	-- ** Csound API
	patchByNameMidiTemp, monoPatchByNameMidiTemp, monoSharpPatchByNameMidiTemp, monoPatchByNameMidiTemp'
    -}
) where

import Control.Monad
import Control.Applicative
import Control.Arrow(second)

import Csound.Typed
import Csound.SigSpace
import Csound.Control.Midi
import Csound.Control.Instr
import Csound.Control.Sf
import Csound.Air.Fx
import Csound.Typed.Opcode(cpsmidinn, ampdb)
import Csound.Tuning

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

data FxSpec a = FxSpec
	{ fxMix :: DryWetRatio
	, fxFun :: Fx a	
	}

{-
-- | A patch. It's an instrument, an effect and default dry/wet ratio.
data Patch a b = Patch 	
    { patchInstr :: Instr a b
    , patchFx	 :: [FxSpec b] }
-}

type Patch1 = Patch Sig
type Patch2 = Patch Sig2

data Patch a 
    = MonoSynt (Instr Sig a)
    | PolySynt (Instr D   a)
    | FxChain [FxSpec a] (Patch a)
    | SplitPatch [(D, (Patch a))]
    | LayerPatch [(Sig, (Patch a))]

dryPatch :: Patch a -> Patch a
dryPatch x = case x of
    MonoSynt instr -> x
    PolySynt instr -> x
    FxChain _ p    -> dryPatch p
    SplitPatch xs  -> dryPatch $ snd $ head xs
    LayerPatch xs  -> dryPatch $ snd $ head xs

{-
-- | Sets the mix of the last effect.
}
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
-}

--------------------------------------------------------------

instance SigSpace a => SigSpace (Patch a) where
	mapSig f x = 
            case x of
                MonoSynt instr -> MonoSynt $ fmap (mapSig f) . instr
                PolySynt instr -> PolySynt $ fmap (mapSig f) . instr
                FxChain fxs p  -> FxChain fxs $ mapSig f p
                SplitPatch xs  -> SplitPatch $ mapSnd f xs
                LayerPatch xs  -> LayerPatch $ mapSnd f xs
            where mapSnd f = fmap (second $ mapSig f) 


wet :: (SigSpace a, Sigs a) => FxSpec a -> Fx a
wet (FxSpec k fx) asig = fmap ((mul (1 - k) asig + ) . mul k) $ fx asig

getPatchFx :: (SigSpace a, Sigs a) => [FxSpec a] -> Fx a
getPatchFx xs = foldr (<=<) return $ fmap wet xs

atNote :: (SigSpace a, Sigs a) => Patch a -> CsdNote D -> SE a
atNote p note@(amp, cps) = case p of
    MonoSynt instr -> instr (sig amp, sig cps)
    PolySynt instr -> instr note
    FxChain fxs p -> getPatchFx fxs =<< atNote p note 
    LayerPatch xs -> onLayered xs $ \x -> atNote x note
    SplitPatch xs -> undefined

atMidi :: (SigSpace a, Sigs a) => Patch a -> SE a
atMidi x = case x of
    MonoSynt instr -> undefined
    PolySynt instr -> midi (instr . ampCps)
    FxChain fxs p -> getPatchFx fxs =<< atMidi p
    LayerPatch xs -> onLayered xs atMidi

atMidiTemp :: (SigSpace a, Sigs a) => Temp -> Patch a -> SE a
atMidiTemp tm x = case x of
    MonoSynt instr -> undefined
    PolySynt instr -> midi (instr . ampCps' tm)
    FxChain fxs p -> getPatchFx fxs =<< atMidiTemp tm p
    LayerPatch xs -> onLayered xs (atMidiTemp tm)
    SplitPatch xs -> undefined

--------------------------------------------------------------
-- sched

-- | Plays a patch with event stream. Supplies a custom value for mixing effects (dry/wet).
-- The 0 is a dry signal, the 1 is a wet signal.
atSched :: (SigSpace a, Sigs a) => Patch a -> Evt (Sco (CsdNote D)) -> SE a
atSched x evt = case x of
    MonoSynt instr -> undefined
    PolySynt instr -> return $ sched instr evt
    FxChain fxs p  -> getPatchFx fxs =<< atSched p evt
    LayerPatch xs -> onLayered xs (\patch -> atSched patch evt)
    SplitPatch xs -> undefined


atSchedUntil :: (SigSpace a, Sigs a) => Patch a -> Evt (CsdNote D) -> Evt b -> SE a
atSchedUntil x evt stop = case x of     
    MonoSynt instr -> undefined
    PolySynt instr -> return $ schedUntil instr evt stop
    FxChain fxs p  -> getPatchFx fxs =<< atSchedUntil p evt stop
    LayerPatch xs -> onLayered xs (\patch -> atSchedUntil patch evt stop)
    SplitPatch xs -> undefined

--------------------------------------------------------------
-- sco
 
-- | Plays a patch with scores. Supplies a custom value for mixing effects (dry/wet).
-- The 0 is a dry signal, the 1 is a wet signal.
atSco :: (SigSpace a, Sigs a) => Patch a -> Sco (CsdNote D) -> Sco (Mix a)
atSco x sc = case x of
    MonoSynt instr -> undefined
    PolySynt instr -> sco instr sc 
    FxChain fxs p  -> eff (getPatchFx fxs) $atSco p sc
    LayerPatch xs -> har $ fmap (\(vol, p) -> atSco (mul vol p) sc) xs
    SplitPatch xs -> undefined
--    eff (getPatchFx p) $ sco (patchInstr p) sc 


onLayered :: (SigSpace a, Sigs a) => [(Sig, Patch a)] -> (Patch a -> SE a) -> SE a
onLayered xs f = fmap sum $ mapM (\(vol, p) -> fmap (mul vol) $ f p) xs

--    getPatchFx a =<< midi (patchInstr a . ampCps)    

{-

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

-- | Plays a patch with midi. Supplies a custom value for mixing effects (dry/wet).
-- The 0 is a dry signal, the 1 is a wet signal.
atMidiTemp :: (SigSpace a, Sigs a) => Temp -> Patch D a -> SE a
atMidiTemp tm a = getPatchFx a =<< midi (patchInstr a . ampCps' tm)

-- | Simplified monosynth patch
atMono :: (SigSpace a, Sigs a) => Patch Sig a -> SE a
atMono = atMono' ChnAll 0.01 0.1

-- | Simplified monosynth patch with custom temperament.
atMonoTemp :: (SigSpace a, Sigs a) => Temp -> Patch Sig a -> SE a
atMonoTemp tm = atMonoTemp' tm ChnAll 0.01 0.1

-- | Simplified monosynth patch (sharp attack and transitions)
atMonoSharp :: (SigSpace a, Sigs a) => Patch Sig a -> SE a
atMonoSharp = atMono' ChnAll 0.005 0.05

-- | Simplified monosynth patch (sharp attack and transitions) with custom temperament.
atMonoSharpTemp :: (SigSpace a, Sigs a) => Temp -> Patch Sig a -> SE a
atMonoSharpTemp tm = atMonoTemp' tm ChnAll 0.005 0.05

-- | Monosynth patch. Plays the patch with function @monoMsg@
--
-- > atMonoMidi midiChn portamentotime releaseTime patch
atMono' :: (SigSpace a, Sigs a) => MidiChn -> D -> D -> Patch Sig a -> SE a
atMono' chn port rel a = getPatchFx a =<< patchInstr a =<< monoMsg chn port rel

-- | Monosynth patch with custom temperament. Plays the patch with function @monoMsgTemp@
--
-- > atMonoMidi midiChn portamentotime releaseTime patch
atMonoTemp' :: (SigSpace a, Sigs a) => Temp -> MidiChn -> D -> D -> Patch Sig a -> SE a
atMonoTemp' tm chn port rel a = getPatchFx a =<< patchInstr a =<< monoMsgTemp tm chn port rel

-- | Monosynth patch. Plays the patch with function @holdMsg@
--
-- > atMonoMidi midiChn portamentotime patch
atHoldMidi :: (SigSpace a, Sigs a) => MidiChn -> D -> Patch Sig a -> SE a
atHoldMidi chn port a = getPatchFx a =<< patchInstr a =<< holdMsg chn port

-- | Monosynth patch with custom temperament. Plays the patch with function @holdMsgTemp@
--
-- > atMonoMidi midiChn portamentotime patch
atHoldMidiTemp :: (SigSpace a, Sigs a) => Temp -> MidiChn -> D -> Patch Sig a -> SE a
atHoldMidiTemp tm chn port a = getPatchFx a =<< patchInstr a =<< holdMsgTemp tm chn port

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
-- sound font patch

sfPatchHall :: Sf -> Patch2
sfPatchHall sf = Patch 
    { patchInstr = \(amp, cps) -> return $ sfCps sf 0.5 amp cps
    , patchFx    = [(FxSpec 0.25 (return . smallHall2))] }

sfPatch :: Sf -> Patch2
sfPatch sf = Patch 
    { patchInstr = \(amp, cps) -> return $ sfCps sf 0.5 amp cps
    , patchFx    = [] }

------------------------------------------------
-- Csound API

-- | Triggers patch with Csound API.
-- It creates a named instruement with given name (first argument).
--
-- It simulates the midi-like instrument. Notes are encoded with messages:
--
-- > i "givenName" 1 pitchKey volumeKey     -- note on
-- > i "givenName" 0 pitchKey volumeKey     -- note off
patchByNameMidi :: (SigSpace a, Sigs a) => String -> Patch D a -> SE a
patchByNameMidi = genPatchByNameMidi cpsmidinn

-- | Triggers patch with Csound API.
-- It creates a named instruement with given name (second argument).
-- It behaves like the function @patchByNameMidi@ but we can specify custom temperament.
patchByNameMidiTemp :: (SigSpace a, Sigs a) => Temp -> String -> Patch D a -> SE a
patchByNameMidiTemp tm = genPatchByNameMidi (cpsmidi'D tm)

-- | Wrapper for function @trigByNameMidi@.
genPatchByNameMidi :: forall a . (SigSpace a, Sigs a) => (D -> D) -> String -> Patch D a -> SE a
genPatchByNameMidi key2cps name p = getPatchFx p =<< trigByNameMidi name go
	where
		go :: (D, D, Unit) -> SE a
		go (pitch, vol, _) = patchInstr p (vel2amp vol, key2cps pitch)


-- | Triggers patch with Csound API.
-- It creates a named instruement with given name (first argument).
--
-- It simulates the midi-like instrument. Notes are encoded with messages:
--
-- > i "givenName" 1 pitchKey volumeKey     -- note on
-- > i "givenName" 0 pitchKey volumeKey     -- note off
--
-- It behaves just like the function @patchByNameMidi@ but it's defined for
-- monophonic patches. For instruments that take in continuous signals not messages/notes.
monoPatchByNameMidi :: (SigSpace a, Sigs a) => String -> Patch Sig a -> SE a
monoPatchByNameMidi name p = monoPatchByNameMidi' 0.01 0.1 name p

-- | Triggers patch with Csound API.
-- It creates a named instruement with given name (first argument).
-- It behaves like the function @monoPatchByNameMidi@ but we can specify custom temperament.
monoPatchByNameMidiTemp :: (SigSpace a, Sigs a) => Temp -> String -> Patch Sig a -> SE a
monoPatchByNameMidiTemp tm name p = monoPatchByNameMidiTemp' tm 0.01 0.1 name p

-- | The monophonic patch with sharper transition from note to note.
monoSharpPatchByNameMidi :: (SigSpace a, Sigs a) => String -> Patch Sig a -> SE a
monoSharpPatchByNameMidi name p = monoPatchByNameMidi' 0.005 0.05 name p

-- | The monophonic patch with sharper transition from note to note.
-- We can specify a custom temperament.
monoSharpPatchByNameMidiTemp :: (SigSpace a, Sigs a) => Temp -> String -> Patch Sig a -> SE a
monoSharpPatchByNameMidiTemp tm name p = monoPatchByNameMidiTemp' tm 0.005 0.05 name p

-- | Generic function fr invocation of monophonic instrument with Csound API.
-- We can specify portamento and release times.
monoPatchByNameMidi' :: (SigSpace a, Sigs a) => D -> D -> String -> Patch Sig a -> SE a
monoPatchByNameMidi' = genMonoPatchByNameMidi' cpsmidinn

-- | Generic function fr invocation of monophonic instrument with Csound API.
-- We can specify portamento and release times. Also we can specify a temperament.
monoPatchByNameMidiTemp' :: (SigSpace a, Sigs a) => Temp -> D -> D -> String -> Patch Sig a -> SE a
monoPatchByNameMidiTemp' tm = genMonoPatchByNameMidi' (cpsmidi'Sig tm)

-- | Wrapper for function @trigByNameMidi@ for mono synth.
genMonoPatchByNameMidi' :: forall a . (SigSpace a, Sigs a) => (Sig -> Sig) -> D -> D -> String -> Patch Sig a -> SE a
genMonoPatchByNameMidi' key2cps portTime relTime name p = getPatchFx p =<< patchInstr p =<< fmap convert (trigNamedMono portTime relTime name)
	where
		convert (vol, pch) = (vel2ampSig vol, key2cps pch)

vel2amp :: D -> D
vel2amp vol = ((vol / 64) ** 2) / 2

vel2ampSig :: Sig -> Sig
vel2ampSig vol = ((vol / 64) ** 2) / 2

-}