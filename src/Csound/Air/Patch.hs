{-# Language ScopedTypeVariables #-}
-- | Patches.
module Csound.Air.Patch(
  
	CsdNote, Instr, Fx, Fx1, Fx2, FxSpec(..), DryWetRatio,
	Patch1, Patch2, Patch(..),

    mapPatchInstr, mapMonoPolyInstr, transPatch, dryPatch, getPatchFx,

	-- atMix, atMixes,

	-- * Midi
	atMidi, -- atMono, atMono', atMonoSharp, atHoldMidi,

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
{-
	-- * Misc
	patchWhen, 
-}
    mixInstr,

	-- * Rever
	withSmallRoom, withSmallRoom', 
	withSmallHall, withSmallHall',
	withLargeHall, withLargeHall',
	withMagicCave, withMagicCave',

	-- * Sound font patches
	sfPatch, sfPatchHall,

    -- * Monosynt params
    onMonoSyntSpec, setMonoSharp, setMonoHold
{-
	-- * Csound API
	patchByNameMidi, monoPatchByNameMidi, monoSharpPatchByNameMidi, monoPatchByNameMidi',

	-- * Custom temperament
	-- ** Midi
	atMidiTemp,
	-- ** Csound API
	patchByNameMidiTemp, monoPatchByNameMidiTemp, monoSharpPatchByNameMidiTemp, monoPatchByNameMidiTemp'
    -}
) where

import Data.Boolean
import Data.Default    
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
import Csound.Types

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

data MonoSyntSpec = MonoSyntSpec
    { monoSyntChn       :: MidiChn  
    , monoSyntHold      :: Bool
    , monoSyntSlideTime :: D
    , monoSyntRelease   :: D }

instance Default MonoSyntSpec where
    def = MonoSyntSpec 
        { monoSyntChn = ChnAll
        , monoSyntHold = False
        , monoSyntSlideTime = 0.01
        , monoSyntRelease = 0.1 }

data Patch a 
    = MonoSynt MonoSyntSpec (Instr Sig a)
    | PolySynt (Instr D   a)
    | FxChain [FxSpec a] (Patch a)
    | SplitPatch (Patch a) D (Patch a)
    | LayerPatch [(Sig, (Patch a))]


mapMonoPolyInstr :: (Instr Sig a -> Instr Sig a) -> (Instr D a -> Instr D a) -> Patch a -> Patch a
mapMonoPolyInstr mono poly x = case x of
    MonoSynt spec instr -> MonoSynt spec (mono instr)
    PolySynt instr      -> PolySynt (poly instr)
    FxChain  fxs p      -> FxChain fxs (rec p)
    LayerPatch xs       -> LayerPatch (mapSnd rec xs)
    SplitPatch a dt b   -> SplitPatch (rec a) dt (rec b)
    where
        rec = mapMonoPolyInstr mono poly

mapPatchInstr :: (Instr D a -> Instr D a) -> Patch a -> Patch a
mapPatchInstr f x = case x of
    MonoSynt _ _ -> x
    PolySynt instr -> PolySynt $ f instr
    FxChain fxs p -> FxChain fxs $ rec p
    LayerPatch xs -> LayerPatch (mapSnd rec xs)
    SplitPatch a dt b -> SplitPatch (rec a) dt (rec b)
    where
        rec = mapPatchInstr f

dryPatch :: Patch a -> Patch a
dryPatch x = case x of
    MonoSynt spec instr -> x
    PolySynt instr      -> x
    FxChain _ p         -> dryPatch p
    SplitPatch a dt b   -> SplitPatch (dryPatch a) dt (dryPatch b)
    LayerPatch xs       -> LayerPatch $ mapSnd dryPatch xs

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
                MonoSynt spec instr -> MonoSynt spec $ fmap (mapSig f) . instr
                PolySynt instr -> PolySynt $ fmap (mapSig f) . instr
                FxChain fxs p  -> FxChain fxs $ mapSig f p
                SplitPatch a dt b -> SplitPatch (mapSig f a) dt (mapSig f b)
                LayerPatch xs  -> FxChain [FxSpec 1 (return . mapSig f)] $ LayerPatch xs

mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd f = fmap (second f) 

wet :: (SigSpace a, Sigs a) => FxSpec a -> Fx a
wet (FxSpec k fx) asig = fmap ((mul (1 - k) asig + ) . mul k) $ fx asig

getPatchFx :: (SigSpace a, Sigs a) => [FxSpec a] -> Fx a
getPatchFx xs = foldr (<=<) return $ fmap wet xs

atNote :: (SigSpace a, Sigs a) => Patch a -> CsdNote D -> SE a
atNote p note@(amp, cps) = case p of
    MonoSynt spec instr -> instr (sig amp, sig cps)
    PolySynt instr -> instr note
    FxChain fxs p -> getPatchFx fxs =<< atNote p note 
    LayerPatch xs -> onLayered xs $ \x -> atNote x note    
    SplitPatch a t b -> getSplit (cps `lessThan` t) (atNote a note) (atNote b note)

getSplit :: (Num a, Tuple a) => BoolD -> SE a -> SE a -> SE a
getSplit cond a b = do
    ref <- newRef 0
    whenElseD cond 
        (mixRef ref =<< a)
        (mixRef ref =<< b)
    readRef ref

atMidi :: (SigSpace a, Sigs a) => Patch a -> SE a
atMidi x = case x of
    MonoSynt spec instr -> monoSynt spec instr
    PolySynt instr -> midi (instr . ampCps)
    FxChain fxs p -> getPatchFx fxs =<< atMidi p
    LayerPatch xs -> onLayered xs atMidi
    SplitPatch a dt b -> genMidiSplitPatch ampCps a dt b
    where
        monoSynt spec instr = instr =<< getArg
            where
                getArg
                    | monoSyntHold spec = holdMsg chn port
                    | otherwise         = monoMsg chn port rel

                port = monoSyntSlideTime spec
                rel  = monoSyntRelease spec
                chn  = monoSyntChn spec

atMidiTemp :: (SigSpace a, Sigs a) => Temp -> Patch a -> SE a
atMidiTemp tm x = case x of
    MonoSynt spec instr -> monoSynt spec instr
    PolySynt instr -> midi (instr . ampCps' tm)
    FxChain fxs p -> getPatchFx fxs =<< atMidiTemp tm p
    LayerPatch xs -> onLayered xs (atMidiTemp tm)
    SplitPatch a cps b -> genMidiSplitPatch (ampCps' tm) a cps b
    where
        monoSynt spec instr = instr =<< getArg
            where
                getArg
                    | monoSyntHold spec = holdMsgTemp tm chn port
                    | otherwise         = monoMsgTemp tm chn port rel

                port = monoSyntSlideTime spec
                rel  = monoSyntRelease spec
                chn  = monoSyntChn spec

genMidiSplitPatch :: (SigSpace a, Sigs a) => (Msg -> (D, D)) -> Patch a -> D -> Patch a -> SE a
genMidiSplitPatch midiArg = genSplitPatch $ \instr -> midi (instr . midiArg)

genSplitPatch :: (SigSpace a, Sigs a) => ((CsdNote D -> SE a) -> SE a) -> Patch a -> D -> Patch a -> SE a
genSplitPatch playInstr a dt b = liftA2 (+) (leftSplit dt a) (rightSplit dt b)
    where
        leftSplit  dt a = onCondPlay ( `lessThan` dt) a
        rightSplit dt a = onCondPlay ( `greaterThanEquals` dt) a

        onCondPlay cond x = case x of
            MonoSynt spec instr -> error "Split doesn't work for monophonic synths. Pleas use only polyphonic synths."
            PolySynt instr -> playInstr (restrictPolyInstr cond instr)
            FxChain fxs p -> getPatchFx fxs =<< onCondPlay cond p
            LayerPatch xs -> onLayered xs (onCondPlay cond)
            SplitPatch a dt b -> liftA2 (+) 
                        (onCondPlay (\x -> cond x &&* (x `lessThan` dt)) a) 
                        (onCondPlay (\x -> cond x &&* (x `greaterThanEquals` dt)) b)

restrictPolyInstr :: (Sigs a) => (D -> BoolD) -> (CsdNote D -> SE a) -> CsdNote D -> SE a
restrictPolyInstr cond instr note@(amp, cps) = do
    ref <- newRef 0
    whenElseD (cond cps) 
        (writeRef ref =<< instr note)
        (writeRef ref 0)
    readRef ref

--------------------------------------------------------------
-- sched

-- | Plays a patch with event stream. Supplies a custom value for mixing effects (dry/wet).
-- The 0 is a dry signal, the 1 is a wet signal.
atSched :: (SigSpace a, Sigs a) => Patch a -> Evt (Sco (CsdNote D)) -> SE a
atSched x evt = case x of
    MonoSynt _ instr -> error "atSched is not defined for monophonic synthesizers"
    PolySynt instr -> playInstr instr
    FxChain fxs p  -> getPatchFx fxs =<< atSched p evt
    LayerPatch xs -> onLayered xs (\patch -> atSched patch evt)
    SplitPatch a t b -> genSplitPatch playInstr a t b
    where playInstr instr = return $ sched instr evt


atSchedUntil :: (SigSpace a, Sigs a) => Patch a -> Evt (CsdNote D) -> Evt b -> SE a
atSchedUntil x evt stop = case x of     
    MonoSynt _ instr -> error "atSchedUntil is not defined for monophonic synthesizers"
    PolySynt instr -> playInstr instr
    FxChain fxs p  -> getPatchFx fxs =<< atSchedUntil p evt stop
    LayerPatch xs -> onLayered xs (\patch -> atSchedUntil patch evt stop)
    SplitPatch a cps b -> genSplitPatch playInstr a cps b
    where playInstr instr = return $ schedUntil instr evt stop

--------------------------------------------------------------
-- sco
 
-- | Plays a patch with scores. Supplies a custom value for mixing effects (dry/wet).
-- The 0 is a dry signal, the 1 is a wet signal.
atSco :: (SigSpace a, Sigs a) => Patch a -> Sco (CsdNote D) -> Sco (Mix a)
atSco x sc = case x of
    MonoSynt _ instr -> error "atSco is not defined for monophonic synthesizers"
    PolySynt instr -> sco instr sc 
    FxChain fxs p  -> eff (getPatchFx fxs) $atSco p sc
    LayerPatch xs -> har $ fmap (\(vol, p) -> atSco (mul vol p) sc) xs
    SplitPatch a cps b -> undefined    
--    eff (getPatchFx p) $ sco (patchInstr p) sc 

scoSplitPatch :: (SigSpace a, Sigs a) => Patch a -> D -> Patch a -> Sco (CsdNote D) -> Sco (Mix a)
scoSplitPatch a dt b sc = har [leftSplit dt a, rightSplit dt b]
    where
        leftSplit  dt a = onCondPlay ( `lessThan` dt) a
        rightSplit dt a = onCondPlay ( `greaterThanEquals` dt) a

        onCondPlay cond x = case x of
            MonoSynt spec instr -> error "Split doesn't work for monophonic synths. Pleas use only polyphonic synths."
            PolySynt instr -> sco (restrictPolyInstr cond instr) sc
            FxChain fxs p -> eff (getPatchFx fxs) $ atSco p sc
            LayerPatch xs -> har $ fmap (\(vol, p) -> atSco (mul vol p) sc) xs
            SplitPatch a dt b -> har 
                        [ onCondPlay (\x -> cond x &&* (x `lessThan` dt)) a
                        , onCondPlay (\x -> cond x &&* (x `greaterThanEquals` dt)) b ]

onLayered :: (SigSpace a, Sigs a) => [(Sig, Patch a)] -> (Patch a -> SE a) -> SE a
onLayered xs f = fmap sum $ mapM (\(vol, p) -> fmap (mul vol) $ f p) xs

--    getPatchFx a =<< midi (patchInstr a . ampCps)    

onMonoSyntSpec :: (MonoSyntSpec -> MonoSyntSpec) -> Patch a -> Patch a
onMonoSyntSpec f x = case x of
    MonoSynt spec instr -> MonoSynt (f spec) instr
    PolySynt instr -> PolySynt instr
    FxChain fxs p -> FxChain fxs $ onMonoSyntSpec f p 
    LayerPatch xs -> LayerPatch $ mapSnd (onMonoSyntSpec f) xs
    SplitPatch a cps b -> SplitPatch (onMonoSyntSpec f a) cps (onMonoSyntSpec f b)

setMonoSharp :: Patch a -> Patch a
setMonoSharp = onMonoSyntSpec (\x -> x { monoSyntSlideTime = 0.005, monoSyntRelease = 0.05 })

setMonoHold :: Patch a -> Patch a
setMonoHold = onMonoSyntSpec (\x -> x { monoSyntHold = True })


transPatch :: D -> Patch a -> Patch a
transPatch k = mapMonoPolyInstr (\instr -> instr . second ( * sig k)) (\instr -> instr . second ( * k))

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
-}

-- | Adds an effect to the patch's instrument.
addInstrFx :: Fx a -> Patch a -> Patch a
addInstrFx f p = mapPatchInstr (\instr -> f <=< instr) p

-- | Appends an effect before patch's effect.
addPreFx :: DryWetRatio -> Fx a -> Patch a -> Patch a
addPreFx dw f p = case p of
    FxChain fxs (PolySynt instr) -> FxChain (addFx fxs) (PolySynt instr)
    FxChain fxs (MonoSynt spec instr) -> FxChain (addFx fxs) (MonoSynt spec instr)
    PolySynt instr -> FxChain fxSpec $ PolySynt instr
    MonoSynt spec instr -> FxChain fxSpec $ MonoSynt spec instr
    LayerPatch xs -> LayerPatch $ mapSnd (addPreFx dw f) xs
    SplitPatch a cps b -> SplitPatch (addPreFx dw f a) cps (addPreFx dw f b)
    where 
        addFx xs = xs ++ fxSpec
        fxSpec = [FxSpec dw f]

-- | Appends an effect after patch's effect.
addPostFx :: DryWetRatio -> Fx a -> Patch a -> Patch a
addPostFx dw f p = case p of
    FxChain fxs rest -> FxChain (fxSpec : fxs) rest
    _                -> FxChain [fxSpec] p
    where fxSpec = FxSpec dw f

{-
----------------------------------------------------------------

-- | Plays the patch when confition is true otherwise it produces silence.
patchWhen :: Sigs b => BoolSig -> Patch a b -> Patch a b
patchWhen cond p = p 
	{ patchInstr = playWhen cond (patchInstr p)
	, patchFx    = fmap (mapFun $ playWhen cond) (patchFx p) }
	where mapFun f x = x { fxFun = f $ fxFun x }

-}

mixInstr :: (SigSpace b, Num b) => Sig -> Patch b -> Patch b -> Patch b
mixInstr k f p = LayerPatch [(k, f), (1, p)]

------------------------------------------------
-- pads

harmonPatch :: (SigSpace b, Sigs b) => [Sig] -> [D] -> Patch b -> Patch b
harmonPatch amps freqs = tfmInstr monoTfm polyTfm
    where
        monoTfm instr = \(amp, cps) -> fmap sum $ zipWithM (\a f -> fmap (mul a) $ instr (amp, cps * f)) amps (fmap sig freqs)
        polyTfm instr = \(amp, cps) -> fmap sum $ zipWithM (\a f -> fmap (mul a) $ instr (amp, cps * f)) amps freqs 

deepPad :: (SigSpace b, Sigs b) => Patch b -> Patch b
deepPad = harmonPatch (fmap (* 0.75) [1, 0.5]) [1, 0.5]

tfmInstr :: ((CsdNote Sig -> SE b) -> (CsdNote Sig -> SE b)) -> ((CsdNote D -> SE b) -> (CsdNote D -> SE b)) -> Patch b -> Patch b
tfmInstr monoTfm polyTfm x = case x of
    MonoSynt spec instr -> MonoSynt spec $ monoTfm instr
    PolySynt instr -> PolySynt $ polyTfm instr
    FxChain fxs p -> FxChain fxs $ rec p
    SplitPatch a cps b -> SplitPatch (rec a) cps (rec b)
    LayerPatch xs -> LayerPatch $ mapSnd rec xs
    where
        rec = tfmInstr monoTfm polyTfm
        mapSnd f = fmap (second f) 

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
sfPatchHall = withSmallHall . sfPatch

sfPatch :: Sf -> Patch2
sfPatch sf = PolySynt $ \(amp, cps) -> return $ sfCps sf 0.5 amp cps

{-

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