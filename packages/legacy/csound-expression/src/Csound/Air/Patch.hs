{-# Language ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, LambdaCase #-}
-- | Patches.
module Csound.Air.Patch(

  CsdNote, Instr, MonoInstr, Fx, Fx1, Fx2, FxSpec(..), DryWetRatio,
  Patch1, Patch2, Patch(..), PolySyntSpec(..), MonoSyntSpec(..),
    SyntSkin, GenInstr, GenMonoInstr, GenFxSpec,
    polySynt, monoSynt, adsrMono, adsrMonoFilter, fxSpec, polySyntFilter, monoSyntFilter, fxSpecFilter,

    mapPatchInstr, mapMonoPolyInstr, transPatch, dryPatch, getPatchFx,
    setFxMix, setFxMixes,
    setMidiChn,

  -- * Midi
  atMidi,

  -- * Events
  atSched, atSchedUntil, atSchedHarp,

  -- * Sco
  atSco,

  -- * Single note
  atNote,

  -- * Fx
    addInstrFx, addPreFx, addPostFx,

    -- ** Specific fx
    fxSig, fxSigMix, fxSig2, fxSigMix2,
    mapFx, mapFx', bindFx, bindFx',
    mapPreFx, mapPreFx', bindPreFx, bindPreFx',

  -- * Pads
  harmonPatch, deepPad,

  -- * Misc
  patchWhen,

    mixInstr,

  -- * Rever
  withSmallRoom, withSmallRoom',
  withSmallHall, withSmallHall',
  withLargeHall, withLargeHall',
  withMagicCave, withMagicCave',

  -- * Sound font patches
  sfPatch, sfPatchHall,

    -- * Monosynt params
    onMonoSyntSpec, setMonoSlide, setMonoSharp,

    -- * Csound API
    patchByNameMidi,

  -- * Custom temperament
  -- ** Midi
  atMidiTemp,
  -- ** Csound API
    patchByNameMidiTemp
) where

import Data.Boolean hiding (cond)
import Data.Text (Text)
import Data.Default
import Control.Monad
import Control.Applicative
import Control.Arrow(second)

import Control.Monad.Trans.Reader
import Csound.Typed hiding (arg)
import Csound.Control.Midi
import Csound.Control.Instr
import Csound.Control.Evt(impulse)
import Csound.Control.Sf
import Csound.Air.Fx
import Csound.Air.Filter(ResonFilter, mlp)
import Csound.Typed.Opcode(cpsmidinn)
import Csound.Tuning
import Csound.Types

import Temporal.Media hiding (rest)
import Csound.IO

-- | Common parameters for patches. We use this type to parametrize the patch with some tpyes of arguments
-- that we'd like to be able to change after patch is already constructed. For instance the filter type can greatly
-- change the character of the patch. So by making patches depend on filter type we can let the user to change
-- the filter type and  leave the algorithm the same. It's like changing between trademarks. Moog sound vs Korg sound.
--
-- The instruments in the patches depend on the @SyntSkin@ through the @Reader@ data type.
--
-- If user doesn't supply any syntSkin value the default is used (`mlp` -- moog low pass filter). Right now
-- the data type is just a synonym for filter but it can become a data type with more parameters in the future releases.
type SyntSkin = ResonFilter

-- | Generic polyphonic instrument. It depends on @SyntSkin@.
type GenInstr a b = Reader SyntSkin (Instr a b)

-- | Generic FX. It depends on @SyntSkin@.
type GenFxSpec a = Reader SyntSkin (FxSpec a)

-- | Generic monophonic instrument. It depends on @SyntSkin@.
type GenMonoInstr a = Reader SyntSkin (MonoInstr a)

-- | Data type for monophonic instruments.
type MonoInstr a = MonoArg -> SE a

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

-- | Fx specification. It;s a pair of dryWet ratio and a transformation function.
data FxSpec a = FxSpec
  { fxMix :: DryWetRatio
  , fxFun :: Fx a
  }

-- | Mono-output patch.
type Patch1 = Patch Sig

-- | Stereo-output patch.
type Patch2 = Patch Sig2

-- | Specification for monophonic synthesizer.
--
-- * Chn -- midi channel to listen on
--
-- * SlideTime -- time of transition between notes
data MonoSyntSpec = MonoSyntSpec
    { monoSyntChn       :: MidiChn
    , monoSyntSlideTime :: Maybe D }

instance Default MonoSyntSpec where
    def = MonoSyntSpec
        { monoSyntChn = ChnAll
        , monoSyntSlideTime = Just 0.008 }

data PolySyntSpec = PolySyntSpec
    { polySyntChn :: MidiChn }

instance Default PolySyntSpec where
    def = PolySyntSpec { polySyntChn = ChnAll }

-- | The patch can be:
--
-- *  a monophonic synt
--
-- * polyphonic synt
--
-- * set of common parameters (@SyntSkin@)
--
-- * patch with chain of effects,
--
-- * split on keyboard with certain frequency
--
-- * layer of patches. That is a several patches that sound at the same time.
--  the layer is a patch and the weight of volume for a given patch.
data Patch a
    = MonoSynt MonoSyntSpec (GenMonoInstr a) -- (GenInstr Sig a)
    | PolySynt PolySyntSpec (GenInstr D   a)
    | SetSkin SyntSkin (Patch a)
    | FxChain [GenFxSpec a] (Patch a)
    | SplitPatch (Patch a) D (Patch a)
    | LayerPatch [(Sig, Patch a)]

smoothMonoSpec :: MonoSyntSpec -> MonoArg -> MonoArg
smoothMonoSpec spec = maybe id smoothMonoArg (monoSyntSlideTime spec)

-- | Constructor for polyphonic synthesizer. It expects a function from notes to signals.
polySynt :: (Instr D a) -> Patch a
polySynt = PolySynt def . return

-- | Constructor for polyphonic synthesizer with flexible choice of the low-pass filter.
-- If we use the filter from the first argument user lately can change it to some another filter. It defaults to mlp.
polySyntFilter :: (ResonFilter -> Instr D a) -> Patch a
polySyntFilter instr = PolySynt def $ reader instr

-- | Constructor for monophonic synth with envelope generator. The envelope generator is synced with note triggering.
-- So it restarts itself when the note is retriggered. The envelope generator is a simple ADSR gennerator see the type @MonoAdsr@.
adsrMono :: (MonoAdsr -> Instr Sig a) -> Patch a
adsrMono f = monoSynt (adsrMonoSynt f)

-- | Constructor for monophonic synth with envelope generator and flexible choice of filter. It's just like @adsrMono@
-- but the user lately can change filter provided in the first argument to some another filter.
adsrMonoFilter :: (ResonFilter -> MonoAdsr -> Instr Sig a) -> Patch a
adsrMonoFilter f = monoSyntFilter (\fltr -> adsrMonoSynt (f fltr))

-- | Constructor for monophonic synthesizer. The instrument is defned on the raw monophonic aruments (see @MonoArg@).
monoSynt :: (MonoInstr a) -> Patch a
monoSynt = MonoSynt def . return

-- | Constructor for monophonic synthesizer with flexible filter choice.
monoSyntFilter :: (ResonFilter -> MonoInstr a) -> Patch a
monoSyntFilter instr = MonoSynt def $ reader instr

-- | Constructor for FX-specification.
--
-- > fxSpec dryWetRatio fxFun
fxSpec :: Sig -> Fx a -> GenFxSpec a
fxSpec ratio fx = return $ FxSpec ratio fx

-- | Constructor for FX-specification with flexible filter choice.
--
-- > fxSpec dryWetRatio fxFun
fxSpecFilter :: Sig -> (ResonFilter -> Fx a) -> GenFxSpec a
fxSpecFilter ratio fx = reader $ \resonFilter -> FxSpec ratio (fx resonFilter)

-- Maps all monophonic and polyphonic patches within the given patch.
mapMonoPolyInstr :: (MonoInstr a -> MonoInstr a) -> (Instr D a -> Instr D a) -> Patch a -> Patch a
mapMonoPolyInstr mono poly x = case x of
    MonoSynt spec instr -> MonoSynt spec (fmap mono instr)
    PolySynt spec instr -> PolySynt spec (fmap poly instr)
    SetSkin skin p      -> SetSkin skin (rec p)
    FxChain  fxs p      -> FxChain fxs (rec p)
    LayerPatch xs       -> LayerPatch (mapSnd rec xs)
    SplitPatch a dt b   -> SplitPatch (rec a) dt (rec b)
    where
        rec = mapMonoPolyInstr mono poly

-- Maps all polyphonic patches within the given patch.
mapPatchInstr :: (Instr D a -> Instr D a) -> Patch a -> Patch a
mapPatchInstr f x = case x of
    MonoSynt _ _ -> x
    PolySynt spec instr -> PolySynt spec $ fmap f instr
    SetSkin skin p -> SetSkin skin (rec p)
    FxChain fxs p -> FxChain fxs $ rec p
    LayerPatch xs -> LayerPatch (mapSnd rec xs)
    SplitPatch a dt b -> SplitPatch (rec a) dt (rec b)
    where
        rec = mapPatchInstr f

-- | Removes all effects from the patch.
dryPatch :: Patch a -> Patch a
dryPatch patch = case patch of
    MonoSynt _ _ -> patch
    PolySynt _ _ -> patch
    SetSkin skin p -> SetSkin skin (dryPatch p)
    FxChain _ p         -> dryPatch p
    SplitPatch a dt b   -> SplitPatch (dryPatch a) dt (dryPatch b)
    LayerPatch xs       -> LayerPatch $ mapSnd dryPatch xs

-- | Sets the dryWet ratio of the effects wwithin the patch.
setFxMix :: Sig -> Patch a -> Patch a
setFxMix a = setFxMixes [a]

-- | Sets the dryWet ratios for the chain of the effects wwithin the patch.
setFxMixes :: [Sig] -> Patch a -> Patch a
setFxMixes ks = \case
    FxChain fxs x -> FxChain (zipFirst (\k q -> fmap (\t -> t { fxMix = k }) q) ks fxs) x
    other -> other
    where
        zipFirst f xs ys = case (xs, ys) of
            (_,    [])   -> []
            ([],   bs)   -> bs
            (a:as, b:bs) -> f a b : zipFirst f as bs

--------------------------------------------------------------

instance SigSpace a => SigSpace (Patch a) where
  mapSig f x =
            case x of
                MonoSynt spec instr -> MonoSynt spec $ fmap (fmap (mapSig f) . ) $ instr
                PolySynt spec instr -> PolySynt spec $ fmap (fmap (mapSig f) . ) $ instr
                SetSkin skin p -> SetSkin skin $ mapSig f p
                FxChain fxs p  -> FxChain fxs $ mapSig f p
                SplitPatch a dt b -> SplitPatch (mapSig f a) dt (mapSig f b)
                LayerPatch xs  -> FxChain [return $ FxSpec 1 (return . mapSig f)] $ LayerPatch xs

mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd f = fmap (second f)

wet :: (SigSpace a, Sigs a) => FxSpec a -> Fx a
wet (FxSpec k fx) asig = fmap ((mul (1 - k) asig + ) . mul k) $ fx asig

-- | Renders the effect chain to a single function.
getPatchFx :: (SigSpace a, Sigs a) => Maybe SyntSkin -> [GenFxSpec a] -> Fx a
getPatchFx maybeSkin xs = foldr (<=<) return $ fmap (wet . flip runSkin maybeSkin) xs

-- | Plays a patch with a single infinite note.
atNote :: (SigSpace a, Sigs a) => Patch a -> CsdNote D -> SE a
atNote = go Nothing
    where
        go maybeSkin q note@(amp, cps) = case q of
            MonoSynt _spec instr -> (runSkin instr maybeSkin) (MonoArg (sig amp) (sig cps) 1 (impulse 0))
            PolySynt _spec instr -> (runSkin instr maybeSkin) note
            SetSkin  skin p -> newSkin skin p
            FxChain fxs p -> getPatchFx maybeSkin fxs =<< rec p
            LayerPatch xs -> onLayered xs rec
            SplitPatch a t b -> getSplit (cps `lessThan` t) (rec a) (rec b)
            where
                rec x = go maybeSkin x note
                newSkin skin x = go (Just skin) x note

runSkin :: Reader SyntSkin a -> Maybe SyntSkin -> a
runSkin instr maybeSkin = runReader instr $ maybe mlp id maybeSkin

getSplit :: (Num a, Tuple a) => BoolD -> SE a -> SE a -> SE a
getSplit cond a b = do
    ref <- newRef 0
    whenElseD cond
        (mixRef ref =<< a)
        (mixRef ref =<< b)
    readRef ref

--------------------------------------------------------------
-- midi

midiChn :: Sigs a => MidiChn -> (Msg -> SE a) -> SE a
midiChn = \case
    ChnAll -> midi
    Chn n  -> midin n
    Pgm pgm chn -> pgmidi pgm chn

-- | Plays a patch with midi.
atMidi :: (SigSpace a, Sigs a) => Patch a -> SE a
atMidi = go Nothing
    where
        go maybeSkin = \case
            MonoSynt spec instr -> monoSyntProc spec (runSkin instr maybeSkin)
            PolySynt spec instr -> midiChn (polySyntChn spec) ((runSkin instr maybeSkin) . ampCps)
            SetSkin skin p -> newSkin skin p
            FxChain fxs p -> getPatchFx maybeSkin fxs =<< rec p
            LayerPatch xs -> onLayered xs rec
            SplitPatch a dt b -> genMidiSplitPatch maybeSkin ampCps a dt b
            where
                newSkin skin p = go (Just skin) p
                rec = go maybeSkin

                monoSyntProc spec instr = instr =<< getArg
                    where
                        getArg = fmap (smoothMonoSpec spec) $ genMonoMsg chn
                        chn  = monoSyntChn spec

-- | Plays a patch with midi with given temperament (see @Csound.Tuning@).
atMidiTemp :: (SigSpace a, Sigs a) => Temp -> Patch a -> SE a
atMidiTemp tm = go Nothing
    where
        go maybeSkin = \case
            MonoSynt spec instr -> monoSyntProc spec (runSkin instr maybeSkin)
            PolySynt spec instr -> midiChn (polySyntChn spec) ((runSkin instr maybeSkin) . ampCps' tm)
            SetSkin skin p -> newSkin skin p
            FxChain fxs p -> getPatchFx maybeSkin fxs =<< rec p
            LayerPatch xs -> onLayered xs rec
            SplitPatch a cps b -> genMidiSplitPatch maybeSkin (ampCps' tm) a cps b
            where
                newSkin skin p = go (Just skin) p
                rec = go maybeSkin

                monoSyntProc spec instr = instr =<< getArg
                    where
                        getArg = fmap (smoothMonoSpec spec) $ genMonoMsgTemp tm chn
                        chn  = monoSyntChn spec


genMidiSplitPatch :: (SigSpace a, Sigs a) => Maybe SyntSkin -> (Msg -> (D, D)) -> Patch a -> D -> Patch a -> SE a
genMidiSplitPatch maybeSkin midiArg = genSplitPatch maybeSkin playMonoInstr playInstr
    where
        playMonoInstr chn cond instr = instr =<< genFilteredMonoMsg chn cond
        playInstr chn instr = midiChn chn (instr . midiArg)

genSplitPatch :: (SigSpace a, Sigs a) => Maybe SyntSkin -> (MidiChn -> (D -> BoolD) -> MonoInstr a -> SE a)  -> (MidiChn -> (CsdNote D -> SE a) -> SE a) -> Patch a -> D -> Patch a -> SE a
genSplitPatch maybeSkin' playMonoInstr playInstr a' dt' b' = liftA2 (+) (leftSplit maybeSkin' dt' a') (rightSplit maybeSkin' dt' b')
    where
        leftSplit  maybeSkin dt a = onCondPlay maybeSkin ( `lessThan` dt)          ( `lessThan` (sig dt))           a
        rightSplit maybeSkin dt a = onCondPlay maybeSkin ( `greaterThanEquals` dt) ( `greaterThanEquals` (sig dt))  a

        onCondPlay maybeSkin cond condSig = \case
            MonoSynt spec instr -> playMonoInstr  (monoSyntChn spec) cond  (restrictMonoInstr condSig $ runSkin instr maybeSkin)
            PolySynt spec instr -> playInstr (polySyntChn spec) (restrictPolyInstr cond (runSkin instr maybeSkin))
            SetSkin  skin p -> onCondPlay (Just skin) cond condSig p
            FxChain fxs p -> getPatchFx maybeSkin fxs =<< onCondPlay maybeSkin cond condSig p
            LayerPatch xs -> onLayered xs (onCondPlay maybeSkin cond condSig)
            SplitPatch a dt b -> liftA2 (+)
                        (onCondPlay maybeSkin (\x -> cond x &&* (x `lessThan` dt))           (\x -> condSig x &&* (x `lessThan` (sig dt))) a)
                        (onCondPlay maybeSkin (\x -> cond x &&* (x `greaterThanEquals` dt))  (\x -> condSig x &&* (x `greaterThanEquals` (sig dt) ))  b)

restrictPolyInstr :: (Sigs a) => (D -> BoolD) -> (CsdNote D -> SE a) -> CsdNote D -> SE a
restrictPolyInstr cond instr note@(_amp, cps) = do
    ref <- newRef 0
    whenElseD (cond cps)
        (writeRef ref =<< instr note)
        (writeRef ref 0)
    readRef ref

restrictMonoInstr :: (Sigs a) => (Sig -> BoolSig) -> MonoInstr a -> MonoInstr a
restrictMonoInstr cond instr arg = instr $ arg { monoGate = monoGate arg * gate2 }
    where
        cps = monoCps arg
        gate2 = ifB (cond cps) 1 0

--------------------------------------------------------------
-- sched

-- | Plays a patch with event stream.
atSched :: (SigSpace a, Sigs a) => Patch a -> Evt (Sco (CsdNote D)) -> SE a
atSched = go Nothing
    where
        go maybeSkin x evt = case x of
            MonoSynt spec instr -> (runSkin instr maybeSkin) =<< (fmap (smoothMonoSpec spec) $ monoSched evt)
            PolySynt _ instr -> playInstr (runSkin instr maybeSkin)
            SetSkin skin p -> newSkin skin p
            FxChain fxs p  -> getPatchFx maybeSkin fxs =<< rec p
            LayerPatch xs -> onLayered xs rec
            SplitPatch a t b -> genSplitPatch maybeSkin (const $ const playMonoInstr) (const playInstr) a t b
            where
                rec a = go maybeSkin a evt
                newSkin skin a = go (Just skin) a evt
                playInstr instr = return $ sched instr evt
                playMonoInstr instr = instr =<< monoSched evt

-- | Plays a patch with event stream with stop-note event stream.
atSchedUntil :: (SigSpace a, Sigs a) => Patch a -> Evt (CsdNote D) -> Evt b -> SE a
atSchedUntil = go Nothing
    where
        go maybeSkin x evt stop = case x of
            MonoSynt _ instr -> playMonoInstr (runSkin instr maybeSkin)
            PolySynt _ instr -> playInstr (runSkin instr maybeSkin)
            SetSkin skin p -> newSkin skin p
            FxChain fxs p  -> getPatchFx maybeSkin fxs =<< rec p
            LayerPatch xs -> onLayered xs rec
            SplitPatch a cps b -> genSplitPatch maybeSkin (const $ const playMonoInstr) (const playInstr) a cps b
            where
                rec a = go maybeSkin a evt stop
                newSkin skin a = go (Just skin) a evt stop
                playInstr instr = return $ schedUntil instr evt stop
                playMonoInstr instr = instr =<< monoSchedUntil evt stop

-- | Plays notes indefinetely (it's more useful for monophonic synthesizers).
atSchedHarp :: (SigSpace a, Sigs a) => Patch a -> Evt (CsdNote D) -> SE a
atSchedHarp x evt = atSchedUntil  x evt mempty

--------------------------------------------------------------
-- sco

-- | Plays a patch with scores.
atSco :: forall a . (SigSpace a, Sigs a) => Patch a -> Sco (CsdNote D) -> Sco (Mix a)
atSco = go Nothing
    where
        go skin x sc = case x of
            MonoSynt _ instr -> monoSco (runSkin instr skin) sc
            PolySynt _ instr -> sco (runSkin instr skin) sc
            SetSkin sk p -> newSkin sk p
            FxChain fxs p  -> eff (getPatchFx skin fxs) $ rec p
            LayerPatch xs -> har $ fmap (\(vol, p) -> rec (mul vol p)) xs
            SplitPatch a cps b -> scoSplitPatch skin a cps b
            where
                rec a = go skin a sc
                newSkin sk a = go (Just sk) a sc

                scoSplitPatch :: Maybe SyntSkin -> Patch a -> D -> Patch a -> Sco (Mix a)
                scoSplitPatch maybeSkin a dt b = har [leftSplit maybeSkin dt a, rightSplit maybeSkin dt b]
                    where
                        leftSplit  mSkin t = onCondPlay mSkin ( `lessThan` t)
                        rightSplit mSkin t = onCondPlay mSkin ( `greaterThanEquals` t)

                        onCondPlay mSkin cond = \case
                            MonoSynt _spec _instr -> error "Split doesn't work for monophonic synths with Scores. Please use only polyphonic synths in this case."
                            PolySynt _spec instr -> sco (restrictPolyInstr cond (runSkin instr mSkin)) sc
                            SetSkin sk p -> onCondPlay (Just sk) cond p
                            FxChain fxs p -> eff (getPatchFx mSkin fxs) $ go mSkin p sc
                            LayerPatch xs -> har $ fmap (\(vol, p) -> go mSkin (mul vol p) sc) xs
                            SplitPatch m t n -> har
                                        [ onCondPlay mSkin (\q -> cond q &&* (q `lessThan` t)) m
                                        , onCondPlay mSkin (\q -> cond q &&* (q `greaterThanEquals` t)) n ]

onLayered :: (SigSpace a, Sigs a) => [(Sig, Patch a)] -> (Patch a -> SE a) -> SE a
onLayered xs f = fmap sum $ mapM (\(vol, p) -> fmap (mul vol) $ f p) xs

--    getPatchFx a =<< midi (patchInstr a . ampCps)

-- | Transform  the spec for monophonic patch.
onMonoSyntSpec :: (MonoSyntSpec -> MonoSyntSpec) -> Patch a -> Patch a
onMonoSyntSpec f x = case x of
    MonoSynt spec instr -> MonoSynt (f spec) instr
    PolySynt spec instr -> PolySynt spec instr
    SetSkin skin p -> SetSkin skin  $ onMonoSyntSpec f p
    FxChain fxs p -> FxChain fxs $ onMonoSyntSpec f p
    LayerPatch xs -> LayerPatch $ mapSnd (onMonoSyntSpec f) xs
    SplitPatch a cps b -> SplitPatch (onMonoSyntSpec f a) cps (onMonoSyntSpec f b)

-- | Sets the midi channel for all instruments in the patch.
setMidiChn :: MidiChn -> Patch a -> Patch a
setMidiChn chn x = case x of
    MonoSynt spec instr -> MonoSynt (spec { monoSyntChn = chn }) instr
    PolySynt spec instr -> PolySynt (spec { polySyntChn = chn }) instr
    SetSkin skin p -> SetSkin skin $ go p
    FxChain fxs p -> FxChain fxs $ go p
    LayerPatch xs -> LayerPatch $ mapSnd go xs
    SplitPatch a cps b -> SplitPatch (go a) cps (go b)
    where go = setMidiChn chn


-- | Sets the monophonic to sharp transition and quick release.
setMonoSharp :: Patch a -> Patch a
setMonoSharp = setMonoSlide 0.004

-- | Sets the slide time for pitch and amplitude of monophomic synthesizers.
setMonoSlide :: D -> Patch a -> Patch a
setMonoSlide slideTime = onMonoSyntSpec (\x -> x { monoSyntSlideTime = Just slideTime })

-- | Transpose the patch by a given ratio. We can use the functions semitone, cent to calculate the ratio.
transPatch :: D -> Patch a -> Patch a
transPatch k = mapMonoPolyInstr (transMonoInstr k) (transPolyInstr k)

transMonoInstr :: D -> MonoInstr a -> MonoInstr a
transMonoInstr k instr = \arg -> instr (arg { monoCps = sig k * monoCps arg })

transPolyInstr :: D -> Instr D a -> Instr D a
transPolyInstr k instr = \(amp, cps) -> instr (amp, k * cps)

-- | Adds an effect to the patch's instrument.
addInstrFx :: Fx a -> Patch a -> Patch a
addInstrFx f p = mapPatchInstr (\instr -> f <=< instr) p

-- | Appends an effect before patch's effect.
addPreFx :: DryWetRatio -> Fx a -> Patch a -> Patch a
addPreFx dw f p = case p of
    FxChain fxs (PolySynt spec instr) -> FxChain (addFx fxs) (PolySynt spec instr)
    FxChain fxs (MonoSynt spec instr) -> FxChain (addFx fxs) (MonoSynt spec instr)
    SetSkin skin q -> SetSkin skin $ addPreFx dw f q
    PolySynt spec instr -> FxChain fxSpec' $ PolySynt spec instr
    MonoSynt spec instr -> FxChain fxSpec' $ MonoSynt spec instr
    LayerPatch xs -> LayerPatch $ mapSnd (addPreFx dw f) xs
    SplitPatch a cps b -> SplitPatch (addPreFx dw f a) cps (addPreFx dw f b)
    _ -> undefined
    where
        addFx xs = xs ++ fxSpec'
        fxSpec' = [return $ FxSpec dw f]

-- | Appends an effect after patch's effect.
addPostFx :: DryWetRatio -> Fx a -> Patch a -> Patch a
addPostFx dw f p = case p of
    FxChain fxs rest -> FxChain (return fxSpec' : fxs) rest
    _                -> FxChain [return fxSpec'] p
    where fxSpec' = FxSpec dw f

--------------------------------------------------------------

-- | Plays a patch when the condition signal is satisfied. Can be useful for switches.
patchWhen :: (Sigs a) => BoolSig -> Patch a -> Patch a
patchWhen cond x = case x of
    MonoSynt spec instr -> MonoSynt spec (fmap (playWhen cond) instr)
    PolySynt spec instr -> PolySynt spec (fmap (playWhen cond) instr)
    SetSkin skin p -> SetSkin skin $ rec p
    FxChain  fxs p      -> FxChain (fmap (fmap $ mapFun (playWhen cond)) fxs) (rec p)
    LayerPatch xs       -> LayerPatch $ mapSnd rec xs
    SplitPatch a cps b  -> SplitPatch (rec a) cps (rec b)
    where
        rec = patchWhen cond
        mapFun f a = a { fxFun = f $ fxFun a }

-- | Mix two patches together.
mixInstr :: (SigSpace b, Num b) => Sig -> Patch b -> Patch b -> Patch b
mixInstr k f p = LayerPatch [(k, f), (1, p)]

------------------------------------------------
-- pads

-- | Harmnoic series of patches.
harmonPatch :: (SigSpace b, Sigs b) => [Sig] -> [D] -> Patch b -> Patch b
harmonPatch amps freqs = tfmInstr monoTfm polyTfm
    where
        monoTfm instr = \arg -> fmap sum $ zipWithM (\a f -> fmap (mul a) $ transMonoInstr f instr arg) amps freqs
        polyTfm instr = \arg -> fmap sum $ zipWithM (\a f -> fmap (mul a) $ transPolyInstr f instr arg) amps freqs

-- | Adds an octave below note for a given patch to make the sound deeper.
deepPad :: (SigSpace b, Sigs b) => Patch b -> Patch b
deepPad = harmonPatch (fmap (* 0.75) [1, 0.5]) [1, 0.5]

-- | Transforms instrument functions for polyphonic and monophonic patches.
tfmInstr :: (MonoInstr b -> MonoInstr b) -> ((CsdNote D -> SE b) -> (CsdNote D -> SE b)) -> Patch b -> Patch b
tfmInstr monoTfm polyTfm x = case x of
    MonoSynt spec instr -> MonoSynt spec $ fmap monoTfm instr
    PolySynt spec instr -> PolySynt spec $ fmap polyTfm instr
    SetSkin  skin p -> SetSkin skin $ rec p
    FxChain fxs p -> FxChain fxs $ rec p
    SplitPatch a cps b -> SplitPatch (rec a) cps (rec b)
    LayerPatch xs -> LayerPatch $ fmap (second rec) xs
    where
        rec = tfmInstr monoTfm polyTfm

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

-- | Sound font patch with a bit of reverb.
sfPatchHall :: Sf -> Patch2
sfPatchHall = withSmallHall . sfPatch

-- | Sound font patch.
sfPatch :: Sf -> Patch2
sfPatch sf = polySynt $ \(amp, cps) -> return $ sfCps sf 0.5 amp cps

------------------------------------------------
-- Csound API


-- | Triggers patch with Csound API.
-- It creates a named instruement with given name (first argument).
--
-- It simulates the midi-like instrument. Notes are encoded with messages:
--
-- > i "givenName" 1 pitchKey volumeKey     -- note on
-- > i "givenName" 0 pitchKey volumeKey     -- note off
patchByNameMidi :: (SigSpace a, Sigs a) => Text -> Patch a -> SE a
patchByNameMidi = genPatchByNameMidi cpsmidinn cpsmidinn

-- | Triggers patch with Csound API.
-- It creates a named instruement with given name (second argument).
-- It behaves like the function @patchByNameMidi@ but we can specify custom temperament.
patchByNameMidiTemp :: (SigSpace a, Sigs a) => Temp -> Text -> Patch a -> SE a
patchByNameMidiTemp tm = genPatchByNameMidi (cpsmidi'Sig tm) (cpsmidi'D tm)

genPatchByNameMidi :: forall a . (SigSpace a, Sigs a) => (Sig -> Sig) -> (D -> D) -> Text -> Patch a -> SE a
genPatchByNameMidi monoKey2cps polyKey2cps name x = go Nothing x
    where
        go maybeSkin = \case
            MonoSynt spec instr -> monoSyntProc spec (runSkin instr maybeSkin)
            PolySynt spec instr -> polySyntProc spec (runSkin instr maybeSkin)
            SetSkin skin p      -> newSkin skin p
            FxChain fxs p       -> getPatchFx maybeSkin fxs =<< rec p
            LayerPatch xs       -> onLayered xs rec
            SplitPatch a cps b  -> splitPatch a cps b
            where
                rec = go maybeSkin
                newSkin skin = go (Just skin)

                monoSyntProc spec instr = instr =<< (fmap (smoothMonoSpec spec . convert) $ trigNamedMono name)
                    where
                        convert a = a { monoAmp = vel2ampSig (monoAmp a), monoCps = monoKey2cps (monoCps a) }

                polySyntProc _spec instr = trigByNameMidi name proc
                    where
                        proc :: (D, D, Unit) -> SE a
                        proc (pitch, vol, _) = instr (vel2amp vol, polyKey2cps pitch)

                splitPatch a cps b = genSplitPatch maybeSkin playMonoInstr playInstr a cps b

                playMonoInstr chn _cond instr = monoSyntProc (def { monoSyntChn = chn }) instr
                playInstr chn instr = polySyntProc (def { polySyntChn = chn }) instr

vel2amp :: D -> D
vel2amp vol = ((vol / 64) ** 2) / 2

vel2ampSig :: Sig -> Sig
vel2ampSig vol = ((vol / 64) ** 2) / 2

{-

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


--------------------------------------------------
-- special functions to add effects

-- | Make an effect out of a pure function.
fxSig :: SigSpace a => (Sig -> Sig) -> GenFxSpec a
fxSig f = fxSpec 1 (return . mapSig f)

-- | Make an effect out of a pure function and specify dry/wet ratio.
fxSigMix :: SigSpace a => Sig -> (Sig -> Sig) -> GenFxSpec a
fxSigMix ratio f = fxSpec ratio (return . mapSig f)

-- | Make an effect out of a stereo pure function.
fxSig2 :: (Sig2 -> Sig2) -> GenFxSpec Sig2
fxSig2 f = fxSpec 1 (return . f)

-- | Make an effect out of a stereo pure function and specify dry/wet ratio.
fxSigMix2 :: Sig -> (Sig2 -> Sig2) -> GenFxSpec Sig2
fxSigMix2 ratio f = fxSpec ratio (return . f)


-- | Adds post fx with pure signal function.
mapFx :: SigSpace a => (Sig -> Sig) -> Patch a -> Patch a
mapFx f = addPostFx 1 (return . mapSig f)

-- | Adds post fx with pure signal function and specifies dry/wet ratio.
mapFx' :: SigSpace a => Sig -> (Sig -> Sig) -> Patch a -> Patch a
mapFx' rate f = addPostFx rate (return . mapSig f)

-- | Adds post fx with effectful signal function.
bindFx :: BindSig a => (Sig -> SE Sig) -> Patch a -> Patch a
bindFx f = addPostFx 1 (bindSig f)

-- | Adds post fx with effectful signal function and specifies dry/wet ratio.
bindFx' :: BindSig a => Sig -> (Sig -> SE Sig) -> Patch a -> Patch a
bindFx' rate f = addPostFx rate (bindSig f)


-- | Adds pre fx with pure signal function.
mapPreFx :: SigSpace a => (Sig -> Sig) -> Patch a -> Patch a
mapPreFx f = addPreFx 1 (return . mapSig f)

-- | Adds pre fx with pure signal function and specifies dry/wet ratio.
mapPreFx' :: SigSpace a => Sig -> (Sig -> Sig) -> Patch a -> Patch a
mapPreFx' rate f = addPreFx rate (return . mapSig f)

-- | Adds pre fx with effectful signal function.
bindPreFx :: BindSig a => (Sig -> SE Sig) -> Patch a -> Patch a
bindPreFx f = addPreFx 1 (bindSig f)

-- | Adds pre fx with effectful signal function and specifies dry/wet ratio.
bindPreFx' :: BindSig a => Sig -> (Sig -> SE Sig) -> Patch a -> Patch a
bindPreFx' rate f = addPreFx rate (bindSig f)

instance RenderCsd Patch1 where
    renderCsdBy opt p = renderCsdBy opt (atMidi p)
    csdArity _ = CsdArity 0 1

instance RenderCsd Patch2 where
    renderCsdBy opt p = renderCsdBy opt (atMidi p)
    csdArity _ = CsdArity 0 2
