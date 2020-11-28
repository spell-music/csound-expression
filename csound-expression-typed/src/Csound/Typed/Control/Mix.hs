{-# Language FlexibleContexts, ScopedTypeVariables, CPP #-}
module Csound.Typed.Control.Mix(
    Mix,
    sco, eff, mix, mixBy, monoSco,
    sco_, mix_, mixBy_,
    Sco, CsdEventList(..), CsdEvent
) where

import Data.Boolean

import Control.Monad.IO.Class
import System.Mem.StableName

import Temporal.Media

import Csound.Dynamic hiding (Instr, Sco, str)

import Csound.Typed.Types
import Csound.Typed.Types.MixSco
import Csound.Typed.GlobalState hiding (notes)
import Csound.Typed.Control.Instr
import Csound.Typed.InnerOpcodes

#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif

toCsdEventList :: Sco a -> CsdEventList a
toCsdEventList = id

singleCsdEvent :: (Sig, Sig, a) -> Sco a
singleCsdEvent (start, duration, content) = del start $ str duration $ temp content

-- | Special type that represents a scores of sound signals.
-- If an instrument is triggered with the scores the result is wrapped
-- in the value of this type.
newtype Mix a = Mix { unMix :: GE M }

type Sco a = Track Sig a

wrapSco :: Sco a -> (CsdEventList a -> GE M) -> Sco (Mix b)
wrapSco notes getContent = singleCsdEvent (0, csdEventListDur evts, Mix $ getContent evts)
    where evts = toCsdEventList notes

-- | Plays a bunch of notes with the given instrument.
--
-- > res = sco instrument scores
sco :: (Arg a, Sigs b) => (a -> SE b) -> Sco a -> Sco (Mix b)
sco instr notes = wrapSco notes $ \events -> do
    events' <- traverse toNote events
    instrId <- saveSourceInstrCachedWithLivenessWatch (funArity instr) (insExp instr)
    return $ Snd instrId events'

-- | Invokes a procedure for the given bunch of events.
sco_ :: (Arg a) => (a -> SE ()) -> Sco a -> Sco (Mix Unit)
sco_ instr notes = wrapSco notes $ \events -> do
    events' <- traverse toNote events
    instrId <- saveSourceInstrCached_ (unitExp $ fmap (const unit) $ instr toArg)
    return $ Snd instrId events'

-- | Applies an effect to the sound. Effect is applied to the sound on the give track.
--
-- > res = eff effect sco
--
-- * @effect@ - a function that takes a tuple of signals and produces
--   a tuple of signals.
--
-- * @sco@ - something that is constructed with 'Csound.Base.sco' or
--   'Csound.Base.eff'.
--
-- With the function 'Csound.Base.eff' you can apply a reverb or adjust the
-- level of the signal. It functions like a mixing board but unlike mixing
-- board it produces the value that you can arrange with functions from your
-- favorite Score-generation library. You can delay it or mix with some other track and
-- apply some another effect on top of it!
eff :: (Sigs a, Sigs b) => (a -> SE b) -> Sco (Mix a) -> Sco (Mix b)
eff ef sigs = wrapSco sigs $ \events -> do
    notes <- traverse unMix events
    instrId <- saveEffectInstr (funArity ef) (effExp ef)
    return $ Eff instrId notes (arityIns $ funArity ef)

-- | Plays a bunch of notes with the given monophonic instrument. See details on type @MonoArg@.
-- The scores contain the pairs of amplitude (0 to 1) and frequency (in Hz).
--
-- > res = monoSco instrument scores
monoSco :: forall a . Sigs a => (MonoArg -> SE a) -> Sco (D, D) -> Sco (Mix a)
monoSco instr notes = wrapSco notes $ \events -> do
    events' <- traverse toNote events
    argId <- saveSourceInstrCached_ (unitExp $ fmap (const unit) $ instrMonoArg toArg)
    instrId <- saveEffectInstr ((funArity instr) { arityIns = 3 }) (effExp effInstr)
    return $ MonoSnd instrId argId events'
    where
        instrMonoArg :: ((D, D), Port Sig3) -> SE ()
        instrMonoArg ((amp, cps), port) =
            modifyPort port $ \(_, _, notnum) -> (sig amp, sig cps, notnum + 1)

        effInstr :: Sigs a => (Sig, Sig, Sig) -> SE a
        effInstr (amp, cps, notnum) = instr (MonoArg amp cps gate (changed [amp, cps, gate]))
            where gate = ifB (notnum ==* 0) 0 1

-- | Renders a scores to the sound signals. we can use it inside the other instruments.
mix :: (Sigs a) => Sco (Mix a) -> a
mix a = flip apInstr unit $ do
    key <- mixKey a
    durE <- toGE $ dur a
    withCache (ExpDur durE) getMixKey saveMixKey key $
        saveMixInstr (mixArity a) =<< toEventList a'
    where a' = toCsdEventList a

-- | Imitates a closure for a bunch of notes to be played within another instrument.
mixBy :: (Arg a, Sigs b) => (a -> Sco (Mix b)) -> (a -> b)
mixBy evts args = flip apInstr args $ do
    key <- mixKey evts
    durE <- toGE $ dur evts'
    withCache (ExpDur durE) getMixKey saveMixKey key $
        saveMixInstr (mixArityFun evts) =<< (toEventList evts')
    where evts' = toCsdEventList $ evts toArg

-- | Converts a bunch of procedures scheduled with scores to a single procedure.
mix_ :: Sco (Mix Unit) -> SE ()
mix_ a = fromDep_ $ hideGEinDep $ do
    key <- mixKey a
    durE <- toGE $ dur a
    withCache (ExpDur durE) getMixProcKey saveMixProcKey key $
        saveMixInstr_ =<< toEventList a'
    where a' = toCsdEventList a

-- | Imitates a closure for a bunch of procedures to be played within another instrument.
mixBy_ :: (Arg a) => (a -> Sco (Mix Unit)) -> (a -> SE ())
mixBy_ evts args = mix_ $ evts args

----------------------------------------------------------

mixKey :: a -> GE MixKey
mixKey = liftIO . fmap (MixKey . hashStableName) . makeStableName

toEventList :: Sco (Mix a) -> GE (CsdEventList M)
toEventList evts = fmap delayAndRescaleCsdEventListM $ traverse unMix $ evts

mixArity :: Sigs b => f (Mix b) -> Int
mixArity = tupleArity . proxy
    where
        proxy :: f (Mix b) -> b
        proxy = const undefined

mixArityFun :: Sigs b => (a -> f (Mix b)) -> Int
mixArityFun = tupleArity . proxy
    where
        proxy :: (a -> f (Mix b)) -> b
        proxy = const undefined
