module Csound.Exp.Mix(
    -- * Container for sounds (triggered with notes and mixers)
    Mix(..), M(..), nchnls,

    effect, effectS, 
    sco, mix, --, midi, pgmidi

    rescaleCsdEventListM
) where

import Data.Traversable(traverse)
import qualified Data.Map    as M

import Csound.Tfm.Tab

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.SE
import Csound.Exp.GE
import Csound.Exp.Instr
import Csound.Exp.Arg
import Csound.Exp.Tuple(Out(..), CsdTuple, fromCsdTuple, toCsdTuple, outArity)
import Csound.Exp.Options

newtype Mix a = Mix { unMix :: GE M } 

data M 
    = Snd InstrId (CsdEventList Note)
    | Eff InstrId (CsdEventList M)    

nchnls :: Out a => f (Mix a) -> Int
nchnls = outArity . proxy  
    where proxy :: f (Mix a) -> a
          proxy = undefined  

-- | Play a bunch of notes with the given instrument.
--
-- > res = sco instrument scores 
--
-- * @instrument@ is a function that takes notes and produces a 
--   tuple of signals (maybe with some side effect)
--  
-- * @scores@ are some notes (see the module "Temporal.Media" 
--   on how to build complex scores out of simple ones)
--
-- Let's try to understand the type of the output. It's @Score (Mix (NoSE a))@. 
-- What does it mean? Let's look at the different parts of this type:
--
-- * @Score a@ - you can think of it as a container of some values of 
--   type @a@ (every value of type @a@ starts at some time and lasts 
--   for some time in seconds)
--
-- * @Mix a@ - is an output of Csound instrument it can be one or several 
--   signals ('Csound.Base.Sig' or 'Csound.Base.CsdTuple'). 
--
-- *NoSE a* - it's a tricky part of the output. 'NoSE' means literaly 'no SE'. 
-- It tells to the type checker that it can skip the 'Csound.Base.SE' wrapper
-- from the type 'a' so that @SE a@ becomes just @a@ or @SE (a, SE b, c)@ 
-- becomes @(a, b, c)@. Why should it be? I need 'SE' to deduce the order of the
-- instruments that have side effects. I need it within one instrument. But when 
-- instrument is rendered i no longer need 'SE' type. So 'NoSE' lets me drop it
-- from the output type. 
sco :: (Arg a, Out b, CsdSco f) => (a -> b) -> f a -> f (Mix (NoSE b))
sco instr notes = singleEvent $ Mix $ do    
    notes'  <- fmap toCsdEventList $ traverse renderNote notes
    instrId <- saveSourceInstrCached instr soundSourceExp
    return $ Snd instrId notes'

renderNote :: (Arg a) => a -> GE [Prim]
renderNote a = tfmNoteStrs =<< tfmNoteTabs (toNote a) 

tfmNoteTabs :: Note -> GE Note
tfmNoteTabs xs = do
    opt <- getOptions
    let xs' = defineNoteTabs (tabFi opt) xs
        tabs = getPrimTabs =<< xs'
    ids <- mapM saveTab tabs
    let tabMap = M.fromList $ zip tabs ids
    return $ substNoteTabs tabMap xs'

tfmNoteStrs :: Note -> GE Note
tfmNoteStrs xs = do    
    ids <- mapM saveStr strs
    let strMap = M.fromList $ zip strs ids
    return $ substNoteStrs strMap xs   
    where strs = getStrings xs


-- | Applies an effect to the sound. Effect is applied to the sound on the give track. 
--
-- > res = mix effect sco 
--
-- * @effect@ - a function that takes a tuple of signals and produces 
--   a tuple of signals.
--
-- * @sco@ - something that is constructed with 'Csound.Base.sco' or 
--   'Csound.Base.mix' or 'Csound.Base.midi'. 
--
-- With the function 'Csound.Base.mix' you can apply a reverb or adjust the 
-- level of the signal. It functions like a mixing board but unlike mixing 
-- board it produces the value that you can arrange with functions from the 
-- module "Temporal.Media". You can delay it mix with some other track and 
-- apply some another effect on top of it!
mix :: (Out a, Out b, CsdSco f) => (a -> b) -> f (Mix a) -> f (Mix (NoSE b))
mix eff sigs = singleEvent $ Mix $ do
    notes <- traverse unMix sigs
    instrId <- saveMixerInstr =<< effectExp eff
    return $ Eff instrId $ toCsdEventList notes 

{-
-- | Triggers a midi-instrument (like Csound's massign). The result type 
-- is a fake one. It's wrapped in the 'Csound.Base.Score' for the ease of mixing.
-- you can not delay or stretch it. The only operation that is meaningful 
-- for it is 'Temporal.Media.chord'. But you can add effects to it with 'Csound.Base.mix'!
midi :: (Out a) => Channel -> (Msg -> a) -> Score (Mix (NoSE a))
midi = genMidi Massign

-- | Triggers a - midi-instrument (like Csound's pgmassign). 
pgmidi :: (Out a) => Maybe Int -> Channel -> (Msg -> a) -> Score (Mix (NoSE a))
pgmidi mchn = genMidi (Pgmassign mchn)

genMidi :: (Out a) => MidiType -> Channel -> (Msg -> a) -> Score (Mix (NoSE a))
genMidi midiType chn f = temp $ Mid $ mkInstr getMidiArity Msg f (Just (midiType, chn))
    where getMidiArity = mkArity (const 0) outArity
-}

-- | Constructs the effect that applies a given function on every channel.
effect :: (CsdTuple a, Out a) => (Sig -> Sig) -> (a -> a)
effect f = toCsdTuple . fmap (toE . f . fromE) . fromCsdTuple

-- | Constructs the effect that applies a given function with side effect 
-- (it uses random opcodes or delays) on every channel.
effectS :: (CsdTuple a, Out a) => (Sig -> SE Sig) -> (a -> SE a)
effectS f a = fmap fromOut $ mapM f =<< toOut a

rescaleCsdEventListM :: CsdEventList M -> CsdEventList M
rescaleCsdEventListM es = 
    es { csdEventListNotes = fmap rescaleCsdEventM $ csdEventListNotes es }

rescaleCsdEventM :: CsdEvent M -> CsdEvent M
rescaleCsdEventM (start, dur, evt) = (start, dur, phi evt)
    where phi x = case x of
            Snd n evts -> Snd n $ rescaleCsdEventList dur evts
            Eff n evts -> Eff n $ rescaleCsdEventListM $ rescaleCsdEventList dur evts

rescaleCsdEventList :: Double -> CsdEventList a -> CsdEventList a
rescaleCsdEventList k (CsdEventList totalDur events) = 
    CsdEventList (k * totalDur) (fmap (rescaleCsdEvent k) events)

rescaleCsdEvent :: Double -> CsdEvent a -> CsdEvent a
rescaleCsdEvent k (start, dur, a) = (k * start, k * dur, a)



