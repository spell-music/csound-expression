{-# Language FlexibleContexts, TupleSections #-}
    module Csound.Exp.Mix(
    -- * Container for sounds (triggered with notes and mixers)
    Mix(..), M(..), runMix,

    effect, effectS,
    sco, mix, midi, pgmidi,

    rescaleCsdEventListM
) where

import Data.Traversable(traverse)
import qualified Data.Map    as M
import Control.Monad.Trans.Writer
import qualified Data.IntMap as IM

import Csound.Tfm.Tab

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.SE
import Csound.Exp.GE
import Csound.Exp.Instr
import Csound.Exp.Arg
import Csound.Exp.Tuple(Out(..), CsdTuple, fromCsdTuple, toCsdTuple)
import Csound.Exp.Options
import Csound.Exp.EventList
import Csound.Exp.Ref

newtype Mix a = Mix { unMix :: GE M } 

data M 
    = Snd InstrId (CsdEventList Note)
    | Eff InstrId (CsdEventList M)    

wrapSco :: (CsdSco f) => f a -> (CsdEventList a -> GE M) -> f (Mix b)
wrapSco notes getContent = singleCsdEvent (0, csdEventListDur evts, Mix $ getContent evts)
    where evts = toCsdEventList notes

-- | Play a bunch of notes with the given instrument.
--
-- > res = sco instrument scores 
--
-- * @instrument@ is a function that takes notes and produces a 
--   tuple of signals (maybe with some side effect)
--  
-- * @scores@ are some notes (see the type class 'Csound.Base.CsdSco')
--
-- Let's try to understand the type of the output. It's @CsdSco f => f (Mix (NoSE a))@. 
-- What does it mean? Let's look at the different parts of this type:
--
-- * @CsdSco f => f a@ - you can think of it as a container of some values of 
--   type @a@ (every value of type @a@ starts at some time and lasts 
--   for some time in seconds)
--
-- * @Mix a@ - is an output of Csound instrument it can be one or several 
--   signals ('Csound.Base.Sig' or 'Csound.Base.CsdTuple'). 
--
-- *NoSE a* - it's a tricky part of the output. 'NoSE' means literaly 'no SE'. 
-- It tells to the type checker that it can skip the 'Csound.Base.SE' wrapper
-- from the type 'a' so that @SE a@ becomes just @a@ or @SE (a, SE b, c)@ 
-- becomes @(a, b, c)@. Why should it be? We need 'SE' to deduce the order of the
-- opcodes that have side effects. We need it within one instrument. But when 
-- instrument is rendered we no longer need 'SE' type. So 'NoSE' lets me drop it
-- from the output type. 
sco :: (Arg a, Out b, CsdSco f) => (a -> b) -> f a -> f (Mix (NoSE b))
sco instr notes = wrapSco notes $ \events -> do    
    events'  <- traverse renderNote events
    instrId <- saveSourceInstrCached instr (return . soundSourceExp)
    return $ Snd instrId events'

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
-- > res = mix effect sco rriedSingleC
--
-- * @effect@ - a function that takes a tuple of signals and produces 
--   a tuple of signals.
--
-- * @sco@ - something that is constructed with 'Csound.Base.sco' or 
--   'Csound.Base.mix'. 
--
-- With the function 'Csound.Base.mix' you can apply a reverb or adjust the 
-- level of the signal. It functions like a mixing board but unlike mixing 
-- board it produces the value that you can arrange with functions from your
-- favorite Score-generation library. You can delay it or mix with some other track and 
-- apply some another effect on top of it!
mix :: (Out a, Out b, CsdSco f) => (a -> b) -> f (Mix a) -> f (Mix (NoSE b))
mix eff sigs = wrapSco sigs $ \events -> do
    notes <- traverse unMix events
    instrId <- saveMixerInstr $ effectExp eff
    return $ Eff instrId notes 

-- | Triggers a midi-instrument (aka Csound's massign). 
midi :: (Out a, Out (NoSE a)) => Channel -> (Msg -> a) -> GE (NoSE a)
midi = genMidi Massign

-- | Triggers a - midi-instrument (aka Csound's pgmassign). 
pgmidi :: (Out a, Out (NoSE a)) => Maybe Int -> Channel -> (Msg -> a) -> GE (NoSE a)
pgmidi mchn = genMidi (Pgmassign mchn)

genMidi :: (Out a) => MidiType -> Channel -> (Msg -> a) -> GE (NoSE a)
genMidi midiType chn instr = do
    setDurationToInfinite
    (reader, expr) <- mkAppendSink $ instr Msg
    instrId <- saveSourceInstr expr
    saveMidi $ MidiAssign midiType chn instrId
    return reader

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
            Snd n evts -> Snd n $ rescaleCsdEventList (dur/localDur) evts
            Eff n evts -> Eff n $ rescaleCsdEventListM $ rescaleCsdEventList (dur/localDur) evts            
            where localDur = case x of
                    Snd _ evts -> csdEventListDur evts
                    Eff _ evts -> csdEventListDur evts

-----------------------------------------------------------------------------------
-- render scores

runMix :: (Out (NoSE a), Out a, CsdSco f) => f (Mix a) -> GE (NoSE a)
runMix sigs = do    
    saveDuration (csdEventListDur events)
    (readRef, writeRef) <- readOnlyRef
    notes <- traverse unMix events
    instrId <- saveMixerInstr $ effectExp writeRef
    let notes' = rescaleCsdEventListM $ toCsdEventList notes 
    saveMixerNotes $ toLowLevelNotesMap $ Eff instrId notes'
    saveAlwaysOnNote instrId
    return readRef
    where events = toCsdEventList sigs

toLowLevelNotesMap :: M -> IM.IntMap LowLevelSco
toLowLevelNotesMap mixNotes = IM.fromList $ execWriter $ phi mixNotes
    where    
        phi :: M -> Writer [(Int, LowLevelSco)] ()
        phi x = case x of
            Eff instrId notes -> 
                let (instrNotes, rest) = onEff notes
                in  tell [(instrIdCeil instrId, instrNotes)] >> mapM_ phi rest
            Snd _ _ -> error "Render.hs:toLowLevelNotesMap no effect instrument, end up in Snd case"

onEff :: CsdEventList M -> (LowLevelSco, [M])
onEff (CsdEventList _ events) = execWriter $ mapM_ phi events
    where phi :: CsdEvent M -> Writer (LowLevelSco, [M]) ()
          phi (start, dur, content) = case content of
            Snd instrId notes -> tellFst $ fmap (instrId, ) $ csdEventListNotes $ delayCsdEventList start notes
            Eff instrId _     -> tell ([(instrId, (start, dur, []))], [content])
          tellFst x = tell (x, [])

