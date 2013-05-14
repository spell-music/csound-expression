{-# Language TypeFamilies, FlexibleContexts #-}
module Csound.Exp.Event(
    Evt(..), Trig, Snap,
    trigger, filterEvt, accumEvt, snapshot,
    stepper, schedule, toggle
) where

import Data.Monoid
import Temporal.Music.Score(Score)

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.Logic
import Csound.Exp.Tuple
import Csound.Exp.Arg
import Csound.Exp.Mix
import Csound.Exp.Logic
import Csound.Exp.GE
import Csound.Exp.GERef
import Csound.Exp.SE
import Csound.Exp.SERef
import Csound.Exp.Instr

import Csound.Render.Channel(event, instrOn, instrOff)

type Bam a = a -> SE ()
type Trig = Evt ()

newtype Evt a = Evt { runEvt :: Bam a -> SE () }

trigger :: BoolSig -> Evt ()
trigger cond = Evt $ \bam -> when cond $ bam ()

instance Functor Evt where
    fmap f evt = Evt $ \bam -> runEvt evt (bam . f)

instance Monoid (Evt a) where
    mempty = Evt $ const $ return ()
    mappend a b = Evt $ \bam -> runEvt a bam >> runEvt b bam

filterEvt :: (a -> BoolD) -> Evt a -> Evt a
filterEvt pred evt = Evt $ \bam -> runEvt evt $ \a ->
    when (toBoolSig $ pred a) $ bam a

accumEvt :: (CsdTuple s) => s -> (a -> s -> (b, s)) -> Evt a -> Evt b
accumEvt s0 update evt = Evt $ \bam -> do
    (readSt, writeSt) <- sensorsSE s0
    runEvt evt $ \a -> do
        s1 <- readSt
        let (b, s2) = update a s1
        bam b

snapshot :: (CsdTuple a) => (Snap a -> b -> c) -> a -> Evt b -> Evt c
snapshot f sig evt = Evt $ \bam -> runEvt evt $ \a -> 
    bam (f (readSnap sig) a)

toBoolSig :: BoolD -> BoolSig
toBoolSig = undefined

initCsdTuple :: CsdTuple s => s -> SE (SE s, s -> SE ())
initCsdTuple s0 = undefined

readSnap :: CsdTuple a => a -> Snap a
readSnap = undefined

-------------------------------------------------------------------
-- snap 

type family Snap a :: *

type instance Snap D   = D
type instance Snap Str = Str
type instance Snap Tab = Tab

type instance Snap Sig = D

type instance Snap (a, b) = (Snap a, Snap b)
type instance Snap (a, b, c) = (Snap a, Snap b, Snap c)
type instance Snap (a, b, c, d) = (Snap a, Snap b, Snap c, Snap d)

--------------------------------------------------
--

evtToBool :: Evt a -> SE BoolSig
evtToBool = undefined

stepper :: CsdTuple a => a -> Evt a -> SE a
stepper initVal evt = do
    (read, write) <- sensorsSE initVal
    runEvt evt $ \a -> write a
    read 

schedule :: (Arg a, Out b, Out (NoSE b)) => (a -> b) -> Evt (D, a) -> GE (SE (NoSE b))
schedule instr evt = do    
    ref <- newGERef defCsdTuple
    instrId <- saveSourceInstr =<< trigExp (writeGERef ref) instr 
    saveAlwaysOnInstr $ scheduleInstr (writeGERef ref) instrId evt
    return $ readGERef ref

scheduleInstr :: (Arg a, Out b) => (b -> SE ()) -> InstrId -> Evt (D, a) -> E
scheduleInstr write instrId evt = execSE $ 
    runEvt evt $ \(dt, a) -> do
        event instrId 0 dt a
  
toggle :: (Arg a, Out b, Out (NoSE b)) => (a -> b) -> Evt a -> Evt c -> GE (SE (NoSE b))
toggle instr onEvt offEvt = do
    ref <- newGERef defCsdTuple
    instrId <- saveSourceInstr =<< trigExp (writeGERef ref) instr 
    saveAlwaysOnInstr $ scheduleToggleInstr (writeGERef ref) instrId onEvt offEvt
    return $ readGERef ref

scheduleToggleInstr :: (Arg a, Out b) => (b -> SE ()) -> InstrId -> Evt a -> Evt c -> E
scheduleToggleInstr write instrId onEvt offEvt = execSE $ do
    runEvt onEvt $ \a -> do
        instrOn instrId a 0
    cond <- evtToBool offEvt
    when cond $ do
        instrOff instrId
        
        

    



