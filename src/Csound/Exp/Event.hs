{-# Language TypeFamilies, FlexibleContexts #-}
module Csound.Exp.Event(
    Evt(..), Trig, Snap, Bam,
    boolToEvt, evtToBool, sigToEvt, filterEvt, accumEvt, snapshot,
    stepper, schedule, toggle
) where

import Data.Monoid
import Data.Boolean((==*))

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.Logic
import Csound.Exp.Tuple
import Csound.Exp.Arg
import Csound.Exp.GE
import Csound.Exp.SE
import Csound.Exp.Ref
import Csound.Exp.Instr

import Csound.Render.Channel(event, instrOn, instrOff)

type Bam a = a -> SE ()
type Trig = Evt ()

newtype Evt a = Evt { runEvt :: Bam a -> SE () }

-- | Converts booleans to events.
boolToEvt :: BoolSig -> Evt ()
boolToEvt cond = Evt $ \bam -> when cond $ bam ()

-- | Triggers an event when signal equals to 1.
sigToEvt :: Sig -> Evt ()
sigToEvt = boolToEvt . ( ==* 1) . kr

instance Functor Evt where
    fmap f evt = Evt $ \bam -> runEvt evt (bam . f)

instance Monoid (Evt a) where
    mempty = Evt $ const $ return ()
    mappend a b = Evt $ \bam -> runEvt a bam >> runEvt b bam

-- | Filters events with predicate.
filterEvt :: (a -> BoolD) -> Evt a -> Evt a
filterEvt cond evt = Evt $ \bam -> runEvt evt $ \a ->
    when (toBoolSig $ cond a) $ bam a

-- | Accumulator for events.
accumEvt :: (CsdTuple s) => s -> (a -> s -> (b, s)) -> Evt a -> Evt b
accumEvt s0 update evt = Evt $ \bam -> do
    (readSt, writeSt) <- sensorsSE s0
    runEvt evt $ \a -> do
        s1 <- readSt
        let (b, s2) = update a s1
        bam b
        writeSt s2

-- | Get values of some signal at the given events.
snapshot :: (CsdTuple a, CsdTuple (Snap a)) => (Snap a -> b -> c) -> a -> Evt b -> Evt c
snapshot f asig evt = Evt $ \bam -> runEvt evt $ \a -> 
    bam (f (readSnap asig) a)

toBoolSig :: BoolD -> BoolSig
toBoolSig (BoolD expr) = BoolSig expr

readSnap :: (CsdTuple (Snap a), CsdTuple a) => a -> Snap a
readSnap = toCsdTuple . fromCsdTuple

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
evtToBool evt = do
    var <- newLocalVar Kr (double 0)
    writeVar var (double 0)
    runEvt evt $ const $ writeVar var (double 1)
    asig <- readVar var
    return $ toBoolSig $ asig ==* (double 1)

-- | Converts events to signals.
stepper :: CsdTuple a => a -> Evt a -> SE a
stepper initVal evt = do
    (readSt, writeSt) <- sensorsSE initVal
    runEvt evt $ \a -> writeSt a
    readSt 

-- | Triggers an instrument with events of pairs @(duration, argument)@. 
schedule :: (CsdTuple a, Arg a, Out b, Out (NoSE b)) => (a -> b) -> Evt (D, a) -> GE (NoSE b)
schedule instr evt = do    
    (reader, writer) <- readOnlyRef
    instrId <- saveSourceInstr $ trigExp writer instr 
    _ <- saveAlwaysOnInstr $ scheduleInstr instrId evt
    return reader

scheduleInstr :: (CsdTuple a, Arg a) => InstrId -> Evt (D, a) -> E
scheduleInstr instrId evt = execSE $ 
    runEvt evt $ \(dt, a) -> do
        event instrId 0 dt a
  
-- | Triggers an instrument with first event stream and stops 
-- it when event happens on the second event stream.
toggle :: (CsdTuple a, Arg a, Out b, Out (NoSE b)) => (a -> b) -> Evt a -> Evt c -> GE (NoSE b)
toggle instr onEvt offEvt = do
    (reader, writer) <- readOnlyRef
    instrId <- saveSourceInstr $ trigExp writer instr 
    _ <- saveAlwaysOnInstr $ scheduleToggleInstr instrId onEvt offEvt
    return $ reader

scheduleToggleInstr :: (CsdTuple a, Arg a) => InstrId -> Evt a -> Evt c -> E
scheduleToggleInstr instrId onEvt offEvt = execSE $ do
    runEvt onEvt $ \a -> do
        instrOn instrId a 0
    cond <- evtToBool offEvt
    when cond $ do
        instrOff instrId
        
