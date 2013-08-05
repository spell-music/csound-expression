{-# Language TypeFamilies, FlexibleContexts, TupleSections #-}
module Csound.Exp.Event(
    Evt(..), Trig, Snap, Bam, readSnap,
    -- * core funs
    boolToEvt, evtToBool, sigToEvt, filterE, 
    accumE, accumSE, filterAccumSE, filterAccumE, snapshot, snaps,
    -- * instrument invocation
    stepper, schedule, scheduleUntil, 
    scheduleHold, autoOff    
) where

import Data.Monoid
import Data.Boolean

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.Logic
import Csound.Exp.Tuple
import Csound.Exp.Arg
import Csound.Exp.GE
import Csound.Exp.SE
import Csound.Exp.Ref
import Csound.Exp.Instr
import Csound.Exp.Numeric

import Csound.Render.Channel(event, instrOn, instrOff, ihold, turnoff, follow, changed)

type Bam a = a -> SE ()
type Trig = Evt ()

newtype Evt a = Evt { runEvt :: Bam a -> SE () }

-- | Converts booleans to events.
boolToEvt :: BoolSig -> Evt ()
boolToEvt b = Evt $ \bam -> when b $ bam ()

-- | Triggers an event when signal equals to 1.
sigToEvt :: Sig -> Evt ()
sigToEvt = boolToEvt . ( ==* 1) . kr

instance Functor Evt where
    fmap f evt = Evt $ \bam -> runEvt evt (bam . f)

instance Monoid (Evt a) where
    mempty = Evt $ const $ return ()
    mappend a b = Evt $ \bam -> runEvt a bam >> runEvt b bam

-- | Filters events with predicate.
filterE :: (a -> BoolD) -> Evt a -> Evt a
filterE pr evt = Evt $ \bam -> runEvt evt $ \a ->
    when (toBoolSig $ pr a) $ bam a

-- | Accumulator for events with side effects.
accumSE :: (CsdTuple s) => s -> (a -> s -> SE (b, s)) -> Evt a -> Evt b
accumSE s0 update evt = Evt $ \bam -> do
    (readSt, writeSt) <- sensorsSE s0
    runEvt evt $ \a -> do
        s1 <- readSt
        (b, s2) <- update a s1
        bam b
        writeSt s2

-- | Accumulator for events.
accumE :: (CsdTuple s) => s -> (a -> s -> (b, s)) -> Evt a -> Evt b
accumE s0 update = accumSE s0 (\a s -> return $ update a s)

-- | Accumulator for events with side effects and filtering. Event triggers
-- only if the first element in the tripplet is true.
filterAccumSE :: (CsdTuple s) => s -> (a -> s -> SE (BoolD, b, s)) -> Evt a -> Evt b
filterAccumSE s0 update evt = Evt $ \bam -> do
    (readSt, writeSt) <- sensorsSE s0
    runEvt evt $ \a -> do
        s1 <- readSt
        (isOn, b, s2) <- update a s1
        when (toBoolSig isOn) $ bam b
        writeSt s2

filterAccumE :: (CsdTuple s) => s -> (a -> s -> (BoolD, b, s)) -> Evt a -> Evt b
filterAccumE s0 update = filterAccumSE s0 $ \a s -> return $ update a s

-- | Get values of some signal at the given events.
snapshot :: (CsdTuple a, CsdTuple (Snap a)) => (Snap a -> b -> c) -> a -> Evt b -> Evt c
snapshot f asig evt = Evt $ \bam -> runEvt evt $ \a -> 
    bam (f (readSnap asig) a)

toBoolSig :: BoolD -> BoolSig
toBoolSig (BoolD expr) = BoolSig expr

readSnap :: (CsdTuple (Snap a), CsdTuple a) => a -> Snap a
readSnap = toCsdTuple . fromCsdTuple

snaps :: Sig -> Evt D
snaps asig = snapshot const asig trig
    where trig = sigToEvt $ changed [asig]

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
stepper v0 evt = do
    (readSt, writeSt) <- sensorsSE v0
    runEvt evt $ \a -> writeSt a
    readSt 

-- | Triggers an instrument and holds notes. Useful for instrument with automatic turnoff 
-- by amplitude (see @autoOff@). 
scheduleHold :: (CsdTuple a, Arg a, Out b, Out (NoSE b)) => (a -> b) -> Evt a -> GE (NoSE b)
scheduleHold instr evt = schedule instr $ fmap (-1, ) evt

autoOff :: (Out a) => a -> SE a
autoOff a = do
    ihold 
    b <- trig
    when b
        turnoff
    return a
    where 
        -- if for dt seconds amplitude is lower than eps 
        -- we should turn the instrument off
        trig = fmap (( <* eps) . kr . flip follow dt . l2) $ toOut a

        dt = 3
        eps = 1e-5

        -- square root norm
        l2 :: [Sig] -> Sig
        l2 xs = sqrt $ sum $ zipWith (*) xs xs 

-- | Triggers an instrument with events of pairs @(duration, argument)@. 
schedule :: (CsdTuple a, Arg a, Out b, Out (NoSE b)) => (a -> b) -> Evt (D, a) -> GE (NoSE b)
schedule instr evt = do    
    (reader, writer) <- appendRef
    instrId <- saveSourceInstr $ trigExp writer instr 
    saveAlwaysOnInstr $ scheduleInstr instrId evt
    return reader

scheduleInstr :: (CsdTuple a, Arg a) => InstrId -> Evt (D, a) -> E
scheduleInstr instrId evt = execSE $ 
    runEvt evt $ \(dt, a) -> do
        event instrId 0 dt a
  
-- | Triggers an instrument with first event stream and stops 
-- it when event happens on the second event stream.
scheduleUntil :: (CsdTuple a, Arg a, Out b, Out (NoSE b)) => (a -> b) -> Evt a -> Evt c -> GE (NoSE b)
scheduleUntil instr onEvt offEvt = do
    (reader, writer) <- appendRef
    instrId <- saveSourceInstr $ trigExp writer instr 
    saveAlwaysOnInstr $ scheduleToggleInstr instrId onEvt offEvt
    return $ reader

scheduleToggleInstr :: (CsdTuple a, Arg a) => InstrId -> Evt a -> Evt c -> E
scheduleToggleInstr instrId onEvt offEvt = execSE $ do
    runEvt onEvt $ \a -> do
        instrOn instrId a 0
    b <- evtToBool offEvt
    when b $ do
        instrOff instrId
     

{-
-- if-then-else with side effects -- consider this to optimize lists in events

guardedSE :: CsdTuple a => [(BoolSig, SE a)] -> SE a -> SE a
guardedSE as el = do
    (readTuple, writeTuple) <- newLocalDefCsdTuple
    let save x = writeTuple =<< x
    whens (fmap (second save) as) (save el)  
    readTuple

newLocalDefCsdTuple :: CsdTuple a => SE (SE a, a -> SE ())
newLocalDefCsdTuple = undefined 
-}
