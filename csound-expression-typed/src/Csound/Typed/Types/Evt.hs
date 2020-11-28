{-# Language TypeFamilies, FlexibleContexts, CPP #-}
module Csound.Typed.Types.Evt(
    Evt(..), Bam, sync,
    boolToEvt, evtToBool, sigToEvt, stepper,
    filterE, filterSE, accumSE, accumE, filterAccumE, filterAccumSE,
    Snap, snapshot, snaps, readSnap
) where

import Data.Monoid
import Data.Default
import Data.Boolean

import Csound.Typed.Types.Prim
import Csound.Typed.Types.Tuple
import Csound.Typed.GlobalState
import Csound.Typed.Control.Ref

import qualified Csound.Typed.GlobalState.Opcodes as C

-- | A stream of events. We can convert a stream of events to
-- the procedure with the function @runEvt@. It waits for events
-- and invokes the given procedure when the event happens.
data Evt a = Evt { runEvt :: Bam a -> SE () }

-- | A procedure. Something that takes a value and suddenly bams with it.
type Bam a = a -> SE ()

instance Functor Evt where
    fmap f a = Evt $ \bam -> runEvt a (bam . f)

instance Default (Evt a) where
    def = Evt $ const $ return ()

#if MIN_VERSION_base(4,11,0)
instance Semigroup (Evt a) where
    (<>) = mappendEvt

instance Monoid (Evt a) where
    mempty  = def

#else

instance Monoid (Evt a) where
    mempty  = def
    mappend = mappendEvt

#endif

mappendEvt a b = Evt $ \bam -> runEvt a bam >> runEvt b bam

-- | Converts booleans to events.
boolToEvt :: BoolSig -> Evt Unit
boolToEvt b = Evt $ \bam -> when1 b $ bam unit

-- | Triggers an event when signal equals to 1.
sigToEvt :: Sig -> Evt Unit
sigToEvt = boolToEvt . ( ==* 1) . kr

-- | Filters events with predicate.
filterE :: (a -> BoolD) -> Evt a -> Evt a
filterE pr evt = Evt $ \bam -> runEvt evt $ \a ->
    when1 (boolSig $ pr a) $ bam a

-- | Filters events with effectful predicate.
filterSE :: (a -> SE BoolD) -> Evt a -> Evt a
filterSE mpr evt = Evt $ \bam -> runEvt evt $ \a -> do
    pr <- mpr a
    when1 (boolSig pr) $ bam a

-- | Accumulator for events with side effects.
accumSE :: (Tuple s) => s -> (a -> s -> SE (b, s)) -> Evt a -> Evt b
accumSE s0 update evt = Evt $ \bam -> do
    (readSt, writeSt) <- sensorsSE s0
    runEvt evt $ \a -> do
        s1 <- readSt
        (b, s2) <- update a s1
        bam b
        writeSt s2

-- | Accumulator for events.
accumE :: (Tuple s) => s -> (a -> s -> (b, s)) -> Evt a -> Evt b
accumE s0 update = accumSE s0 (\a s -> return $ update a s)

-- | Accumulator for events with side effects and filtering. Event triggers
-- only if the first element in the tripplet is true.
filterAccumSE :: (Tuple s) => s -> (a -> s -> SE (BoolD, b, s)) -> Evt a -> Evt b
filterAccumSE s0 update evt = Evt $ \bam -> do
    (readSt, writeSt) <- sensorsSE s0
    runEvt evt $ \a -> do
        s1 <- readSt
        (isOn, b, s2) <- update a s1
        when1 (boolSig isOn) $ bam b
        writeSt s2

-- | Accumulator with filtering. It can skip the events from the event stream.
-- If the third element of the triple equals to 1 then we should include the
-- event in the resulting stream. If the element equals to 0 we skip the event.
filterAccumE :: (Tuple s) => s -> (a -> s -> (BoolD, b, s)) -> Evt a -> Evt b
filterAccumE s0 update = filterAccumSE s0 $ \a s -> return $ update a s

-- | Get values of some signal at the given events.
snapshot :: (Tuple a, Tuple (Snap a)) => (Snap a -> b -> c) -> a -> Evt b -> Evt c
snapshot f asig evt = Evt $ \bam -> runEvt evt $ \a ->
    bam (f (readSnap asig) a)

readSnap :: (Tuple (Snap a), Tuple a) => a -> Snap a
readSnap = toTuple . fromTuple

-- | Constructs an event stream that contains values from the
-- given signal. Events happens only when the signal changes.
snaps :: Sig -> Evt D
snaps asig = snapshot const asig trigger
    where
        trigger = sigToEvt $ fromGE $ fmap C.changed $ toGE asig

-------------------------------------------------------------------
-- snap

-- | A snapshot of the signal. It converts a type of the signal to the
-- type of the value in the given moment. Instances:
--
--
-- > type instance Snap D   = D
-- > type instance Snap Str = Str
-- > type instance Snap Tab = Tab
-- >
-- > type instance Snap Sig = D
-- >
-- > type instance Snap (a, b) = (Snap a, Snap b)
-- > type instance Snap (a, b, c) = (Snap a, Snap b, Snap c)
-- > type instance Snap (a, b, c, d) = (Snap a, Snap b, Snap c, Snap d)
-- > type instance Snap (a, b, c, d, e) = (Snap a, Snap b, Snap c, Snap d, Snap e)
-- > type instance Snap (a, b, c, d, e, f) = (Snap a, Snap b, Snap c, Snap d, Snap e, Snap f)
type family Snap a :: *

type instance Snap D   = D
type instance Snap Str = Str
type instance Snap Tab = Tab

type instance Snap Sig = D

type instance Snap (a, b) = (Snap a, Snap b)
type instance Snap (a, b, c) = (Snap a, Snap b, Snap c)
type instance Snap (a, b, c, d) = (Snap a, Snap b, Snap c, Snap d)
type instance Snap (a, b, c, d, e) = (Snap a, Snap b, Snap c, Snap d, Snap e)
type instance Snap (a, b, c, d, e, f) = (Snap a, Snap b, Snap c, Snap d, Snap e, Snap f)

-- | Converts an event to boolean signal. It forgets
-- everything about the event values. Signal equals to one when
-- an event happens and zero otherwise.
evtToBool :: Evt a -> SE BoolSig
evtToBool evt = do
    var <- newRef (double 0)
    writeRef var (double 0)
    runEvt evt $ const $ writeRef var (double 1)
    asig <- readRef var
    return $ boolSig $ asig ==* (double 1)

-- | Converts events to signals.
stepper :: Tuple a => a -> Evt a -> SE a
stepper v0 evt = do
    ref <- newGlobalRef v0
    runEvt evt $ \a -> writeRef ref a
    readRef ref

-------------------------------------------------------------
-- synchronization

-- | Executes actions synchronized with global tempo (in Hz).
--
-- > runEvtSync tempoCps evt proc
sync :: (Default a, Tuple a) => Sig -> Evt a -> Evt a
sync dt evt = Evt $ \bam -> do
    refVal     <- newRef def
    refFire    <- newRef (0 :: D)

    runEvt evt $ \a -> do
        writeRef refVal  a
        writeRef refFire 1

    fire    <- readRef refFire
    when1 (metro dt  ==* 1 &&* sig fire ==* 1) $ do
        val <- readRef refVal
        bam val
        writeRef refFire 0
    where
        metro :: Sig -> Sig
        metro asig = fromGE $ fmap C.metro $ toGE asig



