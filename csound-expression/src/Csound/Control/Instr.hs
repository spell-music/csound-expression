{-# Language TypeFamilies, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}
-- | We can convert notes to sound signals with instruments.
-- An instrument is a function:
--
-- > (Arg a, Sigs b) => a -> SE b
--
-- It takes a tuple of primitive Csound values (number, string or array) and converts
-- it to the tuple of signals and it makes some side effects along the way so
-- the output is wrapped in the 'Csound.Base.SE'-monad.
--
-- There are only three ways of making a sound with an instrument:
--
-- * Suplpy an instrument with notes (@Mix@-section).
--
-- * Trigger an instrument with event stream (@Evt@-section).
--
-- * By using midi-instruments (see @Csound.Control.Midi@).
--
-- Sometimes we don't want to produce any sound. Our instrument is just
-- a procedure that makes something useful without being noisy about it.
-- It's type is:
--
-- > (Arg a) => a -> SE ()
--
-- To invoke the procedures there are functions with trailing underscore.
-- For example we have the function @trig@ to convert event stream to sound:
--
-- > trig :: (Arg a, Sigs b) => (a -> SE b) -> Evts (D, D, a) -> b
--
-- and we have a @trig@ with underscore to convert the event stream to
-- the sequence of the procedure invkations:
--
-- > trig_ :: (Arg a) => (a -> SE ()) -> Evts (D, D, a) -> SE ()
--
-- To invoke instruments from another instrumetnts we use artificial closures
-- made with functions with trailing xxxBy. For example:
--
-- > trigBy :: (Arg a, Arg c, Sigs b) => (a -> SE b) -> (c -> Evts (D, D, a)) -> (c -> b)
--
-- Notice that the event stream depends on the argument of the type c. Here goes
-- all the parameters that we want to pass from the outer instrument. Unfortunately
-- we can not just create the closure, because our values are not the real values.
-- It's a text of the programm (a tiny snippet of it) to be executed. For a time being
-- I don't know how to make it better. So we need to pass the values explicitly.
--
-- For example, if we want to make an arpeggiator:
--
-- > pureTone :: D -> SE Sig
-- > pureTone cps = return $ mul env $ osc $ sig cps
-- >    where env = linseg [0, 0.01, 1, 0.25, 0]
-- >
-- > majArpeggio :: D -> SE Sig
-- > majArpeggio = return . schedBy pureTone evts
-- >     where evts cps = withDur 0.5 $ fmap (* cps) $ cycleE [1, 5/3, 3/2, 2] $ metroE 5
-- >
-- > main = dac $ mul 0.5 $ midi $ onMsg majArpeggio
--
-- We should use 'Csound.Base.schedBy' to pass the frequency as a parameter to the event stream.
module Csound.Control.Instr(
    -- * Mix
    -- | We can invoke instrument with specified notes.
    -- Eqch note happens at some time and lasts for some time. It contains
    -- the argument for the instrument.
    --
    -- We can invoke the instrument on the sequence of notes (@sco@), process
    -- the sequence of notes with an effect (@eff@) and convert everything in
    -- the plain sound signals (to send it to speakers or write to file or
    -- use it in some another instrument).
    --
    -- The sequence of notes is represented with type class @CsdSco@. Wich
    -- has a very simple methods. So you can use your own favorite library
    -- to describe the list of notes. If your type supports the scaling in
    -- the time domain (stretching the timeline) you can do it in the Mix-version
    -- (after the invokation of the instrument). All notes are rescaled all the
    -- way down the Score-structure.
    Sco, Mix, sco, mix, eff, monoSco,
    mixLoop, sco_, mix_, mixLoop_, mixBy,
    infiniteDur,

    module Temporal.Media,

    -- * Evt

    sched, retrig, schedHarp, schedUntil, schedToggle,
    sched_, schedUntil_,
    schedBy, schedHarpBy,
    withDur, monoSched,

    -- * Api
    -- | We can create named instruments. then we can trigger the named instruments with Csound API.
    -- Csound can be used not as a text to audio converter but also as a shared C-library. There are
    -- many bindings to many languages. For example we can use Python or Android SDK to create UI
    -- and under the hood we can use the audio engine created with Haskell. The concept of named instruments
    -- is the bridge for other lnguages to use our haskell-generated code.
    trigByName, trigByName_,
    trigByNameMidi, trigByNameMidi_,
    turnoffByName,

    -- ** Misc
    alwaysOn, playWhen,

    -- * Overload
    -- | Converters to make it easier a construction of the instruments.
    Outs(..), onArg, AmpInstr(..), CpsInstr(..),

    -- * Imperative instruments
    InstrRef, newInstr, scheduleEvent, turnoff2, negateInstrRef, addFracInstrRef,
    newOutInstr, noteOn, noteOff
) where

import Control.Monad.Trans.Class
import Csound.Dynamic hiding (str, Sco(..), when1, alwaysOn)

import Csound.Typed
import Csound.Typed.Opcode hiding (initc7, metro)
import Csound.Control.Overload
import Temporal.Media(Event(..), mapEvents, temp, str, dur)

import Csound.Control.Evt(metro, repeatE, splitToggle, loadbang)

-- | Mixes the scores and plays them in the loop.
mixLoop :: (Sigs a) => Sco (Mix a) -> a
mixLoop a = sched instr $ withDur dt $ repeatE unit $ metro $ 1 / dt
    where
        dt = dur a
        instr _ = return $ mix a

-- | Mixes the procedures and plays them in the loop.
mixLoop_ :: Sco (Mix Unit) -> SE ()
mixLoop_ a = sched_ instr $ withDur dt $ repeatE unit $ metro $ 1 / dt
    where
        dt = dur a
        instr _ = mix_ a


-- | Invokes an instrument with first event stream and
-- holds the note until the second event stream is active.
schedUntil :: (Arg a, Sigs b) => (a -> SE b) -> Evt a -> Evt c -> b
schedUntil instr onEvt offEvt = sched instr' $ withDur infiniteDur onEvt
    where
        instr' x = do
            res <- instr x
            runEvt offEvt $ const $ turnoff
            return res

-- | Invokes an instrument with toggle event stream (1 stands for on and 0 stands for off).
schedToggle :: (Sigs b) => SE b -> Evt D -> b
schedToggle res evt = schedUntil instr ons offs
    where
        instr = const res
        (ons, offs) = splitToggle evt

-- | Invokes an instrument with first event stream and
-- holds the note until the second event stream is active.
schedUntil_ :: (Arg a) => (a -> SE ()) -> Evt a -> Evt c -> SE ()
schedUntil_ instr onEvt offEvt = sched_ instr' $ withDur infiniteDur onEvt
    where
        instr' x = do
            res <- instr x
            runEvt offEvt $ const $ turnoff
            return res

-- | Transforms an instrument from always on to conditional one.
-- The routput instrument plays only when condition is true otherwise
-- it produces silence.
playWhen :: forall a b. Sigs a => BoolSig -> (b -> SE a) -> (b -> SE a)
playWhen onSig instr msg = do
    ref <- newRef (0 :: a)
    writeRef ref 0
    when1 onSig $ writeRef ref =<< instr msg
    readRef ref

-------------------------------------------------------------------------
-------------------------------------------------------------------------
-- singular

-- | Sets the same duration for all events. It's useful with the functions @sched@, @schedBy@, @sched_@.
withDur :: Sig -> Evt a -> Evt (Sco a)
withDur dt = fmap (str dt . temp)

retrig :: (Arg a, Sigs b) => (a -> SE b) -> Evt a -> b
retrig f = retrigs f . fmap return

-- | Executes some procedure for the whole lifespan of the program,
alwaysOn :: SE () -> SE ()
alwaysOn proc = sched_ (const $ proc) $ withDur (infiniteDur) $ loadbang

--------------------------------------------------------------

-- | Turns off named instruments.
--
-- > turnoffNamedInstr name kmode krelease
--
-- name of the instrument (should be defined with @trigByName@ or smth like that).
--
-- kmode -- sum of the following values:
--
-- 0, 1, or 2: turn off all instances (0), oldest only (1), or newest only (2)
--
-- 4: only turn off notes with exactly matching (fractional) instrument number, rather than ignoring fractional part
--
-- 8: only turn off notes with indefinite duration (p3 < 0 or MIDI)
--
-- krelease -- if non-zero, the turned off instances are allowed to release, otherwise are deactivated immediately (possibly resulting in clicks)
turnoffByName :: String -> Sig -> Sig -> SE ()
turnoffByName name kmode krelease = strTurnoff2 (text name) kmode krelease

strTurnoff2 ::  Str -> Sig -> Sig -> SE ()
strTurnoff2 b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unStr b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "turnoff2" [(Xr,[Sr,Kr,Kr])] [a1,a2,a3]
