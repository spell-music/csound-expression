{-# Language TypeFamilies, FlexibleContexts, FlexibleInstances #-}
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
-- * By using midi-instruments (@Midi@-section).
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
    CsdSco(..), Mix, sco, mix, eff, CsdEventList(..), CsdEvent, 
    mixLoop, sco_, mix_, mixLoop_, mixBy, 

    -- * Midi
    Msg, Channel, midi, midin, pgmidi, ampCps,
    midi_, midin_, pgmidi_,
    monoMsg, holdMsg, monoMsgn, holdMsgn, pgmonoMsg, pgholdMsg,
    -- ** Reading midi note parameters
    cpsmidi, ampmidi,

    -- * Evt  

    -- ** Singlular
    trig, sched, schedHarp, schedUntil, schedToggle,
    trig_, sched_, schedUntil_, 
    trigBy, schedBy, schedHarpBy,
    withDur,

    -- ** Plural
    trigs, scheds, schedHarps, schedUntils,
    trigs_, scheds_, schedUntils_, 
    trigsBy, schedsBy, schedHarpsBy,
    withDurs,

    -- * Overload
    -- | Converters to make it easier a construction of the instruments.
    Outs(..), onArg, MidiInstr(..), AmpInstr(..), CpsInstr(..)
) where

import Data.Boolean

import Csound.Typed
import Csound.Typed.Opcode
import Csound.Control.Overload

import Csound.Control.Evt(metroE, repeatE, splitToggle)

--------------------------------------------------------------------------
-- midi

ampCps :: Msg -> (D, D)
ampCps msg = (ampmidi msg 1, cpsmidi msg)


-- | Mixes the scores and plays them in the loop.
mixLoop :: (CsdSco f, Sigs a) => f (Mix a) -> a
mixLoop a = sched instr $ withDur dur $ repeatE unit $ metroE $ sig $ 1 / dur
    where 
        notes = toCsdEventList a
        dur   = double $ csdEventListDur notes

        instr _ = return $ mix notes

-- | Mixes the procedures and plays them in the loop.
mixLoop_ :: (CsdSco f) => f (Mix Unit) -> SE ()
mixLoop_ a = sched_ instr $ withDur dur $ repeatE unit $ metroE $ sig $ 1 / dur
    where 
        notes = toCsdEventList a
        dur   = double $ csdEventListDur notes

        instr _ = mix_ notes


-- | Invokes an instrument with first event stream and 
-- holds the note until the second event stream is active.
schedUntils :: (Arg a, Sigs b) => (a -> SE b) -> Evt [a] -> Evt c -> b
schedUntils instr onEvt offEvt = scheds instr' $ withDurs (-1) onEvt
    where 
        instr' x = do 
            res <- instr x
            runEvt offEvt $ const $ turnoff
            return res

-- | Invokes an instrument with toggle event stream (1 stands for on and 0 stands for off).
schedToggle :: (Sigs b) => SE b -> Evt D -> b
schedToggle res evt = schedUntil instr on off
    where 
        instr = const res
        (on, off) = splitToggle evt

-- | Invokes an instrument with first event stream and 
-- holds the note until the second event stream is active.
schedUntils_ :: (Arg a) => (a -> SE ()) -> Evt [a] -> Evt c -> SE ()
schedUntils_ instr onEvt offEvt = scheds_ instr' $ withDurs (-1) onEvt
    where 
        instr' x = do 
            res <- instr x
            runEvt offEvt $ const $ turnoff
            return res

-- | Sets the same duration for all events. It's useful with the functions @scheds@, @schedsBy@, @scheds_@. 
withDurs :: D -> Evt [a] -> Evt [(D, a)]
withDurs dt = fmap $ fmap $ \x -> (dt, x) 

-------------------------------------------------------------------------
-------------------------------------------------------------------------
-- singular

-- | Sets the same duration for all events. It's useful with the functions @sched@, @schedBy@, @sched_@. 
withDur :: D -> Evt a -> Evt (D, a)
withDur dt = fmap $ \x -> (dt, x) 

-------------------------------------------------------------------------
-- sinlgular case for event triggers

fromPlural :: (Evt [a] -> b) -> (Evt a -> b)
fromPlural f = f . fmap return

fromPluralBy :: ((c -> Evt [a]) -> c -> b) -> ((c -> Evt a) -> c -> b)
fromPluralBy f instr c = f (fmap return . instr) c

-- | Triggers an instrument with an event stream. The event stream
-- contains triples:
--
-- > (delay_after_event_is_fired, duration_of_the_event, argument_for_the_instrument)
trig :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, D, a) -> b
trig f = fromPlural $ trigs f

-- | It's like the function @trig@, but delay is set to zero.
sched :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, a) -> b
sched f = fromPlural $ scheds f

-- | An instrument is triggered with event stream and delay time is set to zero 
-- (event fires immediately) and duration is set to inifinite time. The note is 
-- held while the instrument is producing something. If the instrument is silent
-- for some seconds (specified in the first argument) then it's turned off.
schedHarp :: (Arg a, Sigs b) => D -> (a -> SE b) -> Evt a -> b
schedHarp dt f = fromPlural $ schedHarps dt f

-- | Invokes an instrument with first event stream and 
-- holds the note until the second event stream is active.
schedUntil :: (Arg a, Sigs b) => (a -> SE b) -> Evt a -> Evt c -> b
schedUntil f eOn eOff = schedUntils f (fmap return eOn) eOff

-- | Triggers a procedure on the event stream.
trig_ :: Arg a => (a -> SE ()) -> Evt (D, D, a) -> SE ()
trig_ f = fromPlural $ trigs_ f

-- | Triggers a procedure on the event stream. A delay time is set to zero.
sched_ :: Arg a => (a -> SE ()) -> Evt (D, a) -> SE ()
sched_ f = fromPlural $ scheds_ f

-- | Invokes an instrument with first event stream and 
-- holds the note until the second event stream is active.
schedUntil_ :: Arg a => (a -> SE ()) -> Evt a -> Evt c -> SE ()
schedUntil_ f eOn eOff = schedUntils_ f (fmap return eOn) eOff

-- | A closure to trigger an instrument inside the body of another instrument.
trigBy :: (Arg a, Sigs b, Arg c) => (a -> SE b) -> (c -> Evt (D, D, a)) -> c -> b
trigBy f = fromPluralBy $ trigsBy f

-- | A closure to trigger an instrument inside the body of another instrument.
schedBy :: (Arg a, Sigs b, Arg c) => (a -> SE b) -> (c -> Evt (D, a)) -> c -> b
schedBy f = fromPluralBy $ schedsBy f

-- | A closure to trigger an instrument inside the body of another instrument.
schedHarpBy :: (Arg a, Sigs b, Arg c) => D -> (a -> SE b) -> (c -> Evt a) -> c -> b
schedHarpBy dt f = fromPluralBy $ schedHarpsBy dt f

-----------------------------------------------------------------------
-- Midi addons

-- mono midi

-- | Produces midi amplitude and frequency as a signal.
-- The signal fades out when nothing is pressed.
-- It can be used in mono-synths. Arguments are portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > monoMsg portamentoTime releaseTime
monoMsg :: D -> D -> SE (Sig, Sig)
monoMsg portTime relTime = do
	(amp, cps, status) <- genAmpCpsSig midi
	return (port amp portTime * port status relTime,  port cps portTime)

-- | Produces midi amplitude and frequency as a signal and holds the 
-- last value till the next one is present.
-- It can be used in mono-synths. Arguments are portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > holdMsg portamentoTime
holdMsg :: D -> SE (Sig, Sig)
holdMsg portTime = do
	(amp, cps) <- genHoldAmpCpsSig midi_
	return (port amp portTime,  port cps portTime)


-- | Produces midi amplitude and frequency as a signal.
-- The signal fades out when nothing is pressed. We can specify a channel.
-- It can be used in mono-synths. Arguments are portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > monoMsgn chnNumber portamentoTime releaseTime
monoMsgn :: Channel -> D -> D -> SE (Sig, Sig)
monoMsgn n portTime relTime = do
	(amp, cps, status) <- genAmpCpsSig (midin n)
	return (port amp portTime * port status relTime,  port cps portTime)

-- | Produces midi amplitude and frequency as a signal and holds the 
-- last value till the next one is present. We can specify a channel.
-- It can be used in mono-synths. Arguments are portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > holdMsgn chnNumber portamentoTime
holdMsgn :: Channel -> D -> SE (Sig, Sig)
holdMsgn n portTime = do
	(amp, cps) <- genHoldAmpCpsSig (midin_ n)
	return (port amp portTime,  port cps portTime)


-- | Produces midi amplitude and frequency as a signal.
-- The signal fades out when nothing is pressed. We can specify a programm number and channel.
-- It can be used in mono-synths. Arguments are portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > pgmonoMsg chnNumber portamentoTime releaseTime
pgmonoMsg :: Maybe Int -> Channel -> D -> D -> SE (Sig, Sig)
pgmonoMsg pg n portTime relTime = do
	(amp, cps, status) <- genAmpCpsSig (pgmidi pg n)
	return (port amp portTime * port status relTime,  port cps portTime)

-- | Produces midi amplitude and frequency as a signal and holds the 
-- last value till the next one is present. We can specify a programm number and channel.
-- It can be used in mono-synths. Arguments are portamento time
-- and release time. A portamento time is time it takes for transition
-- from one note to another.
--
-- > pgholdMsg portamentoTime
pgholdMsg :: Maybe Int -> Channel -> D -> SE (Sig, Sig)
pgholdMsg pg n portTime = do
	(amp, cps) <- genHoldAmpCpsSig (pgmidi_ pg n)
	return (port amp portTime,  port cps portTime)


genAmpCpsSig :: ((Msg -> SE Sig) -> Sig) -> SE (Sig, Sig, Sig)
genAmpCpsSig midiFun = do
	ref <- newGlobalSERef ((0, 0) :: (Sig, Sig))
	let status = midiFun (instr ref)
	let resStatus = ifB (downsamp status ==* 0) 0 1
	(amp, cps) <- readSERef ref
	return (downsamp amp, downsamp cps, resStatus)
	where 
		instr :: SERef (Sig, Sig) -> Msg -> SE Sig
		instr hNote msg = do
			writeSERef hNote (sig $ ampmidi msg 1, sig $ cpsmidi msg)
			return 1		

genHoldAmpCpsSig :: ((Msg -> SE ()) -> SE ()) -> SE (Sig, Sig)
genHoldAmpCpsSig midiFun = do
	ref <- newGlobalSERef ((0, 0) :: (Sig, Sig))
	midiFun (instr ref)	
	(amp, cps) <- readSERef ref
	return (downsamp amp, downsamp cps)
	where 
		instr :: SERef (Sig, Sig) -> Msg -> SE ()
		instr hNote msg = do
			writeSERef hNote (sig $ ampmidi msg 1, sig $ cpsmidi msg)			


