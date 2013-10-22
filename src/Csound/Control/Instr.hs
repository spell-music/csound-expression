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
    sco_, mix_, mixBy, 

    -- * Midi
    Msg, Channel, midi, midin, pgmidi, ampCps,
    midi_, midin_, pgmidi_,
    -- ** Reading midi note parameters
    cpsmidi, ampmidi,

    -- * Evt            
    trig, sched, schedHarp,
    trig_, sched_, 
    trigBy, schedBy, schedHarpBy,
    withDur,

    -- * Overload
    -- | Converters to make it easier a construction of the instruments.
    Instr(..), MidiInstr(..), AmpInstr(..), CpsInstr(..)
) where

import Csound.Typed
import Csound.Typed.Opcode
import Csound.Control.Overload

--------------------------------------------------------------------------
-- midi

ampCps :: Msg -> (D, D)
ampCps msg = (ampmidi msg 1, cpsmidi msg)

-- | Sets the same duration for all events. It's useful with the functions @sched@, @schedBy@, @sched_@. 
--
-- > withDur dur events === fmap (\x -> (dur, x)) events
withDur :: D -> Evt a -> Evt (D, a)
withDur dt = fmap $ \x -> (dt, x) 

