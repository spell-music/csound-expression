-- | We can define a midi-instrument. Then we can trigger the instrument with a midi-keyboard.
module Csound.Control.Midi ( 
    Msg, midi, pgmidi, 
    
    -- * Reading midi note parameters
    cpsmidi, ampmidi, ampCps, pchbend, aftouch,

    -- * Midi-controls
    ctrl7      
) where

import Csound.Exp(Msg)
import Csound.Exp.Mix(midi, pgmidi)
import Csound.Exp.Wrapper
import Csound.LowLevel

-----------------------------------------------------
-- * MIDI

-- | Queries amplitude and frequency of the midi message.
ampCps :: Msg -> (D, D)
ampCps msg = (ampmidi msg, cpsmidi msg)

-- ** Opcodes For Use In MIDI-Triggered Instruments

-- | Get the note number of the current MIDI event, expressed in cycles-per-second. 
--
-- > icps cpsmidi
--
-- doc: <http://www.csounds.com/manual/html/cpsmidi.html>

cpsmidi :: Msg -> D
cpsmidi = const $ constOpc "cpsmidi"

-- | Get the velocity of the current MIDI event. iscal is always 1 (equals to 0dbfs).
--
-- > iamp ampmidi iscal [, ifn]
--
-- doc: <http://www.csounds.com/manual/html/ampmidi.html>
ampmidi :: Msg -> D
ampmidi = const $ constOpc "ampmidi 1"

-- | Get the current pitch-bend value for this channel. 
--
-- > kbend pchbend [imin] [, imax]
--
-- doc: <http://www.csounds.com/manual/html/pchbend.html>
pchbend :: Msg -> Sig 
pchbend = const $ constDiap "pchbend"

-- | Get the current after-touch value for this channel. 
--
-- > kaft aftouch [imin] [, imax]
--
-- doc: <http://www.csounds.com/manual/html/.html>
aftouch :: Msg -> Sig
aftouch = const $ constDiap "aftouch"

-- ** Opcodes For Use In All Instruments

-- | Allows a floating-point 7-bit MIDI signal scaled with a minimum and a maximum range. 
--
-- > idest ctrl7 ichan, ictlno, imin, imax [, ifn]
-- > kdest ctrl7 ichan, ictlno, kmin, kmax [, ifn]
-- > adest ctrl7 ichan, ictlno, kmin, kmax [, ifn] [, icutoff]
--
-- doc: <http://www.csounds.com/manual/html/ctrl7.html>
ctrl7 :: D -> D -> Sig -> Sig -> Sig
ctrl7 = opc4 "ctrl7" [
    (i, replicate 5 i),
    (k, [i, i, k, k, i]),
    (a, [i, i, k, k, i, i])]


constOpc :: Val a => Name -> a
constOpc name = opc0 name [(i, [])]

constDiap :: Val a => Name -> a
constDiap name = opc0 name [(k, [i, i])]

