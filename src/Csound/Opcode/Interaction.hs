-- | Realtime Interaction
module Csound.Opcode.Interaction (

    -----------------------------------------------------
    -- * MIDI

    -- ** Opcodes For Use In MIDI-Triggered Instruments
    cpsmidi, ampmidi, pchbend, aftouch,

    -- ** Opcodes For Use In All Instruments
    ctrl7,

    -----------------------------------------------------
    -- * Human Interfaces

    -- ** Keys
    sensekey,

    -- ** Mouse
    xyin    
) where

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.Cons

import Csound.Render.Sco(Msg)

i = Ir
k = Kr
a = Ar
x = Xr
s = Sr
f = Fr
is n = replicate n i 

-----------------------------------------------------
-- * MIDI

-- ** Opcodes For Use In MIDI-Triggered Instruments

-- | Get the note number of the current MIDI event, expressed in cycles-per-second. 
--
-- > icps cpsmidi
--
-- doc: <http://www.csounds.com/manual/html/cpsmidi.html>

cpsmidi :: Msg -> D
cpsmidi = const $ constOpc "cpsmidi"

-- | Get the velocity of the current MIDI event. 
--
-- > iamp ampmidi iscal [, ifn]
--
-- doc: <http://www.csounds.com/manual/html/ampmidi.html>
ampmidi :: Msg -> D
ampmidi = const $ constOpc "ampmidi"

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
ctrl7 :: I -> I -> Sig -> Sig -> Sig
ctrl7 = opc4 "ctrl7" [
    (i, replicate 5 i),
    (k, [i, i, k, k, i]),
    (a, [i, i, k, k, i, i])]


constOpc :: Val a => Name -> a
constOpc name = opc0 name [(i, [])]

constDiap :: Val a => Name -> a
constDiap name = opc0 name [(k, [i, i])]

-----------------------------------------------------
-- * Open Sound Control and Network

-- ** Open Sound Control

-- ** Remote Instruments

-- ** Network Audio

-----------------------------------------------------
-- * Human Interfaces

-- ** Widgets

-- ** Keys

-- | Returns the ASCII code of a key that has been pressed, or -1 if no key has been pressed. 
--
-- > kres[, kkeydown] sensekey
--
-- doc: <http://www.csounds.com/manual/html/sensekey.html>
sensekey :: (Sig, Sig)
sensekey = mopc0 "sensekey" ([k,k], [])

-- ** Mouse

-- | Sense the cursor position in an output window. When xyin is called the position of the mouse within 
-- the output window is used to reply to the request. This simple mechanism does mean that only one xyin 
-- can be used accurately at once. The position of the mouse is reported in the output window. 
--
-- > kx, ky xyin iprd, ixmin, ixmax, iymin, iymax [, ixinit] [, iyinit]
--
-- doc: <http://www.csounds.com/manual/html/xyin.html>
xyin :: D -> D -> D -> D -> D -> (Sig, Sig)
xyin = mopc5 "xyin" ([k,k], is 7)

-- ** WII

-- ** P5 Glove

