-- | Realtime Interaction
module Csound.Opcode.Interaction (

    -----------------------------------------------------
    -- * MIDI

    -- ** Opcodes For Use In MIDI-Triggered Instruments
    cpsmidi, ampmidi, pchbend, aftouch,

    -- ** Opcodes For Use In All Instruments
    ctrl7,

    -----------------------------------------------------
    -- * Open Sound Control and Network

    -- ** Open Sound Control

    -- ** Remote Instruments

    -- ** Network Audio

    -----------------------------------------------------
    -- * Human Interfaces

    -- ** Widgets

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

cpsmidi :: Msg -> D
cpsmidi = const $ constOpc "cpsmidi"

ampmidi :: Msg -> D
ampmidi = const $ constOpc "ampmidi"

pchbend :: Msg -> Sig 
pchbend = const $ constDiap "pchbend"

aftouch :: Msg -> Sig
aftouch = const $ constDiap "aftouch"

-- ** Opcodes For Use In All Instruments

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

-- kres[, kkeydown] sensekey
sensekey :: (Sig, Sig)
sensekey = mopc0 "sensekey" ([k,k], [])

-- ** Mouse

-- kx, ky xyin iprd, ixmin, ixmax, iymin, iymax [, ixinit] [, iyinit]
xyin :: D -> D -> D -> D -> D -> (Sig, Sig)
xyin = mopc5 "xyin" ([k,k], is 7)

-- ** WII

-- ** P5 Glove

