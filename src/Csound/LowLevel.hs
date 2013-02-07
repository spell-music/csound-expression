-- | Functions to make your own opcodes.
-- You can find a lot of examples in source code (see directory @Csound/Opcode@)
module Csound.LowLevel(
    -- * Types    
    Val, Rate(..), Name, E, toE,

    -- * Types
    Sig, D, I, S, Tab, Spec, CsdTuple, SE, Msg,

    -- * Handy shortcuts
    i, k, a, x, s, f, is, ks, as,

    -- * Standard opcodes

    -- | Example:
    --
    -- > oscil :: Sig -> Sig -> Tab -> Sig 
    -- > oscil = opc3 "oscil" [
    -- >     (Ar, [Xr, Xr, Ir, Ir]),
    -- >     (Kr, [Kr, Kr, Ir, Ir])]

    Spec1, 
    opcs, opc0, opc1, opc2, opc3, opc4, opc5, opc6, opc7, opc8, opc9, opc10, opc11,

    -- * Multiple outputs
    -- | Examples:
    --
    -- > pan2 :: Sig -> Sig -> (Sig, Sig)
    -- > pan2 = mopc2 "pan2" ([Ar, Ar], [Ar, Xr, Ir])
    --
    -- When you don't want to specify precise number of outputs:
    --
    -- > soundin :: CsdTuple a => S -> a
    -- > soundin = mopc1 "soundin" (repeat Ar, Sr : replicate 4 Ir)

    Specs,
    mopcs, mopc0, mopc1, mopc2, mopc3, mopc4, mopc5, mopc6, mopc7,

    -- * Side effects

    -- | Examples:
    --
    -- > delayr :: D -> SE Sig
    -- > delayr a1 = se $ opc1 "delayr" [(Ar, [Ir])] a1
    -- > 
    -- > delayw :: Sig -> SE ()
    -- > delayw a1 = se_ $ opc1 "delayw" [(Xr, [Ar])] a1
    se, se_
) where

import Data.Fix

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
ks n = replicate n k 
as n = replicate n a  


