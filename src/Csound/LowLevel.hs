-- | Functions to make your own opcodes.
-- You can find a lot of examples in source code (see directory @Csound/Opcode@)
module Csound.LowLevel(
    -- * Types    
    Val, Rate(..), Name, E, 

    -- * Types
    Sig, D, I, S, Tab, Spec, CsdTuple, SE, Msg,

    -- * Handy shortcuts
    i, k, a, x, s, f, is, ks, as,

    -- * Standard opcodes

    -- | Example:
    --
    -- > oscil :: Sig -> Sig -> Tab -> Sig 
    -- > oscil = opc3 "oscil" [
    -- >     (a, [x, x, i, i]),
    -- >     (k, [k, k, i, i])]

    Spec1, 
    opcs, opc0, opc1, opc2, opc3, opc4, opc5, opc6, opc7, opc8, opc9, opc10, opc11,

    -- * Multiple outputs
    -- | Examples:
    --
    -- > pan2 :: Sig -> Sig -> (Sig, Sig)
    -- > pan2 = mopc2 "pan2" ([a, a], [a, x, i])
    --
    -- When you don't want to specify precise number of outputs:
    --
    -- > soundin :: CsdTuple a => S -> a
    -- > soundin = mopc1 "soundin" (repeat a, s : is 4)

    Specs,
    mopcs, mopc0, mopc1, mopc2, mopc3, mopc4, mopc5, mopc6, mopc7,

    -- * Side effects

    -- | Examples:
    --
    -- > delayr :: D -> SE Sig
    -- > delayr a1 = se $ opc1 "delayr" [(a, [i])] a1
    -- > 
    -- > delayw :: Sig -> SE ()
    -- > delayw a1 = se_ $ opc1 "delayw" [(x, [a])] a1
    --
    -- Functions that produce no values (procedures) should return value of the type "Xr".

    -- * When standard functions are not enough
    
    -- | Sometimes Csound opcodes take too many parameters. If you want to 
    -- use them, you can always use functions that are defined on lists ("opcs" or "mopcs").
    -- But in this case you have to convert all arguments to the same type "E":
    --
    -- For example:
    --
    -- > oscil :: Sig -> Sig -> Tab -> Sig 
    -- > oscil a1 a2 a3 = opcs "oscil" signature [toE a1, toE a2, toE a3]
    -- >    where signature = [
    -- >            (a, [x, x, i, i]),
    -- >            (k, [k, k, i, i])]
    
    toE,

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


