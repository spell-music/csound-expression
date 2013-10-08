module Csound.Types(
    -- * Primitive types    
    Sig, D, Tab, Str, Spec,     
    BoolSig, BoolD, Val(..),

    -- ** Constructors
    double, int, str, idur,

    -- ** Converters
    ar, kr, ir, sig,
    
    -- ** Numeric functions
    quotSig, remSig, divSig, modSig, ceilSig, floorSig, roundSig, intSig, fracSig,
    quotD, remD, divD, modD, ceilD, floorD, roundD, intD, fracD,        
   
    -- ** Logic functions
    boolSig,        

    -- * Tuples
    Tuple(..), makeTupleMethods,
    
    -- * Instruments    

    -- | An instrument is a function that takes a tpule of csound values as an argument
    -- and returns a tuple of signals as an output. The type of the instrument is:
    --
    -- > (Arg a, Out b) => a -> b

    -- ** Arguments
    Arg(..), makeArgMethods,

    -- ** Outputs
    Out, mapOut, pureOut, bindOut, accumOut, traverseOut
) where

import Csound.Typed.Types

