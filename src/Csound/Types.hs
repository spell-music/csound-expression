module Csound.Types(
    -- * Primitive types    
    Sig, D, Tab, Str, Spec,     
    BoolSig, BoolD, Val(..), SigOrD,

    -- ** Constructors
    double, int, str, idur,

    -- ** Converters
    ar, kr, ir, sig,
    
    -- ** Numeric functions
    quot', rem', div', mod', ceil', floor', round', int', frac',        
   
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

