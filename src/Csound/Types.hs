module Csound.Types(
    -- * Primitive types    
    Sig, D, Tab, Str, Spec, Wspec,    
    BoolSig, BoolD, Val(..), SigOrD,

    -- ** Constructors
    double, int, str, 
    
    -- ** Constants
    idur, getSampleRate, getControlRate, getBlockSize,

    -- ** Converters
    ar, kr, ir, sig,

    -- ** Init values
    withInits, withDs, withSigs, withTabs, 
    withD, withSig, withTab, withSeed,

    -- ** Numeric functions
    quot', rem', div', mod', ceil', floor', round', int', frac',        
   
    -- ** Logic functions
    boolSig, when1, whens, 

    -- ** Aliases 
    -- | Handy for functions that return tuples to specify the utput type
    --
    -- > (aleft, aright) = ar2 $ diskin2 "file.wav" 1
    -- 
    -- or
    --
    -- > asig = ar1 $ diskin2 "file.wav" 1
    Sig2, Sig4, Sig6, Sig8,
    ar1, ar2, ar4, ar6, ar8,

    -- * Tuples
    Tuple(..), makeTupleMethods, Unit, unit,
    -- *** Logic functions
    ifTuple, guardedTuple, caseTuple, 
    
    -- * Instruments    

    -- | An instrument is a function that takes a tpule of csound values as an argument
    -- and returns a tuple of signals as an output. The type of the instrument is:
    --
    -- > (Arg a, Out b) => a -> b

    -- ** Arguments
    Arg,
    -- *** Logic functions
    ifArg, guardedArg, caseArg,

    -- ** Outputs
    Sigs
) where

import Csound.Typed.Types

type Sig2 = (Sig, Sig)
type Sig4 = (Sig, Sig, Sig, Sig)
type Sig6 = (Sig, Sig, Sig, Sig, Sig, Sig)
type Sig8 = (Sig, Sig, Sig, Sig, Sig, Sig, Sig, Sig)

