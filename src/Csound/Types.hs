module Csound.Types (
    Val,
    
    -- * Constants     
    -- | A constant value doesn't change while instrument is playing a note.
    -- Only constants can be passed as arguments to the instruments.
    D, Str,
    withInits,
    
    -- * Tables
    -- | In Csound tables can be treated as primitive values. They can be passed to instruments in the score events.
    -- There are limited set of functions which you can use to make new tables. Look at the following module for details:
    module Csound.Tab,
    
    -- * Signals
    -- | Signals can be audio or control rate. Rate is derived from the code.
    -- If there are rate-collisions, values will be converted to the right rates.    
    -- For example, if you are trying to apply an opcode that expects control
    -- rate signal to some audio rate signal, the signal will be downsampled behind the scenes.
    Sig, Spec,
   
    -- * Booleans
    -- | Use functions from the module "Data.Boolean" to make boolean expressions.
    BoolSig, BoolD,  
    ifTuple, caseTuple, guardedTuple,
    ifArg, caseArg, guardedArg,
    module Data.Boolean,
    
    -- * Side effects
    SE,    

    -- * Tuples
    CsdTuple,
    
    -- ** Multiple outputs helpers
    -- | Functions to help the type checker specify the number of outputs (for opcodes like 'Csound.Opcodes.Data.diskin2').
    -- For example,
    -- 
    -- > (left, right) = ar2 $ diskin2 (str "cool-sound.wav") 1
    --
    -- Without 'Csound.Base.ar2' we have to specify the type of the output explicitly.
    ar1, ar2, ar4, ar6, ar8,

    -- * Converters
    ToSig(..), ar, kr, ir, sig, double, int, str,          
        
) where

import Data.Boolean

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.Tuple
import Csound.Exp.Logic
import Csound.Exp.SE
import Csound.Exp.Cons(withInits)
import Csound.Tab

