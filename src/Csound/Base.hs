-- | Basic types and functions.
--
-- Example (a concert A)
--
-- > module Main where
-- > 
-- > import Csound.Base
-- > 
-- > osc :: Sig -> Sig
-- > osc phs = oscil1 1 phs (genHigh 10 [1])
-- > 
-- > instr :: D -> Out
-- > instr pch = out $ 0.5 * (osc $ kr pch)
-- > 
-- > res = score instr [(0, 1, 440)]
-- > 
-- > main :: IO ()
-- > main = writeFile "tmp.csd" $ renderCsd [res]
--
-- Now you can invoke Csound on tmp.csd and listen to the result with your favourite player.
--
-- > csound tmp.csd -o a.wav
module Csound.Base(

    -- * Types
    Val,
    
    -- ** Constants     
    -- | A constant value doesn't change while instrument is playing a note.
    -- Only constants can be passed as arguments to the instruments.
    D, I, Tab, Str,
    withInits,
    
    -- ** Signals
    -- | Signals can be audio or control rate. Rate is derived from the code.
    -- If there are rate-collisions, values will be converted to the right rates.    
    -- For example, if you are trying to apply an opcode that expects control
    -- rate signal to some audio rate signal, the signal will be downsampled behind the scenes.
    Sig, BoolSig, Spec,
    module Data.Boolean,

    -- ** Side effects
    SE,    

    -- ** Tuples
    CsdTuple,
    
    -- ** Converters
    ToSig(..), ir, int, double, str,          
        
    -- * Making a sound
    
    -- | Let's make some noise. Sound is build from list of tracks ("SigOut").
    Out, SigOut, effect, out, outs,  
     
    -- ** Scores
    -- | We can define an instrument and tell it to play some notes.
    score, Arg(..), ArgMethods, makeArgMethods,

    -- ** Midi
    -- | We can define a midi-instrument. Then we can trigger the instrument with a midi-keyboard.
    Msg, massign, pgmassign,

    -- ** Rendering
    -- | Now we are ready to create a csound-file. The function "renderCsd" creates a 'String' that
    -- contains the description of our music. We can save it to a file and compile it with our @csound@
    -- wizard. 
    renderCsd,
   
    -- ** Opcodes    
    -- | Some colors to paint our soundscapes.
    module Csound.Opcode,

    -- ** Options
    -- | We can set some csound options.
    renderCsdBy,    
    Channel, CtrlId, CsdOptions(..), module Data.Default, mixing, mixingBy
) where

import Data.Default
import Data.Boolean

import Csound.Exp
import Csound.Exp.Cons
import Csound.Exp.Wrapper
import Csound.Opcode
import Csound.Exp.Numeric
import Csound.Exp.Logic

import Csound.Render.Sco
import Csound.Render.Options
import Csound.Render



