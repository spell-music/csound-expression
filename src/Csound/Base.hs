-- | Basic types and functions.
--
-- Example (a concert A)
--
-- > module Main where
-- > 
-- > -- imports everything
-- > import Csound.Base
-- > 
-- > -- Let's define a simple sound unit that 
-- > -- reads in cycles the table that contains a single sine partial.
-- > -- oscil1 is the standard oscillator with linear interpolation.
-- > -- 1 - means the amplitude, cps - is cycles per second and the last argument
-- > -- is the table that we want to read. 
-- > myOsc :: Sig -> Sig
-- > myOsc cps = oscil1 1 cps (sines [1])
-- > 
-- > -- Let's define a simple instrument that plays a sound on the specified frequency.
-- > -- We use kr to convert a constant value to signal and then plug it in the osc unit. 
-- > -- We make it a bit quieter by multiplying with 0.5.
-- > pureTone :: D -> Sig
-- > pureTone cps = 0.5 * (myOsc $ kr cps)
-- > 
-- > -- Let's trigger the instrument from the score section.
-- > -- It plays a single note that starts at 0 and lasts for 1 second and 
-- > -- triggers the instrument 'instr' with frequency of 440 (Hz).
-- > res = score pureTone [(0, 1, 440)]
-- > 
-- > -- Renders generated csd-file to the "tmp.csd".
-- > main :: IO ()
-- > main = writeFile "tmp.csd" $ renderCsd [res]
--
-- Now you can invoke Csound on tmp.csd and listen to the result with your favourite player.
--
-- > csound tmp.csd -o a.wav
--
-- WARNING (for Csound users): the maximum amplitude is 1.0. There is no way to alter it. 
-- don't define your amplitudes with 9000 or 11000. But the good news are: all signals
-- are clipped by 1 so that you can not damage your ears and your speakers by a little typo.
module Csound.Base(

    -- * Types
    Val,
    
    -- ** Constants     
    -- | A constant value doesn't change while instrument is playing a note.
    -- Only constants can be passed as arguments to the instruments.
    D, I, Str,
    withInits,
    
    -- ** Tables
    -- | In Csound tables can be treated as primitive values. They can be passed to instruments in the score events.
    -- There are limited set of functions which you can use to make new tables. Look at the following module for details:
    module Csound.Tab,
    
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
    
    -- | Let's make some noise. Sound is build from list of tracks ('SigOut').
    Out, SigOut, effect, Outs,
     
    -- ** Scores
    -- | We can define an instrument and tell it to play some notes.
    score, Arg(..), ArgMethods, makeArgMethods,

    -- ** Midi
    -- | We can define a midi-instrument. Then we can trigger the instrument with a midi-keyboard.
    Msg, massign, pgmassign,

    -- ** Rendering
    -- | Now we are ready to create a csound-file. The function 'renderCsd' creates a 'String' that
    -- contains the description of our music. We can save it to a file and compile it with our @csound@
    -- wizard. 
    renderCsd,
   
    -- ** Opcodes    
    -- | Some colors to paint our soundscapes.
    module Csound.Opcode,
    
    -- ** Patterns
    -- | Frequently used combinations of opcodes.
    module Csound.Air,        

    -- ** Options
    -- | We can set some csound options.
    renderCsdBy,    
    Channel, CtrlId, CsdOptions(..), module Data.Default, mixing, mixingBy
) where

import Data.Default
import Data.Boolean

import Csound.Air 

import Csound.Exp
import Csound.Exp.Cons
import Csound.Exp.Wrapper
import Csound.Tab
import Csound.Opcode
import Csound.Exp.Numeric
import Csound.Exp.Logic

import Csound.Render.Sco
import Csound.Render.Options
import Csound.Render



