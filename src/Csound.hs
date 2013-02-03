-- | We are trying to make a music with text. We describe music with csound-language.
-- To speed up this process we are going to use Haskell. We are going to create csound files
-- with hasell functions. We build a list of 'SigOut' expressions and then use the function 'csd' 
-- to get a string with Csound-language text. Then we just pass this file to csound in command line:
--
-- > $ csound file.csd
--
-- And we can here our result. 
module Csound(
    -- * Main goals 
    
    -- ** Present
    
    -- | 
    -- * Keep it simple and compact.
    --
    -- * Try to hide low level csound's wiring as much as we can (no ids for ftables, instruments, global variables).
    --
    -- * Don't describe the whole csound in all it's generality but give the user some handy tools
    -- to play with sound.
    --
    -- * No distinction between audio and control rates on the type level. Derive all rates from the context.
    -- If the user plugs signal to an opcode that expects an audio rate signal the argument is converted to the right rate.
    --  
    -- * Watch out for side-effects. There is a special type called 'SE'. It functions as 'IO' in Haskell.     
    --
    -- * Less typing, more music. Use short names for all types. Make library so that all expressions can be
    -- built without type annotations. Make it simple for the compiler to derive all types. Don't use complex type classes.
    -- 
    -- * Make low level opcode definitions simple. Let user define his own opcodes (if they are missing).
    --
    -- * Make it independent from any Score-generation library. Let user choose his favorite library.
    -- By the way, my favourite is the package 'temporal-music-notation' (but i'm biased as an author). You can
    -- find it on hackage alongside with 'temporal-csound-expression'. It brings 'csound-expression' and 'temporal-music-notation'
    -- together. 
    --
    
    -- ** Future
        
    -- |
    -- * Make composable guis. Just plug the slider in the opcode and see it on the screen. Interactive instruments should be easy to make.
    --
    -- * Remove score/instrument barrier. Let instrument play a score within a note and trigger
    -- other instruments. 
    --
    -- * Timing of events. User can set the beat rate and align events by beat events.

    -- * Types
    
    -- ** Constants     
    -- | A constant value doesn't change while instrument is playing a note.
    -- Only constants can be passed as arguments to the instruments.
    D, I, Tab, S,
    
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
    MultiOut,
    
    -- ** Converters
    ToSig(sig), ar, kr,
    int, double, str,          
        
    -- * Making a sound
    
    -- | Let's make some noise. Sound is build from list of tracks. We can define a track in two ways. 
     
    -- ** Scores
    -- | We can define an instrument and tell it to play some notes.
    score, SigOut, Arg(..), out, outs, effect,

    -- ** Midi
    -- | Or we can define a midi-instrument. The we can trigger the instrument with midi-keyboard.
    Msg, massign, pgmassign,

    -- ** Rendering
    -- | Now we are ready to create a csound-file. The function 'csd' creates a 'String' thjat
    -- contains the description of our instrument. We can save it to file and show it to our @csound@
    -- wizard. 
    renderCsd,
   
    -- ** Opcodes    
    -- | Some colors to paint our soundscapes.
    module Csound.Opcode,

    -- ** Options
    -- | We can set some csound options.
    renderCsdBy,    
    CsdOptions(..), module Data.Default, mixing, mixingBy
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



