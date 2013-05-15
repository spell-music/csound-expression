-- | Basic types and functions.
--
-- WARNING (for Csound users): the maximum amplitude is 1.0. There is no way to alter it. 
-- Don't define your amplitudes with 9000 or 11000. But the good news are: all signals
-- are clipped by 1 so that you can not damage your ears and your speakers by a little typo.
module Csound.Base(
    -- * Introduction to Csound for Haskell users
    
    -- | We are going to make electronic music. But what is Csound? And why should we use it?    
    --
    -- Csound is a domain specific programming language. It helps you to define synthesizers and make some music with them (<http://www.csounds.com>). 
    -- Csound was born in 1985 (a bit older than Haskell) at MIT by Barry Vercoe. It's widely used in the academia.
    -- It has a long history. So with Csound we get a lot of music dsp-algorithms ready to be used. It's written in C.
    -- So it's very efficient. It's driven by text, so we can generate it. Csound's community is very friendly (what a coincidence!). 
    -- Csound is very well documented.
    --     
    
    -- ** Making music with Csound
        
    -- | You don't need to know Csound to use this library.
    -- but it's helpful to know the main features of the Csound: how can you create music with Csound in general, 
    -- what design choices were made, basic features and quirks. Csound belongs to the MUSIC N family 
    -- of  programming languages. What does it mean? It means that description of the music is divided in two parts:
    --
    -- 1. Orchestra. User defines instruments
    --
    -- 2. Scores. User triggers instruments with a list of notes
    --
    -- An instrument is something that listens to notes and converts them to signals. 
    -- Note is a tuple: (instrument name, start time, duration, parameters). Parameters cell is
    -- a tuple of primitive types: numbers ('Csound.Base.D'), strings ('Csound.Base.Str') and tables or arrays of numbers ("Csound.Tab").
    -- 
    -- Scores are very simple yet powerful. Csound handles polyphony for you. If you trigger
    -- several notes at the same time on the same instrument you get three instances of the same
    -- instrument running in parallel. It's very cool feature (not so easy thing to do with Pd).
    --
    -- But main strength lies in the Orchestra section. Here you can define the timbres for
    -- your musical journey. Csound is mostly for making strange sounds. How you can do it?
    -- You do it with instruments. An instrument is a sequence of statements that define a flow-graph
    -- for your sound waves. For an instrument you can use predefined sound generators and transformers ("Csound.Opcode" and "Csound.Air").
    -- 
    -- Score/Orchestra division stays in this library too. You define your instruments of the type
    --
    -- > (Arg a, Out b) => a -> b
    --
    -- An instrument is something that converts arguments-like things (tuple of primitive values) to output-like things (tuple of signals).
    --
    -- When you are done with the orchestra section you can trigger the instruments with the function 'Csound.Base.sco'
    --
    -- > sco :: (Arg a, Out b) => (a -> b) -> Score a -> Score (Mix (NoSE b))
    --
    -- It takes an instrument and the bunch of notes for this instrument. The output looks scary but let's try to understand it by bits:
    --
    -- * @Score a@ - you can think of it as a container of some values of type @a@ (every value of type @a@ starts at some time and lasts for some time in seconds)
    --
    -- * @Mix a@ - is an output of Csound instrument it can be one or several signals ('Csound.Base.Sig' or 'Csound.Base.CsdTuple'). 
    --
    -- * @NoSE a@ - it's a tricky part of the output. 'NoSE' means literaly 'no SE'. It tells to the type checker that it can skip the 'Csound.Base.SE' wrapper
    -- from the type 'a' so that @SE a@ becomes just @a@ or @SE (a, SE b, c)@ becomes @(a, b, c)@. Why should it be? I need 'SE' to deduce the order of the
    -- instruments that have side effects. I need it within one instrument. But when instrument is rendered I no longer need 'SE' type. So 'NoSE' lets me drop it
    -- from the output type. 
    --
    -- How to put the values in the container 'Temporal.Music.Score.Score'? There are many functions to construct the 'Temporal.Music.Score.Score'.
    -- They live in the module "Temporal.Music.Score". If you are not familiar with it, you can start with six basic functions. 
    --
    -- * 'Temporal.Music.Score.rest' -- makes a pause that lasts for some time (in seconds).
    --
    -- * 'Temporal.Music.Score.temp' -- makes a score of one note that lasts for one second.    
    --
    -- * 'Temporal.Music.Score.line' -- plays a list of notes in sequence (one after the other).
    -- 
    -- * 'Temporal.Music.Score.chord' -- plays a list of notes in parallel (at the same time).
    --
    -- * 'Temporal.Music.Score.delay' -- delays all notes for some time.
    --
    -- * 'Temporal.Music.Score.stretch' -- change the tempo for all notes by the given ratio.
    --
    -- Let's play something:
    --
    -- > res = stretch 0.5 $ line [ temp a, stretch 2 $ temp b, rest 1, chord [temp a, temp b] ]
    --
    -- There are two handy infix operators for delay and stretch: @(+|)@ and @(*|)@. So we can write the previous score:
    --
    -- > res = 0.5 *| line [ temp a, 2 *| temp b, 1 +| chord [temp a, temp b] ]
    --
    -- If you got used to Csound you can ask -- where is the instrument name in the score? No need to worry about names they are generated automatically.
    --
    -- In Csound to apply some effect one must use the global variables. There are some instruments that produce signals and write them to
    -- the global variables and there is an instrument that functions as mixer. It's turned on for the whole piece and it reads the global
    -- variables and applies the effects to the sound and finally writes it to the file or to the speakers. In this library it's very easy
    -- to apply an effect to the outputs of the instruments. There is a function 'Csound.Base.mix':
    --
    -- > mix :: (Out a, Out b) => (a -> b) -> Score (Mix a) -> Score (Mix (NoSE a))
    --
    -- Looks like the function 'Csound.Base.sco'. But now the first argument is an effect. It takes not a note but a signal (or a tuple of signals)
    -- and gives back some signal. The second argument holds the sound that we'd like to apply the effect to. With this function we can apply reverb or
    -- adjust the gain levels or apply some envelope, any valid csound transformation will do. What's interesting is that we can delay and stretch it, align
    -- with functions 'Temporal.Media.line' and 'Temporal.Media.chord' add another signal to it and apply some fancy effect to the cumulative sound.
    -- (If you are familiar with Csound this functionality is implemented with chnmix and chnget opcodes).
    --  
    
    -- ** Flags and options
    
    -- | Music is defined in two parts. They are Orchestra and Scores. But there is a third one. It's used
    -- to set the global settings like sample rate or control rate values (block size). In this library you
    -- can set the initial values with 'Csound.Base.CsdOptions'.
    
    -- ** Features and quirks
    
    -- *** Audio and control rates
    
    -- | Csound has made a revolution in electronic music technology. It introduced two types of signals.
    -- They are audio rate and control rate signals. The audio rate signals is what we hear and control rate
    -- signals is what changes the parameters of sound. Control rate is smaller then audio rate. It speeds
    -- up performance dramatically. Let's look at one of the sound units (they are called opcodes)
    --
    -- > ares buthp asig, kfreq [, iskip]
    --
    -- It's a butterworth high pass filter as it defined in the Csound. a-sig - means sig at audio rate.
    -- k-freq means freq at control rate (for historical reasons it is k not c). iskip means skip at i-rate.
    -- i-rate means init time rate. It is when an instruments instance is initialized to play a note. i-rate
    -- values stays the same for the whole note. So we can see that signal is filtered at audio rate but
    -- the center frequency of the filter changes at the control rate. In this library I've merged the 
    -- two types together ('Csound.Base.Sig'). If you plug a signal into kfreq we can infer that you want this
    -- signal to be control rate. In Csound some opcodes exist go in pairs. One that produces audio signals
    -- and one that produces control rate signals. By default if there is no constraint for the signal it is rendered
    -- at the audio rate except for those units that produce sound envelopes (like 'Csound.Opcode.Basic.linseg').    
    --
    -- You can change this behaviour with functions 'Csound.Base.ar' and 'Csound.Base.kr'. They set the signal-like things to
    -- audio or control rate. For instance if you want your envelope to run
    -- at control rate, write:
    --
    -- > env = ar $ linseg [0, idur/2, 1, idur/2, 0]
    
    -- *** Table size
    
    -- | For speed table size should be the power of two or power of two plus one (all tables for oscillators). 
    -- In this library you can specify the relative size (see 'Csound.Base.CsdOptions').
    -- I've tried to hide the size definition to make sings easier.     
    
    -- ** How to read the Csound docs
    
    -- | You'd better get acquainted with Csound docs. Docs are very good. How to read them? For instance you want to use an oscillator with cubic interpolation 
    -- so you dig into the "Csound.Opcode.Basic" and find the function:
    --
    -- > oscil3 :: Sig -> Sig -> Tab -> Sig
    --
    -- From Hackage we can guess that it takes two signals and table and returns a signal. It's a clue but a vogue one.
    -- Let's read along, in the docs you can see a short description (taken from Csound docs):
    --
    -- > oscil3 reads table ifn sequentially and repeatedly at a frequency xcps. 
    -- > The amplitude is scaled by xamp. Cubic interpolation is applied for table look up from internal phase values. 
    -- 
    -- and here is the Csound types (the most useful part of it)  
    --
    -- > ares oscil3 xamp, xcps, ifn [, iphs]
    -- > kres oscil3 kamp, kcps, ifn [, iphs]
    --
    -- We see a two versions of the opcode. For audio and control rate signals. By default first is rendered
    -- if we don't plug it in something that expects control rates. It's all about rates, but what can we find out
    -- about the arguments?
    -- 
    -- First letter signifies the type of the argument and the rest is the name. We can see that first signal is amp with x rate.
    -- and the second one is cps with x rate. We can guess that amp is the amplitude and cps is cycles per second. This unit
    -- reads the table with given amplitude (it is a signal) and frequency (it is a signal too). Or we can just read about it
    -- in the docs if we follow the link that comes at the very last line in the comments:
    --
    -- * doc: <http://www.csounds.com/manual/html/oscil3.html>
    -- 
    -- I've said about a-, k- and i-rates. But what is the x-rate? Is it about X-files or something? X means a-rate or k-rate.
    -- You can use both of them for this argument. Let's go through all types that you can find:
    --
    -- * asig -- audio rate ('Csound.Base.Sig')
    --
    -- * ksig -- control rate ('Csound.Base.Sig')
    --
    -- * xsig -- audio or control rate ('Csound.Base.Sig')
    --
    -- * inum -- constant number ('Csound.Base.D')
    --
    -- * ifn -- table ('Csound.Tab.Tab'). They are called functional tables in Csound.
    --
    -- * Sfile -- string, probably a file name ('Csound.Base.Str')
    --
    -- * fsrc -- spectrum ('Csound.Base.Spec'). Yes, you can mess with sound in the space domain.   
    --
    -- Often you will see the auxiliary arguments, user can skip them in Csound. So we can do it in Haskell too.
    -- But what if we want to supply them? We can use the function 'Csound.Base.withInits' for this purpose.

       
    -- ** Example (a concert A)
    
    -- |
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
    -- > myOsc cps = oscili 1 cps (sines [1])
    -- > 
    -- > -- Let's define a simple instrument that plays a sound on the specified frequency.
    -- > -- We use sig to convert a constant value to signal and then plug it in the osc unit. 
    -- > -- We make it a bit quieter by multiplying with 0.5.
    -- > pureTone :: D -> Sig
    -- > pureTone cps = 0.5 * (myOsc $ sig cps)
    -- > 
    -- > -- Let's trigger the instrument from the score section.
    -- > -- It plays a single note that starts at 0 and lasts for 1 second and 
    -- > -- triggers the instrument 'instr' with frequency of 440 (Hz).
    -- > res = sco pureTone $ temp 440
    -- > 
    -- > -- Renders generated csd-file to the "tmp.csd".
    -- > main :: IO ()
    -- > main = writeCsd "tmp.csd" res
    --
    -- Now you can invoke Csound on tmp.csd and listen to the result with your favourite player.
    --
    -- > csound tmp.csd -o a.wav  
    --
    -- That's it @csound@ is a separate program that we have to run to compile our csd-files to sounds.
    -- We can listen to the sound as it runs. It can be configured with flags.
    
    -- ** More examples
    
    -- | You can find many examples at:
    --
    -- * A translation of the Amsterdam catalog of Csound computer instruments: <https://github.com/anton-k/amsterdam>
    --
    -- * Csound expression Tutorial at (TODO).
        
    -- ** References
    
    -- | Got interested in Csound? Csound is very well documented. There are good tutorials, read about it at:
    --    
    -- * Reference manual: <http://www.csounds.com/manual/html/index.html >
    --
    -- * Floss tutorials: <http://en.flossmanuals.net/csound/>
    --
    -- * Amsterdam catalog of Csound computer instruments: <http://www.codemist.co.uk/AmsterdamCatalog/>
    --
    -- * Lots of wonderful real-time examples by Iain McCurdy: <http://iainmccurdy.org/csound.html>
    --
    -- * Outdated but short manual on Csound <http://cara.gsu.edu/courses/csound_users_seminar/csound/3.46/CsIntro.html>
   
    -- * Types
    Val, arity,
    
    -- ** Constants     
    -- | A constant value doesn't change while instrument is playing a note.
    -- Only constants can be passed as arguments to the instruments.
    D, Str,
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
    Sig, Spec,
   
    -- ** Booleans
    -- | Use functions from the module "Data.Boolean" to make boolean expressions.
    BoolSig, BoolD,  
    module Data.Boolean,

    -- ** Side effects
    SE,    

    -- ** Tuples
    CsdTuple,
    
    -- ** Converters
    ToSig(..), ar, kr, ir, sig, double, str,          
        
    -- * Making a sound
    
    -- | Let's make some noise. Sound is build from list of tracks ('SigOut').
    Out,
    
    -- ** Handy short-cuts
    Sig2, Sig3, Sig4, Ksig, Amp, Cps, Iamp, Icps,
     
    -- ** Scores
    -- | We can define an instrument and tell it to play some notes.
    Arg(..), ArgMethods, makeArgMethods,
    Mix, sco, mix, 
    
    -- ** Effects
    effect, effectS,

    -- ** Midi
    -- | We can define a midi-instrument. Then we can trigger the instrument with a midi-keyboard.
    --Msg, midi, pgmidi,

    -- ** Rendering
    -- | Now we are ready to create a csound-file. The function 'renderCsd' creates a 'String' that
    -- contains the description of our music. We can save it to a file and compile it with our @csound@
    -- wizard. 
    renderCsd, writeCsd, playCsd,
    
    -- *** Players (Linux)
    -- | Handy short-cuts for function 'Csound.Base.playCsd'.
    mplayer, totem, 
    
    -- *** Players (Windows)
    -- | Handy short-cuts for function 'Csound.Base.playCsd'.
    --
    -- TODO (you can send me your definitions)

    -- *** Players (OS X)
    -- | Handy short-cuts for function 'Csound.Base.playCsd'.
    --
    -- TODO (you can send me your definitions)

   
    -- ** Opcodes    
    -- | Some colors to paint our soundscapes.
    module Csound.Opcode,
    
    -- ** Patterns
    -- | Frequently used combinations of opcodes.
    module Csound.Air,        

    -- ** Options
    -- | We can set some csound options.
    Channel, CtrlId, CsdOptions(..), module Data.Default,
    coarseFi, fineFi,
        
    renderCsdBy, writeCsdBy, playCsdBy,  
    
    -- *** Players (Linux)
    -- | Handy short-cuts for function 'Csound.Base.playCsdBy'.
    mplayerBy, totemBy
    
    
    -- *** Players (Windows)
    -- | Handy short-cuts for function 'Csound.Base.playCsd'.
    --
    -- TODO (you can send me your definitions)

    -- *** Players (OS X)
    -- | Handy short-cuts for function 'Csound.Base.playCsd'.
    --
    -- TODO (you can send me your definitions)


) where

import Data.Default
import Data.Boolean
import Csound.Air 

import Csound.Exp
import Csound.Exp.Cons
import Csound.Exp.Wrapper
import Csound.Exp.Tuple
import Csound.Exp.Arg
import Csound.Tab
import Csound.Opcode
import Csound.Exp.Numeric
import Csound.Exp.Logic
import Csound.Exp.SE

import Csound.Render.Options
import Csound.Exp.Mix

import Csound.IO


