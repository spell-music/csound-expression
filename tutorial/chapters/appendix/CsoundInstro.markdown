Introduction to Csound for Haskell users
=====================================================
    
We are going to make electronic music. But what is Csound? 
And why should we use it? [Csound](http://www.csounds.com/) is a domain specific programming language. 
It helps you to define synthesizers and make some music with them. 
Csound was born in 1985 (a bit older than Haskell) at MIT by Barry Vercoe. 
It's widely used in the academia. It has a long history. So with Csound we 
get a lot of music DSP-algorithms ready to be used. It's written in C.
So it's very efficient. It's driven by text, so we can generate it. 
Csound's community is very friendly (what a coincidence!). 
Csound is very well documented.
        
We don't need to know Csound to use this library 
but it's helpful to know the main features of the Csound.
How can we create music with Csound in general
What design choices were made, basic features and quirks. 
Csound belongs to the MUSIC N family of  programming languages. 
What does it mean? It means that description of the music is divided in two parts:

* Orchestra. User defines instruments
    
* Scores. User triggers instruments with a list of notes
    
An instrument is something that listens to notes and converts them to signals. 
Note is a tuple: (instrument name, start time, duration, parameters). Parameters cell is
a tuple of primitive types: numbers (`D`), strings (`Str`) and tables 
or arrays of numbers (`Tab`).

## Instruments

An instrument is represented with function that takes a tuple of primitive
values (`Arg`) and converts it to the tuple of signals (`Sigs`) wrapped in the type `SE`:
    
~~~haskell
(Arg a, Sigs b) => a -> SE b
~~~

The `SE` means Side Effect. It's like `IO`-monad but for Csound.

## Events

With instruments we can convert the bunch of notes to the plain signals.
There are several ways to do it. We can trigger an instrument:

* With score

* With event stream

* With midi-device

### The Score

The score is a list of events with some predefined total duration.
An event is a triple that contains:

~~~haskell
(t0, dt, args)
~~~

Where `t0` is a start time, `dt` is a duration of the event, `args` is
a list of arguments for the instrument. 
The Score is represented with the type:

~~~haskell
data CsdEventList a = CsdEventList
    { csdEventListDur       :: Double
    , csdEventListNotes     :: CsdEvent a }
    
type CsdEvent a = (Double, Double, a)
~~~

The start time and duration are in seconds. To invoke an instrument
with Score we can use the functions:

~~~haskell
sco :: (CsdSco f, Arg a, Sigs b) => (a -> SE b)  -> f a -> f (Mix b)
mix :: (CsdSco f, Sigs a) => f (Mix a) -> a
~~~

The type `CsdEventList` is not to be used directly. 
It's a canonical representation of the Csound score. 
We should use something more higher level. That's why 
we don't see it in the signatures. It's referenced indirectly 
with type class `CsdSco`. The types of the type class `CsdSco`
are things that can be converted to the canonical representation.

~~~haskell
class CsdSco f where
    toCsdEventList :: f a -> CsdEventList a
    singleCsdEvent :: CsdEvent a -> f a
~~~

The method `toCsdEventList` converts a given score representation 
to the canonical one. The method `singleCsdEvent` constructs a scores 
that contains only one event. it lasts for one second.

The function `sco` applies an instrument to the score and 
produces the score of signals. Then we can apply the function
mix` to get the mixed signal.

Scores are very simple yet powerful. Csound handles polyphony for us. If we trigger
several notes at the same time on the same instrument we get three instances of the same
instrument running in parallel. It's very cool feature (not so easy thing to do with Pd).

### The event stream

An event stream is something that produces the notes. 
The score contains the predefined notes but event stream
can produce the in real time. 

The event stream is represented with the type:

~~~haskell
newtype Evt a = Evt { runEvt :: Boom a -> SE () }

type Boom a = a -> SE ()
~~~

An event stream is a function that takes a procedure of
type `a -> SE ()` and applies it to all events in the stream.

We have some primitive constructors:

~~~haskell
metroE :: Sig -> Evt ()
~~~

It takes a frequency of the repetition. An empty tuple happens every now and then.
We can process the events with functions:

~~~haskell
repeatE :: a -> Evt b -> Evt a
filterE :: (a -> BoolD) -> Evt a -> Evt a
cycleE  :: Arg a => [a] -> Evt b -> Evt a
oneOf   :: Arg a => [a] -> Evt b -> Evt a
...
~~~

For example, we can substitute all the events with the constant value (`repeatE`),
filter an event stream with predicate or repeat elements in the list (`cycleE`) or
take elements at random (`oneOf`). There are many more functions.

The `BoolD` is a Csound boolean value. It's instance of the type classes from
the package `Boolean`. There is another boolean type `BoolSig` for the signals
of boolean values.

We can trigger instruments on the event streams with functions:

~~~haskell
trig  :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, D, a) -> b
sched :: (Arg a, sigs b) => (a -> SE b) -> Evt (D, a)    -> b
~~~

The function `trig` applies an instrument to the event stream of notes.
A note contains a delay of the event, the event duration and the arguments
for the instrument. The function `sched` is the same as `trig` but
all events happen immediately. 

### The Midi devices

We can trigger an instrument with midi devices:

~~~haskell
midi   :: (Sigs a) => (Msg -> SE a) -> a
midin  :: (Sigs a) => Int -> (Msg -> SE a) -> a
pgmidi :: (Sigs a) => Int -> Maybe Int -> (Msg -> SE a) -> a
~~~

The function `midi` starts to listen for the midi-messages (`Msg`)
on all channels. With function `midin` we can specify the concrete
channel (it's an integer from 1 to 16). The function `pgmidi` is
for assigning an instrument to the midi-program (the first argument) 
and possible channel (the second argument).

We can query midi-messages for amplitude, frequency and other parameters
(we can see the complete list in the module `Csound.Opcode.RealtimeMIDI`):

~~~haskell
cpsmidi :: Msg -> D
ampmidi :: Msg -> D -> D
...
~~~


## Flags and options
    
Music is defined in two parts. They are Orchestra and Scores. 
But there is a third one. It's used to set the global settings 
like sample rate or control rate values (block size). In this library you
can set the initial values with `Csound.Options`.
    
## Features and quirks
    
### Audio and control rates
    
Csound has made a revolution in electronic music technology. 
It introduced two types of signals. They are audio rate and control rate signals. 
The audio rate signals is what we hear and control rate
signals is what changes the parameters of sound. Control rate 
is smaller then audio rate. It speeds up performance dramatically. 
Let's look at one of the sound units (they are called opcodes)
    
~~~
ares buthp asig, kfreq [, iskip]
~~~   

It's a Butterworth high pass filter as it defined in the Csound. 
a-sig - means sig at audio rate. k-freq means freq at control rate 
(for historical reasons it is k not c). iskip means skip at i-rate.
i-rate means init time rate. It is when an instruments instance 
is initialized to play a note. i-rate values stays the same for 
the whole note. So we can see that signal is filtered at audio rate but
the center frequency of the filter changes at the control rate. 
In this library the types are merged together (`Sig`). 
If you plug a signal into `kfreq` we can infer that you want this
signal to be control rate. In Csound some opcodes exist go in pairs. 
One that produces audio signals and one that produces control rate signals. 
By default if there is no constraint for the signal it is rendered
at the audio rate except for those units that produce sound envelopes 
(like `linseg` or `expseg`).  

You can change this behaviour with functions `ar` and 'kr'. 
They set the signal-like things to audio or control rate. For 
instance if you want your envelope to run
at control rate, write:

~~~haskell
env = ar $ linseg [0, idur/2, 1, idur/2, 0]
~~~   

### Table size
    
For speed table size should be the power of two or power of two plus 
one (all tables for oscillators). In this library you can specify the 
relative size (see `Csound.Options`). I've tried to hide the size 
definition to make sings easier.     
    
## How to read the Csound docs
    
You'd better get acquainted with Csound docs. Docs are very good. 
How to read them? For instance you want to use an oscillator 
with cubic interpolation. So you dig into the `Csound.Opcode.SignalGenerators` 
and find the function:

~~~haskell
oscil3 :: Sig -> Sig -> Tab -> Sig
~~~   

From Hackage we can guess that it takes two signals and table 
and returns a signal. It's a clue but a vogue one.
Let's read along, in the docs you can see a short description (taken from Csound docs):

~~~
oscil3 reads table ifn sequentially and repeatedly at a frequency xcps. 
The amplitude is scaled by xamp. Cubic interpolation is applied 
for table look up from internal phase values. 
~~~

and here is the Csound types (the most useful part of it)  

~~~
> ares oscil3 xamp, xcps, ifn [, iphs]
> kres oscil3 kamp, kcps, ifn [, iphs]
~~~    

We see a two versions of the opcode. For audio and control rate signals. 
By default first is rendered if we don't plug it in something that expects 
control rates. It's all about rates, but what can we find out about the arguments?

First letter signifies the type of the argument and the rest is the name. 
We can see that first signal is amp with x rate. and the second one is 
cps with x rate. We can guess that amp is the amplitude and cps is cycles 
per second. This unit reads the table with given amplitude (it is a signal) 
and frequency (it is a signal too). Or we can just read about it
in the docs if we follow the link that comes at the very last line in the comments:

~~~haskell
doc: <http://www.csounds.com/manual/html/oscil3.html>
~~~

We now about a-, k- and i-rates. But what is the x-rate? Is it about X-files 
or something? X means a-rate or k-rate. You can use both of them for 
this argument. Let's go through all types that you can find:


* `asig` -- audio rate (`Sig`)

* `ksig` -- control rate (`Sig`)
    
* `xsig` -- audio or control rate (`Sig`)
    
* `inum` -- constant number (`D`)
    
* `ifn` -- table or 1D-array (`Tab`). They are called functional tables in Csound.
    
* Sfile -- string, probably a file name (`Str`)
    
* fsrc -- spectrum (`Spec`). Yes, you can mess with sound in the space domain.   
    
Often you will see the auxiliary arguments, user can skip them in Csound. 
So we can do it in Haskell too. But what if we want to supply them? 
We can use the function `withInits` for this purpose.
       
## Example (a concert A)
    
~~~haskell
module Main where

-- imports everything
import Csound.Base

-- Let's define a simple sound unit that 
-- reads in cycles the table that contains a single sine partial.
-- oscil1 is the standard oscillator with linear interpolation.
-- 1 - means the amplitude, cps - is cycles per second and the last argument
-- is the table that we want to read. 
myOsc :: Sig -> Sig
myOsc cps = oscili 1 cps (sines [1])

-- Let's define a simple instrument that plays a sound on the specified frequency.
-- We use sig to convert a constant value to signal and then plug it in the osc unit. 
-- We make it a bit quieter by multiplying with 0.5.
pureTone :: D -> SE Sig
pureTone cps = return $ 0.5 * (myOsc $ sig cps)

-- Let's trigger the instrument from the score section.
-- It plays a three notes. One starts at 0 and lasts for one second with frequency of 440,
-- another one starts at 1 second and lasts for 2 seconds, and the last note lasts for 2 seconds
-- at the frequency 220 Hz. 
res = sco pureTone $ CsdEventList 5 [(0, 1, 440), (1, 2, 330), (3, 2, 220)]

-- Renders generated csd-file to the "tmp.csd", invokes the csound on it 
-- and directs the sound to speakers.
main :: IO ()
main = dac $ mix res
~~~

## More examples
    
You can find many examples at:
   
* Examples in the archive with the source code of the library.
    
* A translation of the 
    [Amsterdam catalog of Csound computer instruments](https://github.com/anton-k/amsterdam)
       
## References
    
Got interested in Csound? Csound is very well documented. 
There are good tutorials, read about it at:
   
* [Reference manual](http://www.csounds.com/manual/html/index.html)
    --
* [Floss tutorials](http://en.flossmanuals.net/csound/)
    --
* [Amsterdam catalog of Csound computer instruments](http://www.codemist.co.uk/AmsterdamCatalog/)
    --
* Lots of wonderful real-time examples by [Iain McCurdy](http://iainmccurdy.org/csound.html)
    --
* Outdated but short [manual on Csound](http://cara.gsu.edu/courses/csound_users_seminar/csound/3.46/CsIntro.html)

------------------------------------------------------------------

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)

