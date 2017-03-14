Introduction to Csound for Haskell users
=====================================================
    
We are going to make electronic music. But what is Csound? 
And why should we use it? [Csound](http://csound.github.io/) is a domain specific programming language. 
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

The underlying score is a list of events. But we'd like to have a musical structure on top of it.
To organize the events in musical way we use the `Track` type from 
the package [`temporal-media`](https://hackage.haskell.org/package/temporal-media/docs/Temporal-Media.html).

The `Track` can be thought of as a list of events with a total duration in seconds of the whole segment.
The vent is an aforementioned tripple of start time, duration and content:

~~~haskell
data Event t a = Event {
    eventStart :: t
    eventDur :: t
    eventContent :: a
}
~~~

The Track data type is obscure. In CE we use a more specific data type where time is set to 
constant Csound numbers (`D`s):

~~~haskell
data Track t a 

type Sco a = Track D a
~~~

We can create values of type `Track` with smart constructors and methods from the generic type classes
for composition of temporal media. Here is the lis of most common functions. For convenience the signatures
are specified to `Sco` data-type. But the actual functions come 
from the [list of classes](https://hackage.haskell.org/package/temporal-media-0.6.1/docs/Temporal-Class.html).

* Create an `Sco` with single event that lasts for one second and starts right away:

    ~~~haskell
    temp :: a -> Sco a
    ~~~

* Create a silence that lasts for the given duration:

    ~~~haskell
    rest :: D -> Sco a
    ~~~

* Harmonic composition. All scores are played together at the same time. The total duration equals to the maximum of all durations

    ~~~haskell
    har :: [Sco a] -> Sco a
    ~~~

* Melodic composition. Scores are played one after another. The total duration equals to the sum of all durations.

    ~~~haskell
    mel :: [Sco a] -> Sco a
    ~~~

* Stretch the Score in time domain by given constant factor. It makes the music slower or faster:

    ~~~haskell
    str :: D -> Sco a -> Sco a
    ~~~

* Delay the Score by the given time:

    ~~~haskell
    del :: D -> Sco a -> Sco a
    ~~~

* Loop several times:

    ~~~haskell
    loopBy :: Int -> Sco a -> Sca a
    ~~~

* Loop forever

    ~~~haskell
    loop  :: Sco a -> Sco a
    ~~~

* `Functor` instance to map over content:

    ~~~haskell
    fmap :: (a -> b) -> Sco a -> Sco b
    ~~~

To invoke an instrument with Score we can use the functions:

~~~haskell
sco :: (Sigs b, Arg a) => (a -> SE b) -> Sco a -> Sco (Mix b)
mix :: Sigs a => Sco (Mix a) -> a
~~~

The function `sco` applies an instrument to the score and 
produces the score of signals. Then we can apply the function
mix` to get the mixed signal. 

Why do we need the two steps to convert the score to audio signal?
The cool thing about this approach is that we can use the composition functions like `hor`, `mel` or `str`
after we applied the instrument to scores. We can think of `sco` as a function or a single player
in the orchestra. It applies a single instrument to the notes. But after that we'd like to be able
to create an orchestration. We need to combine the parts from several players. If we convert
to audio signal right away we will loose the information on the musical structore.

So use the `sco` for a single player in  your orchestra and combine all the parts from
different players with usual composition functions. At the last moment to send the audio to speakers
or write to file use the `mix` function. 

The wrapper `Mix` is needed to suppress the `Functor` instance. It's not possible to apply
the transformations to the notes that contain signals (there are some implementational details
that doesn't allow thsi to happen). 

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
metro :: Sig -> Evt Unit
~~~

It takes a frequency of the repetition. The `Unit` type is a Csound alias for `()`. We need it 
for implementation reasons but the meaning is the same. The `unit` can contain only a single value.
It's often used to represent the instruments that take no arguments. An empty tuple happens every now and then.
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

And the `Evt` is a Functor and also `Monoid`:

~~~haskell
fmap :: (a -> b) -> Evt a -> Evt b

mempty  :: Evt a
mappend :: Evt a -> Evt a -> Evt a
~~~

With `fmap` we map over the all values of the event. The `mempty` is a silent event. Nothing is going to happen on `mempty`.
The `mappend` joins the events from several sources to a single event stream. 

We can trigger instruments on the event streams with function:

~~~haskell
sched :: sched :: (Sigs b, Arg a) => (a -> SE b) -> Evt (Sco a) -> b
~~~

The `sched` takes an event of scores and applies an instrument when something happens.

### The Midi devices

We can trigger an instrument with midi devices:

~~~haskell
type Channel = Int

midi   :: (Sigs a) => (Msg -> SE a) -> SE a
midin  :: Sigs a => Channel -> (Msg -> SE a) -> SE a
pgmidi :: Sigs a => Maybe Int -> Channel -> (Msg -> SE a) -> SE a
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

ampCps :: Msg -> (D, D)
~~~

The second argument of `ampmidi` is a scaling factor
or maximum value for amplitude. The `ampCps` reads both parameters.
There also parameters for sensing control messages, aftertouch, bend and other midi-specific information. 


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
We can use the function `withInits` for this purpose or `withD`, `withDs` (for lists of `D`s), `withTab`.
It is used like this:

~~~haskell
oscil3 1 220 (sines [1, 0, 0.25]) `withD` 0.25
~~~

We have specified an aux parameter that changes the initial phase.
       
## Example (Hello Wrold)

The simplest possible program that produces a sound:

~~~haskell
module Main where
 
-- imports everything
import Csound.Base

-- Renders generated csd-file to the "tmp.csd".
-- press Ctrl-C to stop
main :: IO ()
main = dac $ osc 440
~~~

It plays a concert `A` with a signal.
The `osc` takes in a frequency and produces a pure tone signal.

## Example (a concert A with scores)
    
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
res = sco pureTone $ mel $ fmap temp [440, 330, 220]

-- Renders generated csd-file to the "tmp.csd", invokes the csound on it 
-- and directs the sound to speakers.
main :: IO ()
main = dac $ mix res
~~~

## Example (a concert A with event stream of scores)

Let's play that sequence forever with event streams.

~~~haskell
scores :: Sco D
scores = str 0.25 $ mel $ fmap temp [440, 330, 220]

main2 = dac $ sched pureTone $ fmap (const $ scores) $ metro 0.5
~~~

We create an event stream of ticks that happen twice a second 

~~~haskell
metro 0.5
~~~

Then we map all the ticks to the same scores:

~~~
fmap (const $ scores)
~~~

And we `schedule` the pureTone instrument from the previous example to play the notes.

## More examples
    
You can find many examples at:
   
* Examples in the archive with the source code of the library.
    
* A translation of the 
    [Amsterdam catalog of Csound computer instruments](https://github.com/anton-k/amsterdam)

* [csound-bits](https://github.com/anton-k/csound-bits) repository contains some ideas and sketches.  
       
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

