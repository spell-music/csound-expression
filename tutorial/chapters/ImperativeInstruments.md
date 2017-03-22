Imperative instruments (or Csound way)
=======================================================

The CE let's do the music design with functional primitives.
The instrument is a function and w eapply it to the bunch of notes.
But some features of the Csound language con not be expressed in that way.
For this reasom there are primitives that let us invoke the instruments in Csound way.

What do I mean by Csound way? In Csound each instrument has a unique identifier (it's integer or string).
It looks like this in csound code:

~~~
instr InstrName

... body of the instrument ...
... some useful work goes on here ...

endin
~~~

With that identifier `InstrName` we can schedule an event or invoke the instrument with the note:

~~~
schedule InstrName, startTime, duration, arg1, arg2, ... , argN
~~~

We can write it anywhere in the code and the given instrument is going to be invoked.
Note that Csound handles polyphony for us. If we trigger the instrument with the same 
name twice two separate invocations are going to be scheduled. Each note is played within 
it's own context.

Csound has some interesting tricks. If duration is negative the instrument is gong to be played forever.
Until we invoke the `turnoff` opcode within the body of the instrument or we call `turnoff2` which
allows us to stop the instrument by name. 

Another interesting trick is to be able to schedule the notes with identifiers. We can start the note
with integer identifier and stop exactly that note when we need it. This behavior is done by using
fractional part of the instrument name (it works only instrument name is an integer).

Suppose we have an instrument called 12. Than we can trigger three infinite notes:

~~~
schedule 12.1, 0, -1, 220
schedule 12.2, 0, -1, 330
schedule 12.3, 0, -1, 440
~~~

And then we can stop the specific note:

~~~
turnoff2 12.2
~~~

Another interesting trick is to be able to update the values of the instrument arguments
without stopping the instrument. We can do it if the instrument integer name is negative.

In CE all this kitchen is hidden and instrument names are automatically generated.
Right now I don't have a suitable functional model for behaviors like this. But there are
cases when they are very useful. So finally I decided to expose the Csound-like functions
for creation of instruments:

### Instrument reference

There is a special opaque data type for instrument names (or references). It's called `InstrRef`:

~~~haskell
data InstrRef a = ...
~~~
 
It has a single parameter which is an argument of the instrument body function.
We can create instrument references with functions:

For procedures (produce no output):

~~~haskell
newInstr :: Arg a => (a -> SE ()) -> SE (InstrRef a)
~~~

For signal generating functions:

~~~haskell
newOutInstr :: (Sigs b, Arg a) => (a -> SE b) -> SE (InstrRef a, b)
~~~

The `newOutInstr` returns a reference and a variable that holds a mixed output
of all invocations of the given instrument.

Then we can schedule an event:

~~~haskell
scheduleEvent :: Arg a => InstrRef a -> D -> D -> a -> SE ()
scheduleEvent instrRef startTime duration arguments
~~~

Let's create a simple example. We are going to do it not in the interpreter but in text file:

~~~haskell
module Main where

import Csound.Base

instr cps = return $ fades 0.05 0.1 * osc (sig cps)

main = dac $ do
    (ref, outSig) <- newOutInstr instr
    runEvt (cycleE [220, 330, 440, 660] $ metro 2) $ \cps -> scheduleEvent ref 0 0.2 cps
    return outSig
~~~

Let's import the file in the ghci and run the main function. 
With `newOutInstr` we create an instrument reference and result signal.
With `runEvt` we trigger the procedure that schedules the notes when
the event happens on the even stream. The event stream cycles over list
of frequencies with frequency 2 Hz.

With function `turnoff2` we can stop a held note or given invocation of the instrument.

~~~haskell
turnoff2 :: InstrRef a -> Sig -> Sig -> SE ()
turnoff2 instrRef mode releaseTime
~~~

The mode is sum of the following values: 

* 0, 1, or 2: turn off all instances (0), oldest only (1), or newest only (2) 

* 4: only turn off notes with exactly matching (fractional) instrument number, rather than ignoring fractional part 

* 8: only turn off notes with indefinite duration (p3 < 0 or MIDI) 

`releaseTime`  if non-zero, the turned off instances are allowed to release, otherwise are deactivated immediately (possibly resulting in clicks).

Let's look at simple example:

~~~haskell
main = dac $ do
    (ref, outSig) <- newOutInstr instr
    runEvt loadbang $ \_ -> scheduleEvent ref 0 (-1) 220
    runEvt (impulseE 2) $ \_ -> turnoff2 ref 0 0    
    return outSig
~~~

With `loadbang` we schedule an event right away. With `impulseE` we create an event that is going to be happen later. 
First we start an infinite instance of the event and then we stop it after 2 seconds.

### Fractional instrument names

We can start or stop specific instance of the instrument with fractional instrument names.

~~~haskell
addFracInstrRef :: D -> D -> InstrRef a -> InstrRef a
addFracInstrRef maxSize noteId instrRef = ...
~~~

The `maxSize` is the maximum size of the note identifiers. The noteId is the note identifier.
This function adds fractional part to the instrument reference.

Then we can use `scheduleEvent` and `turnoff2` functions to start and stop the specific notes.

### Negation of the instrument name

We can negate the instrument name with function:

~~~haskell
negateInstrRef :: InstrRef a -> InstrRef a
~~~

--------------------------------------------------------

* <= [Creating VST-plugins with Cabbage](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/CabbageTutorial.md)

* => Happy Haskelling / Csounding

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)
