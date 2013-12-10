Quickstart guide
===========================

Installation
------------------

Let's install everything. First thing we need is a 
[csound compiler](http://www.csounds.com/resources/downloads/).
When it's installed properly we can type in the terminal:

~~~
> csound
~~~


It will print the long message. Ubunut/Dbian users can install the csound with `apt-get`:

~~~
> sudo apt-get install csound csound-gui
~~~

Next thing is a working Haskell environment with `ghc` and `cabal-install`
It can be installed with [Haskell Platform](http://www.haskell.org/platform/).
If it works to install the `csound-expression` we can type in the terminal:

~~~
> cabal install csound-expression
~~~

Let's have a break and take a cup of tea. The library contains 
a lot of modules to install. 

The first sound
------------------------

Let's start the `ghci` and load the main module `Csound.Base`. It exports
all modules:

~~~
> ghci
Prelude> :m +Csound.Base
Prelude Csound.Base> 
~~~

We can play a sine wave with 440 Hz:

~~~ 
Prelude Csound.Base> dac $ osc 440
~~~

Pressing Ctrl+C stops the sound. The expression `osc 440` makes the sine wave and
the function `dac` makes a file `tmp.csd` in the current directory invokes the `csound`
on it and sends the output to speakers.

Let's modify the signal. It has a constant amplitude envelope but we can change it 
with `linseg` function. 

~~~
Prelude Csound.Base> dac $ linseg [0, 1, 1, 3, 0] * osc 440
~~~

The function `linseg` creates a signal of linear segments

~~~
linseg [a, durA, b, durB, c, durC, ...]
~~~

It starts from the `a` value then in `durA` seconds it goes to the value `b`,
then in `durB` seconds it goes to the value `c` etc. It stays constant at the
final value. So the expression `linseg [0, 1, 1, 3, 0]` produces an linear envelope
that starts from `0` then reaches `1` (maximum for the sound signal) in one second
and finally goes to zero in 3 seconds.

Let's add a 5% vibrato to the sound:

~~~
Prelude Csound.Base> dac $ linseg [0, 1, 1, 3, 0] * osc (440 * (1 + 0.05 * osc 5))
~~~

We can factor out the envelope and apply it to the amplitude and the vibrato:

~~~
Prelude Csound.Base> let env = linseg [0, 1, 1, 3, 0]
Prelude Csound.Base> dac $ env * osc (440 * (1 + 0.05 * env * osc 5))
~~~

We can apply an envelope to any parameter. Let's make a slide, and change 
the waveform to sawtooth:

~~~
Prelude Csound.Base> dac $ env * saw (220 + 220 * env)
~~~

We can play a tone with three odd harmonics:


~~~
Prelude Csound.Base> dac $ env * oscBy (sines [1, 0, 0.5, 0, 0.25]) 440
~~~

We can play an mp3 file:

~~~
Prelude Csound.Base> dac $ ar2 $ mp3in $ text "/home/anton/listen/The Kinks-Waterloo Sunset.mp3"
~~~

We used the functions:

~~~
ar2   :: (Sig, Sig) -> (Sig, Sig)
text  :: String -> Str 
mp3in :: Tuple a => Str -> a
~~~

The function `mp3in` takes a Csound-string with the filename and produces a sound of the mp3 file.
The function `ar2` forces the output to be a pair of signals. The function `text`
converts the Haskell strings to Csound strings.

Primitive types
--------------------

Let's take a look at the types of the functions:

~~~
osc :: Sig -> Sig
saw :: Sig -> Sig
oscBy :: Tab -> Sig -> Sig
linseg :: [D] -> Sig
text :: String -> Str
mp3in :: Tuple a -> Str -> a
~~~

These functions almost all primitive types of the library. Let's look at the types:

`Sig`

:   a numeric signal (a stream of numbers)

`D`

:   a number

`Str`

:   a string

`Tab`

:   an array of numbers (See the module `Csound.Tab` for constructors of the arrays)

`Spec`

:   a spectrum of the signal

`Tuple`

:   type class of tuples of csound values

`Arg`

:   type class of scalar csound values (they are not signals or spectrums)

`Sigs`

:   type class of tuples of signals.

`Unit`

:   the csound tuple of zero length. It's constructed with the function `unit`

Side effects
--------------------------

Functions that produce a random values or procedures are wrapped
in the special type `SE` (or *side effect*). Let's look at the white noise
signature:

~~~
noise :: Sig -> Sig -> SE Sig
~~~

It takes an amplitude and the beta of low pass filter and returns a signal that
si wrapped in the `SE`. The type `SE` is a `Functor`, `Applicative` and `Monad`
so we can use the standard functions to process it:

~~~
Prelude Csound.Base> dac $ fmap (env * ) $ noise 1 0
~~~

Midi
-----------------------

We can make a sound in real time with midi keyboard.
Midi instrument has the type:

~~~
instr :: Sigs a => Msg -> SE a
~~~

The type `Msg` signifies the midi-messages, the type class `Sigs`
contains all tuples of signals. Let's make a simple midi instrument:

~~~
Prelude Csound.Base> let instr msg = return $ 0.5 * sig (ampmidi msg 1) * fades 0.01 0.5 * osc (sig $ cpsmidi msg)
~~~

The `fades` function adds fade in and fade out phases to the signal with
the specified given length. With the functions `ampmidi` and `cpsmidi`
we can read the amplitude and frequency from the midi message.
The function `sig` converts numbers to signals.

Now we can play it with the function `midi`:

~~~
Prelude Csound.Base> dac $ midi instr
~~~

If we don't have a hardware midi-device we can use a virtual midi-keyboard.
To do it we need to use `vdac` in place of `dac`:

~~~
Prelude Csound.Base> vdac $ midi instr
~~~

The function `midi` takes a mid-instrument and starts to listen 
on all channels for the events. If we want to specify the concrete channel 
we should use the funtion `midin`:

~~~
midin :: Sigs a => Int -> (Msg -> SE a) -> a
~~~

What if we want to play a pure tone on the first channel and the 
sawtooth on the second one? We can just add the resulting signals.
First let's take a waveform as a parameter for the instrument:

~~~
Prelude Csound.Base> let instr f msg = return $ 0.5 * sig (ampmidi msg 1) * fades 0.01 0.5 * f (sig $ cpsmidi msg)
~~~

Now let's set up everything and add some reverb:

~~~
Prelude Csound.Base> vdac $ 0.5 * nreverb ((midin 1 $ instr osc) + (midin 2 $ instr saw)) 1 0.2
~~~

The example shows that applying the effect is as simple as applying a function.

We can use a shortcut in defining the midi-instrument. 
There is a class that converts expressions to midi-instruments:

~~~
class MidiInstr a where
    type MidiInstrOut a :: *

    onMsg :: a -> Msg -> SE (MidiInstrOut a)
~~~

The only one method `onMsg` takes something and converts it to midi-instruments.
We can define an instrument from the wave form:

~~~
Prelude Csound.Base> vdac $ midi $ onMsg osc
~~~

Here the method takes a function that converts a frequency to signal.
The method gets the frequency from the midi message, converts it to 
a constant signal, applies the given waveform to it and multiplies everything
on the amount of the amplitude taken from the midi message. 

The type class `MidiInstr` contains many useful instances. You can convert
anything that expects an amplitude and frequency and produces the tuple of signals:

~~~
(D, D) -> Sig
(D, D) -> SE Sig
(D, D) -> (Sig, Sig)
(D, D) -> SE (Sig, Sig)
...
~~~

Event streams
---------------------

We can trigger an instrument with the event streams. Event stream 
contains some primitive scalar csound-values (they are not signals).

The most simple event stream is created with the function `metroE`:

~~~
metroE :: Sig -> Evt Unit
~~~

It creates a stream of repeating events. the first argument is 
the frequency of the repetition (in Hz). Let's create a stream of notes:

~~~
Prelude Csound.Base> let notes = fmap (const 440) $ metroE 2
~~~

Now we can trigger the instrument on the stream with the function `sched`:

~~~
sched :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, a) -> b
~~~

It takes an instrument, the event stream and produces the mixed signal.
An instrument is a function it takes a tuple of primitive Csound values
(type class `Arg`) and produces a tuple of signals (type class `Sigs`).
An event stream contains a list of pairs. It's duration of the note and 
the parameter for the instrument. 

Let's create a simple instrument:

~~~
Prelude Csound.Base> let instr x = return $ 0.5 * osc (sig x)
~~~

And triger the notes:

~~~
Prelude Csound.Base> dac $ sched instr $ withDur 0.25 notes

<interactive>:63:34:
    Couldn't match type `Integer' with `D'
    Expected type: Evt D
      Actual type: Evt Integer
    In the second argument of `withDur', namely `notes'
    In the second argument of `($)', namely `withDur 0.25 notes'
    In the second argument of `($)', namely
      `sched instr $ withDur 0.25 notes'
~~~

We've got an error message about using `Integer` in place of `D`.
Let's look at the type of the `notes`:

~~~
Prelude Csound.Base> :t notes
notes :: Evt Integer
~~~
 
The `ghci` converts numeric literals to integers, but we need a csound integer.
Let's give the `ghci` a hint:

~~~
Prelude Csound.Base> let notes = fmap (const (440::D)) $ metroE 2
Prelude Csound.Base> :t notes
notes :: Evt D
~~~

Now we can invoke the instrument and hear the result:

~~~
Prelude Csound.Base> dac $ sched instr $ withDur 0.25 notes
~~~

The function `withDur` appends a constant value for the duration of the note
to all events on the stream. Let's make out events more intersting.
We can play a list of events in the loop and make it faster:

~~~
Prelude Csound.Base> let notes = cycleE [440::D, 330, 220] $ metroE 4
~~~

Or we can play them at random and skip some elements with the inverse of the given frequency:

~~~
Prelude Csound.Base> let notes = randSkip 0.75 $ oneOf [440::D, 330, 220] $ metroE 4
~~~

Event streams are monoids. We can merge two event streams together with 
the function `mappend`. Let's merge two streams. One plays a note 440 every 3
beat and another plays a 330 every 7 beat.

~~~
Prelude Csound.Base> let notes = let m = metroE 4 in (mappend (every 0 [2] $ repeatE (440 :: D) m) (every 0 [7] $ repeatE 330 m))
~~~

We used the functions:

~~~
repeatE :: a -> Evt b -> Evt a              -- repeats the same event
every   :: Int -> [Int] -> Evt a -> Evt a   -- skips events from the stream 
                                            -- in beat patterns
every firstSkip beatPattern evt
~~~

Beat pattern is a sequence of integers. Every integer `n`
in the sequence means play one beat and skip `(n-1)` beats.

There are many functions defined for events. We can find them
in the module `Csound.Control.Evt`.

Score
------------------

We can play a list of notes. 

~~~
Prelude Csound.Base> let notes = CsdEventList 4 [(0, 1, 440::D), (1, 1, 220), (2, 2, 330)]
~~~

The type `CsdEventList` contains the total duration of the scores
and the list of triplets: `(startTime, duration, instrumentArguments)`. 
Now we can trigger the instrument:

~~~
Prelude Csound.Base> dac $ mix $ sco instr notes
~~~

The functions:

~~~
sco :: (Arg a, Sigs b, CsdSco f) => (a -> SE b) -> f a -> f (Mix b)
mix :: (Sigs a, CsdSco f) => f (Mix a) -> a
~~~

The funciton `sco` takes an instrument and a list of notes and
converts it to the list of sounds. The function `mix` converts
the list of sounds to the single sound. But what is the type class `CsdSco`?
It's a generic type of the values that can be converted to the `CsdEventList`
We can find out the complete definition in the module `Csound.Control.Instr`.

The library `csound-expression` is meant to be open to any score-generation libraries.
To use our favourite library we should make in instance for the type class `CsdSco`.

There is an instance for the type `Score` from the library `temporal-music-notation`.
It's in the separate package `temporal-csound`. We can install it with cabal-install: 

~~~
> cabal install temporal-csound
~~~

Now we can load the csound with module `Csound`:

~~~
> ghci
Prelude> :m +Csound
~~~

It reexports the module `Csound.Base` and brings the definition of the instance 
for `CsdSco` in the scope. There are seven main functions to remember:

~~~
-- Constructs a score with the single note (it lasts for one second)
temp :: a -> Score a

-- Constructs a score that contains nothing and lasts for some time.
rest :: Double -> Score a

-- Stretches the score in the time domain with the given coefficient.
-- It gets faster or slower.
str  :: Double -> Score a -> Score a

-- Delays all events with the given amount of time.
del  :: Double -> Score a -> Score a

-- A sequential composition of scores. It's short for melody.
-- It plays the scores one after the other.
mel  :: [Score a] -> Score a

-- A parallel composition. It's short for harmony.
-- It plays all scores at the same time.
har  :: [Score a] -> Score a

-- Repeats the score several times.
loop :: Int -> Score a -> Score a
~~~

Let's make a simple tune:

~~~
Prelude Csound> let ns = fmap temp [220, 330, 440::D]
Prelude Csound> let notes = str 0.25 $ mel [loop 2 (mel ns), har ns]
Prelude Csound> dac $ mix $ sco instr notes
~~~

We can hear the distortion in the final chord. A sound signal is
a function that can not exceed the value of 1. It's clipped before
it's send to the speakers. We can scale the sound to remove the distortion:

~~~
Prelude Csound> dac $ 0.5 * (mix $ sco instr notes)
~~~

What if we don't want or tune to fade out?
We can apply an effect to it. First let's define
a fader instrument. It takes a signal and multiplies
it with an linear envelope:
~~~
Prelude Csound> let fader x = return $ linseg [1, idur * 0.5, 1, idur * 0.5, 0] * x
~~~

The constant `idur` is the hack that let's us querry the total duration of the note.
If you know the Csound it's equivallent to the argument `p3` of the instrument.
So the fader let's the signal to be unchanged for the first half of the note
and then reduces it to zero in the second one.

Now we can use the `eff` function:

~~~
eff :: (Sigs a, Sigs b, CsdSco f) => (a -> SE b) -> f (Mix a) -> f (Mix b)
~~~

It applies the effect to the unmixed list of sounds:

~~~
Prelude Csound> dac $ mix $ eff fader $ sco instr notes
~~~

Offline rendering
-------------------

Sometimes we can render the csound file to sound files much faster
then real-time rendering. It's more convinient to listen to the result
in the media player. we can take a closer look at some details.

We can render the file without playing (the function `csd`) and
specify the wav-file as the output in the options.

~~~
Prelude Csound> csdBy (setOutput "tmp.wav") $ mix $ eff fader $ sco instr notes
~~~

It saves the file to the `tmp.csd` with output set to `tmp.wav` 
and renders it with `csound`. The `csound` then creates a wav-file.

And now we can listen to it in the player:

~~~
Prelude Csound> :!mplayer tmp.wav
~~~

There are shortcut functions for Linux-users: `mplayer`, `totem`.
They save the output to file and invoke the given media-player on it.

~~~
Prelude Csound> mplayer $ mix $ eff fader $ sco instr notes
~~~

Adaptors
--------------------

Instrumwnts can return a tuple of signals or a single signal or
a tuple signals wrapped in the type `SE`. There are many different
variants of the output. But often we want to process the output as 
a single signal. The output is always a container of signals.
To simplify this task there is a class `SigSpace`:

~~~
class SigSpace a where
    mapSig  :: (Sig -> Sig) -> a -> a
    bindSig :: (Sig -> SE Sig) -> a -> SE a
~~~

With method from this class we can easily apply the effects to the
different types of the output. There is a very often used special case:

~~~
mul :: SigSpace a => Sig -> a -> a
mul k = mapSig ( * k)
~~~

It scales the output.

Sometimes our intruments are pure functions. But all functions
that invoke instruments require them to return a result that is wrapped
in the type `SE`. Often we can lift the instrument on the fly
with methods from the special classes:

~~~
class Instr a where
    type InstrOut a :: *
    type InstrIn  a :: *
    onArg :: a -> InstrIn a -> SE (InstrOut a)   

class CpsInstr a where
    type CpsInstrOut a :: *
    onCps :: a -> (D, D) -> SE (CpsInstrOut a)

class AmpInstr a where
    type AmpInstrOut a :: *
    onamp :: a -> D -> SE (AmpInstrOut a)
~~~

The method `onArg` unifies the pure and non pure instruments.
We can use it like this:

~~~
Prelude Csound> let notes = temp (440 :: D)
Prelude Csound> let instr cps = osc $ sig cps
Prelude Csound> dac $ mix $ sco (onArg instr) notes
~~~

Now we don't need to wrap the output in the type `SE` with the method `return`.

The method `onCps` defines the instruments that take an amplitude
and frequency as input. For example, we can convert to this type of instrument
a waveform:

~~~
Prelude Csound> let notes = temp (0.5 :: D, 440 :: D)
Prelude Csound> dac $ mix $ sco (onCps osc) notes
~~~

The method `onAmp` defines the instruments that take only an amplitude.
They are drum sounds or noises. It can construct instruments from constants
by scaling the sound with the input amplitude.

~~~
Prelude Csound> let notes = str 0.25 $ loop 4 $ mel [temp (0.5::D), rest 1]
Prelude Csound> let instr = noise 1 0
Prelude Csound> dac $ mix $ sco (onAmp instr) notes
~~~

Catalog of the instruments
------------------------------

We can find many pre-defined instruments in the package `csound-catalog`.
Let's install it:

~~~
> cabal install csound-catalog
~~~

Let's try it in the interpreter:

~~~
> ghci
Prelude> :m +Csound
Prelude> :m +Csound.Catalog
Prelude Csound Csound.Catalog> vdac $ mul 0.3 $ midi $ onMsg (mul (fades 0.01 3) . vibraphone1)
~~~

or

~~~
Prelude Csound Csound.Catalog> vdac $ mul 0.5 $ midi $ onMsg (mul (fades 0.01 3) . delayedString)
~~~

or

~~~
Prelude Csound Csound.Catalog> vdac $ mul 0.15 $ bayAtNight $ midi $ onMsg (mul (fades 1 2) . stringPad 1)
~~~

Gui elements
---------------------------------

With Gui elements (`Csound.control.Gui`) we can update 
the synth paramters online. There are sliders, knobs, numeric fields, rollers. 
The Gui element contains three parts:

~~~
SE (Gui, Input a, Output a)

Gui                     -- visual representation of the element
Input  a = a            -- reads  the current value 
Output a = a -> SE ()   -- writes the value to the element
~~~

Some elements can only read values (no ountput), some of the can only show 
the values (no input), some of them are static elements (no inputs and outputs).  

Let's create a siple pure tone sound and update volume and frequency
with sliders:

~~~
import Csound.Base

main = dac $ do
    (gVol, vol) <- slider "volume"    (linSpan 0 1) 0.5
    (gCps, cps) <- slider "frequency" (expSpan 100 1000) 440
    panel $ ver [gVol, gCps]
    return $ vol * osc cps
~~~

The function slider expects a label, value diapason and initial value.
It returns a pair of visual representation and the current value:

~~~
type Source a = SE (Gui, Input a)

slider :: String -> ValSpan -> Double -> Source a
~~~

First, we create to sliders for volume and frequency:

~~~~
    (gVol, vol) <- slider "volume"    (linSpan 0 1)      0.5
    (gCps, cps) <- slider "frequency" (expSpan 100 1000) 440
~~~

The first has linear diapason from 0 to 1 (linSpan) and the
second has exponential diapason from (expSpan). 
Then we place our GUI elements on the screen. we
create a panel that contains the elements with vertical placement:

~~~
    panel $ ver [gVol, gCps]
~~~

The sliders are automatically aligned. We can find the other layout
function in the module `Csound.Control.Gui.Layout`. We 
can modify the appearance of the elements with functions
from the module `Csound.Control.Props`.

At the end we return the signal that depends on 
the values of the GUI elements:

~~~
    return $ vol * osc cps
~~~

Keyboard events
-------------------------

We can listen for the keyboard events with functions: `keyIn`, `charOn`, `charOff`.

~~~
data KeyEvt = Press Key | Release key
data Key = CharKey Char | F1 | F2 | ... | LeftShift | RightShift | ...

keyIn   :: KeyEvt -> Evt Unit
charOn  :: Char -> Evt Unit
charOff :: Char -> Evt Unit
~~~

`charOn` and `charOff` are handy shortcuts for

~~~
charOn  = keyIn . Press   . CharKey
charOff = keyIn . Release . CharKey
~~~

For example, we can trigger the note with key 'a':

~~~
instr _ = return $ mul (fades 0.5 1) $ osc 440

res = schedUntil instr (charOn 'a') (charOff 'a')

main = dac res
~~~

The `schedUntil` funtion triggers the instrument with
the first event stream and holds the notes while 
the second event stream is silent.





