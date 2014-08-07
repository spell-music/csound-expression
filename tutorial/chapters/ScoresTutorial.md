
Scores
======================================

Right now we know how to produce a single sound:

~~~
> dac $ osc (220 + 50 * osc 0.5)
~~~

We know how to trigger an instrument with a  midi device:

~~~
> vdac $ midi $ onMsg $ \x -> osc (x + 50 * osc 0.5)
~~~

We know how to trigger the sound with an event stream:

~~~
> dac $ sched (const $ return $ osc 440) $ withDur 0.25 $ metroE 2
~~~

We can even trigger the instruments with GUI. Since some of them
can produce event streams.

But can we play tabs or scores? 

Triggering an instrument with the scores
----------------------------------

The Csound originally have
the two parts: orchestra and scores. The orchestra lists all the instruments
and in the scores we can find the notes that trigger the instruments.
The note consists of four parts:

~~~
(nameOfTheInstrument, startTime, duration, argumentsForTheInstrument)
~~~

In the Csound it's written like this:

~~~
i2 0 5 0.5 440
~~~

It plays the instrument 2 at 0 second for a 5 seconds. The first argument
is 0.5 (can be an amplitude) and the second is 440 (can be a frequency).

### The type CsdEventList

The scores contains a list of statements like this. In the csound-expression we encode this 
information with a special type `CsdEventList`:

~~~
type CsdEvent a = (Double, Double, a)

data CsdEventList a = CsdEventList 
  { csdEventListDuration :: Double
  , csdEventListNotes    :: [CsdEvent a]
  }
~~~

The `CsdEventList` contains all notes for a single instrument to play.

We can trigger the instrument with the function `sco`:

~~~
sco :: (CsdSco f, Arg a, Sigs b) => (a -> SE b) -> f a -> f (Mix b)
sco instrument scores = ...
~~~

### Type class `CsdSco`


But where is the `CsdEventList`? The  `CsdEventList` is an instance
of the type class `CsdSco`:

~~~
class Functor f => CsdSco f where
	toCsdEventList :: f a -> CsdEventList a
	singleCsdEvent :: CsdEvent a -> f a
~~~

The `CsdEventList` is a cannonical representation for the score-like types.
The intention is to make the library open to many score-representatinons.
We should not force the user to use the most proper variant. We let it be
whatever it's best for the user.

There are unknown types: `Arg` and `Mix`. Let's study them briefly.

### Arguments for the instrument

At the moment of triggering an instrument can take only certain types
of arguments. They are constant numbers (`D`), strings (`Str`) or arrays (`Tab`).
The type class `Arg` defines this restriction:

~~~
class Tuple a => Arg a

Arg D	 
Arg Str	 
Arg Unit	 
Arg Tab	 
(Arg a, Arg b) => Arg (a, b)	 
(Arg a, Arg b, Arg c) => Arg (a, b, c)	 
(Arg a, Arg b, Arg c, Arg d) => Arg (a, b, c, d)
...
~~~

We can use three primitive types or tuples of them.
An instrument that needs no arguments takes in an empty tuple `Unit`.


### Getting the `Mix` done

The result is a score again but the value of the score is wrapped 
in the type `Mix`:

~~~
sco :: (CsdSco f, Arg a, Sigs b) => (a -> SE b) -> f a -> f (Mix b)
~~~

It's a special type that represents a scores of sound signals.
When we make a score of signals we can not use the sound signals
directly. To get the result we have to mix it down to the single signal:

~~~
mix :: (Sigs a, CsdSco f) => f (Mix a) -> a
~~~

Let's play a chord:

~~~
> dac $ mix $ sco (return . osc . sig) (CsdEventList 3 [(0, 1, 220), (1, 1, 330), (2, 1, 440)])
~~~

### Processing the unmixed signals

We can not process tyhe signals directly. If we want to apply an effect we 
need to use a function `eff`:

~~~
eff :: (CsdSco f, Sigs a, Sigs b) => (a -> SE b) -> f (Mix a) -> f (Mix b)
~~~

Let's process the output:

~~~
> let x = sco (return . osc . sig) (CsdEventList 3 [(0, 1, 220), (1, 1, 330), (2, 1, 440)])
> dac $ mix $ eff (return . mul (linseg [0, 1.5, 1, 1.5, 0])) x
~~~

We apply fade in and fade out to the result signal.

### Playing in the loop

Sometimes the scores we made is so interesting that we want
to listen to it over and over again. We can make it with a function:

~~~
mixLoop :: (CsdSco f, Sigs a) => f (Mix a) -> a
~~~

It takes unmixed scores of signals and produces a signal that loops over
the give score.

Let's play the chord in the loop:

~~~
> dac $ mixLoop x
~~~

The better scores with temporal-music-notation
------------------------------------------

Composability of the sound signals
------------------------------------------

Instrument adapters
-----------------------------------------

Instruments can return a tuple of signals or a single signal or
a tuple of signals wrapped in the type `SE`. There are many different
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
class Outs a where
    type SigOuts a :: *
    toOuts :: a -> SE (SigOuts a)
    
onArg :: Outs b => (a -> b) -> (a -> SE (SigOuts b))   

class CpsInstr a where
    type CpsInstrOut a :: *
    onCps :: a -> (D, D) -> SE (CpsInstrOut a)

class AmpInstr a where
    type AmpInstrOut a :: *
    onamp :: a -> D -> SE (AmpInstrOut a)
~~~

The method `onArg` unifies the pure and non-pure instruments.
We can use it like this:

~~~
> let notes = temp (440 :: D)
> let instr cps = osc $ sig cps
> dac $ mix $ sco (onArg instr) notes
~~~

So we don't need to wrap the output in the type `SE` with the method `return`.

The method `onCps` defines an instrument that takes amplitude
and frequency as input. For example, we can convert to this type of instrument
a waveform:

~~~
> let notes = temp (0.5 :: D, 440 :: D)
> dac $ mix $ sco (onCps osc) notes
~~~

The method `onAmp` defines an instrument that takes only amplitude.
They are drum sounds or noises. It can construct instruments from constants
by scaling the sound with the input amplitude.

~~~
> let notes = str 0.25 $ loop 4 $ mel [temp (0.5::D), rest 1]
> let instr = noise 1 0
> dac $ mix $ sco (onAmp instr) notes
~~~

The sound processing procedures
-----------------------------------------

Sometimes we don't want to hear anything or play anything. 
We just want the Csound to do something usefull. We can use it
as a sound processor. We can read the sound from the file transform it
and save it to the file. 

It's an easy thing to do. Since the `RenderCsd` contains the instance

~~~
RenderCsd (SE ())
~~~

We can just use the function

~~~
csd :: RenderCsd a => a -> IO ()
~~~

To invoke the csound on the rendered file. 
Also we can render the procedure to the csound file and
use the csound to invoke it. That's how we can skip the rendering step. 

Let's process a sound file. We can create a sound file that contains a pure tone:

~~~
> let len = 2
> csd $ writeWav1 "osc220.wav" $ setDur len $ mul (linseg [0, 1, 1, 1, 0]) $ osc 220
~~~

We write the files to the disk with function:

~~~
writeWav1 :: String -> Sig -> SE ()
writeWav1 fileName asig
~~~

It writes mono signals to wav files. There is a function for stereo signals:

~~~
writeWav :: String -> (Sig, Sig) -> SE ()
writeWav fileName sigs
~~~

Note how fast it was. We don't need to wait for the signal to be heard.
We can produce a twenty minutes in just half a minute. It can be usefull
for generated ambient music. 

Let's process this file and write the output to another file. 
We are going to transpose this file. We are going to make it an octave higher:

~~~
> csd $ writeWav "osc440.wav" $ setDur (lengthSnd "osc220.wav") $ mapSig (scaleSpec 2) $ readSnd "osc220.wav"
~~~

We used the function `lengthSnd` to calculate the length of the first file in seconds.

There is a more generic function to write signals to files:

~~~
writeSigs :: FormatType -> SampleFormat -> String -> [Sig] -> SE ()
writeSigs formatType sdampleFormat fileName sigs = ...
~~~

It writes several signals to the disk. We can specify the format type. Only four formats are allowed:
`Wav`, `Aiff`, `Raw` and `Ircam`. Also we can specify s sample format. You can study all sample formats
in the docs (see the module `Csound.Air` at the section *Writing sound files*).

----------------------------------------------------

* <= [Events](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/EventsTutorial.md)

* => [Real-world instruments show case](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/InstrumentsShowCase.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)