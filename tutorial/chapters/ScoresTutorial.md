
Scores
======================================

Right now we know how to produce a single sound:

~~~haskell
> dac $ osc (220 + 50 * osc 0.5)
~~~

We know how to trigger an instrument with a  midi device:

~~~haskell
> vdac $ midi $ onMsg $ \x -> osc (x + 50 * osc 0.5)
~~~

We know how to trigger the sound with an event stream:

~~~haskell
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

~~~haskell
(nameOfTheInstrument, startTime, duration, argumentsForTheInstrument)
~~~

In the Csound it's written like this:

~~~haskell
i2 0 5 0.5 440
~~~

It plays the instrument 2 at 0 second for a 5 seconds. The first argument
is 0.5 (can be an amplitude) and the second is 440 (can be a frequency).

### The type CsdEventList

The scores contains a list of statements like this. In the csound-expression we encode this 
information with a special type `CsdEventList`:

~~~haskell
type CsdEvent a = (Double, Double, a)

data CsdEventList a = CsdEventList 
  { csdEventListDuration :: Double
  , csdEventListNotes    :: [CsdEvent a]
  }
~~~

The `CsdEventList` contains all notes for a single instrument to play.

We can trigger the instrument with the function `sco`:

~~~haskell
sco :: (CsdSco f, Arg a, Sigs b) => (a -> SE b) -> f a -> f (Mix b)
sco instrument scores = ...
~~~

### Type class `CsdSco`


But where is the `CsdEventList`? The  `CsdEventList` is an instance
of the type class `CsdSco`:

~~~haskell
class Functor f => CsdSco f where
	toCsdEventList :: f a -> CsdEventList a
	singleCsdEvent :: CsdEvent a -> f a
~~~

The `CsdEventList` is a canonical representation for the score-like types.
The intention is to make the library open to many score-representations.
We should not force the user to use the most proper variant. We let it be
whatever it's best for the user.

There are unknown types: `Arg` and `Mix`. Let's study them briefly.

### Arguments for the instrument

At the moment of triggering an instrument can take only certain types
of arguments. They are constant numbers (`D`), strings (`Str`) or arrays (`Tab`).
The type class `Arg` defines this restriction:

~~~haskell
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

~~~haskell
sco :: (CsdSco f, Arg a, Sigs b) => (a -> SE b) -> f a -> f (Mix b)
~~~

It's a special type that represents a scores of sound signals.
When we make a score of signals we can not use the sound signals
directly. To get the result we have to mix it down to the single signal:

~~~haskell
mix :: (Sigs a, CsdSco f) => f (Mix a) -> a
~~~

Let's play a chord:

~~~haskell
> dac $ mix $ sco (return . osc . sig) (CsdEventList 3 [(0, 1, 220), (1, 1, 330), (2, 1, 440)])
~~~

### Processing the unmixed signals

We can not process the signals directly. If we want to apply an effect we 
need to use a function `eff`:

~~~haskell
eff :: (CsdSco f, Sigs a, Sigs b) => (a -> SE b) -> f (Mix a) -> f (Mix b)
~~~

Let's process the output:

~~~haskell
> let x = sco (return . osc . sig) (CsdEventList 3 [(0, 1, 220), (1, 1, 330), (2, 1, 440)])
> dac $ mix $ eff (return . mul (linseg [0, 1.5, 1, 1.5, 0])) x
~~~

We apply fade in and fade out to the result signal.

### Playing in the loop

Sometimes the scores we made is so interesting that we want
to listen to it over and over again. We can make it with a function:

~~~haskell
mixLoop :: (CsdSco f, Sigs a) => f (Mix a) -> a
~~~

It takes unmixed scores of signals and produces a signal that loops over
the give score.

Let's play the chord in the loop:

~~~haskell
> dac $ mixLoop x
~~~

The better scores with temporal-music-notation
------------------------------------------

Notice how boring is the creation of the value for scores:

~~~haskell
> CsdEventList 3 [(0, 1, 220), (1, 1, 330), (2, 1, 440)]
~~~

It's on purpose. The `CsdEventList` is not to be used directly. The user
should seek for better alternatives. The `CsdEventList` is just a canonical
type and it does not provide any functions to make the construction
of scores easier.

In this section we are going to study an alternative. There is only one right now.
There is an instance of `CsdSco` for the type `Score` from the package `temporal-music-notation`.
The package defines handy function for building complex scores out of simpler ones.

To use this package we have to install the package `temporal-csound`. 
It defines the instance and provides some other useful things. 

~~~haskell
> :q
leaving ghci...
$ cabal-install temporal-csound
~~~

When everything is properly installed we can load in the interpreter
the module `Csound`. 

~~~haskell
$ ghci
> :m +Csound
~~~

Let's study the main functions!

### The basic functions

The score is something that contains a list of notes and the total duration (in seconds).
The note contains the time when it starts, the duration, and the parameter.

The interesting thing is that we can build complex scores out of simple ones.
there is a tiny set of functions to do it. The simplest functions are:

~~~haskell
temp :: a -> Score a
rest :: Dounle -> Score a
~~~

The function `temp` lifts the value to `Score`. It creates a `Score`
that lasts for one second and contains a single note that starts right now
and lasts for one second. The note contains the value that was given in 
the first argument of the `temp`.

The function `rest` creates a `Score` that contains no notes and lasts 
for a given amount of time (in seconds).

That's how we can build Scores out of simple values.
Let's look at how we can combine the Scores.

There are four simple functions:

~~~haskell
str, del :: Double -> Score a -> Score a

mel, har :: [Score a] -> Score a
~~~

The `str` is short for `stretch`. It stretches the notes in time domain.
That's how we can make a single note that lasts for 2 seconds:

~~~haskell
> str 2 $ temp "Hello"
~~~

The `del` is short for *delay*. It delays all events by the
given amount of time.


The two other functions can group the lists of scores. 
The first one, `mel` is short for *melody*. It sequence 
one note after another. The second one `har` is short for 
`harmony`. It plays several Scores together.

Let's arrange the list of letters one after another:

~~~haskell
> mel $ fmap temp "Hello"
~~~

We can use the function `render` to see the list of events in the score:

~~~haskell
> mapM_ print $ render $ mel $ fmap temp "Hello"
Event {eventStart = 0.0, eventDur = 1.0, eventContent = 'H'}
Event {eventStart = 1.0, eventDur = 1.0, eventContent = 'e'}
Event {eventStart = 2.0, eventDur = 1.0, eventContent = 'l'}
Event {eventStart = 3.0, eventDur = 1.0, eventContent = 'l'}
Event {eventStart = 4.0, eventDur = 1.0, eventContent = 'o'}
~~~

We can see that the start time was properly arranged.
We can query the duration of the scores with the function `dur`:

~~~haskell
> dur $ mel $ fmap temp "Hello"
5.0
~~~

Let's arrange the letters so that they are played in chord:

~~~haskell
> dur $ har $ fmap temp "Hello"
1.0
> mapM_ print $ render $ har $ fmap temp "Hello"
Event {eventStart = 0.0, eventDur = 1.0, eventContent = 'H'}
Event {eventStart = 0.0, eventDur = 1.0, eventContent = 'e'}
Event {eventStart = 0.0, eventDur = 1.0, eventContent = 'l'}
Event {eventStart = 0.0, eventDur = 1.0, eventContent = 'l'}
Event {eventStart = 0.0, eventDur = 1.0, eventContent = 'o'}
~~~

We can see that every note starts at the same time and lasts
the same amount of time. we can combine both results together:

~~~haskell
> let e1 = mel $ fmap temp "Hello"
> let e2 = har $ fmap temp "World"
> dur $ mel [e1, str 4 e2]
9.0
~~~

Nice thing is that score is a Functor:

~~~haskell
> :m +Data.Char
> mapM_ print $ render $ fmap toUpper $ mel [e1, str 4 e2]
Event {eventStart = 0.0, eventDur = 1.0, eventContent = 'H'}
Event {eventStart = 1.0, eventDur = 1.0, eventContent = 'E'}
Event {eventStart = 2.0, eventDur = 1.0, eventContent = 'L'}
Event {eventStart = 3.0, eventDur = 1.0, eventContent = 'L'}
Event {eventStart = 4.0, eventDur = 1.0, eventContent = 'O'}
Event {eventStart = 5.0, eventDur = 4.0, eventContent = 'W'}
Event {eventStart = 5.0, eventDur = 4.0, eventContent = 'O'}
Event {eventStart = 5.0, eventDur = 4.0, eventContent = 'R'}
Event {eventStart = 5.0, eventDur = 4.0, eventContent = 'L'}
Event {eventStart = 5.0, eventDur = 4.0, eventContent = 'D'}
~~~

But enough with strings let's play something.

Let's create a scores:

~~~haskell
> let notes = fmap temp [1, 5/4, 3/2, 2]
> let n1 = mel notes
> let n2 = har notes
> let ns = fmap (double . (440 * )) $ str 0.5 $ mel [n1, str 4 n2]
~~~

Let's invoke it with an instrument:

~~~haskell
> dac $ mul 0.15 $ mix $ sco (return . mul (fades 0.1 0.1) . osc . sig) ns 
~~~

The type `Score` is an instance of `CsdSco` so we can use it with 
functions `sco` and `mix`. What's interesting we can combine the scores of
signals:

~~~haskell
> let q f = sco (return . mul (fades 0.1 0.1) . f . sig) ns
> let q1 = q osc
> let q2 = q saw
> dac $ mul 0.1 $ mix $ mel [q1, q2]
~~~

We can use the functions `str`, `mel`, `har` and other before mixing.
Even if they contain the signals.


### Other useful functions

Let's briefly study some other functions:

Replicates the score N-times:

~~~haskell
loop :: Int -> Score a -> Score a
~~~

Takes only a portion of the notes. It creates aclip
that contains only notes that lie within a given interval:

~~~haskell
slice :: Double -> Double -> Score a -> Score a
~~~

Filters the events:

~~~haskell
filterEvents :: (Event Dur a -> Bool) -> Score a -> Score a
~~~

The package `temporal-music-notation` is not only for arrangement of Scores.
It defines the types for `Volume`, `Pitch` and `Note`. there are plenty of functions
to make the creation of scores easy. The detailed study goes beyond the scope
of this article but interested user can study the docs for the package.

Composability of the sound signals
------------------------------------------

What's interesting is that we can use all of the sound producing
techniques together. The sound that is created with an instrument
and event stream is just a signal. We can use it in our midi-device
or in another instrument and then we can trigger it with scores. 
The possibilities are infinite.

Let's create a pulse of pure sines:

~~~haskell
> let instr x = mul 0.5 $ 
	sched (return . mlp 1500 0.6 . mul (fades 0.05 0.05) . osc . sig) $ 
	withDur 0.2 $ devt x $  metroE 2
~~~

The result of the function `instr` is just a signal we can use
it like any other signal. We can trigger the instrument with a midi device:

~~~haskell
> vdac $ mul 0.5 $ midi $ onMsg instr
~~~

We can process it somehow:

~~~haskell
> vdac $ mul 0.5 $ midi $ onMsg (mul (utri 0.5 * fades 0.1 1.5) . instr)
~~~

We can make our instrument little bit more interesting. We are going to 
play pure major chords:

~~~haskell
> let instr x = mul 0.25 $ 
		sched (return . mlp 1500 0.6 . mul (fades 0.05 0.05) . osc . sig) $ 
		withDur 0.2 $ fmap (* x) $ cycleE [1, 5/4, 3/2, 2] $  metroE 8
~~~

The midi instrument is the same:

~~~haskell
> vdac $ mul 0.5 $ midi $ onMsg (mul (utri 0.5 * fades 0.1 1.5) . instr)
~~~
 
Instrument adapters
-----------------------------------------

Instruments can return a tuple of signals or a single signal or
a tuple of signals wrapped in the type `SE`. There are many different
variants of the output. But often we want to process the output as 
a single signal. The output is always a container of signals.
To simplify this task there is a class `SigSpace`:

~~~haskell
class SigSpace a where
    mapSig  :: (Sig -> Sig) -> a -> a
    bindSig :: (Sig -> SE Sig) -> a -> SE a
~~~

With method from this class we can easily apply the effects to the
different types of the output. There is a very often used special case:

~~~haskell
mul :: SigSpace a => Sig -> a -> a
mul k = mapSig ( * k)
~~~

It scales the output.

Sometimes our instruments are pure functions. But all functions
that invoke instruments require them to return a result that is wrapped
in the type `SE`. Often we can lift the instrument on the fly
with methods from the special classes:

~~~haskell
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

~~~haskell
> let notes = temp (440 :: D)
> let instr cps = osc $ sig cps
> dac $ mix $ sco (onArg instr) notes
~~~

So we don't need to wrap the output in the type `SE` with the method `return`.

The method `onCps` defines an instrument that takes amplitude
and frequency as input. For example, we can convert to this type of instrument
a waveform:

~~~haskell
> let notes = temp (0.5 :: D, 440 :: D)
> dac $ mix $ sco (onCps osc) notes
~~~

The method `onAmp` defines an instrument that takes only amplitude.
They are drum sounds or noises. It can construct instruments from constants
by scaling the sound with the input amplitude.

~~~haskell
> let notes = str 0.25 $ loop 4 $ mel [temp (0.5::D), rest 1]
> let instr = noise 1 0
> dac $ mix $ sco (onAmp instr) notes
~~~

The sound processing procedures
-----------------------------------------

Sometimes we don't want to hear anything or play anything. 
We just want the Csound to do something useful. We can use it
as a sound processor. We can read the sound from the file transform it
and save it to the file. 

It's an easy thing to do. Since the `RenderCsd` contains the instance

~~~haskell
RenderCsd (SE ())
~~~

We can just use the function

~~~haskell
csd :: RenderCsd a => a -> IO ()
~~~

To invoke the Csound on the rendered file. 
Also we can render the procedure to the Csound file and
use the Csound to invoke it. That's how we can skip the rendering step. 

Let's process a sound file. We can create a sound file that contains a pure tone:

~~~haskell
> let len = 2
> csd $ writeWav1 "osc220.wav" $ setDur len $ mul (linseg [0, 1, 1, 1, 0]) $ osc 220
~~~

We write the files to the disk with function:

~~~haskell
writeWav1 :: String -> Sig -> SE ()
writeWav1 fileName asig
~~~

It writes mono signals to wav-files. There is a function for stereo signals:

~~~haskell
writeWav :: String -> (Sig, Sig) -> SE ()
writeWav fileName sigs
~~~

Note how fast it was. We don't need to wait for the signal to be heard.
We can produce a twenty minutes in just half a minute. It can be useful
for generated ambient music. 

Let's process this file and write the output to another file. 
We are going to transpose this file. We are going to make it an octave higher:

~~~haskell
> csd $ writeWav "osc440.wav" $ setDur (lengthSnd "osc220.wav") $ mapSig (scaleSpec 2) $ readSnd "osc220.wav"
~~~

We used the function `lengthSnd` to calculate the length of the first file in seconds.

There is a more generic function to write signals to files:

~~~haskell
writeSigs :: FormatType -> SampleFormat -> String -> [Sig] -> SE ()
writeSigs formatType sdampleFormat fileName sigs = ...
~~~

It writes several signals to the disk. We can specify the format type. Only four formats are allowed:
`Wav`, `Aiff`, `Raw` and `Ircam`. Also we can specify s sample format. You can study all sample formats
in the docs (see the module `Csound.Air` at the section *Writing sound files*).

----------------------------------------------------

* <= [Events](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SamplesTutorial.md)

* => [Real-world instruments show case](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/LiveWidgetsTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)