Play live with notes
============================================

We have learned how to play audio files.
All our functions return signals which we can send to audio speakers.
We have learned how to create instruments. 

But music is often consists of series of events (notes)
which trigger instruments. How do we play notes?

## Playing with notes

In Csound we can trigger instruments with notes.
A note has three components:

```haskell
data Note a = Note
  { noteStart    :: D           -- delay from current time in seconds
  , noteDuration :: D           -- how long instrument will play
  , noteArgs     :: a           -- initial arguments to the instrument
  }
```

We trigger an instrument at start time of a note,
how long does it last and arguments that we pass to an instrument.

In Csound instruments are procedures:

```haskell
Arg a => a -> SE ()
```

Instrument takes in a tuple of initial values and does something useful for some time.
The class `Arg` is a tuple of Csound values that can be used as constants.
it excludes signals. So we can not pass a signal as initial argument as 
note happens in a moment and we can only take a snapshot of  a signal in 
the moment of the note trigger to pass it as argument to an instrument.

Examples of primitive types that can be used as initial arguments: 

* `D` - numbers
* `Str` - strings
* `Tab` - pre-loaded tables with doubles

In vanila Csound we can not produce output from the signal.
The output of procedure is `SE ()`. Where `SE` means side-effect.
It's an analof off Haskell's `IO` only for Csound.
If we want to redirect the output to speakers we use function
to write audio to speakers:

```haskell
writeOut :: Sigs a => a -> SE ()
```

Where `Sigs` is a tuple of audio signals.

To use instrument we need to define it:

```haskell
newProc :: Arg a => (a -> SE ()) -> SE (InstrRef a)
```

We pass a function and it returns a reference to the instrument which we can use.
Under the hood library allocates new integer id for an instrument and
renders Csound code to instrument body.

After that we can use `InstrRef` to trigger the instrument.
We cn do it with function `play`:

```haskell
play :: Arg a => InstrRef a -> [Note a] -> SE ()
```

It can be triggered from the instrument, anywhere in the code.
If we want to play a melody we can trigger it several times with
different arguments.

Also if you familiar with Csound we have a function `schedule`:

```haskell
schedule :: InstrRef a -> D -> D -> a -> SE ()
```

It plays a single note.

Let's define our first instrument and play it with notes.
we define an instrument that takes in volume a file and plays  it back:

```haskell
playFile :: (D, Str) -> SE ()
playFile (amp, file) = writeOuts $ mul (sig amp) $ diskin file
```

We have one new funtion:

```haskell
sig :: D -> Sig
```

It just creates a constant signal that always produce the same value.
Let's register an instrument and play notes with it:

```haskell
playProc :: IO ()
playProc = dac $ do
  playId <- newProc playFile
  schedule playId 0 3 (0.5, file1)
  schedule playId 4 5 (0.8, file1)
  schedule playId 10 5 (0.4, file1)
```

Also we can play notes with function `play`:

playProc2 :: IO ()
playProc2 = dac $ do
  playId <- newProc playFile

  play playId
    [ Note 0 3 (0.5, file1)
    , Note 4 5 (0.8, file1)
    , Note 10 5 (0.4, file1)
    ]

## Playing on condition

In previous example we manualy triggered instrument with functions `schedule` and `play`.
Note is triggered only once. But what if we want to repeat the notes by metronome?
We can also trigger the notes on condition when signal equals to some value.
Regarding the metronome example we have function `metro`:

```haskell
metro :: Sig -. Sig
metro freq
```

It produces 1's with given frequency and produces 0 inbetween.

We can trigger notes when output of the metro equals to 1.

```haskell
playPeriodic = dac $ do
  playId <- newProc playFile

  when1 (metro 0.2 `equals` 1) $ do
    schedule playId 0 2 (0.8, file1)
    schedule playId 4 1 (0.5, file1)
```

The expression `metro 0.2` triggers 1s once per 5 seconds.
When it equals to 1 we `schedule` two notes.
Csound is polyphonic by default so if we trigger notes at the same time
the output will be sumed up.

Another useful function to trigger notes on condition is `trigger`.
You can study the [Csound docs for it](https://www.csounds.com/manual/html/trigger.html).

That was really cool we can play the instruments indefinetely and we create rythmic structures.
For example we can define instrument that also plays with various speed.
And we can create a little melody based on it:

```haskell
playFile2 :: (D, D, Str) -> SE ()
playFile2 (amp, speed, file) = writeOuts $ mul (sig amp) $ loopWav file (sig speed)
```

With it we can create initerisitng sound scapes out of single file and various playback times:

```haskell
playPeriodic2 = printCsd $ do
  playId <- newProc playFile2

  when1 (metro 0.25 `equals` 1) $ do
    play playId
      [ Note  0  1 (1,      1,  file1)
      , Note  2  2 (0.5,  0.5,  file1)
      , Note  3  1 (0.7,    2,  file1)
      , Note  4  2 (0.5,  0.5,  file1)
      , Note  2  4 (0.2, -1,    file1)
      , Note  8  8 (0.2, -0.5,  file1)
      , Note 10 18 (0.5, -0.25, file1)
      ]
```

also we can change the audio file. So many things to explore!

### Release time

You may have noticed a pops or clicks on when note is turned off.
It's caused by abrupt change in audio signal from some value to 0. 
We can make smooth transitions if we fade out the signal on release time.
for that we can use function `linsegr`:

```haskell
linsegr :: [D] -> D -> D -> Sig
linsegr vals releaseTime finalValue
```

It behaves like `linseg`. It creates a linear segments of values 
only when note is turned off instrument is allowed to play for relaeseTime
more and `linsegr` goes from current value to the final value.

We can fade out with it:

```
playFile2 :: (D, D, Str) -> SE ()
playFile2 (amp, speed, file) = writeOuts $ mul (sig amp * fadeOut) $ loopWav file (sig speed)
  where
    fadeOut = linsegr [1] 0.1 0
```

Note that we don't have any clicks or pops with it.

### Infinite playback and turn off

We can create note that runs forever if we use `-1` as duration
We cun turn off such instrument with functions:

```haskell
turnoff2   :: InstrRef a -> Sig -> Sig -> SE ()
turnoff2_i :: InstrRef a ->   D ->   D -> SE ()


trunoff2 instrRef releaseTime mode
```

The `turnoff2`  can be used during performance of the instrument
and `turnoff2_i` on initialisation of an instrument.

Also we can turnoff the instrument from it's own body on condition.
It can be usefull if we want to turn the instrument off
but when to turn it off will be known only during it's performance on 
some condition:

```
turnoffSelf :: D -> D -> ()
stopSelf :: SE ()
```

## Trigger instrument from MIDI

Csound supports MIDI protocol. We can receive and send MIDI messages in real-time.
To receive midi messages we need to assign a midi channel to an instrument.
We do it with function `massign`:

```haskell
massign :: D -> InstrRef a -> SE ()
massign channel instrId
```

The valid midi channel is 1 to 16, if we set 0 then instrument 
listens for midi messages on all channels.





{-
Also we have more canvenient function that plays a note and produces output signal:

```haskell

newInstr :: (Arg a, Sigs outs) => MixMode -> D -> (a -> SE outs) -> SE (InstrRef a, outs)
```

it registers an instrument and produces a pair of instrument name to trigger
the instrument and output signals that instrument writes to.
-}

