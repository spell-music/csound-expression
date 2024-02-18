Play a single sample
==============================

To create a sampler let's accomplish a most basic task.
We would like to play a single audio file.

Throughtout the guide we will use `csound-core` library.
We can open ghci and load it with

```
ghci
> import Csound.Core
```

It loads the main Csound functions in scope. 

### Play a wav file from disk

The most simple way to play a file is to use `diskin` function:

```
diskin :: Str -> (Sig, Sig)
```

It reads stereo WAV file from disk. Let's discuss the types:

* `Str` - is a Csound string, it's an instance of `IsString` class
   so we can use function `fromString` to get Csound strings out of Haskell ones
   or we can just use extension OverloadedStrings to convert automatically.

* `Sig` - is a **sig**nal. It's astream of digits that can be audio signal or control signal
   Audio signals are updated much faster than control rate signals. We set the update rates
   in the options. By default audio rate is 44100 Hz and control rate `audio_rate / 64`.

Let's activate the `OverloadedStrings` for ease of use in the termial:

```
> :set -XOverloadedStrings
```

With this extensions every consant string can be coerced into any type that
has instance of the class `IsString`. We need it to convert to Csound strings.
Let's play a file:

```
> dac $ diskin "my-file.wav"
hit Ctrl+C to stop
```

The function `dac` sends audio signal to speakers. 
If your settings are correct you will hear a sound.

Note that our Haskell code only creates Csound code. It does not cowrks with audio.
It just produces strings of Csound code. To get the audio we need to compile it with csound.

Under the hood the `dac` command creates Csound code, sets proper flags for
Real-time audio rendering, saves the code to temporal file and invokes csound command on it.

Let's study the type:

```
dac :: RenderCsd a => a -> IO ()
```

The `RenderCsd` is a class that contains all the types that we can render to Csound code.
In our case it's pair of audio signals: `(Sig, Sig)`.

### Instroduction to Csound model for music

So now that we can play files from dis with `dac` and `diskin` functions
Let's briefly discuss the actuall Csound code. It's not necessary to understand
Csound to use the Haskell library but still it's good to know the Csound terminology
and the model of music.

We can print the actual Csound code with function: `printCsd`.
Let study the Csound code for our simple file playback:

```csound
> printCsd 

<CsoundSynthesizer>

<CsOptions>

--nodisplays

</CsOptions>

<CsInstruments>

sr = 44100
ksmps = 64
nchnls = 2
0dbfs = 1.0
giPort init 1
opcode FreePort, i, 0
xout giPort
giPort = giPort + 1
endop


instr 1
ar0, ar1 diskin2 "/home/anton/over-minus.wav"
out ar0, ar1
endin

</CsInstruments>

<CsScore>

f0 6.048e7
i 1 0.0 -1.0

</CsScore>


</CsoundSynthesizer>
```

Csound code is in XML file. It has 3 sections:

* `CsOptions` - command line flags
* `CsInstruments` - contains instruments to be called with notes
* `CsScore` - scores to call the instruments

In `CsOptions` we have single option `--nodisplays` that turns off Csound debug echo-prints.

The `CsInstruments` starts with settings of global parameters. 
They are assigned only once on program setup:

```
sr = 44100           ; audio sample rate
ksmps = 64           ; control rate sample rate
nchnls = 2           ; number of output channels
0dbfs = 1.0          ; maximum output amplitude (0 descibels full-scale)

giPort init 1           ; it's some internal function that we need 
opcode FreePort, i, 0   ;
xout giPort             ;  internal function to allocate fresh integer ID's
giPort = giPort + 1     ;
endop                   ;
```

In the comments I've outlined all settings tha are set. These are global settings for our performans.

Let's look at the rendered instruments. we have only one:

```csound
instr 1
ar0, ar1 diskin2 "/home/anton/over-minus.wav"
out ar0, ar1
endin
```

Instrument starts with key word `instr` followed by instrument identifier (1 in our case) 
and ends with `endin`.
Inside the instrument we have code that does something. It's procedure that 
runs for some time.

In Csound model we have a bunch of instruments and we can trigger them from other instruments
or with scores.

In the Score section we have just two statemnts:


```
f0 6.048e7        ; set total time (signifies infinity)
i 1 0.0 -1.0      ; play a note
```

We can skip `f0` it's just needed to set the total duration. 
In our case it's virtual infinity.

The interesting case is `i` statement. It tells compiler to
trigger instroment `1`, starting from `0` seconds and lasting for `-1` (infinity) seconds.

An instrument is audio procedure that runs for some time. In our case we don't 
pass any arguments to file but we can do that. 
Let's study the instrument:

```
ar0, ar1 diskin2 "/home/anton/over-minus.wav"
out ar0, ar1
```

It invokes function `diskin2` to produce stream of audio from file and outputs
to variables `ar0` and `ar1`. Then it invokes function `out` that sends audio signals
to Csound program output. It can be speakers or audio file on disk.

So in Csound we define procedures that are run on control rate.
We can invoke those procedures with real-time events from score or
from other instruments.

## Play mp3 file

With diskin we play `wav` files. If we want to play mp3 files
we can use mp3in:

```haskell
mp3in :: Str -> (Sig, Sig)
```

## Play files from memory

With `diskin` and `mp3in` we read files from disk. Sometimes this can 
be not fast  enough for real-time processing as reading files from RAM.
To read file from RAM we need to load it to table. In Csound they are called f-tables.
It's single dimension array that can be loaded to memory and read and updated during performance.

To load WAV-file to RAM we use function:

```haskell
data WavChn = WavLeft | WavRight | WavAll

wavs :: String -> Double -> WavChn -> Tab
wavs file skipTime channel
```

It reads WAV from `file` and can skip so many seconds on read
and can read specific channel or both.

Also we have similiar function to load MP3s:

```
data Mp3Chn = Mp3Mono | Mp3Stereo | Mp3Left | Mp3Right | Mp3All

mp3s :: String -> Double -> Mp3Chn -> Tab
```

We have also shortcuts to read files: 

* `wavAll` - read stereo wav file from 0 (also `wavLeft` and `wavRight`)
* `mp3All` - read stereo mp3 file from 0

We can play the loaded tables with function `losc`:


```haskell
losc :: Tab -> Sig -> Sig2
```

It plays stereo files in loops with given freuency. 
If we set negative freuency the file will be playded in reverse.

Let's play some file:

```haskell
> tbeat= wavAll "beat.wav"
> dac $ losc beat 1
```

Experiment with different playback times.

Note that the requency is a signal we can switch between modes in real time 
which is super cool. Let's play with different playbacks.
For that we need to create stable stright line segments.
For that we use function `linseg`:

```haskell
linseg :: [D] -> Sig
linseg [val1, duration1, val2, duration2, val3, duration3, ... ]
```

The type `D` - is a consant double in Csound.
It creates a control rate signal which starts at value and then for duration in seconds
it goes in stright line to the next value, then after next duration in seconds goes to the next value
and so on, it holds as constant the last value.

Let's change the speed of playback:

```haskell
> speed = linseg [1, 5, 1, 0, -1, 5, -1, 5, 0.5]
> dac $ losc beat speed
```

In this example we first play for 5 seconds with normal speed (`[1, 5, 1]`) then imidiately jump 
to reversed playback `[1, 0, -1]` then play for 5 seconds in reversed mode `[-1, 5, -1]` 
and then go for 5 seonds to slowed down playback `[-1, 5, 0.5]`.

also we have helper functions to oscillate over files in memory with various speed:

```haskell
oscWav, oscMp3 :: String -> Sig -> (Sig, Sig)
oscWav :: String -. Sig -> Sig
```

Note that changing the speed also changes the pitch of the sound.
If you want playback with different speeds that preserves pitch have a look at functions
`mincer`, and `filescal.`

### Auxilliary arguments and defaults

We can also change playback for files played from disk.
But this is a bit tricky. We need to set the default value for speed of playback.
In Csound functions can take various number of arguments and usually Csound users
can update default values by supplying more arguments to the function.
But in Haskell it's fixed. 

To fix that we have special function called `withInits`:

```haskell
withInits :: (Tuple a, Tuple defs) => a -> defs -> a
withInits input defaults = output
```

It adds arbitrary amount of arguments to the value
that was created with Csound function.

Note the class `Tuple`. It signifies any tuple of Csound values.
So let's add init argument to the diskin opcode andplay it at twice speed:

```haskell
dac $ diskin "beat.wav" `withInits` (2 :: Sig)
```

Let's also play it in reverse:

```haskell
dac $ diskin "beat.wav" `withInits` (-1 :: Sig)
```

But we hear no sound. This is because diskin has different modes of playback
and by default when it reaches the end it stops producing audio. 
But we want a warp mode. For that we set another default argument:

```haskell
dac $ diskin "beat.wav" `withInits` (-1 :: Sig, 0 :: D, 1 :: D)
```

we set three aux arguments: speed of playback, start time and warp mode.
You can find out the details on official Csound documentation [for diskin](https://www.csounds.com/manual/html/diskin2.html).
Feel free to reach for Csound docs for corresponding functions it's very helpful.

Note on `Sig` and `D` in the inits. If you see in Csound args `k` or `a` prefix
it means that value is a signal with control or audio rate. And we can put `Sig`
on haskell level to such arguments. If you see `i` prefix it means that 
argument is constant and we can provide only `D`.

## How to change the volume of the signal

Ok now we can play files back and forth from wavs and mp3s. 
What if we want to change the volume?

It's very simple. The audio signals are instance of `Num`. 
It means that they support all numeric operations. And to change
the volume we can just multiply signal by a constant.

The constant is in `[0, 1]` to make it quiter and above 1 to make it sound louder.
Let's change the volume:

```
-- quiter
> dac $ 0.5 * diskin "beat.wav"

-- louder
> dac $ 2 * diskin "beat.wav"
```

All files should stay in range `[-1, 1]` otherwise it might introduce clipping and distortion.

### On linear vs logarithmic change in volume

Note that volume changes non-lineary. And linear scale does not represents how we 
percieve change in loudness. It's a subtle topic but to make transition smooth we'd
better measure in decibels (which is log-scale).

To make transition smooth we can use `ampdbfs` which converts dBFS valumes
to absolute amplitude. Almost all mixers measured in dbFS.
The loudest is 0 and lower values are negative.

Let's compare the transition:

```haskell
-- linear
> vol = linseg [0, 10, 0.8]
> (aleft, aright) = diskin "beat.wav"
> dac (vol * aleft, vol * aright)

-- logarithmic
> vol = ampdbfs (linseg [-20, 10, -1])
> (aleft, aright) = diskin "beat.wav"
> dac (vol * aleft, vol * aright)
```

So we make it louder na dlouder for 10 seconds.
Notice how in the first example percieved loudness has reached maximum almost on 5th second
and almost has not changed sonce then. But in the second example transiiton was more smooth.

It's better to use `ampdbfs`  if you program volume knobs and controls related to volume/loudness.
The same holds for pitch. Absolute pitch transition is also percieved as linear
if it changes exponentialy in absolute values.

### Convenient way to change volume

We have special function to multiply signals by single signal.
It' called `mul`:

```haskell
class SigSpace a where
  mapSig ::  (Sig -> Sig) -> a -> a

mul :: SigSpace a => Sig -> a -> a
```

It makes it more convenient to change volume in case of stereo or mono signals:

```
> dac $ mul 0.5 $ diskin "beat.wav"
```

## Conclusion

We have covered a great deal of information while we learned how
to play audio samples with Csound. Let's recap:

* We have primitive types

   * `Str` - strings
   * `D` - constant doubles
   * `Sig` - audio or control rate signals (streams of doubles)
   * `Tab` - tables to load arrays of doubles in memory. So far we have used it to store audio files
        but it can be aslo used for many other purposes

* Audio signals can be played with function `dac`, also we can print the generated Csound code with
   `printCsd` or save it file with `writeCsd`. The cool part of it is after that 
    we can use it with Csound alone and no Haskell is needed. and Csound can play `.csd` files
    on very low-consumption devices such as mibiles or Raspberry-Pi or even in the WEB-browser.

* To play files from disk we can use `diskin` or `mp3in`. To loop over wave table swith various
   speed we have functions: `loopWav` and `loopWav1`. Mp3 does not support various speed
  while reading from disk.

* To play files from memory we load them to `tables` with `wavs`, `mp3s` or `wavAll,` `mp3All`
   and play them back with `losc`. Also we have helper functions for that `oscWav` and `oscMp3`.

* We can create linear segments with function `linseg`. 
  Also there is the same function for exponential segments called `expseg`.
  It can be used to create control signals that vary in real-time.

* we can seup auxilliary arguments for Csound functions with functions `withInits`.

* The class `Tuple` menans all sorts of tuple of primitive Csound values,

* The `Sig` and `D` support all numeric operators. We can change amplitude with plain old multiplication
  and make polyphony with addition.

* There is a class to apply transformation to all underlying signals `SigSpace` and most 
  frequently we use it to change volume with function `mul`.

* Volume is percieved non-lineary. Use `ampdbfs` for smooth change in volume.
  Also we have function `dbfs` for realtive change. `dbfs` for that function is measured from 0 to 1
  but changes in log-scale. There is a function `gainslider` adapted to use with midi
  where input is scaled to `[0, 127]`
