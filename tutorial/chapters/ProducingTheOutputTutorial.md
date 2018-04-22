Rendering Csound files
==========================================

We know how to play the sound live. We can use the function `dac`
for it. Also we know how to use virtual midi-device. We can use `vdac` for it.
But there are many other ways to render the Csound file. Let's study them.
The functions that we are going to look at live in the
module [Csound.IO](http://hackage.haskell.org/package/csound-expression-3.3.2/docs/Csound-IO.html).

Producing the Csound code
---------------------------------------------------

The csound-expression library at its core is a Csound file
generator. The most basic thing it can do is to make
a `String` that contains the Csound code.

~~~haskell
renderCsd :: RenderCsd a => a -> IO String
~~~

It takes something renderable and produces a `String`.
We can write the String to the file with function:

~~~haskell
writeCsd :: RenderCsd a => String -> a -> IO ()
writeCsd fileName csd = ...
~~~

These functions are useful if we want to use the Csound code
without Haskell. For instance we can take it on some computer
that doesn't have the Haskell installed on it and run it with Csound.
It can be used on mobile devices inside of the other programs
with Csound API. We can send it to our friend by mail. So that
he can render it at home and hear the music.

Saving the output to sound-file
------------------------------------------------------

We can write the output to wav-file or aiff-file with function:

~~~haskell
writeSnd :: RenderCsd a => String -> a -> IO ()
writeSnd fileName csd = ...
~~~

Let's write a 10 seconds of concert A (440 Hz). We can use it
for tuning:

~~~haskell
> writeSnd "A.wav" $ setDur 10 $ osc 440
~~~

The audio is going to be rendered off-line. The good thing
about off-line rendering is that it can happen much faster than real clock time.
It depends on the complexity of the sound units. It's not limited
with real-time constraints. So we can render a file with 30 minutes
very quickly.

Playing live
------------------------------------------------------

We have already seen these functions. You can guess them:

~~~haskell
dac  :: RenderCsd a => a -> IO ()
vdac :: RenderCsd a => a -> IO ()
~~~

The `dac` is for sending the sound to sound card and
the `vdac` is for playing with virtual midi device.
If you have the real midi-controller you can use it
with `dac` function. Just use the plain `midi`-function
and everything should work out of the box.

Playing the sound with player
--------------------------------------------------------

We can render the file to sound file and play it with sound player.
Right now only Linux players are supported:

~~~haskell
mplayer, totem :: RenderCsd a => a -> IO ()
~~~

Render-able types
----------------------------------------------------------

It's time to take a closer look at the arguments of the functions.
What does type class `RenderCsd` mean?

We have seen how we can play a mono and stereo signals with it.
But can we do anything else? Yes, we can.

We can render the signals or tuples of signals.

~~~haskell
Sig, (Sig, Sig), (Sig, Sig, Sig, Sig)
~~~

They can be wrapped in the type `SE` (they can contain side effects)

~~~haskell
SE Sig, SE (Sig, Sig), SE (Sig, Sig, Sig, Sig)
~~~

We can listen on the sound card ports for input signals.
Yes, we can use the Csound as a sound-effect. Then we render
a function:

~~~haskell
(Sigs a, Sigs b) => RenderCsd (a -> b)
(Sigs a, Sigs b) => RenderCsd (a -> SE b)
~~~

We can render a procedure:

~~~haskell
SE ()
~~~

In this case we are using Csound to do something useful
but without making any noise about it. Maybe we are going
to manipulate some sound-files or receive Midi-messages
and silently print them on the screen.

There is also support for GUIs. We are going to encounter it soon.
The signal that is wrapped in the UI is also can be rendered:

~~~
Source Sig, Source (Sig, Sig), ...
~~~

Options
----------------------------------------------------

We don't care much about sound rates for the output or
what sound card to use or what size does internal sound buffers have.
But if we do?

Can we alter the sample rate? The default is 44100. It's good enough
for real-time performance. If we want to produce the high quality audio
we need to alter the defaults. That's where the `Options` are handy.

If we look at the module [Csound.IO](http://hackage.haskell.org/package/csound-expression-3.3.2/docs/Csound-IO.html)
we shortly notice that there are duplicate functions that ends with `By`

~~~haskell
dacBy 		:: RenderCsd a => Options -> a -> IO ()
writeCsdBy 	:: RenderCsd a => Options -> String -> a -> IO ()
writeSndBy 	:: RenderCsd a => Options -> String -> a -> IO ()
...
~~~

They take in one more argument. It's `Options`
([Csound.Options](http://hackage.haskell.org/package/csound-expression-3.3.2/docs/Csound-Options.html)).
With `Options` we can do a lot of fine tuning.
we can alter audio sample rate, alter the default size for
functional tables, assign settings for JACK-instruments and so on.

That's how we can alter the sound-rates:

~~~haskell
> let opt = setRates 96000 64
> writeSndBy opt result
~~~

The sound rates contain two integers. The former is the result audio rate
and the latter is for the length of the single audio array. The latter is called
block size (it's `ksmps` in Csound). It affects the rate of control signals.
We produce the audio signals in frames of the `blockSize` length.
When we are done with one frame we can listen for the control signals
and then apply them in the production of the next frame.

The cool thing about `Options` is that it's a `Monoid`.
We can use the default `Options` and alter only the things
we need to alter without the need to redefine the other things.
Let's see how we can combine different settings:

~~~haskell
> let opt = setRates 96000 64 <> def { tabFi = coarseFi 15 }
> writeSndBy opt result
~~~

We combine the two options with Monoid's `mappend` function.
The first option is for rate and the second set's higher degree
of fidelity for functional tables. It affects the default table size.
By default it's 13th degree of 2. But we have set it to 15.

Global config parameters
-----------------------------------

We can create files that have some parameters which we can change with command line arguments.
We render Haskell code to Csound file. After that we can run the file anywhere where Csound is installed.
What if we want to change some aspects of performance without the need for recompilation.
Some parameters like OSC-port or MIDI control change mappings. We can do it with Csound-Macroses.

To read the macros there are functions:

~~~haskell
readMacrosString :: String -> String -> Str
readMacrosDouble :: String -> Double -> D
readMacrosInt :: String -> Int -> D
~~~

They take name and initial value which is used if no flag is set on command line.
Let's create a pure tone with frequency parametrized with macros:

~~~haskell
> freq = readMacrosDouble "FREQ" 440
> dac $ osc (sig freq)
~~~

As simple as this! After rendering we have the file `tmp.csd` with Csound code.
Let's run it with different frequencies:

~~~
> csound tmp.csd --omacro:FREQ=330

> csound tmp.csd --omacro:FREQ=800
~~~

With flag `--omacro:Name=Value` we can assign the flag with given `Name`.
Notice that of course no type-checking is happening. If we assign integers to strings
bad things can happen.

Altering the defaults
-------------------------------------

The library comes with good default options that are most common and
should produce good result out of the box. Csound provides many fine-tuning
arguments with options that suit best for specific hardware and OS configuration.

If the defaults of the library are not best for us we can use `dacBy` or
other `-By` variants to alter the defaults but this way we should use them all the time!
Not so easy to use.

The library provides solution for this case. We can set alter the defaults
per folder and per system. The engine looks for defaults in special file called `.csound-expression-rc`
(it's non human readable format). First it looks for it in the current directory, if
it's found the options are applied, then if nothing is found it looks in the user HOME
directory, if nothing is found there than it resolves to the common defaults that
are defined in the library.

To create our own profile for defaults we can invoke the function:

~~~haskell
saveUserOptions :: Options -> IO ()
~~~

It saves the options in the file `.csound-expression-rc` in the current directory.
So if we invoke it in the our home directory those options would become the
new global defaults.

So for example if we want to use 48 kHz  by default and don't want to see
real-time events on the screen while csound plays. We can save the options:

~~~haskell
> saveUserOptions (setRates 48000 128 <> noTrace)
~~~


Common options
-------------------------------------


We can access all the  options that are available in Csound with the module [Csound.Options](http://hackage.haskell.org/package/csound-expression-5.3.1/docs/Csound-Options.html).
You can look at all Csound flags at the [Csound docs](http://csound.com/docs/manual/CommandFlagsCategory.html).

But some options are so much important that there are special shortcuts to access them.
Let's take review.

* Set rates - alter audio rate of rendering and block size
  The affect the audio quality of the audio and control signals.
  The higher the better, but if they are too high we can end up with
  not enough CPU for real-time problems.

~~~haskell
setRates :: Int -> Int -> Options
setRates audioRate blockSize
~~~

* Set buffer size - When we render audio in real time the audio
   is not sent directly to speakers instead it's written to special
  buffers where audio-card looks for them when it needs them.
  This options affect the latency of the system or how fast it can response.

~~~haskell
setBufs :: Int -> Int -> Options
setBufs hardwareBuf ioBuf
~~~

 There are two types of buffers:

   * hardware buffer - Number of audio sample-frames held in the DAC hardware buffer. This is a threshold on which software
        audio I/O (above) will wait before returning. A small number reduces audio I/O delay; but the value is often hardware
        limited, and small values will risk data lates.

   * IO buffer - Number of audio sample-frames per sound i/o software buffer. Large is efficient, but small will reduce audio
          I/O delay and improve the accuracy of the timing of real time events. The default is 256 on Linux, 1024 on MacOS X,
          and 4096 on Windows. In real-time performance, Csound waits on audio I/O on NUM boundaries. It also processes audio
          (and polls for other input like MIDI) on orchestra ksmps boundaries. The two can be made synchronous. For
          convenience, if NUM is negative, the effective value is ksmps * -NUM (audio synchronous with k-period boundaries).
          With NUM small (e.g. 1) polling is then frequent and also locked to fixed DAC sample boundaries.

* Activates audio input and output (on by default):

~~~haskell
setAdc, setDac :: Options
~~~

* Keeps the command line output to minimum, no trace messages: `noTrace :: Options`

* Sets how much output do we need (for level value consult the Csound docs).

~~~haskell
setMessageLevel :: Int -> Options
~~~

* Uses csound instance as JACK unit, defined a name for it: `setJack :: String -> Options`

* Set midi-device to all: `setMa :: Options`

* Sets the output to nosound: `setSilent :: Options`. It can be useful for audio processing tasks
  if we work with files.

* Sets the output string code: `setOutput :: String -> Options`

* Sets the input string code: `setInput :: String -> Options`

* Sets the device for real-time output: `setDacBy :: String -> Options`

* Sets the device for real-time input: `setAdcBy :: String -> Options`

* Sets different real-time audio drivers:

~~~haskell
setAlsa, setCoreAudio, setMme :: Options
~~~

* Changes the midi device string id: `setMidiDevice :: String -> Options`

* Sets default gain for output audio: `setGain :: Double -> Options`

----------------------------------------------------

* <= [Signals everywhere](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SignalTfm.md)

* => [Basics of sound synthesis](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SynthTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)