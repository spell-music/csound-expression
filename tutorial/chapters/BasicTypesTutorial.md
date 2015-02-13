Basic types
=====================================

Let's look at the basic types of the library. 

Signals (Sig)
----------------------

We are going to make a sound. So the most frequently used type is a signal. It's called `Sig`. 
The signal is a steam of numbers that is updated at a certain rate.
Actually it's a small array of doubles. For every cycle the sound-engine
updates it. It can see only one frame at the given time. 

Conceptually we can think that signal is a list of numbers.
A signal is an instance of type class `Num`, `Fractional` and `Floating`. 
So we can treat signals like numbers. We can create them with numeric
constants, add them, multiply, subtract, divide, process with 
trigonometric functions. 

We assume that we are in ghci session and the module `Csound.Base` is loaded.

~~~
$ ghci
> :m +Csound.Base 
~~~

So let's create a few signals:

~~~{.haskell}
> let x = 1 :: Sig
> let y = 2 :: Sig
> let z = (x + y) * 0.5
~~~

Constants are pretty good but not that interesting as sounds.
The sound is time varying signal. It should vary between -1 and 1.
It's assumed that 1 is a maximum volume. Everything beyond the 1
is clipped. 

Let's study the [simple waveforms](http://public.wsu.edu/~jkrug/MUS364/audio/Waveforms.htm):

~~~{.haskell}
osc, saw, tri, sqr :: Sig -> Sig
~~~

They produce sine, sawtooth, triangle and square waves. 
The output is band limited (no aliasing beyond [Nyquist](http://en.wikipedia.org/wiki/Nyquist_frequency)). 
The waveform function takes in a frequency (and it's also a signal) and produces
a signal that contains wave of certain shape that is repeated with given frequency (in Hz).

Let's hear a sound of the triangle wave at the rated of 220 Hz:

~~~{.haskell}
> dac $ tri 220
~~~

We can press `Ctrl+C` to stop the sound from playing. If we know the
time in advance we can set it with the function `setDur`:

~~~{.haskell}
> dac $ setDur 2 $ tri 220
~~~

Right now the sound plays only for 2 seconds. The `setDur` function
should be used only once. Right before the sending output to the `dac`.

We can vary the frequency with slowly moving oscillator:

~~~{.haskell}
> dac $ tri (220 + 100 * osc 0.5)
~~~

If we use the `saw` in place of `tri` we can get a more harsh
siren-like sound.

We can adjust the volume of the sound by multiplying it:

~~~{.haskell}
> dac $ mul 0.5 $ saw (220 + 100 * osc 0.5)
~~~

Here we used the special function `mul`. We could
just use the normal Haskell's `*`. But `mul` is more
convenient. It can work not only for signals but for 
tuples of signals (if we want a stereo playback) 
or signals that contain side effects (wrapped in the monad).
So the `mul` is preferable.  

Constant numbers (D)
----------------------------------------------

Let's study two another useful functions:

~~~{.haskell}
leg, xeg :: D -> D -> D -> D -> Sig
~~~

They are Linear and eXponential Envelope Generators.
They create [ADSR-envelopes](http://en.wikipedia.org/wiki/Synthesizer#ADSR_envelope).

They take in a four arguments. They are: 

* **attack time**: time for signal to reach the 1 (in seconds)

* **decay time**: time for signal to reach the sustain level (in seconds)

* **sustain level**: the value for sustain level (between 0 and 1) 

* **release time**: how many seconds it takes to reach the zero after release. 

We can notice the new type `D` in the signature. It's for constant doubles.
We can think that it's a normal value of type `Double`. It's a `Double` that is
embedded in the Csound. From the point of implementation we don't calculate
these doubles but use them to generate the Csound code.

Let's create a signal that is gradually changes it's pitch:

~~~{.haskell}
> dac $ saw (50 + 150 * leg 2 2 0.5 1)
~~~

Notice that signal doesn't reaches the release phase. It's not a mistake!
The release happens when we release a key on the midi keyboard.
We don't use any midi here so the release never happens. 

But we can try the virtual midi device:

~~~{.haskell}
> vdac $ midi $ onMsg $ \x -> saw (x + 150 * leg 2 2 0.5 1)
~~~

Right now don't bother about the functions `midi` and `onMsg`.
We are going to take a closer look at then in the chapter *User interaction*.
That's how we plug in the midi-devices.

The value of type `D` is just like a Haskell's `Double`. We can do all the 
Double's operations on it. It's useful to know how to convert doubles to `D`'s
and how to convert `D`'s to signals:

~~~{.haskell}
double :: Double -> D
sig    :: D -> Sig
~~~

There are more generic functions:

~~~{.haskell}
linseg, expseg :: [D] -> Sig
~~~

They can construct the piecewise linear or exponential functions.
The arguments are:

~~~{.haskell}
linseg [a, timeAB, b, timeBC, c, timeCD, d, ...]
~~~

They are alternating values and time stamps to progress 
continuously from one value to another. Values for `expseg`
should be positive (above 0 and not 0).

There are two more generic functions for midi notes:

~~~{.haskell}
linsegr, expsegr :: [D] -> D -> D -> Sig
~~~

The two last arguments are the release time and the final value for release stage. 
They are usefull for midi-instruments.

Another frequently used functions are

~~~{.haskell}
fadeIn  :: D -> Sig
fadeOut :: D -> Sig

fades   :: D -> D -> Sig
fades fadeInTime fadeOutTime = ...
~~~

They produce more simple envelopes. The `fadeIn` rises
in the given amount of seconds form 0 to 1. The `fadeOut`
does the opposite. It's 1 from the start and then it 
fades out to zero in given amount of seconds but only
after release. The `fades` combines both functions.


Strings (Str)
-----------------------------------

The friend of mine has made a wonderful record in Ableton.
I have a wav-file from her and want to beep-along with it.
I can use a `diskin2` opcode for it:

~~~{.haskell}
diskin2 :: Tuple a => Str -> Sig -> a
diskin2 fileName playBackSpeed = ...
~~~

It takes in a name of the file and playback speed and
produces a tuple of signals. We should specify how many outputs
are in the record by specifying precise the type of the tuple.
There are handy helpers for this:

~~~{.haskell}
ar1 :: Sig -> Sig
ar2 :: (Sig, Sig) -> (Sig, Sig)
ar3 :: (Sig, Sig, Sig) -> (Sig, Sig, Sig)
ar4 :: (Sig, Sig, Sig, Sig) -> (Sig, Sig, Sig, Sig)
~~~

Every function is an identity. It's here only to help the type inference.
So we have a stereo wav-file, and we want to play it at normal speed:

~~~{.haskell}
> let sample = toMono $ ar2 $ diskin2 (text "fuzzy.wav") 1
~~~

We don't care right now about the stereo so we have converted
everything to mono with function.

~~~{.haskell}
toMono :: (Sig, Sig) -> Sig
~~~

The first argument of the `diskin2` is not a Haskell's `String`.
It's a Csound's string so it has a special name `Str`. It's just
like `D`'s  for `Double`'s. We used a converter function to
lift the Haskell string to Csound one:

~~~{.haskell}
text :: String -> Str
~~~

The `Str` has instance of `IsString` so if we are using
the extension `OverloadedStrings` we don't need to call the function `text`.

Ok, we are ready to play along with it:

~~~{.haskell}
> let sample = toMono $ ar2 $ diskin2 (text "fuzzy.wav") 1
> let meOnKeys = midi $ onMsg osc
> vdac $ mul 0.5 $ meOnKeys + sample
~~~

Notice how simple is the combining midi-devices output
with the regular signals. The function `midi` produces 
a normal signal. We can use it anywhere. 

There are useful shortcuts that let us use a normal Haskell strings:

~~~{.haskell}
readSnd :: String -> (Sig, Sig)
loopSnd :: String -> (Sig, Sig)
loopSndBy :: D -> String -> (Sig, Sig)
readWav :: Sig -> String -> (Sig, Sig)
loopWav :: Sig -> String -> (Sig, Sig)
~~~

The functions with `read` play the sound files only once. 
The functions with `loop` repeat over the sample over and over.
With `loopSndBy` we can specify the time length of the loop period.
The `readWav` and `loopWav` can read the file with given speed.
The 1 is a normal speed. The -1 is playing in reverse.
Negative speed works only for `loopWav`.

So we can read our friends record like this:

~~~{.haskell}
let sample = loopSnd "fuzzy.wav"
~~~

If we want only a portion of the sound to be played we can use the
function:

~~~{.haskell}
takeSnd :: D -> Sig -> Sig
~~~

It takes only given amount of seconds from the input signal
and fills the rest with silence. It's interesting that we can loop not
only with samples but with regular signals too:

~~~{.haskell}
repeatSnd :: D -> Sig -> Sig
~~~

It loops the signal over given amount of time (in seconds).
We can try it out:

~~~{.haskell}
> dac $ repeatSnd 3 $ leg 1 2 0 0 * osc 220
~~~

Tables (Tab)
-------------------------------------

We have studied the four main waveform functions: `osc`, `tri`, `saw`, `sqr`.
But what if we want to create our own waveform. How can we do it?

What if we want not a pure sine but two more partials. We want
a sum of sine partials and a first harmonic with the amplitude of 1
the second is with 0.5 and the third is with 0.125.

We can do it with `osc`:

~~~{.haskell}
> let wave x = mul (1/3) $ osc x + 0.5 * osc (2 * x) + 0.125 * osc (3 * x)
> vdac $ midi $ onMsg $ mul (fades 0.1 0.5) . wave
~~~

But there is a better way for doing it. Actually the oscillator reads
a table with a fixed waveform. It reads it with a given frequency and
we can hear it as a pitch. Right now our function contains three `osc`.
Each of them reads the same table. But the speed of reading is different.
It would be much better if we could write the static waveform with
three harmonics in it and read it with one oscillator. It would be much
more efficient. Think about waveforms with more partials.

We can achieve this with function:

~~~{.haskell}
oscBy :: Tab -> Sig -> Sig
~~~

It creates an oscillator with a custom waveform. The static waveform is encoded
with value of type `Tab`. The `Tab` is for one dimensional table of doubles.
In the Csound they are called functional tables. They can be created 
with GEN-routines. We don't need to create the tables directly. Like filling
each cell with a value (going through the table in the loop). There are plenty
of functions that can create specific tables.

Right now we want to create a sum of partials or harmonic series.
We can use the function sines:

~~~{.haskell}
sines :: [Double] -> Tab
~~~

Let's rewrite the example:

~~~{.haskell}
> let wave x = oscBy (sines [1, 0.5, 0.125]) x
> vdac $ midi $ onMsg $ mul (fades 0.1 0.5) . wave
~~~

You can appreciate the simplicity of these expressions
if you try to make it directly in the Csound. But you don't 
need to! There are better ways and here is one of them.

What if we want not 1, 2, and third partials but 1, 3, 7 and 11?
We can use the function:

~~~{.haskell}
sines2 :: [(PartialNumber, PartialStrength)] -> Tab
~~~

It works like this:

~~~{.haskell}
> let wave x = oscBy (sines2 [(1, 1), (3, 0.5), (7, 0.125), (11, 0.1)]) x
~~~

### The table size

What is the size of the table? We can create the table of the given size.
By default it's 8196. The more size the better is precision. 
For efficiency reason the tables size in most cases should be 
equal to some degree of 2. We can set the table size with one of the functions:

~~~{.haskell}
lllofi, llofi, lofi, midfi, hifi, hhifi, hhhifi
~~~

The `lllofi` is the lowest fidelity and the `hhhfi` is the highest fidelity.

We can set the size explicitly with:

~~~{.haskell}
setSize :: Int -> Tab -> Tab
~~~

### The guard point

If you are not familiar with Csound's conventions you are probably 
not aware of the fact that for efficiency reasons Csound requires 
that table size is equal to power of 2 or power of two plus one 
which stands for guard point (you do need guard point if your intention 
is to read the table once but you don't need the guard point if you 
read the table in many cycles, then the guard point is the the first point of your table).

If we read the table once we have to set the guard point with function:

~~~{.haskell}
guardPoint :: Tab -> Tab
~~~

There is a short-cut called just `gp`. We should use it with `exps` or `lins`.

### Specific tables

There are a lot of GEN-routines [available](http://hackage.haskell.org/package/csound-expression-3.3.2/docs/Csound-Tab.html). 
Let's briefly discuss the most usefull ones.

We can write the specific numbers in the table if we want:

~~~{.haskell}
doubles :: [Double] -> Tab
~~~

Linear and exponential segments:

~~~{.haskell}
consts, lins, exps, cubes, splines :: [Double] -> Tab
~~~

Reads samples from files

~~~{.haskell}
wavs :: String -> Double -> Int -> Tab
mp3s :: String -> Double -> Tab
~~~

Harmonic series:

~~~{.haskell}
type PartialStrength = DoubleSource
type PartialNumber = DoubleSource
type PartialPhase = DoubleSource
type PartialDC = Double

sines  :: [PartialStrength] -> Tab
sines2 :: [(PartialNumber, PartialStrength)] -> Tab
sines3 :: [(PartialNumber, PartialStrength, PartialPhase)] -> Tab
sines4 :: [(PartialNumber, PartialStrength, PartialPhase, PartialDC)] -> Tab
~~~

Special cases for harmonic series:

~~~{.haskell}
sine, cosine, sigmoid :: Tab
~~~

Side effects (SE)
------------------------------------------------

The `SE`-type is for functions that work with side effects.
They can produce effectful value or can be used just for the 
side effect. 

For example every function that generates random numbers 
uses the type `SE`.

To get the white or pink noise we can use:

~~~{.haskell}
white :: SE Sig
pink  :: SE Sig
~~~

Let's listen to the white noise:

~~~{.haskell}
> dac $ mul 0.5 $ white
~~~

We can get the random numbers with linear interpolation.
The output values lie in the range of -1 to 1:

~~~{.haskell}
rndi :: Sig -> SE Sig
~~~

We can get the constant random numbers (it's like sample and hold
function with random numbers):

~~~{.haskell}
rndh :: Sig -> SE Sig
~~~

We can use the random number generators as LFO.
The `SE` is a `Functor`, `Applicative` and `Monad`.
We rely on these properties to get the output:

~~~{.haskell}
> let instr lfo = 0.5 * saw (440 + lfo)
> dac $ fmap instr (20 * rndi 5)
~~~

There are unipolar variants: `urndh` and `urndi`.
The output ranges form 0 to 1 for them.

Note that the function `dac` can work not only signals but
also on the signals that are wrapped in the type `SE`.

Let's take a break and listen to the filtered pink noise:

~~~haskell
> dac $ mul 0.5 $ fmap (mlp (on 50 2500 $ tri 0.2) 0.3) $ pink 
~~~

The function `on` is usefull for mapping the range (-1, 1) to
a different interval. In the expression `on 50 2500 $ tri 0.2`
oscillation happens in the range `(50, 2500)`. There is another 
usefull function `uon`. It's like `on` but it maps from the range `(0, 1)`.

The essence of the `SE Sig` type lies in the usage of random values.
In the pure code we can not distinguish between these two expressions:

~~~haskell
x1 = let a = rndh 1 in a + a
x2 = rndh 1 + rndh 1
~~~ 

For `x1` we want only one random value but
for `x2` we want two random values.

The value is just a tiny piece of code (we don't evaluate expressions
but use them to generate csound code).
The renderer performs common subexpression elimination.
So the examples above would be rendered in the same code.

We need to tell to the renderer when we want two random values.
Here comes the SE monad (Side Effects for short).

~~~haskell
x1 = do
  a <- rndh
  return $ a + a

x2 = do
  a1 <- rndh
  a2 <- rndh
  return $ a1 + a2
~~~

The SE was introduced to express the randomness. 
But then it was usefull to expres many other things.
Procedures for instance. They don't produce signals
but do smth usefull:

~~~
procedure :: SE ()
~~~ 

The `SE` is used for allocation of delay buffers in the functions.

~~~haskell
deltap3 :: Sig -> SE Sig
delayr :: D -> SE Sig
delayw :: Sig -> SE ()
~~~

The `deltap3` is used to allocate the delay line. After allocation
we can read and write to delay lines with `delayr` and `delayw`.

The `SE` is used for allocation of local or global variables (see the type `SERef`
in the module `Csound.Control.SE`). 

For convinience the `SE Sig` and `SE` of tuples of signals is instance of `Num`. 
We can sum and multiply the signals wrpapped in the `SE`. That's code is ok:

~~~haskell
> dac $ white + 0.5 * pink
> dac $ white + return (osc 440)
~~~


Mutable values
-------------------------------------------------

We can create mutable variables. It works just like
the normal Haskell mutable variables. We can create
a reference and the we should use the functions
on the reference to read and write values.

There are two types of the variables: local and global variables.
The local variables are visible only within one Csound instrument.
The global variables are visible everywhere.


We can create a reference to the mutable variable with functions:

~~~{.haskell}
newSERef 		:: Tuple a => a -> SE (SERef a)
newGlobalSERef  :: Tuple a => a -> SE (SERef a)
~~~

They take in an initial value and create a value of the type `SERef`:

~~~{.haskell}
data SERef a = SERef 
	{ writeSERef :: a -> SE ()
	, readSERef  :: SE a
	}
~~~

We can write and read values from reference.


Tuples (Tuple)
-------------------------------------------------

Some of the Csound functions are producing several outputs.
Then the output is represented with `Tuple`. It's a special
type class that contains all tuples of Csound values.

There is a special case. The type `Unit`. It's Csound's alias
for Haskell's `()`-type. It's here for implementation reasons.

We have already encountered the tuples when we have studied 
the function `diskin2`. 

~~~{.haskell}
diskin2 :: Tuple a => Str -> Sig -> a
~~~

In Csound the functions can produce varied amount of arguments.
The number of arguments is specified right in the code. But Haskell
is different. The function can produce only certain number of arguments.
To relax this rule we can use the special type class `Tuples`. 
Now we can return different number of arguments. But we have to 
specify them with type signature. There are helpers to make it easier:

~~~{.haskell}
ar1 :: Sig -> Sig
ar2 :: (Sig, Sig) -> (Sig, Sig)
ar3 :: (Sig, Sig, Sig) -> (Sig, Sig, Sig)
ar4 :: (Sig, Sig, Sig, Sig) -> (Sig, Sig, Sig, Sig)
~~~

The Signal space (SigSpace)
---------------------------------------

We often want to transform the signal which is wrapped 
in the other type. It can be a monophonic signal. 
If it's just a pure `Sig` then it's not that difficult.
We can apply a function and get the output. But what
if the signal is stereo or what if it's wrapped in the `SE`.
But it has a signal(s) that we want to process. We can use
different combinations of the function `fmap`. But there is
a better way. 

We can use the special type class `SigSpace`:

~~~{.haskell}
class Num a => SigSpace a where
	mapSig :: (Sig -> Sig) -> a -> a
	bindSig :: (Sig -> SE Sig) -> a -> SE a
~~~

There are lots of instances. For signals, tuples of signals,
tuples of signals wrapped in the `SE`, the signals that come 
from UI-widgets such as knobs and sliders.

If you are too lazy to write `mapSig` there is a shortcut `at` for you.
It's the same as `mapSig`. That's how we can filter a noise. The `linseg`
creates a stright line between the points `1500` and `250` that lasts for `5` seconds:

~~~haskell
> dac $ at (mlp (linseg [1500, 5, 250]) 0.1) $ white
~~~

It let's us apply signal transformation functions to values of 
many different types. The one function that we have already seen is `mul`:

~~~{.haskell}
mul :: SigSpace a => Sig -> a -> a
~~~

It can scale the signal or many signals.

There is another cool function. It's `cfd`:

~~~{.haskell}
cfd :: SigSpace a => Sig -> a -> a -> a
~~~

It's a crossfade between two signals. The first signal
varies in range 0 to 1. It interpolates between second
and third arguments.

Also we can use bilinear interpolation with four signals

~~~haskell
cfd4 :: SigSpace a => Sig -> Sig -> a -> a -> a -> a -> a
cfd4 x y asig1 asig2 asig3 asig4
~~~

We can imagine that we place four signals on the corners of the unipolar square.
we can move within the square with `x` and `y` signals. The closer we get to the
corner the more prominent becomes the signal that sits in the corner and other
three become quiter. The corner to signal map is:

* `(0, 0)` is for `asig1`

* `(1, 0)` is for `asig2`

* `(1, 1)` is for `asig3`

* `(0, 1)` is for `asig4`

The `cfds` can operate on many signals. The first list
length equals the second one minus one.

~~~{.haskell}
cfds :: SigSpace a => [Sig] -> [a] -> a
~~~

Another usefull function is 

~~~{.haskell}
wsum :: SigSpace a => [(Sig, a)] -> a
~~~

It's a weighted sum of signals. Can be useful for mixing
sounds from several sources.

The signal outputs (Sigs)
-------------------------------------

It's a tuple of signals. It's for mono, stereo and other sound-outputs.

~~~{.haskell}
class Tuple a => Sigs a
~~~

Spectrums (Spec)
-----------------------------------

We can extract a spectrum from the signal. It's an advanced type. 
The simplest function to create a spectrum is:

~~~{.haskell}
toSpec   :: Sig -> Spec
fromSpec :: Spec -> Sig
mapSpec  :: (Spec -> Spec) -> Sig -> Sig
~~~

With `Spec` we can apply spectral transformations to signal.
we can create a vocoder effect with it for instance or scale a pitch
or crossfade between several timbres.

We can interpolate between several signals:

~~~{.haskell}
cfdSpec :: Sig -> Spec -> Spec -> Spec
cfdsSpec :: [Sig] -> [Spec] -> Spec
~~~

To scale the pitch there are handy shortcuts:

~~~{.haskell}
scaleSpec :: Sig -> Sig -> Sig
scalePitch :: Sig -> Sig -> Sig
~~~

`ScaleSpec` scales the frequency of the signal in Hz ratios
but `scalePitch` does it in semitones.

If have a spectrum we can process it with many functions from the 
module [Spectral processing](http://hackage.haskell.org/package/csound-expression-opcodes-0.0.0/docs/Csound-Typed-Opcode-SpectralProcessing.html).


----------------------------------------------------

* <= [Introduction](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/Intro.md)

* => [Rendering Csound files](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ProducingTheOutputTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)