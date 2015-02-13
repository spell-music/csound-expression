
Basics of sound synthesis
====================================

Let's explore the sound synthesis with Haskell. We are going to 
study the subtractive synthesis. In subtractive synthesis we start with a complex waveform
and then filter it and apply cool effects. That's how we make
interesting instruments with subtractive synthesis. Let's look at
the basic structure of synthesizer.

Basic structure of synthesizer
---------------------------------------------

Let's imagine that we have a piano midi controller. When we press the key
we get the pitch (what note do we press) and volume (how hard do we press it).
So the input is the volume and pitch. An instrument converts it to the sound wave.
The sound wave should naturally respond to the input parameters. When we hit harder
it should be louder and when we press the lower notes it should be lower in
pitch and possibly richer in timbre. 

To get the sound wave we should ask ourselves: what is an instrument (or timbre)? 
How it is constructed? What parts should it contain? 

The subtractive synthesis answers to these questions with the following scheme:

~~~
			+----------+     +--------+      +-------+     +--------+
Pitch, --->	| Wave Gen |---->| Filter |----->| Gain  |---->| Effect |---> Sound
Velocity	+----------+     +--------+      +-------+     +--------+
		
		+-----+   +----+
		| LFO |   | EG |
		+-----+   +----+
~~~


Synth has six main units:

* **Wave generator (VCO):** It defines the basic spectrum of the sound. It's often defines the pitch of the sound.
    We create the static sound waveform with the given pitch. Sometimes we produce the noise
    with it (for percussive sounds).

* **Filter (VCF):**  Filter controls the brightness of the timbre. 
	With filter we can vary the timbre in time or make it dynamic.

* **Amplifier or gain (VCA):** With gain we can adjust the volume of the sound (scale the amplitude).

* **Processor of effects (FX):** With effects we can make the sound cool and shiny. It can be delay, reverb,
	flanger, chorus, vocoder, distortion, name your favorite effect.

Units to make our sounds alive (we can substitute the dumb static numbers with
time varied signals that are generated with LFO's or EG's):

* **Low frequency generator (LFO):** A low frequency oscillator generates waves at low frequency (0 to 50 Hz). 
	It's used to change the parameters of the other units in time.
	LFO's are used to change parameters periodically.

* **Envelope generator (EG):** An envelope creates a piecewise function (linear or exponential). It slowly varies in time.
    We can describe the steady changes with EG's. It's often used to 
    control the volume of the sound. For example the sound can start from the
    maximum volume and then it fades out gradually. 

Enough with theory! Let's move on to practice! 
Let's load the csound-expression to the REPL and define
the basic virtual midi instrument:

~~~haskell
> ghci
Prelude> :m +Csound.Base
Prelude Csound.Base> let run f = vdac $ midi $ onMsg f
~~~

Wave generator
--------------------------------

Wave generator defines the timbre content. What spectrum do we need in the sound?
There are four standard waveforms: sine, sawtooth, square and triangle.
The standard waveforms are represented with functions:

~~~haskell
osc, saw, sqr, tri :: Sig -> Sig
~~~

All functions take in a frequency (it can vary with time, so it's a signal).

The most simple is sine wave or pure tone. It represents the sine function. 
In csound-expression the pure sine is generated with function `osc`. Let's listen to it.

~~~haskell
> run osc
~~~


It starts to scream harshly when you press several notes. It happens
due to distortion. Every signal is clipped to the amplitude of 1. 
The function `osc` generates waves of the amplitude 1. So when we press
a single note it's fine. No distortion takes place. But when we press
several notes it starts to scream because we add several waves and the amplitude
goes beyond 1 and clipping results in distortion and leads to the harsh sound.
If we want to press several keys we can scale the output sound:

~~~haskell
> run $ mul 0.25 . osc
~~~

Pure tone contains only one partial in the spectrum. It's the most naked sound.
We can make it a little bit more interesting with different waves. The next wave is
triangle:

~~~haskell
> run $ mul 0.25 . tri
~~~

Little bit more rich in harmonics is square wave:

~~~haskell
> run $ mul 0.25 . sqr
~~~

The most rich is a saw wave:

~~~haskell
> run $ mul 0.25 . saw
~~~

All sounds are very 8-bit and computer-like. That's because they
are static and contain no variance. But that's only beginning.
We can see that we are going to use the scaling all the time so why not
to move it inside our runner function. Also we scale the pitch by 2
to make pitch lower:

~~~haskell
> let run k f = vdac $ midi $ onMsg (mul k . f . (/ 2))
~~~

Now we can run the saw wave like this:

~~~haskell
> run 0.25 saw
~~~

We can make our waves a little bit more interesting with
additive synthesis. We can add together several waves (something that resembles harmonic series):

~~~haskell
run 0.15 $ \x -> saw x + 0.25 * sqr (2 * x) + 0.1 * tri (3 * x)
~~~

Or we can introduce the higher harmonics:

~~~haskell
run 0.15 $ \x -> saw x + 0.25 * tri (7 * x) + 0.15 * tri (13 * x)
~~~

Gain
---------------------------------

Gain or amplifier can change the amplitude of the sound. We already
did it. When we scale the sound with number it's an example of the gain.
But instead of scaling with number we can give the output a shape.
That's where the envelope generators come in the play. 

Dynamic changes
---------------------------------

To make our sounds more interesting we can vary it parameters in time.
We are going to study two types of variations. They are slowly moving variations and rapid periodic ones.
The former are envelope generators (EG) and former are Low frequency oscillators (LFO).
Let's make our sound more interesting by shaping it's amplitude. 
That's how we change the volume in time.

### Envelope generator

Envelope generators produce piecewise functions. Most often they are linear or exponential.
In csound-expression we can produce piecewise functions with two function: `linseg` and `expseg`.

~~~haskell
linseg, expseg :: [D] -> Sig
~~~

They take in a list of timestamps and values and produce piecewise signal.
Here is an example:

Let's look at the input list:

~~~haskell
linseg [a, t_ab, b, t_bc, c, t_cd, d, ...]
~~~

It constructs a function that starts with the value `a` then moves
linearly to the value `b` for `t_ab` seconds, then goes from `b` to `c`
in `t_bc` seconds and so on. For example, let's construct the function that 
starts at 0 then goes to 1 in 0.5 seconds, then proceeds to 0.5 in 2 seconds,
and finally fades out to zero in 3 seconds:

~~~haskell
linseg [0, 0.5, 1, 2, 0.5, 3, 0]
~~~

There are two usefull functions for midi instruments: 

~~~haskell
linsegr, expsegr :: [D] -> D -> D -> Sig
~~~

They take two additional parameters for release of the note. 
Second argument is a time of the release and the last argument 
is a final value. All values for `expsegr` should be positive.

For example we can construct a saw that slowly fades out after
release:

~~~haskell
run 0.25 $ \cps -> expsegr [0.001, 0.1, 1, 3, 0.5] 3 0.001 * saw cps 
~~~

We can make a string-like sound with long fade in:

~~~haskell
run 0.25 $ \cps -> linsegr [0.001, 1, 1, 3, 0.5] 3 0.001 * (tri cps + 0.5 * tri (2 * cps) + 0.1 * sqr (3 * cps))
~~~

#### ADSR envelope

Let's study the most common shape for envelope generators. 
It's attack-decay-sustain-release envelope (ADSR). This shape
consists of four stages: attack, decay, sustain and release.
In the attack amplitude goes from 0 to 1, in the decay it goes
from 1 to specified sustain level and after note's release it 
fades out completely. 

Here is a definition:

~~~haskell
 adsr a d s r = linseg [0,      a, 1, d, s, r, 0]
xadsr a d s r = expseg [0.0001, a, 1, d, s, r, 0.0001]
~~~

There are two more function that wait for note release 
(usefull with midi-instruments):

~~~haskell
 madsr a d s r = linsegr [0,      a, 1, d, s] r, 0
mxadsr a d s r = expsegr [0.0001, a, 1, d, s] r, 0.0001
~~~

The functions `madsr` and `mxadsr` are original Csound functions.
They are used so often so there are short-cuts `leg` and `xeg`.
They are linear and exponential envelope generators.

So we can express the previous example like this:

~~~haskell
run 0.25 $ \cps -> leg 1 3 0.5 3 * saw cps
~~~

The EGs are for slowly changing control signals. 
Let's study some fast changing ones.

### Low frequency oscillator

Low frequency oscillator is just a wave form (`osc`, `saw`, `sqr` or `tri`)
with low frequency (0 to 20 Hz). It's inaudible when put directly
to speakers but it can produce interesting results when it's used
as a control signal.

Let's use it for vibrato:

~~~haskell
run 0.25 $ \cps -> leg 1 3 0.5 3 * saw (cps * (1 + 0.1 * osc 5))
~~~

Or we can make a tremolo if we modify an amplitude:

~~~haskell
run 0.25 $ \cps -> osc 5 * leg 1 3 0.5 3 * saw cps
~~~

The lfo-frequency can change over time:

~~~haskell
run 0.25 $ \cps -> osc (5 * leg 1 1 0.2 3) * leg 2 3 0.5 3 * saw cps
~~~

Also we can change the shape of the LFO. We can use `saw`, `tri` or `sqr`
in place of `osc`. 

With EG's and LFO's we can make our instruments much more interesting.
We can make them alive. They can control any parameter of the synth.
We are aware of two types of control signals. We can alter pitch (vibrato)
or result amplitude (amplitude envelope, tremolo). But there are many more
parameters. Let's study new way of controlling sound. Let's study brightness.


There is a special function to make the LFOs more explicit:

~~~haskell
type Lfo = Sig

lfo :: (Sig -> Sig) -> Sig -> Sig -> Lfo
lfo shape depth rate = depth * shape rate 
~~~

It takes the waveform shape, depth of the LFO and rate as arguments.

### Setting the range for changes

The LFOs are ranging in the interval (-1, 1). The EGs are ranging
in the interval (0, 1). Often we want to change the range. 

We can do t with simple arithmetic:

From `(0, 1)` to `(a, b)`:

~~~haskell
> let y = a + b * x
~~~

Or from (-1, 1) to `(a, b)`:

~~~haskell
> let y = a + b * (x + 1) / 2
~~~

It happens so often that there are special functions that abstracts these patterns:

From `(0, 1)` to `(a, b)`:

~~~haskell
uon :: Sig -> Sig -> Sig -> Sig
uon a b x = ...

let y = uon a b x
~~~ 

Or from (-1, 1) to `(a, b)`:

~~~haskell
on :: Sig -> Sig -> Sig -> Sig
on a b x = ...

let y = on a b x
~~~

The function `on` can be used with LFOs and `uon` can be used with EGs.

### Looping envelope generators

Since the version 4.3 we can use a lot of looping envelope generators.
They work as step sequencers. 

Let's see how we can use LFO's to turn the sound in the patters of notes.
Let's take a boring white noise and turn it in to equally spaced bursts:

~~~haskell
> dac $ mul (usqr 4) white
~~~

We have multiplied the noise with unipolar square wave. We can change the shape of envelope
if we multiply the noise with sawtooth wave:

~~~haskell
> dac $ mul (usaw 4) white
~~~

We can reverse the envelope:

~~~haskell
> dac $ mul (1 - usaw 4) white
~~~

We can create a simple drum pattern this way:

~~~haskell
dac $ mul (usaw 2) white + mul (usqr 1 * (1 - usqr 4)) (return $ saw 50)
~~~

But the real drummer don't kicks all notes with the same volume we need
a way to set accents. We can do it with special functions. 
They take in a list of accents and they scale the unipolar LFO-wave.
Let's look at `sqrSeq`. It creates a sequence of squares which are scaled
with given pattern:

~~~haskell
> dac $ mul (sqrSeq [1, 0.5, 0.2, 0.5] 4) $ white
~~~

We can create another pattern for sawtooth wave:

~~~haskell
> let b1 = mul (sqrSeq [1, 0.5, 0.2, 0.5] 4) $ white
> let b2 = mul (sawSeq [0, 0, 1] 2) $ white
> let b3 = return $ mul (triSeq [0, 0, 1, 0] 4) $ osc (sqrSeq [440, 330] 1)
> dac $ b1 + b2 + b3
~~~

We can use these functions not only for amplitudes. We can
control other parameters as well. 

~~~
> dac $ tri $ constSeq [220, 220 * 5/4, 330, 440] 8
~~~

The `constSeq` creates a sequence of constant segments. 
The cool thing about wave sequencers is that the values in the
sequence are signals. We can change them easily.

~~~haskell
> dac $ tri $ constSeq [220, 220 * 5/4, 330, constSeq [440, 220 * 4/ 3] 1] 8
~~~

~~~haskell
> let b3 = return $ mul (triSeq [0, 0, 1, 0] 4) $ osc (stepSeq [440, 330] 0.25)
~~~

The function `stepSeq` creates a sequence of constant segments. The main difference 
with `constSeq` is that all values are placed in a single period. The period 
of `constSeq` is a single line but the period of `stepSeq` is the sequence of 
const segments. We can create arpeggiators this way:

Let's create a simple bass line:

~~~haskell
> dac $ mlp (400 + 1500 * uosc 0.2) 0.1 $ saw (stepSeq [50, 50 * 9/ 8, 50 * 6 / 5, 50 * 1.5, 50, 50 * 9 / 8] 1)
~~~

We are using the function `mlp`. It's a moog low pass filter (the arguments: cut off frequency, resonance and the signal). 
We modulate the center frequency with LFO.

There are many more functions. We can create looping adsr sequences with `adsrSeq` and `xadsrSeq`.
We can loop over generic line segments with `linSeq` and `expSeq`. We can create 
sample and hold envelopes with `sah`. We can find the functions in the module `Csound.Air.Envelope`.

Let's create a simple beat with step sequencers. 
The first line is the steady sound of kick drum:

~~~haskell
> let kick = osc (100 * linloop [1, 0.1, 0, 0.9, 0])
> dac kick
~~~

The kick is a pure sine wave that is rapidly falls in pitch. We are using the function `linloop`
to repeat the pitch changes. The `linloop` is just like `linseg` but it repeats over and over.
Let's create a simple snare:

~~~haskell
> snare2 = at (hp 500 23) $ mul (sqrSeq [0, 0, 1, 0, 0, 0, 0.5, 0.2] 4) $ pink
> dac $ return kick + snare
~~~

We use high pass filtered pink noise. We create the drum pattern with square waves. 
The function `at` is the generic `map` for signal-like values:

~~~haskell
at :: (SigSpace a) => (Sig -> Sig) -> a -> a
~~~

We wrap the kick in the `SE` monad to add it to the snare wave.
Let's add a hi-hat. The hi-hat is going to be filtered white noise
with sequence of saw envelopes:

~~~haskell
> let hiHat = at (mlp 2500 0.1) $ mul (sawSeq [1, 0.5, 0.2, 0.5, 1, 0, 0, 0.5] 4) $ white
> dac $ mul 0.5 $ return kick + snare + hiHat
~~~

Let's add some pitched sounds. Also we can make the kick louder:

~~~haskell
> let ticks = return $ mul (sqrSeq [0, 0, 0, 0, 1, 1] 8) $ osc 440
> dac $ mul 0.3 $ return (mul 2.4 kick) + return ticks + snare + hiHat
~~~

### Using GUIs as control signals

We can change parameters with UI-elements such as sliders and knobs. 
it's not the place to discuss GUIs at length. But I can show you a couple of tricks.

We have a simple audio wave:

~~~haskell
> dac $ mlp 1500 0.1 $ saw 110
~~~

It's  a filtered sawtooth wave. Let's plugin a knob to change the volume:

~~~haskell
> dac $ lift1 (\amp -> mul amp $ mlp 1500 0.1 $ saw 110) $ uknob 0.5
~~~

The `uknob` creates a knob that outputs a unipolar signal (it belongs to the interval `[0, 1]`).
The argument is the initial value of the knob. The `lift1` maps over the value of the `knob`.
The `uknob` returns not the signal itself but the signal and the GUI-element. With lift1 we
can easily transform control signal to audio wave.

What if we want to change the frequency? It's best to change the frequency with eXponential
control signals (the change is not linear but exponential). we can use the function `xknob`:

~~~haskell
> dac $ hlift2 (\amp cps -> mul amp $ mlp 1500 0.1 $ saw cps) (uknob 0.5) (xknob 50 600 110)
~~~

The `xknob` takes in three values. They are the minimum and maximum values and the initial value.
The `hlift2` can join two UI-control signals with functions. It aligns the visual representation
horizontally. The `vlift2` aligns visuals vertically.

Let's change the parameters of the filter with sliders:

~~~haskell
> dac $ vlift2 (\(amp, cps) (cflt, q)  -> mul amp $ mlp cflt q $ saw cps) 
	(hlift2 (,) (uknob 0.5) (xknob 50 600 110)) 
	(vlift2 (,) (xslider 250 7000 1500) (mul 0.95 $ uslider 0.5))
~~~

We can see the picture of the talking robot.
The `uslider` and `xslider` work just like `uknob` and `xknob` but
they lokk like sliders. Notice the scaling of the value of the second slider with `mul`.
It's as simple as that.

There are functions `hlift3`, `hlift4` and `hlift4` to combine more widgets.
The `hlift2'`, `hlift3'` . Notice the last character also take in scaling parameters
for visual objects. We can define four knobs with different sizes:

~~~haskell
> dac $ mul 0.5 $ hlift4' 8 4 2 1 
	(\a b c d -> saw 50 + osc (50 + 3 * a) + osc (50 + 3 * b) + osc (50 + c) + osc (50 + d)) 
	(uknob 0.5) (uknob 0.5) (uknob 0.5) (uknob 0.5)
~~~

Another usefull widget is ujoy. It creates a couple of signal which control xy
coordinates on the plane:

~~~haskell
> dac $ lift1 (\(a, b) -> mlp (400 + a * 5000) (0.95 * b) $ saw 110) $ ujoy (0.5, 0.5)
~~~

To use exponential control signals wwe should try the function `joy`:

~~~haskell
joy :: ValSpan -> ValSpan -> (Double, Double) -> Source (Sig, Sig)
~~~

The `ValSpan` can be linear or exponential. Both functions take in minimum and maximum values:

~~~haskell
linSpan, expSpan :: Dounle -> Double -> ValSpan
~~~

Let's look at the simple example:

~~~haskell
> dac $ lift1 (\(amp, cps) -> amp * tri cps) $ joy (linSpan 0 1) (expSpan 50 600) (0.5, 110)
~~~

Filter
--------------------------------

We can control brightness of the sound with filters. A filter can 
amplify or attenuate some harmonics in the spectrum. There are 
four standard types of filters: 

**Low pass filter** (LP) attenuates all harmonics higher than a given center frequency. 

**High pass filter** (HP) attenuates all harmonics lower than a given center frequency.

**Band pass filter** (BP) amplifies harmonics that are close to center frequency and
attenuates all harmonics that are far away. 

**Band reject filter** or notch filter (BR)  does the opposite to the BP-filter.
It attenuates all harmonics that are close to the center frequency.

A filter is very important for the synth. The trade mark of the synth
is defined by the quality of its filters. 

The strength of attenuation is represented by the ratio of how much decibels
the harmonic is weaker per octave from the center frequency. The greater the number
the stronger the filter.

In csound-expression there are plenty of filters. Standard filters are:

~~~haskell
lp, hp, bp, br :: Sig -> Sig -> Sig -> Sig
~~~

The first parameter is center frequency, the second one is resonance 
and the last argument is the signal to modify.

There is an emulation of the Moog low pass filter:

~~~haskell
mlp :: Sig -> Sig -> Sig -> Sig 
~~~

The arguments are: central frequency, resonance, the input signal.

We can change parameters in real-time with EG's and LFO's.
Let's create an envelope and apply it to the amplitude and center frequency:

~~~haskell
> let env = mxadsr 0.1 0.5 0.3 1
> run (0.15 * env) (lp (1500 * env) 1.5 . saw)
~~~

Normal values for resonance range from 1 to 100. We should carefully 
adjust the scaling factor after filtering. Filters change the volume of the signal.

We can align the center frequency with pitch. So that if we make pitch higher 
the center frequency gets higher and we get more bright sounds:

~~~haskell
> run (0.15 * env) (\x -> lp (x + 500 * env) 3.5 $ saw x)
~~~

We can make a waveform more interesting with new partials.

~~~haskell
> run (0.1 * env) (\x -> lop (x + 2500 * env) 3.5 $ saw x + 0.3 * tri (3 * x) + 0.1 * tri (4 * x))
~~~

We can apply an LFO to the resonance.

~~~haskell
run (0.15 * env) (\x -> lop (x + 500 * env) (7 + 3 * sqr 4) $ saw x)
~~~

Also we can apply LFO to the frequency:

~~~haskell
run (0.15 * env) (\x -> lop (x + 500 * env) (7 + 3 * sqr 4) $ saw (x * (1 + 0.1 * osc 4)))
~~~

Effects
---------------------------------

We can make our sounds much more interesting with effects!
Effect transforms the sound of the instrument in some way. 
There are several groups of effects. Some of them affect only amplitude,
while the other alter frequency or phase or place sound in acoustic 
environment.

To apply effect to the sound we have to modify our runner function. 
Right now all arguments control the sound that is produced with the 
single note. But we want to alter the total sound that goes out of 
the instrument. It includes the mixed sound from all notes that are played.
Let's modify our definition for function `run`:

~~~haskell
let run eff k f = vdac $ eff $ midi $ onMsg (mul k . f . (/ 2))
~~~	

The first argument now applies some effect to the output signal.

### Time/Based

#### Reverb

Reverb places the sound in some room, cave or hall.
We can apply reverb with function `reverTime`:

~~~haskell
reverTime :: Sig -> Sig -> Sig
~~~

It expects the reverb time (in seconds) as a first argument and the signal as
the second argument.

~~~haskell
run (reverTime 1.5) (0.05 * env) (\x -> lp (x + 500 * env) (7 + 3 * sqr 4) $ saw x)
~~~

There is also a function `rever1`:

~~~haskell
rever1 :: Sig -> Sig -> (Sig, Sig)
~~~

It's base on very cool Csound unit `reverbsc`. It takes in feedback level (0 to 1)
and input signal and produces the processed output. There are several ready to use
shortcuts: `smallRoom`, `smallHall`, `lhaskellargeRoom`, `largHall` and `magicCave`.

Let's place our sound in the magic cave:

~~~haskell
run magicCave (0.05 * env) (\x -> lp (x + 500 * env) (7 + 3 * sqr 4) $ saw x)
~~~

You can hear how dramatically an effect can change the sound.

### Delay

Delay adds some echoes to the sound. the simplest function is `echo`:

~~~haskell
echo :: D -> Sig -> Sig -> SE Sig
echo dt fb asig
~~~

It takes the delay time, the ratio of signal attenuation (reflections will be weaker by this amount)
and the input signal. Notice that the output is wrapped in the `SE`-monad. `SE` means side effect.
It describes some nasty impure things. This function allocates the buffer of memory to hold
the delayed signal. So thats why the output contains side-effects.

Let's try it out:

~~~haskell
run (echo  0.5 0.4) (0.05 * env) (\x -> lp (x + 500 * env) (7 + 3 * sqr 4) $ saw x)
~~~

Let's add some reverberation:

~~~haskell
run (fmap smallHall . echo  0.5 0.4) (0.05 * env) (\x -> lp (x + 500 * env) (7 + 3 * sqr 4) $ saw x)
~~~

We are using the `fmap` function to apply the next effect in chain to the value with side-effects.
The `SE`-wrapper type is `Monad` and hence it's `Applicative` and `Functor`.
The `echo` function is a specification of generic function:

~~~haskell
fdelay :: D -> Sig -> Sig -> Sig -> SE Sig
fdelay len fbk mix asig
~~~

It takes a delay time, ratio of sound attenuation, the mix level (we add the initial sound 
with processed one which is scaled by amount of `mix`) and the input signal.

There is the last most generic function `fvdelay`. With it we can vary the delay time: 

~~~haskell
fvdelay :: D -> Sig -> Sig -> Sig -> Sig -> SE Sig
fvdelay maxDelTime delTime fbk mix asig
~~~

It takes the maximum delay time and the delay time which is signal (it must be bounded by `maxDelTime`). 
Other arguments are the same.

Multitap delays can be achieved with function

~~~haskell
fvdelays :: D -> [(Sig, Sig)] -> Sig -> Sig -> SE Sig
fvdelays maxDelTime delTimeAndFbk  mix asig
~~~

The list holds tuples of delay times and attenuation ratio for each delay line.

### Distortion

A distortion can make our sound scream. We can use the function 

~~~haskell
distortion :: Sig -> Sig -> Sig
distortion gain asig
~~~ 

It takes a distortion level as first parameter. It ranges from 1 to infinity.
The bigger it is the harsher the sound.

### Pitch/Frequency

Let's review briefly some other cool effects.

#### Chorus

Chorus makes sound more natural by adding slightly transformed versions of the original sound:

~~~haskell
chorus :: Sig -> Sig -> Sig -> SE Sig
chorus rate depth asig
~~~

Beside the input signal chorus takes two arguments that range from 0 to 1.
They represent the chorus rate and depth.

#### Flanger

The next two effects are useful for creating synthetic sounds or
adding electronic flavor to the natural sounds.

The flanger can be applied with function `flange`:

~~~haskell
flange :: Lfo -> Sig -> Sig -> Sig -> Sig -> Sig
flange lfo fbk mx asig
~~~

Where arguments are: an LFO signal, feedback level, balance level between 
pure and processed signals and an input signal. 

Let's apply a flanger:

~~~haskell
run (flange (lfo tri 0.9 0.05) 0.9 0.5) (0.05 * env) (\x -> lp (x + 500 * env) (7 + 3 * sqr 4) $ saw x)
~~~

#### Phaser

The phaser is a special case of flanger effect. It processes the signal with series
of all-pass filters. We can simulate a sweeping phase effect with phaser. 

There are three types of phasers. The simplest one is

~~~haskell
phase1 :: Sig -> Lfo -> Sig -> Sig -> Sig -> Sig
phase1 ord lfo fbk mx asig
~~~

The arguments are: the order of phaser (an integer value, 
it represents the number of all-pass filters in chain, 4 to 2000, the better is 8,
the bigger the number the slower is algorithm), an LFO for 
phase sweeps (depth is in range acoustic waves, something around 5000 is good start, 
the rate is something between 0 and 20 Hz), amount of feedback, the balance between
pure and processed signals, the input signal.

There are two more phasers:

~~~haskell
harmPhase, powerPhase :: Sig -> Lfo -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
harmPhase ord lfo q sep fbk mx asig = ...
~~~

The arguments are: order of phaser, LFO-signal for frequency sweep, resonance of the filters (0 to 1),
separation of the peaks, feedback level (0 to 1), balance level.

Noise
--------------------------------

We can make our sounds more interesting by introducing randomness.
There are several ways to create random signals (including noise).

We can create a sequence of random numbers that change linearly 
with given frequency. Also this unit can be used as LFO.

~~~haskell
rnds, urnds :: Sig -> SE Sig

 rnds amplitude frequency
urnds amplitude frequency
~~~

The `urnds `varies between 0 and 1. The `rnds` varies between -1 and 1.

We can generate colored noises with: 

~~~haskell
white, pink :: SE Sig
~~~

Let's create a simple wind instrument:

~~~haskell
> let instr x = do { cfq <- 2000 * urnds 0.5; asig <- white; return $ mlp (x + cfq) 0.6 asig }
~~~

We filter the white noise with filter. The center frequency randomly varies
above the certain threshold. Let's hear the wind:

~~~haskell
run id (0.5 * fadeOut 1.5) instr
~~~

Complex waves
--------------------------------

Let's study how can we made our waveforms more interesting.
We can apply several simple techniques to achieve it.

### Reading sound signals from files

We can reuse the sound signals. The music is everywhere and we can
take a somebody else's music as a start point.

There are handy functions for reading the sound from files:

~~~haskell
readSnd :: String -> (Sig, Sig)
loopSnd :: String -> (Sig, Sig)

readSnd fileName = ...
~~~

The `readSnd` plays the file only once. The `loopSnd` repeats the file
over and over again. There is another useful function:

~~~haskell
loopSndBy :: D -> String -> (Sig, Sig)
~~~

It takes the duration of the loop-period as a first argument.

These functions can read files in many formats including `wav` and `mp3`.
If your sound sample is stored in the `wav` or `aiff` format we can
read it with the given speed. The speed is a signal. It can change with time.
We can create interesting effects with it:

~~~haskell
loopWav :: Sig -> String -> (Sig, Sig)
loopWav speed fileName = ...
~~~

The normal playback is a speed that equals `1`. We can play it in reverse
if we set the speed to `-1`.

The output is a stereo signal. If we want to force it to mono we can use the function:

~~~haskell
toMono :: (Sig, Sig) -> Sig
~~~

It produces the mean of two signals.

### Additive synthesis

The simplest one is additive synthesis. We add two or more waveforms so
that they form harmonic series.

~~~haskell
> run id (0.25 * env) (\x -> saw x + 0.5 * sqr (2 * x) + 0.15 * tri (3 * x))
~~~

### Stacking together several waveforms

When several violins play in the orchestra the timbre is quite 
different from the sound of the single violin. Though timbre of each
instrument is roughly the same the result is different. It happens 
from the slightly detuned sound of the instruments. We can recreate this 
effect by stacking together several waveforms that are slightly detuned.
It can be achieved with function:

~~~haskell
chorusPitch :: Int -> Sig -> (Sig -> Sig) -> (Sig -> Sig)
chorusPitch numberOfCopies chorusWidth wave = ...
~~~

It takes the integer number of copies and chorus width.
Chorus width specifies the radius of the detunement.

~~~haskell
> run id (0.25 * env) (chorusPitch 8 0.5 saw)
~~~

### Ring modulation

Ring modulation can add metallic flavor to the sound.
It multiplies the amplitude of the signal by LFO.

~~~haskell
run id (0.25 * env) (mul (osc (30 * env)) . chorusPitch 8 0.5 saw)
~~~

### Diving deeper

Csound contains thousands of audio algorithms. It's impossible
to cover them all in depth in the short guide. But we can explore
them. They reside in the separate package `csound-expression-opcodes` 
that is re-exported by the module `Csound.Base`. Take a look in the docs.
there are links to the originall Csound docs. Maybe you can find your
own unique sound somewhere in this wonderful forest of algorithms.

The modules `Csound.Typed.Opcode.SignalGenerators`, `Csound.Typed.Opcode.SignalModifiers` 
and `Csound.Typed.Opcode.SpectralProcessing` are good place to start the journey.


----------------------------------------------------

* <= [Rendering Csound files](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ProducingTheOutputTutorial.md)

* => [User interaction](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/UserInteractionTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)