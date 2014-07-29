Basic types
=====================================

Let's look at the basic types of the library. We are going to make a sound. 
So the most frequently type is a signal. It's called `Sig`. 

Signals (Sig)
----------------------

The signal is a steam of numbers that is updated at a certain rate.
Actually it's a small array of doubles. For every cycle the sound-engine
updates it. It can see only one frame at the given time. 

Conceptually we can think that signal is a list of numbers.
A signal is an instance of type class `Num`, `Fractional` and `Floating`. 
So we can treat signals like numbers. We can create them with numeric
constants, add them, mulyiply, subtract, divide, process with 
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
The output is bandlimited (no aliasing beyond [Nyquist](http://en.wikipedia.org/wiki/Nyquist_frequency)). 
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


Constant numbers (D)
----------------------------------------------

Let's study two another usefull functions:

~~~{.haskell}
leg, xeg :: D -> D -> D -> D -> Sig
~~~

They are **l**inear and e**x**ponential **e**nvelope **g**enerators.
They create [ADSR-envelopes](http://en.wikipedia.org/wiki/Synthesizer#ADSR_envelope).

They take in a four arguments. They are: 

* **attack time**: time for signal to reach the 1 (in seconds)

* **decay time**: time for signal to reach the sustain level (in seconds)

* **sustain level**: the value for sustain level (between 0 and 1) 

* **release time**: how many seconds it takes to reach the zero after release. 

We can notice the new type `D` in the signature. It's for constant **d**oubles.
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

The value of type `D` is just like a haskell's `Double`. We can do all the 
double's operations on it. It's usefull to know how to convert doubles to `D`'s
and how to convert `D`'s to signals:

~~~{.haskell}
double :: Double -> D
sig    :: D -> Sig
~~~

There are more generic functions:

~~~
linseg, expseg :: [D] -> Sig
~~~

They can construct the piecewise linear or exponential functions.
The arguments are:

~~~
linseg [a, timeAB, b, timeBC, c, timeCD, d, ...]
~~~

They are alternating values and time stamps to progress 
continuously from one value to another. Values for `expseg`
should be positive (above 0 and not 0).

There are two more generic functions for midi notes:

~~~
linseg, expseg :: [D] -> D -> D -> Sig
~~~

The two last arguments are the release time and the final value for release stage. 
They are usefull for midi-instruments.

Strings
-----------------------------------

Tables
-------------------------------------

Spectrums
-----------------------------------

Tuples
-------------------------------------

Arguments
-------------------------------------

The Signal space
---------------------------------------

The signal outputs
-------------------------------------



----------------------------------------------------

* <= [Introduction](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/Intro.md)

* => [Rendering Csound files](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ProducingTheOutputTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)