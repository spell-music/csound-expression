Argument modifiers
===========================

Argument modifiers make it easy to add an LFO or small amount of noise to parameters
of synthesizer. 

Let's consider a plain sine wave:

~~~haskell
> dac $ osc 220
~~~

Let's add a vibrato:

~~~haskell
> dac $ osc (220 * (1 + 0.05 * osc 2))
~~~

What if we want to add a noisy vibrato:

~~~haskell
> dac $ osc (220 * (1 + 0.05 * white))

<interactive>:6:30:
    Couldn't match expected type ‘Sig’ with actual type ‘SE Sig’
    In the second argument of ‘(*)’, namely ‘white’
    In the second argument of ‘(+)’, namely ‘0.05 * white’
~~~

Ooops, we've got an error! That's because `osc 2` has type `Sig`
and `white` has type `SE Sig`. There is a type mismatch and 
compiler just reminds us about it.

But can we abstract out this pattern of vibrato and devise such a function
that we can easily use both types `Sig` and `SE Sig`. There is such a function!

It's defined in the module `Csound.Air.ModArg`. It's called `modArg1`. Let's see it in action:

~~~haskell
> dac $ modArg1 0.05 (osc 2) osc 220
~~~

It takes three parameters:

~~~haskell
> modArg1 depth modSignal function
~~~

It transforms a function so that the first argument is modulated with `modSignal`
with given `depth`. It's defined so that we can use both types `Sig` and `SE Sig`
for `modSig`:

~~~haskell
> dac $ modArg1 0.05 white osc 220
~~~

It might seem that `modArg1` takes in four arguments but the last argument `220`
is the argument for modified function. We may write it like this to clarify it:

~~~haskell
> let vibrOsc = modArg1 0.05 white osc
> dac $ vibrOsc 220
~~~

`modArg1` can modify functions with up to four parameters. The output of the function
should be one of the following types:

~~~haskell
Sig, Sig2, SE Sig, SE Sig2
~~~

Also there are siblings: `modArg2`, `modArg3` and `modArg4`. They can modify second,  third and fourth arguments of the function.
All functions take in depth of modulation, modulation signal and the function to transform.
The functions are defined so that the wiring is hidden from the user. If modulated
signal is pure it's just applied to the argument if it contains side effects than 
function output will have side effects too! 

Let's look at another example. Let's modulate the filter's center frequency:

~~~haskell
> dac $ at (modArg1 0.17 (osc 2) mlp 1750 0.2) $ white
~~~

We can  also modulate second argument too:

~~~haskell
dac $ at (modArg2 0.4 (osc 8) (modArg1 0.17 (osc 2) mlp) 1750 0.5) $ white
~~~

We can add some noise to the modulation:

~~~haskell
> dac $ at (modArg2 0.4 (osc 8) (modArg1 0.17 (mul (uosc 2) white) mlp) 1750 0.5) $ white
~~~

## Delayed modulation

Sometimes we want the modulation to start aftter some initial delay.
Take the vibrato for instance. Often there s no vibrato at the attack
and then it starts to rise. We can simulate it with the function:

~~~haskell
delModArg1 delTime riseTime depth modSig function
~~~

It takes in two more parameters. The first is time of delay and the second
is time to rise the modulation depth from zero to the given maximum amount.
Let's take a look at the example:

~~~haskell
> dac $ delModArg1 0.5 1 0.03 (osc 4) osc 220
~~~

The cool thing to know about modulation signal is that it's a signal.
It's parameters can vary too. Let's increase the vibrato rate over time:

~~~haskell
dac $ delModArg1 0.5 1 0.03 (osc (linseg [3, 3, 8, 4, 4])) osc 220
~~~

The function `delModArg` is also defined for 1, 2, 3, 4 arguments.

## Predefined patterns of modulation

There common ways to modulate signals. Let's look at some of them.
For every pattern `N` can be 1, 2, 3 or 4. The full list of functions can be found in the module `Csound.Air.ModArg`.

### Oscillators

The modulation most often happens with some LFO. There are predefined functions:

~~~haskell
oscArgN depth rate function
~~~

Also there are LFOs with other wave shapes: `triArgN`, `sqrArgN`, `sawArgN`.
There are LFOs with random phases: `rndOscArgN`, `rndTriArgN`, `rndSqrArgN`, `rndSawArgN`.
There are delayed versions of these functions all of them has prefix `del`.

Let's revrite the vibrato example:

~~~haskell
> dac $ oscArg1 0.05 4 osc 220
~~~

Let's delay the vibrato and make it saw-tooth shape:

~~~haskell
> dac $ delSawArg1 0.5 1 0.05 4 osc 220
~~~

### Noise generators

We can add some noise to parameters to imitate aliveness of the acoustic instruments.
There are severl types of noises:

* White Nose: `noiseArgN`.

* Pink noise: `pinkArgN`.

* gauss noise: `gaussArgN`.

* gauss noise with frequency of generation of new random values: `gaussiArgN depth cps`.

* jitter noise: `jitArgN depth cpsMin cpsMax`. It generates random nombers from -1 to 1 
   within the given interval of frequency of generation of new numbers.

The rest arguments are the same as with oscillators. They are `depth` and `function`.
The first argument is always depth of modulation.

It's a common trick to add some liveness to the sound with randomizing the parameters.
We add a bit of noise or randomness to the center frequency of the filter or to the resonance.
It makes the insturments more interesting.

Let's create a Pad sound with no modulation:

~~~haskell
> vdac $ midi $ onMsg $ mul (fades 0.5 0.5) . at (mlp 1200 0.15) . saw
~~~

Let's add a vibrato:

~~~haskell
> vdac $ midi $ onMsg $ mul (fades 0.5 0.5) . at (mlp 1200 0.15) . delOscArg1 0.3 0.8 4 saw
~~~

Let's modulate the parameters of the filter:

~~~haskell
> vdac $ midi $ onMsg $ mul (fades 0.5 0.5) . at ((gaussArg1 0.31 (noiseArg2 0.2 mlp)) 1000 0.15) . delOscArg1 0.3 0.8 0.013 4 saw
~~~

Let's add a reverb:

~~~haskell
> vdac $ mixAt 0.25 largeHall2 $ midi $ onMsg $ mul (fades 0.5 0.5) . at ((gaussArg1 0.31 (noiseArg2 0.2 mlp)) 1000 0.15) . delOscArg1 0.3 0.8 0.013 4 saw
~~~

We can lower the center frequency and increase the volume, to make sound more spacy:

~~~haskell
> vdac $ mul 2.5 $ mixAt 0.25 largeHall2 $ midi $ onMsg $ mul (fades 0.5 0.5) . at ((gaussArg1 0.31 (noiseArg2 0.2 mlp)) 550 0.15) . delOscArg1 0.3 0.8 0.013 4 saw
~~~

### Envelopes

Also there are predefined functions for common envelopes:

~~~haskell
 adsrArgN depth att dec sust rel function   -- linear

xadsrArgN depth att dec sust rel function   -- exponential
~~~

Also there are delayed versions that add initial delay time:

~~~haskell
 delAdsrArgN delTime depth att dec sust rel depth function   -- linear

delXadsrArgN delTime depth att dec sust rel depth function   -- exponential
~~~

Note that there is no riseTime as it's the same as attack portion of the envelope.
It's often useful to modulate the center frequency of the envelope:

~~~haskell
> dac $ at (adsrArg1 1 0.5 0.5 0.1 0.3 mlp 1500 0.1) $ saw 110
~~~

-------------------------------------------

* <= [Granular synthesis](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/GranularSynthesisTutorial.md)

* => [Csound API. Using generated code with another languages](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/CsoundAPI.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)
