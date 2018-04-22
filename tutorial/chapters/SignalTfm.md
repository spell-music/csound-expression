Signals everywhere
==========================================

The signal type `Sig` is the key aspect of the library!
So let's make good friends with it!

We will learn how to transform signals in this chapter
and find out some interesting peculiarities of it.

Control-rate vs audio-rate signals
-------------------------------------------------

In CE we have a type `Sig` to represent both audio and control signals.
For ease of use some context is hidden from the user. But sometimes we need
to distinguish them.

Audio signals work on audio rate (typical values are 44.1 KHz or 48 KHz) while control signals
are updated on much lower rate (like 1/64 or 1/128 fraction of audio rate).
Using control signals can save a lot of CPU.

For instance we can use them with LFOs or envelope generators or
to modify with time any sort of parameter of synthesizer.

If you want to know the gory details of it.
Under the hood the audio- and control-rate signals are represented with different data structures.
Audio-rate signal is array of doubles and control rate is just a single double value.
But for the ease of use they are represented with the same type in CE.

The smart engine makes coversions behind the scenes. But if we want we can
give it a hint:

~~~haskell
kr :: Sig -> Sig   -- enforces control-rate
ar :: Sig -> Sig   -- enforces audio-rate
ir :: Sig -> D     -- takes a snapshot of the signal (init-time rate or constant)
~~~

### Mutable values with control signals

By default `newRef` or `newGlobalRef` create placeholders for audio-rate signals.
But if we want them to hold control-rate signals we have to use special variants:

~~~{.haskell}
newCtrlRef          :: Tuple a => a -> SE (Ref a)
newGlobalCtrlRef    :: Tuple a => a -> SE (Ref a)
~~~

If signals are created with them they are control-rate signals.


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

We can use the special type class `SigSpace` :

~~~{.haskell}
class Num a => SigSpace a where
  mapSig :: (Sig -> Sig) -> a -> a
~~~

There are lots of instances. For signals, tuples of signals,
tuples of signals wrapped in the `SE`, the signals that come
from UI-widgets such as knobs and sliders.

If you are too lazy to write `mapSig` there is a shortcut `at` for you.
It's the same as `mapSig`. Thats how we can filter a noise. The `linseg`
creates a straight line between the points `1500` and `250` that lasts for `5` seconds:

~~~haskell
> dac $ at (mlp (linseg [1500, 5, 250]) 0.1) $ white
~~~

It let us apply signal transformation functions to values of
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

Another usefull function is weighted sum

~~~{.haskell}
wsum :: SigSpace a => [(Sig, a)] -> a
~~~

It's a weighted sum of signals. Can be useful for mixing
sounds from several sources.

If we take a closer look at the function `mapSig`:

~~~haskell
 mapSig :: (Sig -> Sig) -> a -> a
~~~

We can notice that it can only transform value with pure functions.
Pure functions have no side-effects and are deterministic in nature.
But what if we want to make something random and weird?
Like filtering the signal with some random function:

Take for example `jitter`:

~~~haskell
jitter :: Sig -> Sig -> Sig -> SE Sig
jitter kamp kcpsMin kcpsMax
~~~

It creates random line segments with amplitude between `(-kamp, +kamp)`
with frequency of change in the interval `(kcpsMin, kcpsMax)`.
It's very useful to introduce some natural changes.
The output is random so it's wrapped in the `SE`.

Let's filter saw-tooth with it, as a filter we use `blp`,
which is Butterworth low-pass filter:

~~~haskell
> env = on 50 1500 $ jitter 1 0.5 2
> filt x = at (\cps -> blp cps x) env
~~~

So we apply our random `env` to the center frequency of the filter
and process signal `x` with it.

To apply a function to signal we can use the method `bindSig`
from the class `BindSig`. It works hust like `SigSpace`
only it's designed for effectful processing:

~~~haskell
class Num a => BindSig a where
  bindSig :: (Sig -> SE Sig) -> a -> SE a
~~~

Here is the result:

~~~haskell
> dac $ bindSig filt $ (saw 220 + sqr 110)
~~~

To apply it to the white noise we need more quirky expression:

~~~haskell
> dac $ bindSig filt =<< white
~~~

The new thing is operator `=<<`, which comes from the `Monad` type class.
It's standard way to apply effectful transformations to effectful values in the Haskell.
It's simplified type is:

~~~haskell
(=<<) :: (a -> SE b) -> SE a -> SE b
~~~

We can write the whole book on explanation of the `Monad`. But right now
just remember the signature. It allows us to plug effectful values
to effectful computations. It's just like `fmap` but with effectful input.

Generic At-class
------------------------------------

Wow so many conversions going on: `SigSpace`, `BindSig` or even combo of `Monad` with `BindSig`.
The head can go in rounds. This happens because Haskell is mmm.. well.. strongly
typed language and sometimes can be restrictive on types.

But it can be very awkward at times. imagine the expression:

~~~haskell
mapSig (f . g . h) expr
~~~

Here dot is Haskell way to compose functions. (we plug input of rightmost to the input of next and so on).
But now we change the `f` to `ef` which has side effects. And then we need to
change `mapSig` to `bindSig` and moreover if `expr` changes to effectful `expr`
we need to add `=<<` in proper place.

~~~haskell
bindSig (eff . g . h) =<< expr'
~~~

This can be quite annoying when we want to quickly test some effects on input signals.
To solve this we can use the very generic function `at`.
It calculates by the types of inputs the right conversion for it.
So it can be used inplace of `mapSig`, `bindSig` and many others.
It's kind of swiss army knife to apply any signal processing function to anything.

The type is very generic. We use some clever hackery to make things right.

~~~haskell
at :: At a b c => (a -> b) -> c -> AtOut a b c
~~~

Just use it! It's convenient. Let's look at examples:

In place of `mapSig`:

~~~haskell
dac $ at (mlp (linseg [1500, 5, 250]) 0.1) $ white
~~~

In place of `bindSig`:

~~~haskell
> env = on 50 1500 $ jitter 1 0.5 2
> filt x = at (\cps -> blp cps x) env

> dac $ at filt (saw 220 + sqr 110)
> dac $ at filt white
~~~

Notice no need for monadic operator when we switch from
pure waves to white noise!

The signal outputs (Sigs)
-------------------------------------

It's a tuple of signals. It's for mono, stereo and other sound-outputs.

~~~{.haskell}
class Tuple a => Sigs a
~~~

SigSpace for stereo transformations
----------------------------------

There are special variants of `SigSpace` and `BindSpace`
which are useful for stereo-effects. The process inputs with stereo transformations:

~~~haskell
class SigSpace2 a where
  mapSig2 :: (Sig2 -> Sig2) -> a -> a
~~~

and also

~~~haskell
class SigSpace2 a => BindSig2 a where
  bindSig2 :: (Sig2 -> SE Sig2) -> a -> SE a
~~~

And of course `at` unifies them all!.

----------------------------------------------------

* <= [Basic types](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/BasicTypesTutorial.md)

* => [Rendering Csound files](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ProducingTheOutputTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)
