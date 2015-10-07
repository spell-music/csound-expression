Batteries included
===========================

There are plaenty of beatiful instruments designed for the library!
They can be found at the package `csound-catalog` (available on Hackage).

The main type for the instrument is a `Patch`. The patch is an instrument function
to be played and the chain of effects:

~~~haskell
type CsdNote a = (a, a)
type Instr a b = CsdNote a -> SE b 

type Fx a = a  -> SE a
type DryWetRatio = Sig

data FxSpec a = FxSpec
    { fxMix :: DryWetRatio
    , fxFun :: Fx a 
    }

data Patch a b = Patch
    { patchInstr :: Instr a b
    , patchFx    :: [FxSpec b]
    }
~~~

An instrument is a function that takes in a pair of values.
The pair contains amplitude and frequency in Hertz.
The instrument produces a signal or a tuple of signals wrapped in the `SE`-monad.

An effect is a function that transforms incoming signals and dry/wet ratio.
Dry/wet ratio defines how much of the original signal is going to be processed.
The `0 is for "dry" unprocessed signal. The `1` is for fully processed signal.

Effects are applied as in math application from last item in the list to the first.

Let's define a simple patch:

~~~haskell
> let instr (amp, cps) = mul (0.45 * sig amp * fades 0.01 0.1) $  
    fmap (mlp (2500 * leg 0.1 0.75 0.65 0.2) 0.1) $ 
    rndTri (sig cps) + rndTri (2.01 * sig cps)
~~~

The instrument is a couple of triangular waves with random phases.
They are processed with moog low-pass filter, then the siimple envelope 
(fade in and out) is applied. 

The effect is just a bit of reverb:

~~~haskell
> let fxs = [FxSpec 0.25 (return . smallHall2)]
~~~

Let's create a patch:

~~~haskell
> let p = Patch (fmap fromMono . instr) fxs
~~~

There are plenty usefull functions defined for patches. Let's play the patch with midi keyboard:

~~~haskell
> vdac $ atMidi p
~~~

The `atMidi` function takes in a patch and applies all necessary functions
to make a midi-instrument. We can play a single held note:

~~~haskell
> dac $ atNote (0.75, 220)
~~~

We can play the patch with event stream:

~~~haskell
> let notes = cycleE [(1, 220), (0.5, 220 * 5/ 4), (0.25, 330), (0.5, 440)]
> dac $ atSched p (withDur 0.15 $ notes $ metro 6)
~~~

We can also play Scores with patch:

~~~haskell
> let ns = fmap temp [(0.5, 220), (0.75, 330), (1, 440)]
> let notes = str 0.25 $ mel [mel ns, har ns, rest 4]
> dac $ mul 0.75 $ mix $ atSco p notes
~~~

We can change the amount of dry/wet apllied to the signal.

~~~haskell
atMix   :: Sig   -> Patch a -> Patch a
atMixes :: [Sig] -> Patch a -> Patch a
~~~

The `atMix` changes the ratio only for the last effect.
With `atMixes` we can tune all ratios.
Also there is a function to play unprocessed dry signal:

~~~haskell
dryPatch :: Patch a b -> Patch a b
~~~

We can add effects to the given patch. We can insert the effect
in the beginning and to the end of the chain:

~~~haskell
addPreFx, addPostFx :: DryWetRatio -> Fx b -> Patch a b -> Patch a b
~~~

Let's add a delay effect to
the defined patch:

~~~haskell
vdac $ atMidi $ addPreFx 1 (at $ echo 0.35 0.65) p
~~~

There are some usefull functions for Pads:

~~~haskell
deepPad :: (Fractional a, Sigs b, SigSpace b) => Patch a b -> Patch a b
~~~

It adds some weight to the timbre. It's defined with more generic function:

~~~haskell
harmonPatch :: (Fractional a, Sigs b, SigSpace b) => [Sig] -> [a] -> Patch a b -> Patch a b
harmonPatch amplitudes harmonics patch
~~~

It plays a given patch as harmonic series.

But let's study some predefined patches. We should install the `csound-catalog` package.
Then we need to import the module `Csound.Patch` and try some goodies (you can use `dac`
instead of `vdac` if you have a real midi device):

~~~
>:m +Csound.Patch
> vdac $ atMidi vibraphone1

> vdac $ atMidi dreamPad

> vdac $ atMidi $ deepPad razorPad

> vdac $ atMidi epianoBright 

> vdac $ atMidi xylophone

> vdac $ atMidi scrapeDahina 

> vdac $ atMidi noiz

> vdac $ atMidi mildWind

> vdac $ atMidi toneWheelOrgan

> vdac $ atMidi $ addPreFx 1 (at $ echo 0.35 0.65) banyan

> vdac $ atMidi caveOvertonePad

> vdac $ atMidi flute

> vdac $ atMidi hulusiVibrato

> vdac $ atMidi shortBassClarinet

> vdac $ atMidi $ withDeepBass 0.75 pwBass

> vdac $ atMidi pwEnsemble

> vdac $ atMidi albertClockBellBelfast 
~~~

There are 200+ of other instruments to try out! You can find the complete list
in the module `Csound.Patch`.

-------------------------------------------

* <= [Events](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/EventsTutorial.md)

* => [Sound fonts](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SoundFontsTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)
