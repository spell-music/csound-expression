Batteries included
===========================

There are plenty of beautiful instruments designed for the library!
They can be found at the package `csound-catalog` (available on Hackage).

Let's load the module:

~~~haskell
> :m +Csound.Patch
~~~

Now we can play a patch:

~~~
> vdac $ mul 0.75 $ atMidi dreamPad
> vdac $ mul 0.75 $ atMidi vibraphone1
~~~

With `vdac` we can play midi instruments on a virtual midi-keyboard.
If you have a real midi-keyboard attached to your computer you can
use the plain old `dac`. This way we can play the patches with real MIDI-device:

~~~
> dac $ mul 0.75 $ atMidi toneWheelOrgan
~~~

The main type for the instrument is a `Patch`. The patch is a  data-type
that holds a complete set up for the instrument to be played live or
to apply it to musical scores. 

Let's look at the definition of the `Patch`:

~~~haskell
data Patch a 
    = MonoSynt MonoSyntSpec (Instr Sig a)
    | PolySynt (Instr D a)
    | FxChain [FxSpec a] (Patch a)
    | SplitPatch (Patch a) D (Patch a)
    | LayerPatch [(Sig, Patch a)]
~~~

Let's discuss each case.

### Polyphonic instruments

Patch can be a polyphonic synth:

~~~haskell
PolySynt (Instr D a)

type CsdNote a = (a, a)
type Instr a b = CsdNote a -> SE b  
~~~

It converts notes to signals. With polyphonic instrument we can
play several notes at the same time. The note is apair of amplitude and frequency.

Also patch can be a monophonic synth. The monophonic synth can play
only one note at the time (like flute or voice). But it converts not just
notes but signals of amplitude and frequency.

Let's play a polyphonic patch:

~~~haskell
> vdac $ atMidi whaleSongPad
~~~

We can change the volume by signal multiplication:

~~~haskell
> vdac $ mul 0.5 $ atMidi whaleSongPad
~~~

### Monophonic instruments

~~~haskell
MonoSynt MonoSyntSpec (Instr Sig a)

data MonoSyntSpec = MonoSyntSpec
    { monoSyntChn       :: MidiChn  
    , monoSyntHold      :: Bool
    , monoSyntSlideTime :: D
    , monoSyntRelease   :: D }
~~~

The monosynt has some specific parameters to control the playback.
The synth channel is a channel for midi messages. The hold is a boolean
that specifies whether we need to hold the notes. When hold is on the synth
is going to play current note forever until the next note is pressed.
The slide time is a time of transition between the notes. 
It's time to slide the frequency. The release time is a time for how long
the synth plays after the key is released or note duration has finished.

Let's play a monophonic synth:

~~~haskell
> vdac $ atMidi nightPadm
~~~

By convention many polyphonic synthesizers have monophonic sibling
with an `m` as a suffix.

### Patch with chain of effects

Also we can apply a chain of effects to the patch:

~~~haskell
FxChain [FxSpec a] (Patch a)

type DryWetRatio = Sig

data FxSpec a = FxSpec
    { fxMix :: DryWetRatio
    , fxFun :: Fx a 
    }

type Fx a = a -> SE a    
~~~

An effect is a function that transforms signals (can be as single signal or a tuple of signals).
An effect unit comes with a main parameter that's called dry/wet ratio. It signifies the ratio of
unprocessed (dry) and processed signal (wet). And the fx chain contains a list of pairs of ratios 
and effect functions. Note that the list is reversed (like in haskell dot notation). The first 
function in the list is going to be applied at the last moment.

Almost all predefined patches have a bit of reverb. We can strip down the effect
with useful function `dryPatch`:

~~~haskell
> vdac $ atMidi $ dryPatch nightPadm
~~~

It throws away all the effects.

### Layered patch

Sometimes we want to play several instruments by the same key press. 
We can achieve it with ease by layered patches:

~~~haskell
LayerPatch [(Sig, Patch a)]
~~~

This case contains a list of pairs. Each element contains a volume of the layer and the patch of the layer.
The output is a mix of the outputs from all patches in the list.

Let's layer the vibraphone with pad-sound:

~~~haskell
> vdac $ atMidi $ LayerPatch [(0.5, nightPad), (1, vibraphone1)]
~~~

### Split patch

On the modern synthesizer we can find a useful function that is called `split`.
With split we can play two instruments at the same keyboard. We define a split point (frequency or pitch)
and two instruments. One is going to be active for all instruments below the pitch and another one
is going to be active for all notes above the given pitch.

~~~haskell
SplitPatch (Patch a) D (Patch a)
~~~

We specify the split with the frequency value. We can use `cpspch` to convert
frequencies to pitches. Here is how we can split the keyboard by the note C:

~~~haskell
> SplitPatch instr1 (cpspch 8.00) instr2
~~~

Let's play a pad in the low register and electric piano in the upper one:

~~~haskell
vdac $ atMidi $ SplitPatch dreamPad (cpspch 8.00) epiano2
~~~

## How to play a patch

There are many ways to play the patch. We have already discovered how to play with
midi device, but we also can play it with other ways.

We can trigger a single note:

~~~haskell
dac $ atNote overtonePad (0.5, 110)
~~~

We can apply the patch to scores:

~~~haskell
> let ns = fmap temp [(0.5, 220), (0.75, 330), (1, 440)]
> let notes = str 0.25 $ mel [mel ns, har ns, rest 4]
> dac $ mul 0.75 $ mix $ atSco banyan notes
~~~

We can play the patch with an event stream:

~~~haskell
> let notes = cycleE [(1, 220), (0.5, 220 * 5/ 4), (0.25, 330), (0.5, 440)]
> dac $ atSched hammondOrgan (withDur 0.15 $ notes $ metro 6)
~~~

## Useful functions for patches

We can play a dry patch (throw away all the effects).
Can be useful if you like the tone of the instruments 
but want to apply you own effect:

~~~haskell
vdac $ atMidi $ dryPatch $ scrapeXylophone
~~~


We can transpose a patch two semitones up:

~~~haskell
> vdac $ atMidi $ transPatch (semitone 2) $ dahina
~~~

or one octave down:

~~~haskell
> vdac $ atMidi $ transPatch (octave (-1)) $ dahina
~~~

We can add an effect to the patch. Note that we can append from both ends of the fx-chain.
Let's add a delay to the sound:

~~~haskell
> vdac $ atMidi $ addPreFx 0.5 (at $ echo 0.25 0.75) banyan
~~~

We can use the function `addPostFx` to add the effect to the end of the chain
and the function `addPreFx` to add effect to the beginning of the effect chain.

We can change the amount of dry-wet ratio for the last effect in the chain with function `setFxMix`:

~~~haskell
vdac $ atMidi $ setFxMix 0 $ addPostFx 0.5 (at $ echo 0.25 0.75) banyan
~~~

In this example we add post-delay, but set the ratio to zero so we can hear no delay.

We can also set a list of ratios with function `setFxMixes :: [Sig] -> Patch a -> Patch a`.

With `deepPad` we can add a second note that plays an octave below to deepen the sound:

~~~haskell
> vdac $ atMidi $ deepPad cathedralOrgan
~~~

There is a more generic function that let's us to add any number of harmonics 
that are played with the given patch:

~~~haskell
harmonPatch :: (SigSpace b, Sigs b) => [Sig] -> [D] -> Patch b -> Patch b
~~~

We can quickly fuse two patches together with function `mixInstr`:

~~~
mixInstr :: (SigSpace b, Num b) => Sig -> Patch b -> Patch b -> Patch b
~~~

We can create patches out of soundfonts! This way we can quickly turn our PC
into rompler. Check the soundfont section of the guide for the details on the type `Sf`.

~~~
sfPatchHall :: Sf -> Patch2
sfPatch     :: Sf -> Patch2
~~~

There other functions. See the full list at the module `Csound.Air.Patch`.

## There are many beautiful instruments

Let's study some predefined patches. We should install the `csound-catalog` package.
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
