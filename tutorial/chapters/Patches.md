Batteries included
===========================

There are plenty of beautiful instruments designed for the library!
They can be found at the package `csound-catalog` (available on Hackage).

Let's load the module:

~~~haskell
> import Csound.Patch
~~~

Now we can play a patch:

~~~haskell
> vdac $ mul 0.75 $ atMidi dreamPad
> vdac $ mul 0.75 $ atMidi vibraphone1
~~~

With `vdac` we can play midi instruments on a virtual midi-keyboard.
If you have a real midi-keyboard attached to your computer you can
use the plain old `dac`. This way we can play the patches with real MIDI-device:

~~~haskell
> dac $ mul 0.75 $ atMidi toneWheelOrgan
~~~

There are many predefined instruments. Some goodies to try out:
vibraphone1, dreamPad, razorPad, epianoBright, xylophone, scrapeDahina,
noiz, mildWind, toneWheelOrgan, banyan, caveOvertonePad, flute, hulusiVibrato,
shortBassClarinet, pwBass, pwEnsemble, albertClockBellBelfast etc.

The main type for the instrument is a `Patch`. The patch is a  data-type
that holds a complete set up for the instrument to be played live or
to apply it to musical scores.

Let's look at the definition of the `Patch`:

~~~haskell
data Patch a
    = MonoSynt MonoSyntSpec (GenMonoInstr a)
    | PolySynt PolySyntSpec (GenInstr D   a)
    | SetSkin SyntSkin (Patch a)
    | FxChain [GenFxSpec a] (Patch a)
    | SplitPatch (Patch a) D (Patch a)
    | LayerPatch [(Sig, Patch a)]
~~~

We can

* create monophonic (`MonoSynt`) synthesizers

* create polyphonic (`PolySynt`) synthesizers

* set generic parameters for synthesizers (`SetSkin`) such as filter type,

* add effects (`FxChain`)

* split keyboard on sections (`SplitPatch`). We can assign different patches to each section.

* play several patches at the same time (`LayerPatch`)


### Generic parameters for Patches

The Gen-prefix for instruments and effects refers to one peculiarity of the Patch data type.
I'd like to be able to change some common parameters of the instrument after it's already constructed.
Right now we can change only the type of the low-pass filter but some more parameters can be added in the future.

In Haskell we can do this with `Reader` data type. `Reader` lets us parametrize the values with some common arguments
and we can supply those arguments later.

The value: `Reader a b`  means that the result value `b` depends on argument `a` which we provide later.

~~~haskell
type SyntSkin = ResonFilter

type GenInstr a b = Reader SyntSkin (Instr a b)
type GenFxSpec a = Reader SyntSkin (FxSpec a)
type GenMonoInstr a = Reader SyntSkin (MonoInstr a)
~~~

So the polyphonic and monophonic synthesizers and effects are parametrized with value of the type `SyntSkin`.
The `SyntSkin` should contain some common parameters. We can change the filter type with it. By default it's set
to `mlp` (moog low pass filter). But we can set other types with the constructor `SetSkin skin patch`.

Let's discuss each case of the Patch data type.

### Polyphonic instruments

Patch can be a polyphonic synth:

~~~haskell
PolySynt PolySyntSpec (GenInstr D a)

type GenInstr a b = Reader SyntSkin (Instr a b)

type CsdNote a = (a, a)
type Instr a b = CsdNote a -> SE b

data PolySyntSpec = PolySyntSpec
    { polySyntChn :: MidiChn }
~~~

It converts notes to signals. With polyphonic instrument we can
play several notes at the same time. The note is a pair of amplitude and frequency.

With `PolySyntSpec` we can specify midi channel to play the instrument.
The `PolySyntSpec` has default instance with which we listen for midi messages on all channels.


Let's play a polyphonic patch:

~~~haskell
> vdac $ atMidi whaleSongPad
~~~

We can change the volume by signal multiplication:

~~~haskell
> vdac $ mul 0.5 $ atMidi whaleSongPad
~~~

We have played the predefined polyphonic synth. But we can also create our own! We can do it directly with constructor `PolySynt`
and with smart constructors:

~~~haskel
polySynt :: (Instr D a) -> Patch a

polySyntFilter :: (ResonFilter -> Instr D a) -> Patch a
~~~

Let's create a simple polyphonic instrument. It' just plays pure sines waves with percussive shape of amplitude:

~~~haskell
> instr (amp, cps) = return $ sig amp * xeg 0.01 4 0.001 2 * osc (sig cps)
> patch = polySynt instr
> vdac $ atMidi patch
~~~

Reminder: `xeg` -- creates exponential ADSR-envelope, `osc` is for pure sine wave, with `sig` we convert
constant. We use `return` to wrap the result in the SE-type.

We can do it in the regular Haskell-file:

~~~haskell
import Csound.Base

instr :: CsdNote D -> SE Sig
instr (amp, cps) = return $ sig amp * env * wave
    where
        env  = xeg 0.01 4 0.001 2
        wave = osc (sig cps)


patch = polySynt instr

main = vdac $ atMidi patch
~~~

With `polySyntFilter` we can let the user decide which type of filter is going to be used:

~~~haskell
polySyntFilter :: (ResonFilter -> Instr D a) -> Patch a
~~~

We can create a simple instrument with generic filter:

~~~haskell
import Csound.Base

instr :: ResonFilter -> CsdNote D -> SE Sig
instr resonFilter (amp, cps) = return $ filter $ sig amp * env * wave
    where
        env  = xeg 0.01 4 0.001 2
        wave = sqr (sig cps)
        filter = resonFilter (200 + 1500 * env) 0.25

patch = polySyntFilter instr

main = vdac $ atMidi patch
~~~

We can alter the filter with `SetSkin` constructor.

### Monophonic instruments

Patch can be a monophonic synth. The monophonic synth can play
only one note at the time (like flute or voice). But it converts not just
notes but signals of amplitude and frequency.

~~~haskell
MonoSynt MonoSyntSpec (GenMonoInstr a)

type GenMonoInstr a = Reader SyntSkin (MonoInstr a)

type MonoInstr a = MonoArg -> SE a

data MonoArg = MonoArg
    { monoAmp  :: Sig
    , monoCps  :: Sig
    , monoGate :: Sig
    , monoTrig :: Sig }

data MonoSyntSpec = MonoSyntSpec
    { monoSyntChn       :: MidiChn
    , monoSyntSlideTime :: Maybe D }    -- portamento for amplitude and frequency
~~~

It looks like polyphonic synth but monophonic synt argument type is a bit more complicated.
It contains four components:

* amplitude signal `monoAmp`

* frequency signal `monoCps`

* mask of when instrument is on `monoGate`. It equals to `1` when any note is played or `0` otherwise.

* trigger signal. It equals to 1 when note is triggered and `0` otherwise.

The argument type is designed to be used with the function adsr140 it creates complex ADSR envelope
which is retriggered when note is played. There is a shortcut to create the ADSR-function out of the arguments:

~~~haskell
-- | ADSR that's used in monophonic instruments.
type MonoAdsr = Sig -> Sig -> Sig -> Sig -> Sig

monoAdsr :: MonoArg -> MonoAdsr
~~~

we can create monophonic instruments with smart constructors:

~~~haskell
monoSynt :: (MonoInstr a) -> Patch a
monoSyntFilter :: (ResonFilter -> MonoInstr a) -> Patch a

adsrMono :: (MonoAdsr -> Instr Sig a) -> Patch a
adsrMonoFilter :: (ResonFilter -> MonoAdsr -> Instr Sig a) -> Patch a
~~~

The `monoSynt` creates the synt out of raw value of type `MonoArg`.
The `adsrMono` converts the arguments to a simpler form. It provides
a retriggering adsr-envelope which is syncronized with notes and a pair of amplitude and frequency signals.
With `Filter` suffix we can parametrize the insturment by low-pass filter.

Let's create a very basic mono-insturment:

~~~haskell
instr adsrFun (amp, cps) = return $ amp * env * osc (port cps 0.007)
    where env = adsrFun 0.01 4 0.001 2

patch = adsrMono instr

main = vdac $ atMidi patch
~~~

We add a portamento to frequency signal to make transition between the notes smooth.

Let's play some predefined monophonic synth:

~~~haskell
> vdac $ atMidi nightPadm
~~~

By convention many polyphonic synthesizers have monophonic sibling
with an `m` as a suffix.

### Patch with chain of effects

Also we can apply a chain of effects to the patch:

~~~haskell
FxChain [GenFxSpec a] (Patch a)

type GenFxSpec a = Reader SyntSkin (FxSpec a)

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

We can create effects in the chain with smart constructors:

~~~haskell
type Fx a = a -> SE a

fxSpec :: Sig -> Fx a -> GenFxSpec a
fxSpecFilter :: Sig -> (ResonFilter -> Fx a) -> GenFxSpec a
~~~

Almost all predefined patches have a bit of reverb. We can strip down the effect
with useful function `dryPatch`:

~~~haskell
> vdac $ atMidi $ dryPatch nightPadm
~~~

It throws away all the effects.

Also there are speciall functions to add effects to existing patch.
We can add effects to the both ends of the chain:

~~~haskell
addPreFx, addPostFx :: DryWetRatio -> Fx a -> Patch a -> Patch a
~~~

There are functions to add monophonic effects:

~~~haskell
mapFx  :: SigSpace a => (Sig -> Sig)    -> Patch a -> Patch a
bindFx :: BindSig a  => (Sig -> SE Sig) -> Patch a -> Patch a
~~~

There are variants to specify dry/wet ratio:

~~~haskell
mapFx'  :: SigSpace a => Sig -> (Sig -> Sig)    -> Patch a -> Patch a
bindFx' :: BindSig a  => Sig -> (Sig -> SE Sig) -> Patch a -> Patch a
~~~

Let's add a delay to our patch:

~~~
> vdac $ mapFx' 0.5 (echo 0.5 0.65) patch
~~~

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

Note that with split you can combine monophonic instruments with polyphonic ones.
We can play even several monophonic instruments in the diferent areas of the keyboard.

## How to play a patch

There are many ways to play the patch. We have already discovered how to play with
midi device, but we also can play it with other ways.

We can trigger a single note:

~~~haskell
dac $ atNote overtonePad (0.5, 110)
~~~

We can apply the patch to scores:

~~~haskell
> ns = fmap temp [(0.5, 220), (0.75, 330), (1, 440)]
> notes = str 0.25 $ mel [mel ns, har ns, rest 4]
> dac $ mul 0.75 $ mix $ atSco banyan notes
~~~

We can play the patch with an event stream:

~~~haskell
> notes = cycleE [(1, 220), (0.5, 220 * 5/ 4), (0.25, 330), (0.5, 440)]
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
> vdac $ atMidi $ addPreFx 0.5 (return . (mapSig $ echo 0.25 0.75)) banyan
~~~

We can use the function `addPostFx` to add the effect to the end of the chain
and the function `addPreFx` to add effect to the beginning of the effect chain.

We can change the amount of dry-wet ratio for the last effect in the chain with function `setFxMix`:

~~~haskell
vdac $ atMidi $ setFxMix 0 $ addPostFx 0.5 (return . (mapSig $ echo 0.25 0.75)) banyan
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

~~~haskell
mixInstr :: (SigSpace b, Num b) => Sig -> Patch b -> Patch b -> Patch b
~~~

We can create patches out of soundfonts! This way we can quickly turn our PC
into rompler. Check the soundfont section of the guide for the details on the type `Sf`.

~~~haskell
sfPatchHall :: Sf -> Patch2
sfPatch     :: Sf -> Patch2
~~~

There other functions. See the full list at the module `Csound.Air.Patch`.

We can set a midi channel for all instruments in the patch with function `setMidiChn`:

~~~haskell
setMidiChn :: MidiChn -> Patch a -> Patch a
~~~

## There are many beautiful instruments

Let's study some predefined patches. We should install the `csound-catalog` package.
Then we need to import the module `Csound.Patch` and try some goodies (you can use `dac`
instead of `vdac` if you have a real midi device):

~~~haskell
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

> vdac $ atMidi fmBass2

> vdac $ atMidi hulusiVibrato

> vdac $ atMidi shortBassClarinet

> vdac $ atMidi $ withDeepBass 0.75 pwBass

> vdac $ atMidi pwEnsemble

> vdac $ atMidi albertClockBellBelfast

> vdac $ atMidi $ vibhu 65
~~~

There are 200+ of other instruments to try out! You can find the complete list
in the module `Csound.Patch`.

-------------------------------------------

* <= [Events](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/EventsTutorial.md)

* => [FX family](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/FxFamily.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)
