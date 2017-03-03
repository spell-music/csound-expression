Family of effects
====================================

There is a family of effects that are common to electronic music. 
The module `Csound.Air.Fx.FxBox' defines many typical effects. 
This code can turn your code into a pedalboard! We can use effects as spice for the tibre.

It defines useful functions for typical guitar effects and defines
shortcuts to quickly add the effects to your instrument, also it has 
support for UI. We can not only add effects but also  tweak them in real time
just like we do it with guitar stompboxes.

All effects are kindly provided by Iain McCurdy and recoded from the original csound files.

To make things more fun I've given names to all instruments. So let's get aquinted to
the family of effects:

* `adele` - analog delay

* `pongy` - ping-pong delay

* `tort`  - distortion

* `fowler` - envelope follower

* `revsy` - reverses audio stream

* `flan` - flanger

* `phasy` - phaser

* `crusher` - bit crusher

* `chory` - chorus

* `pany` - auto-pan

* `tremy` - tremolo

* `ringo` -- ring modulation


Reverbs:

~~~
    room, chamber, hall, cave
~~~

Almost all effects have normalized parameters (belong to the interval 0 to 1).

## Effects

### Adele - analog delay

It's single delay line with low-pass filter in the feedback:

~~~haskell
adele :: Sigs a => Balance -> DelayTime -> Feedback -> ToneSig -> a -> a
~~~

arguments are:

* `balance` -- dry/wet ratio (0, 1)

* `delay-time`  measured in seconds

* `feedback` level (0, 1)

* `tone` -- low-pass filter center frequency (0, 1)

If tone is low the echoes are muddy (or muted) and if it's high the echoes are as bright as original signal.

### Pongy - ping-pong delay

It's a special version of ping-pong delay. The dry/wet ratio and feedback are controlled with the same parameter.

~~~haskell
pongy :: MixAt Sig2 (SE Sig2) a => Feedback -> DelayTime -> a -> AtOut Sig2 (SE Sig2) a
pongy feedback delayTime
~~~

The signature can look scary but it's just a stereo function that is lifted with `at` function to work on many types.
It can be used like this:

~~~haskell
dac $ pongy $ loopWav 1 "vox.wav"
~~~

### Reverbs

There are usefull functions to easily add a reverb:

~~~haskell
room, chamber, hall, cave :: MixAt Sig2 Sig2 a => Balance -> a -> AtOut Sig2 Sig2 a
~~~

The first argument is dry/wet ratio.


### Tort - distortion

Distortion can make your instrument scream.

~~~haskell
tort :: Sigs a => DriveSig -> ToneSig -> a -> a
~~~

The arguments are:

* `drive` -- amount of distortion (0, 1).

* `tone` -- the level of center frequency of the low-pass filter (0, 1).

### Fowler - envelope follower

Envelope follower applies a low-pass filter to the audio and the center frequency is controlled by the amplitude of the signal (RMS-level).

~~~haskell
fowler :: Sigs a => SensitivitySig -> BaseCps -> Resonance -> a -> a
~~~

Arguments:

* sensitivity -- sensitivity of the envelope follower (suggested range: 0 to 1)

* baseFrequencyRatio -- base frequency of the filter before modulation by the input dynamics (range: 0 to 1)

* resonance -- resonance of the lowpass filter (suggested range: 0 to 1)

### Revsy - reversing the audio

An effect that reverses an audio stream in chunks

~~~haskell
revsy :: Sigs a => TimeSig -> a -> a
~~~

`time` -- the size of the chunck in seconds.

### Flan - flanger

A flanger effect following the typical design of a so called 'stomp box' 

~~~haskell
flan :: Sigs a => RateSig -> DepthSig -> DelayTime -> Feedback -> a -> a
~~~

Arguments

* `rate` -- rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)

* `depth` -- depth of the lfo of the effect (range 0 to 1)
    
* `delayTime` -- static delay offset of the flanging effect (range 0 to 1)

* `feedback` -- feedback and therefore intensity of the effect (range 0 to 1)

### `phasy` - phaser

An phase shifting effect that mimics the design of a so called 'stomp box'

~~~haskell
phasy :: Sigs a => RateSig -> DepthSig -> BaseCps -> Feedback -> a -> a
phasy rate depth freq fback ain
~~~

Arguments:

* `rate` -- rate of lfo of the effect (range 0 to 1)

* `depth` -- depth of lfo of the effect (range 0 to 1)
    
* `freq` -- centre frequency of the phase shifting effect in octaves (suggested range 0 to 1)
    
* `fback` -- feedback and therefore intensity of the effect (range 0 to 1)
   

### Crusher - bit crusher

~~~haskell
crusher :: Sigs a => BitsReductionSig -> FoldoverSig -> a -> a
crusher  bits fold ain = ...
~~~

'Low Fidelity' distorting effects of bit reduction and downsampling (foldover)

Arguments

* `bits` -- bit depth reduction (range 0 to 1)

* `fold` -- amount of foldover (range 0 to 1)

### Chory - stereo chorus

~~~haskell
chory :: RateSig -> DepthSig -> WidthSig -> Sig2 -> Sig2
chory rate depth width (ainLeft, ainRight)
~~~

Arguments

* `rate`  -- rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)

* `depth` -- depth of the lfo of the effect (range 0 to 1)

* `width` -- width of stereo widening (range 0 to 1)

* `ainX`  -- input stereo signal

### Pany - autopan

~~~haskell
pany :: TremWaveSig -> DepthSig -> RateSig -> Sig2 -> Sig2
pany wave rate depth ain
~~~

Arguments:

* `wave` -- waveform used by the lfo (0=sine 1=triangle 2=square)

* `rate` -- rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)

* `depth` -- depth of the lfo of the effect (range 0 to 1)

Also there are special functions with LFO-wave set to specific wave: `oscPany`, `triPany`, `sqrPany`.

### Tremy - tremolo

~~~haskell
tremy :: Sigs a => TremWaveSig -> DepthSig -> RateSig -> a -> a
tremy wave rate depth ain
~~~

; Arguments:

* `wave` -- waveform used by the lfo (0=sine 1=triangle 2=square)

* `rate` -- rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)

* `depth` -- depth of the lfo of the effect (range 0 to 1)


Also there are special functions with LFO-wave set to specific wave: `oscTremy`, `triTremy`, `sqrTremy`.

### Ringo - An ring modulating effect with an envelope follower

~~~hskell
ringo :: Sigs a => Balance -> RateSig -> EnvelopeModSig -> a -> a
ringo balance rate envelopeMod
~~~

* balance   --  dry / wet mix of the output signal (range 0 to 1)

* rate  --  frequency of thew ring modulator *NOT IN HERTZ* (range 0 to 1)

* envelopeMod   --  amount of dynamic envelope following modulation of frequency (range 0 to 1)

## Presets

Sometimes we want to quickly add some effect. We don't care that much about particular numbers for parameters.
We just want to add a bit of distortion, lot's of delay and spoonful of flanger. To achieve that easily we have
a predefined presets for every member of fx-family. 

The preset name is a name of the member followed by a number 1 to 5 (means small to large coloring). For some members (`adele` and `tort`)
it has auxiliary suffix `m` (muted) or `b` (bright) like `adele2m` or `tort3b`. This suffix relates to the effects 
that have built-in low-pass filter or tone parameter.

## UI stompboxes

If we use prefix `ui` we can create an image of our effect that looks like guitar stompbox.
Let's take a distortion fr instance:

~~~haskell
type Fx a = a -> SE a

uiTort2 :: Sigs a => Source (Fx a)
~~~

We can combine the effects with functions:

~~~haskell
fxHor, fxVer :: [Source (Fx a)] -> Source (Fx a)

fxGrid :: Int -> [Source (Fx a)] -> Source (Fx a)
fxGrid numberOfColumns fxs = ...
~~~

All these functions stack the effects in the list
and align visuals. The visuals can be stacked horizontally, vertically
or placed on a square grid.

Let's create a chain of effects and apply it to the input signal:

~~~haskell
> let pedals ain = lift1 (\f -> f ain) $ fxHor [uiFlan1, uiAdele2 0.25 0.5, uiHall 0.2, uiGain 0.4]

> let player = atMidi $ dryPatch vibraphone1

> vdac $ pedals =<< player
~~~

With `uiGain` we can change the volume of the output.

Noticw how we used a standard monadic bind operator (`=<<`) to apply the effects to the signal.
How does it work? Let's check out the types:

~~~haskell
> :t pedals
pedals :: Sig2 -> Source (SE Sig2)
> :t player
player :: SE Sig2
~~~

And bind expects the types to be:

~~~
(=<<) :: Monad m => (a -> m b) -> m a -> m b
~~~

The `SE` is a monad but the `Source` doesn't seem to match for `SE b` part of signature.
It's ok! The `Source` is an alias for

~~~haskell
type Source a = SE (Gui, Input a)
~~~

So the uderlying type of `pedals` is:

~~~haskell
pedals :: Sig2 -> SE (Gui, Input (SE Sig2))
~~~

and it's just the right food for bind operator. 

Also we can apply the UI-widget with FX processing function with the help of the function `fxApply`:

~~~haskell
fxApply :: Source (a -> SE b) -> a -> Source b
~~~

If the argument is wrapped in `SE` we can use the bind operator `=<<`:

~~~haskell
fxApply fx =<< atMidi hammondOrgan
~~~

### Composing mono and stereo effects

It's often happens when chain starts with monophonic processing units (`Sig -> SE Sig`)
and then proceeds with stereophonic processing units (`Sig2 -> SE Sig2`). The reverb is often
used as mono to stereo  transition.  To make it easy to create chains of effects from mixed up
units there are analogs of functions `fxHor` and `fxVer`. They have suffix `MS` for Mono-To-Stereo:

~~~haskell
fxHorMS, fxVerMS :: 
    [Source Fx1] -> 
    Maybe (Source (Sig -> SE Sig2)) -> 
    [Source Fx2] -> 
    Source (Sig -> SE Sig2)
~~~

Type seems to be complicated but let's break it apart. The chain starts with the list of monophonic effects:

~~~haskell
[Source Fx1] -> 
~~~

Recall that `Fx1` is an alias for `Sig -> SE Sig`. Then we encounter a possible bridge from mono to stereo signals:

~~~haskell
Maybe (Source (Sig -> SE Sig2)) ->
~~~

It's wrapped in maybe type. We have to options. We can explicitly define the effect that takes us from mono to stereo (reverb is often used at this place).
Also we can just omit it with `Nothing` case and then the identity mono to stereo converter will be inserted.

Next we proceed with the chain of stereo effects:

~~~haskell
[Source Fx2] -> 
~~~

At the output we get UI-widget with the mono to stereo effect:

~~~haskell
Source (Sig -> SE Sig2)
~~~

An example:

~~~haskell
> let fx = fxHorMS [uiTort1, uiFlan2] def [uiChamber2]
> :t fx
fx :: Source (Sig -> SE Sig2)
~~~

We create the ui widget with a bit of distortion, slightly more flanger and not too big reverb.
We use `def` as an alias for maybe's constructor `Nothing`. We can apply the effect to the imput signal
received from say guitar pluged into audio card.

~~~haskell
> dac $ onCard2 $ \(aLeft, aRight) -> fxApply fx aLeft
~~~

The `onCard2` is a helper function to derive the types. It passes the argument through unchanged but it has more strict type signature. 
The `dac` is to much overloaded for this case. We can do without it but then we need to specify the types explicitly.

-------------------------------------------

* <= [Real-world instruments show case](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/Patches.md)

* => [Sound fonts](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SoundFontsTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)

