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
adele :: Balance -> DelayTime -> Feedback -> ToneSig -> Sig -> Sig
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
tort :: DriveSig -> ToneSig -> Sig -> Sig
~~~

The arguments are:

* `drive` -- amount of distortion (0, 1).

* `tone` -- the level of center frequency of the low-pass filter (0, 1).

### Fowler - envelope follower

Envelope follower applies a low-pass filter to the audio and the center frequency is controlled by the amplitude of the signal (RMS-level).

~~~haskell
fowler :: SensitivitySig -> BaseCps -> Resonance -> Sig -> Sig
~~~

Arguments:

* sensitivity -- sensitivity of the envelope follower (suggested range: 0 to 1)

* baseFrequencyRatio -- base frequency of the filter before modulation by the input dynamics (range: 0 to 1)

* resonance -- resonance of the lowpass filter (suggested range: 0 to 1)

### Revsy - reversing the audio

An effect that reverses an audio stream in chunks

~~~haskell
revsy :: TimeSig -> Sig -> Sig
~~~

`time` -- the size of the chunck in seconds.

### Flan - flanger

A flanger effect following the typical design of a so called 'stomp box' 

~~~haskell
flan :: RateSig -> DepthSig -> DelayTime -> Feedback -> Sig -> Sig
~~~

Arguments

* `rate` -- rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)

* `depth` -- depth of the lfo of the effect (range 0 to 1)
    
* `delayTime` -- static delay offset of the flanging effect (range 0 to 1)

* `feedback` -- feedback and therefore intensity of the effect (range 0 to 1)

### `phasy` - phaser

An phase shifting effect that mimics the design of a so called 'stomp box'

~~~haskell
phasy :: RateSig -> DepthSig -> BaseCps -> Feedback -> Sig -> Sig
phasy rate depth freq fback ain
~~~

Arguments:

* `rate` -- rate of lfo of the effect (range 0 to 1)

* `depth` -- depth of lfo of the effect (range 0 to 1)
    
* `freq` -- centre frequency of the phase shifting effect in octaves (suggested range 0 to 1)
    
* `fback` -- feedback and therefore intensity of the effect (range 0 to 1)
   

### Crusher - bit crusher

~~~haskell
crusher :: BitsReductionSig -> FoldoverSig -> Sig -> Sig
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

### Tremy - tremolo

~~~haskell
tremy :: TremWaveSig -> DepthSig -> RateSig -> Sig2 -> Sig2
tremy wave rate depth ain
~~~

; Arguments:

* `wave` -- waveform used by the lfo (0=sine 1=triangle 2=square)

* `rate` -- rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)

* `depth` -- depth of the lfo of the effect (range 0 to 1)


### Ringo - An ring modulating effect with an envelope follower

~~~hskell
ringo :: Balance -> RateSig -> EnvelopeModSig -> Sig -> Sig
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
type FxFun = Sig2 -> SE Sig2 

uiTort2 :: Source FxFun
~~~

We can combine the effects with functions:

~~~haskell
fxHor, fxVer :: [Source FxFun] -> Source FxFun

fxMatrix :: Int -> [Source FxFun] -> Source FxFun
fxMatrix numberOfColumns fxs = ...
~~~

All these functions stack the effects in the list
and align visuals. The visuals can be stacked horizontally, vertically
or placed on a square grid.

Let's create a chain of effects and apply it to the input signal:

~~~haskel
> let pedals ain = lift1 (\f -> f ain) $ fxHor [uiFlan1, uiAdele2 0.25 0.5, uiHall 0.2, uiGain 0.4]

> vdac $ pedals =<< (atMidi $ dryPatch vibraphone)
~~~

With `uiGain` we can change the volume of the output.

-------------------------------------------

* <= [Real-world instruments show case](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/Patches.md)

* => [Sound fonts](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SoundFontsTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)

