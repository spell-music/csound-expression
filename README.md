Csound-expression guide
===============================================================================

[![Join the chat at https://gitter.im/anton-k/csound-expression](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/anton-k/csound-expression?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Welcome to the simplest textual synthesizer.

~~~{.haskell}
> dac $ osc 440
~~~

Csound-expression is a Haskell framework for computer music.
With the help of the library we can create our instruments on the fly. 
A couple of lines in the interpreter is enough to get the cool sound going
out of your speakers. It can be used for simple daily sound-file processing 
or for a full-blown live performances. It's available on [Hackage](http://hackage.haskell.org/package/csound-expression).

Let's look at how we can create computer music with Haskell. 

-------------------------------------------------------------------


* [Introduction](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/Intro.md)

* [Basic types](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/BasicTypesTutorial.md)

* [Rendering Csound files](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ProducingTheOutputTutorial.md)

* [Basics of sound synthesis](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SynthTutorial.md)

* [User interaction](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/UserInteractionTutorial.md)

* [Scores](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ScoresTutorial.md)

* [Events](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/EventsTutorial.md)

* [Real-world instruments show case](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/Patches.md)

* [SoundFonts](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SoundFontsTutorial.md)

* [Custom temperament. Microtonal music](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/Tuning.md)

* [Samples](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SamplesTutorial.md)

* [Signal segments](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SignalSegmentsTutorial.md)

* [Widgets for live performances](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/LiveWidgetsTutorial.md)

* [Padsynth algorithm](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/Padsynth.md)

* [Granular synthesis](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/GranularSynthesisTutorial.md)

* [Arguments modulation](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ModArg.md)

* [Csound API. Using generated code with another languages](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/CsoundAPI.md)

-------------------------------------------------------------------

Appendix:

* [Introduction to Csound for Haskellers](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/appendix/CsoundInstro.markdown)

* [Overview of the library](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/appendix/Overview.markdown)

-------------------------------------------------------------------

WARNING: the library works best within ghci. The real-time sound rendering 
function `dac` spawns a child process in the background which may continue 
to execute after you stop the main process that runs the programm. 
It's not so in vim but it happens in the Sublime Editor and when you 
invoke `runhaskell`. So the best is to write you program in the separate 
file and then load it in the ghci and invoke the function `main`
(which runs the sound rendering with the function `dac`).


---------------------------------------------------------------------

News
-----------------------------

**The 4.9.0 is out! New features:**

csound-expression

* Functions for creation of FM-synthesizers. We can create
the whole graph of FM-units (with feedback). Check out the module `Csound.Air.Fm`

* Support for Monosynth patches. See atMono in the module `Csound.Air.Patch`

* Easy to use Binaural panning. See the module `Csound.Air.Pan`

* Construction of patches for sound fonts (`sfPatch`, `sfPatchHall`). 

* Table of tables. We can create a table that contains tables.

* Harmonic oscillators for subtractive synth: `buz` and `gbuz`

* Reverbs for patches. It's very easy to add a reverb to your  patch
 (`withSmallHall patch`, `withLargeHall patch`, etc)

* Some bug-fixes

csound-catalog

* Many mono-synth were added. You can use them with function `atMono`
  in place of `atMidi`. The mono versions of patches have suffix `m`.

* SHARC instruments. SHARC db contains a FFT-samples for sustain notes. 
   It includes many orchestra instruments. There are many new patches that
   use natural sounding timbres taken from the SHARC library. 
   Check out functions `soloSharc`, `padSharc`, `dreamSharc`.

csound-sampler

* Handy function `withBpm` allows to query current bpm with in the scope
  of expression.

* Sampler mappers were generalized.

* Char trigering functions are synchronized with bpm.

**The 4.8.3 is out! New features:**

This is a very important release to me.
It tries to solve the problem present in the most open source music-production libraries.
It's often the pack of beautiful sounds/timbres is missing. User is presented with
many audio primitives but no timbres are present to show the real power of the framework.
This release solves this problem. See the friend package csound-catalog on Hackage.
It defines 200+ beautiful instruments ready to be used. 

The csound-expression defines a new type called `Patch` for description of an instrument 
with  a chain of effects. It's good place to start the journey to the world of music production.

There are new functions for synchronized reaction on events. The triggering of events 
can be synchronized with given BPM.

There examples are fixed and should work.

The library is updated for GHC-7.10!

**The 4.8.2 is out! New features:**

This release improves oscillators in many ways.
Adds phase control to many standard oscillators.
There are functions to detune oscillator and create unisions of oscillators 
(multioscillators or chorus effect). 

Adds support for randomly generating events (with random frequency).

**The 4.8 is out! New features:**

A multitap looper is implemented (see `Csound.Air.Looper`). It's  a powerful widget 
with lots of controls. We can create unlimited number of taps.
And the length of the loops doesn't have to be the same for all taps.
We can insert effects and even external controllers. And all this is packed
as a simple function that produces a widget and the output signal.
Three types of loopers are available one is for raw signal inputs,
another for midi instruments and the last one is for soundfonts.
You can see it in action at [youtube](https://www.youtube.com/watch?v=cQQt9bu_x-A).

There are lots of new step sequencers available. 
Pre 4.8 step sequencers could only produce signals
with equal time segments but new step sequencers can 
play a tiny melodies. The API of temporal-media is 
supported for step sequencers (see `Csound.Air.Envelope`).
There is a new type called `Seq`. It's for step sequencers
that can play monophonic melodies.

There is a type class for humanization of envelopes.
It works for linseg and step sequencers. It adds some amount
of randomness to durations or values (see `Csound.Air.Envelope` `HumanValue`
and `HumanTime`). 

A midi chooser ui-box was implemented (see `Csound.Air.Live`, `hmidiChooser`, `uiMidi`). 
It makes it easy to choose a midi instrument among several alternatives. There are stand alone
widgets and widgets implemented as an effect-box.

The class `Compose` from `temporal-media` package was broken 
to two classes: `Harmony` (with function `hor`) 
and `Melody` (with function `mel`).

**The 4.7 is out! New features:**

The Scores are redesigned! The low level `CsdEventList` is substituted
with more advanced and flexible type `Sco`. The instruments are triggered not
with pairs or triplets (individual events) but with scores!

The lib now depends on common APIs for delaying and composing values.
There are common type classes for composition. 

There is a simple API for composition of samples, notes and signal segments.
The `mel` plays units sequentially, The `hor` plays units at the same time.
The `del` delays the unit by given amount of time, The `lim` limits the unit in time.
the `loop` creates infinite loops. The `loopBy` creates finite loops.
The list of all functions can be found in the package temporal-media. See 
the module `Temporal.Class`.

I need to update the guide for changes!

**The 4.6 is out! New features:**

* Granular delays and effects (see `Csound.Air.Granular`)

* It's possible to create tables not only for reading but also for writing.
  We can create sound buffers (see `newTab` and `newGlobalTab` in the module `Csound.Tab`). 

* Hyper Vectorial Synthesis (HVS). Easy to use functions for HVS (see `Csound.Air.Hvs`)
   With HVS we can control lots of parameters with a couple of sliders. 
   The HVS can reduce the size of control parameters by interpolating between snapshots of parameters.

* New spectral functions for spectral fusion: `crossSpecFilter` and `crossSpecVocoder` (see `Csound.Air.Spec`)

* New effect for playing input samples in segments (back and forth) `trackerSplice` 
  (original design by Rory Walsh). With it we can extract segments of live audio and 
  repeat them or play in reverse.

**The 4.5 is out! New features:**

* Easy to use granular synthesis (see `Csound.Air.Granular`)

* Support for opcode `mincer`. It's possible to scale pitch and tempo 
  of audio files independently (see `Csound.Air.Wav` ram reading functions). 

**The 4.4 is out! New features:**

* Signal segments.  With signal segments we can schedule audio signals
    with event streams. We can limit audio signals with clicks of the buttons
    or some other live events. We can retrigger samples, play them in sequence and 
    perform many more actions shich are tied to the event streams. 

* Triggering samples with keyboard and midi-events (see `Csound.Air.Sampler`). 

