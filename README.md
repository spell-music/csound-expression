Csound-expression guide
===============================================================================

[![Join the chat at https://gitter.im/anton-k/csound-expression](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/anton-k/csound-expression?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Welcome to the simplest textual synthesizer.

~~~{.haskell}
> dac $ osc 440
~~~

Csound-expression is a Haskell framework for computer music.
With the help of the library we can create our instruments on the fly. 
A few lines in the interpreter is enough to get the cool sound going
out of your speakers. It can be used for simple daily sound-file processing 
or for a full-blown live performances. It's available on [Hackage](http://hackage.haskell.org/package/csound-expression).

Let's look at how we can create computer music with Haskell. 

-------------------------------------------------------------------


* [Introduction](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/Intro.md)

* [Basic types](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/BasicTypesTutorial.md)

* [Rendering Csound files](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ProducingTheOutputTutorial.md)

* [Basics of sound synthesis](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SynthTutorial.md)

* [User interaction](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/UserInteractionTutorial.md)

* [Events](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/EventsTutorial.md)

* [SoundFonts](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SoundFontsTutorial.md)

* [Samples](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SamplesTutorial.md)

* [Scores](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ScoresTutorial.md)

* [Signal segments](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SignalSegmentsTutorial.md)

* [Granular synthesis](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/GranularSynthesisTutorial.md)

* [Widgets for live performances](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/LiveWidgetsTutorial.md)

* [Real-world instruments show case](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/InstrumentsShowCase.md)

-------------------------------------------------------------------

Appendix:

* [Quickstart guide](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/appendix/QuickStart.markdown)

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
