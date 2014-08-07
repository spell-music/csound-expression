Csound-expression guide
===============================================================================

Welcome to the simplest textual synthesizer.

~~~{.haskell}
> dac $ osc 440
~~~

Csound-expression is a Haskell framework for computer music.
With the help of the library we can create our instruments on the fly. 
A few lines in the interpreter is enough to get the cool sound going
out of your speakers. It can be used for simple daily sound-file processing 
or for a full-blown live performances.

Let's look at how we can create computer music with Haskell. 

-------------------------------------------------------------------


* [Introduction](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/Intro.md)

* [Basic types](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/BasicTypesTutorial.md)

* [Rendering Csound files](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ProducingTheOutputTutorial.md)

* [Basics of sound synthesis](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SynthTutorial.md)

* [User interaction](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/UserInteractionTutorial.md)

* [Events](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/EventsTutorial.md)

* [Scores](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ScoresTutorial.md)

* [Real-world instruments show case](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/InstrumentsShowCase.md)

* [Appendix A: Quickstart guide]()

* [Appendix B: Introduction to Csound for Haskellers]()

* [Appendix C: Overview of the library]()

-------------------------------------------------------------------

WARNING: the library works best within ghci. The real-time sound rendering 
function dac spawns a child process in the background which may continue 
to execute after you stop the main process that runs the programm. 
It's not so in vim but it happens in the Sublime Editor and when you 
invoke runhaskell. So the best is to write you program in the separate 
file and then load it in the ghci and invoke the function main 
(which runs the sound rendering with the function dac).
