Csound-expression guide
===============================================================================

Welcome to the simplest text based synthesizer. 

~~~{.haskell}
> dac $ osc 440
~~~

Csound-expression is a framework for creation of computer music.
It's a Haskell library to make Csound much more friendly.
It generates Csound files out of Haskell code.

Csound is really awesome. It features unlimited polyphony, hundreds of synth-units 
including FM, granular synth, frequency domain transforms and many more.
Midi and OSC control, compatible with JACK. With JACK it's easy to use with your DAW
of choice. It can run on mobile devices and even in the web browser. 
It has the support for GUI-widgets. It can be used for simple daily 
sound-file processing or for a full-blown live performances.

But Csound is clumsy. It's an old  C-style language. We can boost it with 
functional programming. The Csound-expresion gives you eloquence of Haskell 
combined with power of Csound.

With the help of the library we can create our instruments on the fly. 
A few lines in the interpreter is enough to get the cool sound going
out of your speakers. Some of the features of the library are heavily inspired
by reactive programming. We can invoke the instruments with event streams.
Event streams can be combined in the manner of reactive programming. 
The GUI-widgets are producing the event streams as a control messages. 
Moreover with Haskell we get all standard types and functions like 
lists, maps, trees. It's a great way to organize functions and data.

Let's look at how we can create computer music with Haskell. 
If you are a Csounder that stumbled upon this page and got interested then
it's better to learn some Haskell. The basic level is enough to use 
the library. I recommend the great book on the subject 
[Learn you a Haskell for a Great Good](http://learnyouahaskell.com/) by Miran Lipovaca.

* Instalation guide

* 10 minutes guide for the impatient

* Introduction to Csound for Haskell users

* Basic types

* Rendering Csound files

* Basics of sound synthesis

* User interaction

* Events

* Scores

* Real-world instruments show case

* Overview of the library (most used functions)
