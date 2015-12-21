
Csound API. Using generated code with another languages
==================================================================

The cool thing about Csound is that it's not only a text to audio converter.
It's also a C-library! Also it has bindings to many languages! 
Python, Java, Clojure, Lua, Clojure, Csharp,  C++, racket, VB!
Also it works on Android, iOS, and RaspPi. 

We can create audio engine with Haskell and then we can 
wrap it in the UI written in some another language!
Let's look at how it can be done.

Interaction with generated code.
-------------------------------------------------------

We can interact with Csound with two main methods. 

* Channels for updating values.

* Named instruments for triggering instruments.
    
### Channels

With channel we can update specific value inside Csound code.
We can create a global channel and then send write or read values 
with another program.

We can make a channel and the we can read/write values.
We can pass four types of values:

* Constant doubles (`chnGetD` / `chnSetD`)

* Control rate signals. Signals to control the audio (`chnGetCtrl` / `chnSetCtrl`)

* Audio rate signals. Signals that encode the audio output. (`chnGetSig` / `chnSetSig`)

* Strings (`chnGetStr` / `chnSetStr`)

Let's create a pair of channels to control the volume and the frequency of audio signal:

~~~haskell
module Main where

volume      = makeChn "volume"
frequency   = makeChn "frequency"

instr = do
    vol <- chnGetCtrl volume
    cps <- chnGetCtrl frequency
    return (vol * osc cps)

main = writeCsd "osc.csd" instr
~~~

So we have a file `main.csd` that encodes our audio engine.
Let's create Python program to control it with sliders.
We are going to use the the python's built in UI-library tkinter.
Also to use Csound API we need to install the python bindings.
On Debian/Ubuntu we can install it with `apt-get`:

~~~
> sudo apt-get python-csound
~~~

On Windows it's installed with Csound installer. You can download it
from the official Csound site. On OSX we can install it with `brew`.

### How to use Csound API

There is a cool GitHub project [Csound API examples](https://github.com/csound/csoundAPI_examples)
that shows how to ue the Csound with various languages.
We can quikly check how to use the audio engine that we have generated
with our language of choice. The python examples is based on the information
from this repo.

So let's create a couple of sliders and plug them to Csound:

~~~python

~~~






We can trigger Csound instruments with notes. 

With channels we can update a continuous signal.






---------------------------------------------------------

* <= [Granular synthesis](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/GranularSynthesisTutorial.md)

* => Happy Haskelling / Csounding

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)