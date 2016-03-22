
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

import Csound.Base

volume      = text "volume"
frequency   = text "frequency"

instr = do
    vol <- chnGetCtrl volume
    cps <- chnGetCtrl frequency
    return (vol * osc cps)

main = writeCsd "osc.csd" instr
~~~

So we have a file `main.csd` that encodes our audio engine.
Let's create Python program to control our audio.
We are going to use Csound API and we need to install the python bindings.
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


~~~python
import csnd6

class Ctrl:
    def __init__(self, volume, frequency):
        engine = csnd6.Csound()
        engine.SetOption("-odac")
        engine.Compile("osc.csd") 

        thread = csnd6.CsoundPerformanceThread(engine) 
        thread.Play()              

        self.engine = engine
        self.thread = thread

        self.set_volume(volume)
        self.set_frequency(frequency)

    def set_volume(self, volume):
        self.engine.SetChannel("volume", volume)

    def set_frequency(self, frequency):
        self.engine.SetChannel("frequency", frequency)

    def close(self):
        self.thread.Stop()        
        self.thread.Join()        
~~~

We create an object that can start a Csound engine and update volume and frequency.
In the initialization step we create an audio engine? load file "osc.csd" to it
and start csound in the separate thread:

~~~python
        engine = csnd6.Csound()
        engine.SetOption("-odac")
        engine.Compile("osc.csd") 

        thread = csnd6.CsoundPerformanceThread(engine) 
        thread.Play()
~~~

Then we save the state for the object:

~~~python
        self.engine = engine
        self.thread = thread
~~~

and set the initial values for frequency and volume:

~~~python
        self.set_volume(volume)
        self.set_frequency(frequency)
~~~

These functions update values for csound channels. 
So with channels we can propagate changes from python to csound:

~~~python
    def set_volume(self, volume):
        self.engine.SetChannel("volume", volume)
~~~

The last method `close` stops the engine:

~~~
    def close(self):
        self.thread.Stop()        
        self.thread.Join()        
~~~

What's interesting with thism code is that we can control our engine within 
the python interpreter. It's very simple skeleton for creation of Live coding with python and haskell combo!
Let's try some commands. Navigate to the directory with our python file `oscil.py`:

~~~python
$ python
> from oscil import Ctrl
> c1 = Ctrl(0.5, 220)
> c1.set_frequency(440)
> c1.set_volume(0.3)
> c1.set_volume(0.1)
> c1.close()
~~~

We can instantiate several Csound audio engines!

~~~python
> c1 = Ctrl(0.5, 220)
> c2 = Ctrl(0.3, 330)
> c3 = Ctrl(0.6, 110)
> c3.set_frequency(150)
>
> for c in [c1, c2, c3]:
>   c.close()
~~~

We can trigger Csound instruments with notes. 

With channels we can update a continuous signal.






---------------------------------------------------------

* <= [Granular synthesis](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/GranularSynthesisTutorial.md)

* => Happy Haskelling / Csounding

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)