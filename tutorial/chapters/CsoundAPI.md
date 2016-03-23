
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

### How to use channels with Python

There is a cool GitHub project [Csound API examples](https://github.com/csound/csoundAPI_examples)
that shows how to ue the Csound with various languages.
We can quikly check how to use the audio engine that we have generated
with our language of choice. We are going to illustrate the 
Csound API workflow with Python. The python examples is based on the information
from this repo.


~~~python
import csnd6

class Controll:
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
> from oscil import Controll
> c1 = Controll(0.5, 220)
> c1.set_frequency(440)
> c1.set_volume(0.3)
> c1.set_volume(0.1)
> c1.close()
~~~Controll

We can instantiate several Csound audio engines!

~~~python
> c1 = Controll(0.5, 220)
> c2 = Controll(0.3, 330)
> c3 = Controll(0.6, 110)
> c3.set_frequency(150)
>
> for c in [c1, c2, c3]:
>   c.close()
~~~

With channels we can update a continuous signal.
With Csound API we can also trigger the instruments with notes or messages.

### Messages

We can send messages to instruments. To send the message we need to know
the numeric identifier of the instrument. When we use Csound directly
we know what numbers do we assign to the instruments. But Haskell wrapper
hides this process from us. 

Csound also provides named instruments. We can assign not only unique numeric
value to the instrument, but also a name as a string. There is no need to use
the named instruments in the haskell wrapper since we can use plain haskell values
to construct instruments and framework will take care about allocation
of integer identifiers. But named insturments can help us when we want 
to trigger instrument with program that is written in another language through Csound API.

There is a function:

~~~haskell
trigByName :: (Arg a, Sigs b) => String -> (a -> SE b) -> SE b
trigByName name instrument = aout
~~~

It takes an instrument name and instrument definition and creates
an instrument with the given name. We can not use this instrument 
with Haskell. There are no way to trigger it. But we can trigger it
with Csound API. 

All basic Csound API functions can be found in the module `Csound.Control.Instr` (see the API section).

Let's write a simple program:

~~~haskell
module Main where

import Csound.Base

instr :: (D, D) -> SE Sig
instr (amp, cps) = return $ (sig amp) * fades 0.01 0.1 * osc (sig cps)

main = writeCsd "message.csd" $ trigByName "osc" instr
~~~

If we run this code with `runhaskell` it will produce the `message.csd` file that 
contains the definition of our audio engine.

We create an instrument  that has name `osc`. It takes in amplitude and frequency and produces mono output.

The Csound API the csound thread object has a method `InputMessage`.
That takes in a string with Csound note-triggering expression. 
If you know the Csound the syntax of `i-score` statment should be straightforward to you.
But don't skip the next section. It explains not only the Csound syntax but also
how it's related to Haskell code.

#### Csound i-score statment

The Csound musicians trigger insturmnets with `i`-score statements. It can look like this:

~~~
i "osc" 0 10 0.5 220
~~~

The `i` is special syntax for `i`-statement. Then goes the list of arguments that are separated with spaces.
The first argument is the instrument identifier. It's an integer number or string (note the mandatory double quotes).
Then we can see two parameters that are hidden from the haskell user. It's delay to trigger the note
and note duration. Both are in seconds. In the example we have a note with no delay that lasts for 10 seconds.
Then we can see the arguments that our haskell-instrument  takes in. They are amplitude value and frequency value.

The `InputMessage` code for our python code looks like this:

~~~python
    def play(self, delay, duration, volume, frequency):
        self.thread.InputMessage("i \"%s\" %f %f %f %f" % ("osc", delay, duration, volume, frequency))
~~~

We use python string-formating syntax to substitute `f`'s with floats and `s`'s with strings.
Note the escaped double quotes in the python code!

#### Example continued

Now we are ready to look at the python code:

~~~python
import csnd6

class Audio:
    def __init__(self):
        engine = csnd6.Csound()
        engine.SetOption("-odac")
        engine.Compile("message.csd") 

        thread = csnd6.CsoundPerformanceThread(engine) 
        thread.Play()              

        self.engine = engine
        self.thread = thread        

    def play(self, delay, duration, volume, frequency):
        self.thread.InputMessage("i \"%s\" %f %f %f %f" % ("osc", delay, duration, volume, frequency))

    def close(self):
        self.thread.Stop()        
        self.thread.Join()     
~~~

The initialization and termination of audio engine are the same as in the previous example.
The new funtion is `play`. The syntax is already explained. We take in dleay to trigger the note,
note's duration and pair of our Haskell parameters (amplitude and frequency).

Let's try out our engine in the python interpreter:

~~~python
$ python
> from message import Audio
> c = Audio()
> c.play(1, 3, 0.5, 220)
> c.play(0, 2, 0.3, 330)
> c.close()
~~~

#### Triggering instruments as procedures

Sometimes we don't want to produce the sound as the response to messages.
Sometimes we want to update some parameters. You can imagine a drone sound going on
or arpeggiator and we want to update a note or LFO rate with message. 
To do it we can use the function:

~~~haskell
trigByName_ :: (Arg a) => String -> (a -> SE ()) -> SE ()
trigByName_ name instrument = aout
~~~

Note the underscore at the end. It creates a named procedure.
The procedure can be called with Csound API just in the same way as
an ordinary instrument. It's useful to know the `turnoff` function.
It turns the instrument off. By default all Csound instrument last
for some time. With `turnoff` we can simulate instant reaction procedure.
It does some work (robably updates the global parameters) and then 
it turns itself off. The pattern of usage looks like this:

~~~haskell
procedure args = do
    doSomeStuff
    turnoff

main = trigByName_ "update_param" procedure 
~~~

#### Creation of MIDI-controlled instruments

If we want to create a VST plugin we want to be able to control
our csound insturment in the MIDI-like manner. 
We want to send note on and note off messages. This functionality
can be simulated with `trigByName_` function and global variables.
There are predefined library function that already implement this 
behavior:

~~~haskell
trigByNameMidi :: (Arg a, Sigs b) => String -> ((D, D, a) -> SE b) -> SE b
trigByNameMidi name instrument = ...
~~~

The instrument takes in two mandatory arguments: pitch and amplitude midi-keys.
It produces an audio signal as output. We can use it with Csound API 
just as in previous examples. We have special format for Csound arguments
to simulate note-on/off behavior:

~~~
i "givenName" delay duration 1 pitchKey volumeKey auxParams     -- note on
i "givenName" delay duration 0 pitchKey volumeKey auxParams     -- note off
~~

Alongside with delay and duration we have another hidden argument. It's  the fourth argument. 
It's `1` for note on and `0` for note off. Which note to turn off is determined by pitch key.

There is a procedure version of the function:

~~~haskell
trigByNameMidi_ :: (Arg a, Sigs b) => String -> ((D, D, a) -> SE ()) -> SE ()
trigByNameMidi_ name instrument = ...
~~~

#### Monophonic MIDI-controlled instruments

The monophonic instruments need special treatment:

~~~haksell
trigNamedMono :: D -> D -> String -> SE (Sig, Sig)
trigNamedMono portamentoTime releaseTime name = ...
~~~

The function is located at the module `Csound.Control.Midi` (see section Mono-midi synth).

The argument list for Csound is the same as for normal midi instruments.

~~~
i "givenName" 1 delay duration pitchKey volumeKey     -- note on
i "givenName" 0 delay duration pitchKey volumeKey     -- note off
~~~

#### MIDI-controlled patches

There are predefined midi-like named functions for patches (see section Csound API at the module `Csound.Air.Patch`):

~~~haskell
patchByNameMidi :: (SigSpace a, Sigs a) => String -> Patch D a -> SE a
patchByNameMidi name patch = ...

monoPatchByNameMidi :: (SigSpace a, Sigs a) => String -> Patch Sig a -> SE a
monoPatchByNameMidi name patch = ...

monoSharpPatchByNameMidi :: (SigSpace a, Sigs a) => String -> Patch Sig a -> SE a
monoSharpPatchByNameMidi name patch = ...
~~~

If you are interested in non-trivial application that uses Csound API
you can look at the python synthesizer called [tiny-synth](https://github.com/anton-k/tiny-synth).
It uses functions for named midi-controlled patches. It features 100+ patches from the standard
collection of `csound-expression` instruments. 

## Example: Audio player

Let's create a command line audio player

---------------------------------------------------------

* <= [Granular synthesis](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/GranularSynthesisTutorial.md)

* => Happy Haskelling / Csounding

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)