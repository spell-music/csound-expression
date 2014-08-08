
User interaction
=============================

Let's explore the ways of how we can interact with instruments.

Midi-instruments
------------------------------------

The simplest way to create a responsive instrument is
to make a midi-instrument. A Midi-instrument is something 
that expects a midi-message and produces sound output.
A midi-message contains pitch and volume of the note and
possibly some control data (to change the parameters of the synth).

The midi-message is represented with opaque type:

~~~haskell
data Msg

cpsmidi :: Msg -> D   		-- extract frequency (Hz)
ampmidi :: Msg -> D -> D  	-- extract amplitude (0 to second argument)

ampCps  :: Msg -> (D, D)    -- ampmidi & cpsmidi, amplitude is 0 to 1
~~~

We can extract amplitude and frequency (Hz) with function `ampCps`.

The midi-intrument listens for message on the specified channel (It's 
an integer from 1 to 16):

~~~haskell
type Channel = Int
~~~

The simplest function for midi instruments is:

~~~haskell
midi :: Sigs a => (Msg -> SE a) -> a
~~~

It creates a signal that is produced from the output of midi-instrument.
A midi-instrument listens for messages on all channels.

There are two more refined functions:

~~~haskell
midin  :: Sigs a => Channel -> (Msg -> SE a) -> a
pgmidi :: Sigs a => Maybe Int -> Channel -> (Msg -> SE a) -> a
~~~

They allow to specify a midi-channel and probably a midi-program.
Shortly after creation of the Midi-protocol it was understood that
1 to 16 channels is not enough. So there come the programs. You can 
specify a midi instrument with 16 channels and 128 programs.
We can specify a program with function `pgmidi`.

If you have a real midi-keyboard connected to your computer (most often with USB)
you can start to play along with it in csound-expression. Just type:

~~~haskell
> ghci
> :m +Csound.Base
> let instr msg = return $ 0.25 * (fades 0.1 1) * (sig $ ampmidi msg 1) * saw (sig $ cpsmidi msg)
> dac $ smallRoom $ midi instr
~~~

If we don't have the midi-device we can test the instrument with virtual one.
We need to use `vdac` in place of `dac`:

~~~haskell
> vdac $ smallRoom $ midi instr
~~~

We have created a simple saw-based instrument. The function `fades` adds 
the attack and release phase for the instrument. It fades in with time of the
first argument and fades out after release with time of the second argument.
We used a lot the function `sig :: D -> Sig`. It's just a converter.
It constructs signals from the constant values.

Yo can notice how long and boring the expression for the instrument is.
Instrument expects a midi-message. Then we have to extract amplitude and frequncy
and convert it to signals and apply to the instrument. It's a typical pattern
that repeats over and over again. There is a type class that converts functions
to midi-instruments. It's called `MidiInstr`:

~~~haskell
class MidiInstr a where
	type MidiInstrOut a :: *
	onMsg :: a -> Msg -> SE (MidiInstrOut a)
~~~

It converts a value of type `a` to midi-instrument. 
There are plenty of instances for this class. We can check them out
in the docs. Among them we can find the instance for the type:

~~~haskell
Sig -> Sig
~~~

It's assumed that single argument is a frequency (Hz). This instrument
is a wave-form. To convert it to midi-instrument we apply midi-frequency to it
(it's converted to signal) and scale it with midi-amplitude. So we can redefine
our instrument like this:

~~~haskell
> let instr = onMsg $ mul (0.25 * fades 0.1 1) . saw
> dac $ smallRoom $ midi instr
~~~

The function `mul` scales the signal-output like types.
They are all tuples of `Sig` probably wrapped in the type `SE`.


### Continuous midi-instruments

So far every midi-instrument has triggered the instrument in the separate note instance.
In the end we get the sum of all notes. It's polyphonic mode. But what if we
want to use synth in monophonic mode. So that frequency and amplitude are continuous
signals that we can use in the other instruments. 

There are two functions for this mode:

~~~haskell
monoMsg     :: D -> D -> SE (Sig, Sig)
monoMsg portamentoTime releaseTime

holdMsg :: D ->   -> SE (Sig, Sig) 
holdMsg portamentoTime
~~~

Both of them produce amplitude and frequency as time varied signals.
The former fades out when nothing is pressed and the latter holds the
last value until the next one is present. 

The first argument for both of them is portamento time. It's
time in second that it takes for transition from one value to another.
The function `monoMsg` takes another parameter that specifies a release time.
Time it takes for the note to fade out or fade in.

Let's play with these functions:

~~~haskell
> vdac $ fmap smallRoom $ fmap (\(amp, cps) -> amp * tri cps) $ monoMsg 0.1 1
> vdac $ fmap smallRoom $ fmap (\(amp, cps) -> amp * tri cps) $ holdMsg 0.5 
~~~


Midi-controls
------------------------------------

If our midi-device has some sliders or knobs we can 
send the control-messages. Control messages allow us 
to change parameters for the instruments during performance.

We can use the function `ctrl7`:

~~~haskell
ctrl7 :: D -> D -> D -> D -> Sig
ctrl7 chno ctrlId imin imax
~~~

It expects the channel number (where we listen for the control messages),
the identity number of control parameter, and two parameters for minimum 
and maximum of the output range. Let's apply the filter to the output of
the previous example:

~~~haskell
> vdac $ fmap smallRoom $ fmap (\(amp, cps) -> amp * mlp (ctrl7 1 1 50 5000) (ctrl7 1 2 0.1 0.9) (tri cps)) $ holdMsg 0.5
~~~

We can look at sound response to the filter parameters in real-time.

Another function that is worth to mention is:

~~~haskell
initc7 :: D -> D -> D -> SE ()
initc7 chno ctrlId val 				-- value ranges from 0 to 1
~~~

It sets the initial value for the midi control.

~~~haskell
> let ctrl = 1
> let out = fmap smallRoom $ fmap (\(amp, cps) -> amp * mlp (ctrl7 1 ctrl 50 5000) 0,5 (tri cps)) $ holdMsg 0.5
> dac $ do { initc7 1 ctrl 0.5; out }
~~~

Unfortunately the function `initc7` doesn't work with virtual midi. It's only for real midi-devices.

There are three more functions to make things more easy:

~~~haskell
midiCtrl7 :: D -> D -> D -> D -> D -> SE Sig
midiCtrl7 chanNum ctrlNum initVal min max
~~~

It combines the functions `ctrl7` and `initc7`. So that we don't have to 
specify the same channel number and control number twice. 

There are functions for specific ranges

~~~haskell
midiCtrl, umidiCtrl :: D -> D -> D -> SE Sig
~~~

They are the same as midiCtrl7, but former sets the range to `[-1, 1]` and
the latter to `[0, 1]`.

Basics of GUI
------------------------------------

If we don't have real sliders and knobs we can use the virtual ones.
It can be done easily with GUI-elements. Csound has support for GUI-widgets.
GUI-widgets live in the module `Csound.Control.Gui`.

Let's study how can we use them. First of all let's define the notion of
widget. A widget is something that contains graphical representation 
(what do we see on the screen) and behaviour (what can it do).

A slider for instance is represented as a moving small line segment in the box. 
It's a graphical representation of the slider. At the same time the slider can give us a time
varying signal. It's behaviour of the slider. There are different types of behaviour.
Some widgets can produce the values (like sliders or buttons). They are sources. 
Some widgets can wait for the value (like text box that shows the value on the screen).
They are sinks. Some widgets can do all this in the same time and some widgets can 
do neither (like static text. It's only visible but it can not do anything).

In the Haskell type system we can express it like this:

~~~haskell
data Gui    -- visual representation

type Widget a b = SE (Gui, Output a, Input b, Inner)

type Input a = a 				-- produces a value
type Output a = a -> SE ()		-- waits for a value
type Inner = SE ()				-- does smth useful

type Sink a = SE (Gui, Output a)	-- value consumer
type Source a = SE (Gui, Input a)   -- value producer
type Display = SE Gui 				-- static element
~~~

Let's look at the definition of the slider:

~~~haskell
slider :: String -> ValSpan -> Double -> Source Sig
slider tag valueRange initValue
~~~

The slider expects a tag-name, value range and initial value.
It produces a `Source`-widget that contains a signal.

The value type specifies the value range and the type of
the change of the value (it can be linear or exponential).

~~~haskell
linSpan, expSpan :: Double -> Double -> ValSpan

linSpan min max
expSpan min max
~~~

Let's define a slider in the ghci:

~~~haskell
> let vol = slider "volume" (linSpan 0 1) 0.5
> dac $ do { (gui, v) <- vol; panel gui; return (v * osc 440) }
~~~

We can control the volume of the concert A note with the slider!
To see the slider we have to place it on the window. That is why
we used the function `pannel`:

~~~haskell
pannel :: Gui -> SE ()
~~~

It creates a window and renders the graphical representation of
the GUI on it. You can notice the strange quirk of the slider
it updates the values in reverse. The top is lowest value
and the bottom is for the highest value. It's strange implementation
of the vertical sliders in the Csound. We can only take it for granted.

Ok, ok. That it's good but how about using two sliders at the same time? 
We can create the second slider and place it right beside the other with
function `hor`. It groups a list of widgets and shows them side by side:

~~~haskell
> let vol = slider "volume" (linSpan 0 1) 0.5
> let pch = slider "pitch" (expSpan 20 3000) 440
> dac $ do { (vgui, v) <- vol; (pgui, p) <- pch ; panel (hor [vgui, pgui]); return (v * osc p) }
~~~

Try to substitute `hor` for `ver` and see what happens.

### The layout functions

We can see how easy it's to use the `hor` and `ver`. Let's study all
layout functions:

~~~haskell
hor :: [Gui] -> Gui
ver :: [Gui] -> Gui

space :: Gui
sca   :: Double -> Gui -> Gui

padding :: Int -> Gui -> Gui
margin  :: Int -> Gui -> Gui
~~~

The functions `hor` and `ver` are for horizontal and vertical grouping of the elements.
The `space` creates an empty space. The `sca`  can scale GUIs. The `margin` and `padding`
are well .. mm .. for setting the margin and padding of the element in pixels.

We can stack as many sliders as we want. Let's explore the low-pass filtering of 
the saw waveform.

~~~haskell
> let cfq = slider "center frequency" (expSpan 100 5000) 2000
> let q = slider "resonance" (linSpan 0.1, 0.9) 0.5
> dac $ do { 
	(vgui, v) <- vol; 
	(pgui, p) <- pch; 
	(cgui, c) <- cfq; 
	(qgui, qv) <- q; 
	panel (ver [vgui, pgui, cgui, qgui]); 
	return (v * mlp c qv (saw p)) 
}
~~~

### Widgets

#### Knobs

There are many more widgets. Let's turn some sliders into knobs.
The knob is a sort of circular slider:

~~~haskell
> let vol = knob "volume" (linSpan 0 1) 0.5
> let pch = knob "pitch" (expSpan 20 3000) 440
> dac $ do { 
	(vgui, v) <- vol; 
	(pgui, p) <- pch; 
	(cgui, c) <- cfq; 
	(qgui, qv) <- q; 
	panel (ver [vgui, pgui, hor [cgui, qgui]]); 
	return (v * mlp c qv (saw p)) 
}
~~~

Now the sliders look to big we can change it with function `sca`:

~~~haskell
	...
	panel (ver [vgui, pgui, sca 1.5 $ hor [cgui, qgui]]); 
	...
~~~

#### Numeric values

Numeric creates a time varying signal like a slider. 
But it's graphical representation is different. It's 
a box with a number inside it. You can change the value by dragging
the mouse from the box.

~~~haskell
numeric :: String -> ValDiap -> ValStep -> Double -> Source Sig
numeric tag valueDiapason valueStep initialValue
~~~


#### Buttons

Let's create a switch button. We can use a `toggleSig` for it:

~~~haskell
toggleSig :: String -> Source Sig
~~~

This function just creates a button that produces a signal that
is 1 whenthe button is on and 0 when it's off.

~~~haskell
> let switch = toggleSig "On/Off"
> dac $ do { 
	(sgui, sw) <- switch;
	(vgui, v) <- vol; 
	(pgui, p) <- pch; 
	(cgui, c) <- cfq; 
	(qgui, qv) <- q; 
	panel (ver [vgui, pgui, hor [sgui, cgui, qgui]]); 
	return (sw * v * mlp c qv (saw p)) 
}
~~~

We can make the gradual change wit portamento:

~~~haskell
...
	return (port sw 0.7 * v * mlp c qv (saw p)) 
...
~~~

Buttons can produce the event streams:

~~~haskell
button :: String -> Source (Evt Unit)
~~~

The event stream `Evt a` is something that can apply a procedure of
the type `a -> SE ()` to the value when it happens.

There is a function:

~~~haskell
runEvt :: Evt a -> (a -> SE ()) -> SE ()
~~~

Also event streams can trigger notes with:

~~~haskell
trig  :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, D, a) -> b
sched :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, a) -> b
~~~
The function `trig` invokes an instrument `a -> SE b` when 
the event happens. The note is a triple `(D, D, a)`. It's
`(delayTime, durationTime, instrumentArgument)`. The function 
`sched` is just like `trig` but delay time is set to zero
for all events. So that we need only a pair in place of the triple.

Let's create two buttons that play notes:

~~~haskell
> let n1 = button "330"
> let n2 = button "440"
> let go x evt = sched (const $ instr x) (withDur 2 evt)
> let instr x = return $ fades 0.1 0.5 * osc x
>  dac $ do { 
	(g1, p1) <- n1; 
	(g2, p2) <- n2; 
	panel $ hor [g1, g2]; 
	return $ mul 0.25 $ go 330 p1 + go 440 p2 
}
~~~

The new function `withDur` turns a single value into
pair that contsants a duration of the note in the first cell.

We can do it with a little bit more simple expression if we know
that events are functors and monoids. With Monoid's append we can get 
a single event stream that contains events from both event streams.

Let's redefine our buttons:

~~~haskell
> let n1 = mapSource (fmap (const (330 :: D))) $ button "330"
> let n2 = mapSource (fmap (const (440 :: D))) $ button "440"
~~~

The function `mapSource` maps over the value of the producer widget.
Right now every stream contains a value for the frequency with it.
Let's merge two streams together and invoke the instrument on the
single stream. The result should be the same:

~~~haskell
> let instr x = return $ fades 0.1 0.5 * osc x
> dac $ do { 
	(g1, p1) <- n1; 
	(g2, p2) <- n2; 
	panel (hor [g1, g2]); 
	return $ mul 0.25 $ sched (instr . sig) (withDur 2 $ p1 <> p2)   
}
~~~

#### Box

With boxes we can just show the user some message. 

~~~haskell
box :: String -> Display 
~~~

Let's say something to the user. 

~~~haskell
> dac $ do { 
	gmsg <- box "Two buttons. Here we are."
	(g1, p1) <- n1; 
	(g2, p2) <- n2; 
	panel (ver [gmsg, hor [g1, g2]]); 
	return $ mul 0.25 $ sched (instr . sig) (withDur 2 $ p1 <> p2)   
}
~~~

#### Radio-buttons

Radio buttons let the user select a value from the set of choices.

~~~haskell
radioButton :: Arg a => String -> [(String, a)] -> Int -> Source (Evt a)
~~~

Let's redefine our previous example:

~~~haskell
> let ns = radioButton "two notes" [("330", 330 :: D), ("440", 440)] 0
> dac $ do { 
	(gui, p) <- ns; 
	panel gui; 
	return $ mul 0.25 $ sched (instr . sig) (withDur 2 p) 
}
~~~

#### Meter

We have studied a lot of sources. Is there any sink-widgets?
The `meter` is the one. It let's us monitor the value of the signal:
It shows the output as the slider:

~~~haskell
> let sa = slider "a" (linSpan 1 10) 5
> let sb = slider "b" (linSpan 1 10) 5
> let res = setNumeric "a + b" (linDiap 2 20) 1 10
> dac $ do { 
	(ga, a) <- sa; 
	(gb, b) <- sb; 
	(gres, r) <- res; 
	panel $ ver [ga, gb, gres]; 
	r (a + b) 
}
~~~

### Making reusable widgets

We can make reusable widgets with functions:

~~~haskell
sink    :: SE (Gui, Output a) -> Sink a
source  :: SE (Gui, Input a) -> Source a
display :: SE Gui -> Display
~~~

Let's make a reusable widget for a Moog low-pass filter. 
It's a producer or source. It's going to produce a 
transformation `Sig -> Sig`:

~~~haskell
import Csound.Base

mlpWidget :: Source (Sig -> Sig)
mlpWidget = source $ do
	(gcfq, cfq) <- slider "center frequency" (expSpan 100 5000)  2000
	(gq,   q)   <- slider "resonance"        (linSpan 0.01 0.9)  0.5	
	return (ver [gcfq, gq], mlp cfq q)
~~~

Let's save this definition in the file and load it in ghci. 
Now we can use it as a custom widget:

~~~haskell
> dac $ do { 
	(g, filt) <- mlpWidget; 
	panel g; 
	return $ mul 0.5 $ filt $ saw 220 
}
~~~

Notice that a widget can produce a function as a value!

Let's define another widget for saw-oscillator:

~~~haskell
sawWidget :: Source Sig
sawWidget = source $ do
	(gamp, amp) <- slider "amplitude" (linSpan 0 1) 0.5	
	(gcps, cps) <- slider "frequency" (expSpan 50 10000) 220
	return (ver [gamp, gcps], amp * saw cps)	
~~~

Now let's use them together:

~~~haskell
 dac $ do { 
 	(gw, wave) <- sawWidget; 
 	(gf, filt) <- mlpWidget; 
 	panel $ ver [gw, gf]; 
 	return $ filt wave 
 }
~~~

Open sound control protocol (OSC)
--------------------------------------

Open sound control is a modern data transfer protocol
that should supersede the Midi protocol. It's much more
lightweight and efficient. It can be used over network to
orchestrate a lot of instruments.

We can send or receive the data over network on the specified port.
We should declare the port, the address of the data and 
the type of the expected data.

The port is an integer. The address is a path-like string:

~~~haskell
"/foo/bar"
"/note"
~~~

The type of the data is a string of special characters.
The string can contain the characters "cdfhis" which stand 
for character, double, float, 64-bit integer, 32-bit integer, and string.

There are special type synonyms for all these terms:

~~~haskell
type OscPort = Int
type OscAddress = String
type OscType = String
type OscHost = String
~~~

There are two modes. We can listen for the OSC-messages or we can 
send them. 

### Listening for messages

To listen for the events we have to create a background process.
It waits for messages on the given port:

~~~haskell
initOsc :: OscPort -> OscRef
initOsc port
~~~

We can specify an integer port. It gives us a reference to the process
which should be used in the function `listenOsc`:

~~~haskell
listenOsc :: Tuple a => OscRef -> OscAddress -> OscType -> Evt a
listenOsc ref addr type =
~~~

The function `listenOsc` produces a stream of OSC-messages that are
coming on the given port, address and have a certain type.


### Sending messages

To send OSC-messages we can use the function `sendOsc`:

~~~haskell
sendOsc :: Tuple a => OscHost -> OscPort -> OscAddress -> OscType -> Evt a -> SE ()
~~~

The Osc-messages are coming from the event-stream. We send them
to the machine with given host name (an empty string means the local machine).
We also specify the OSC-address (it's a path-like string) and type of the messages.


Jack-instruments
---------------------------------------

With Jack-interface (native for Linux, also there are ports for OSX and PC) 
we can stream the output of one program to the input of another one. 
With Jack we can use our Csound instruments in DAW-software 
(like Ardour, Cubase, Abletone or BitWig).

We can create Jack-instrument if we set the proper options.
We have to set the name of the instrument:

~~~haskell
setJack :: String -> Options
setJack clientName
~~~

We have to set the proper rates (audio and control rates)

~~~haskell
setRates :: Int -> Int -> Options
setRates sampleRate blockSize
~~~

Sample rate is a resolution of the output audio (typical values are 44100 or 48000).
It should be the same as for the JACK.
The block size is how many samples are in the control period.
We have to process the control signals at the lower rate. The `blockSize`
specifies the granularity of the control signals (typical values are 64, 128, 256).

We have to set the hardware and software buffers (It's `B` and `b` flags in the Csound):

~~~haskell
setBufs :: Int -> Int -> Options
setBufs totalBufferSize  singlePeriodSize
~~~

To send or receive the values from the JACK Csound uses the buffer.
We have to define the size of the whole buffer (the first argument)
and the one period of the buffer (it should be integer multiplier of
the blockSize). 

To set all these properties we need to use the `Monoid` instance for `Options`.
We need to append all the options:

~~~haskell
> options = mconcat [ setJack "anInstrument", setRates 44800 64, setBufs 192 64 ] 
> dacBy options asig
~~~


----------------------------------------------------

* <= [Basics of sound synthesis](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SynthTutorial.md)

* => [Events](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/EventsTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)