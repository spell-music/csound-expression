
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
midi :: Sigs a => (Msg -> SE a) -> SE a
~~~

It creates a signal that is produced from the output of midi-instrument.
A midi-instrument listens for messages on all channels.

There are two more refined functions:

~~~haskell
midin  :: Sigs a => Channel -> (Msg -> SE a) -> SE a
pgmidi :: Sigs a => Maybe Int -> Channel -> (Msg -> SE a) -> SE a
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
> import Csound.Base
> instr msg = return $ 0.25 * (fades 0.1 0.5) * (sig $ ampmidi msg 1) * saw (sig $ cpsmidi msg)
> dac $ room 0.25 $ fmap fromMono $ midi instr
~~~

If we don't have the midi-device we can test the instrument with virtual one.
We need to use `vdac` in place of `dac`:

~~~haskell
> vdac $ room 0.25 $ fmap fromMono $ midi instr
~~~

We have created a simple saw-based instrument. The function `fades` adds
the attack and release phase for the instrument. It fades in with time of the
first argument and fades out after release with time of the second argument.
We used a lot the function `sig :: D -> Sig`. It's just a converter.
It constructs signals from the constant values. The function `fromMono`
converts mono signal to stereo. The `mixAt` pplies an effect with given
dry/wet ratio. The value 0 is all dry and the 1 is all wet.

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
> instr = onMsg $ mul (0.25 * fades 0.1 1) . saw
> dac $ room 0.25 $ fmap fromMono $ midi instr
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
data MidiChn = ChnAll | Chn Int | Pgm (Maybe Int) Int

monoMsg :: MidiChn -> D -> D -> SE (Sig, Sig)
monoMsg portamentoTime releaseTime

holdMsg :: MidiChn -> D -> SE (Sig, Sig)
holdMsg portamentoTime
~~~

Both of them produce amplitude and frequency as time varied signals.
The former fades out when nothing is pressed and the latter holds the
last value until the next one is present.

The first argument for both of them is specification of the midi channel.
The second argument is portamento time. It's
time in second that it takes for transition from one value to another.
The function `monoMsg` takes another parameter that specifies a release time.
Time it takes for the note to fade out or fade in.

Let's play with these functions:

~~~haskell
> vdac $ chamber 0.2 $ fmap (\(amp, cps) -> amp * tri cps) $ monoMsg ChnAll 0.1 1
> vdac $ chamber 0.2 $ fmap (\(amp, cps) -> amp * tri cps) $ holdMsg ChnAll 0.5
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
> vdac $ room 0.2 $ fmap (\(amp, cps) -> amp * mlp (ctrl7 1 1 50 5000) (ctrl7 1 2 0.1 0.9) (tri cps)) $ holdMsg ChnAll 0.5
~~~

You can try to use the first slider at the virtual midi. It should control the filter parameters in real-time.

Another function that is worth to mention is:

~~~haskell
initc7 :: D -> D -> D -> SE ()
initc7 chno ctrlId val 				-- value ranges from 0 to 1
~~~

It sets the initial value for the midi control.

~~~haskell
> ctrl = 1
> out = fmap smallRoom $ fmap (\(amp, cps) -> amp * mlp (ctrl7 1 ctrl 50 5000) 0.5 (tri cps)) $ holdMsg ChnAll 0.5
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


### Note on installation of GUI

However GUI support is now discontinued in Csound. We can still install it and use it 
but it's not packaged with Csound right away. So it means we need to apply some effort to install
it but bear with I'll guide you through the process:

* install `fltk` package

* Find out your csound installation path:

```
> whereis csound
csound: /usr/local/bin/csound /usr/local/lib/csound /usr/include/csound
```
For me the path prefix is `/usr/local/lib`

* clone [csound plugins](https://github.com/csound/plugins) repo. Make sure that you are on latest **`develop`** branch

* Follow instructions on their installation guide but also make sure to setup the flags `CMAKE_INSTALL_PREFIX` and `USE_LIB64`
    on cmake. For me it looks:
    
    ```
    cmake -DCMAKE_INSTALL_PREFIX=/usr/local -DUSE_LIB64=0 ../
    ```
* After `sudo make install` we can check that it was installed alright by command:

   ```
   > csound -z1 2>&1 | grep FLpanel
   ```
    
   It should produce output.
 
Somewhile ago I've struggled with installation. For me problem was that I used it on master branch 
and master branch has a bug in installation copy path. Hopefully you will avoid that and 
you can have great synths with UIs. It's worth it!

### Continue

Let's study how can we use them. First of all let's define the notion of
widget. A widget is something that contains graphical representation
(it's what we see on the screen) and behaviour (what we can do with it).

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
> vol = slider "volume" (linSpan 0 1) 0.5
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

Ok. That it's good but how about using two sliders at the same time?
We can create the second slider and place it right beside the other with
function `hor`. It groups a list of widgets and shows them side by side:

~~~haskell
> vol = slider "volume" (linSpan 0 1) 0.5
> pch = slider "pitch" (expSpan 20 3000) 440
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

resizeUi :: (Double, Double) -> Gui -> Gui
~~~

The functions `hor` and `ver` are for horizontal and vertical grouping of the elements.
The `space` creates an empty space. The `sca`  can scale GUIs. The `margin` and `padding`
are well .. mm .. for setting the margin and padding of the element in pixels.

We can stack as many sliders as we want. Let's explore the low-pass filtering of
the saw waveform.

~~~haskell
> cfq = slider "center frequency" (expSpan 100 5000) 2000
> q = slider "resonance" (linSpan 0.1 0.9) 0.5
> :set +m
> dac $ do {
	(vgui, v) <- vol;
	(pgui, p) <- pch;
	(cgui, c) <- cfq;
	(qgui, qv) <- q;
	panel (ver [vgui, pgui, cgui, qgui]);
	return (v * mlp c qv (saw p))
}
~~~

With `:set +m` we enter the multiline mode in REPL.
The last expressions spreads across multiple lines.

We can scale the whole window size with function `resizeUi`. It takes in a pair of x and y scale factors.
The difference between `sca` and `resizeUi` is that `sca` scales the *relative* size of the widget
within a container but `resizeUi` affects the default all *absolute* sizes of UI-widgets.

The typical usage of resizeUi is when we created an intricated UI with many widgets
and it doesn't fits the screen we can make everything smaller by calling something like `resizeUi (0.75, 0.75)` on it.

If you find yoursel using this function to much it's probably the defaults of the library are not
good for your screen. You can set the scaling factor globaly with options (see parameter `csdScaleUI`).
We would like to make our own `dac` function that overrides the defaults:

~~~haskell
run = dacBy (def { csdScaleUI = Just (1.5, 1.5) })
~~~

And then we can use our `run` function in place of `dac`.

Note that there is a handy function to rescale sources. The widget source is the most frequently used.
So there is a shortcut to easily adjust the sizes:

 ~~~haskell
 resizeSource :: (Double, Double) -> Source a -> Source a
 resizeSource factorXY = mapGuiSource (resizeUi factorXY)
 ~~~

### Applicative style GUIs

Let's turn back to the example with pitch and volume sliders:

~~~haskell
> vol = slider "volume" (linSpan 0 1) 0.5
> pch = slider "pitch" (expSpan 20 3000) 440
> dac $ do { (vgui, v) <- vol; (pgui, p) <- pch ; panel (ver [vgui, pgui]); return (v * osc p) }
~~~

There is a much more convenient way of writing widgets like this.
We can use applicative style GUIs. There are functions that combine
visual representation and behavior of the UIs at the same time:

~~~haskell
lift1 :: (a -> b) -> Source a -> Source b

hlift2 :: (a -> b -> c) -> Source a -> Source b -> Source c
vlift2 :: (a -> b -> c) -> Source a -> Source b -> Source c

hlift3 :: (a -> b -> c -> d) -> Source a -> Source b -> Source c -> Source d
vlift3 :: (a -> b -> c -> d) -> Source a -> Source b -> Source c -> Source d

hlift4, vlift4 :: ...
hlift5, vlift5 :: ...
~~~

It takes a function to combine the outputs of the widget and the prefix
is responsible for catenation of the visuals. `h` -- means horizontal and
`v` -- vertical.

Let's rewrite the last line of our example:

~~~haskell
> dac $ vlift2 (\v p -> v * tri p) vol pch
~~~

The `(Sigs a => Source a)` is also renderable type and we can apply `dac` to it.
The cool thing is that result of the `vlift2` is an ordinary source-widget. We can
apply another `lift`-function to it.

Let's add a filter:

~~~haskell
> cfq = slider "center frequency" (expSpan 100 5000) 2000
> q = slider "resonance" (linSpan 0.1 0.9) 0.5
> filter = vlift2 (\cps res -> mlp cps res) cfq q
~~~

Notice that our widget produces a function. Like any real functional programming language
Haskell can do it! Let's apply the filter:

~~~haskell
> wave = vlift2 (\v p -> v * tri p) vol pch
> dac $ hlift2 ($) filter wave
~~~

Also there are functions to stack a list of similar widgets:

~~~haskell
hlifts, vlifts :: ([a] -> b) -> [Source a] -> Source b
~~~

Let's create a widget to study harmonics:

~~~haskell
> harmonics cps weights = mul (1.3 / sum weights) $
		sum $ zipWith (\n w -> w * osc (cps * n))(fmap (sig . int) [1 .. ]) weights
> dac $ mul 0.75 $ hlifts (harmonics 110) (uslider 0.75 : (replicate 9 $ uslider 0))
~~~

The `uslider` is convenient alias for creation of linear unipolar anonymous sliders.
The only value it takes is an initial value. That's it! We have created a widget
with ten sliders for harmonic series exploration with just two lines of code!
Also there is a function `xslider` for exponential anonymous sliders. It has the
arguments:

~~~haskell
type Range a = (a, a)

xslider :: Range Double -> Double -> Source Sig
xslider (min, max) init
~~~

The same functions are defined for knobs: `uknob`, `xknob`.

Let's add a couple of controls. We want to change pitch and volume.
We already have the required sliders `vol` and `pch`:

~~~haskell
> harms = hlifts (flip harmonics) (replicate 10 $ uslider 0)
> dac $ vlift3 (\amp cps f -> amp * f cps) vol pch harms
~~~

it's ok by the audio but the picture is ugly. The problem is that `vlift3`
gives the same amount of space to all widgets. But we want to change the proportions.
Right for this task there are functions:

~~~haskell
hlift2', vlift2' :: Double -> Double -> (a -> b -> c) -> Source a -> Source b -> Source c

hlift3', vlift3' :: Double -> Double -> Double -> (a -> b -> c -> d) -> Source a -> Source b -> Source c -> Source d

hlift4', vlift4' :: ...
hlift5', vlift5' :: ...

vlifts', hlifts' :: [Double] -> ([a] -> b) -> [Source a] -> Source b
~~~

They take in scaling factors for each widget. Let's see how they can help us to solve the problem:

~~~haskell
> dac $ vlift3' 0.15 0.15 1 (\amp cps f -> amp * f cps) vol pch harms
~~~

### Widgets

#### Knobs

There are many more widgets. Let's turn some sliders into knobs.
The knob is a sort of circular slider:

~~~haskell
> vol = knob "volume" (linSpan 0 1) 0.5
> pch = knob "pitch" (expSpan 20 3000) 440
> dac $ vlift3 (\v p f -> v * f (saw p))  vol pch filter
~~~

Also there are aliases. Produces unipolar linear anonymous knob:

~~~haskell
uknob :: Double -> Source Sig
uknob initVal = ...
~~~

Produces exponential anonymous knob:

~~~haskell
type Range a = (a, a)

xknob :: Range Double -> Double -> Source Sig
xknob (min, max) init
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
toggleSig :: String -> Bool -> Source Sig
~~~

This function just creates a button that produces a signal that
is 1 whenthe button is on and 0 when it's off. The button is
initialized with value Bool.

~~~haskell
> switch = toggleSig "On/Off" true
> dac $ vlift3 (\v p (sw, f) -> sw * v * f (saw p))  vol pch (hlift2 (,) switch filter)
~~~

We can make the gradual change wit portamento:

~~~haskell
..	smooth 0.7 sw * v * mlp c qv (saw p) ..
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
type Sco a = Track Sig a -- holds notes
sched :: (Arg a, Sigs b) => (a -> SE b) -> Evt (Sco a) -> b
~~~

The `sched` invokes the instrument on event stream.
Each event contains score of notes.

Let's create two buttons that play notes:

~~~haskell
> n1 = button "330"
> n2 = button "440"
> instr x = return $ fades 0.1 0.5 * osc x
> go x evt = sched (const $ instr x) (withDur 1 evt)
> dac $ hlift2 (\p1 p2 -> mul 0.25 $ go 330 p1 + go 440 p2) n1 n2
~~~

The new function `withDur` turns a single value into Score with single note with passed duration.

We can do it with a little bit more simple expression if we know
that events are functors and monoids. With Monoid's append we can get
a single event stream that contains events from both event streams.

Let's redefine our buttons:

~~~haskell
> n1 = mapSource (fmap (const (330 :: D))) $ button "330"
> n2 = mapSource (fmap (const (440 :: D))) $ button "440"
~~~

The function `mapSource` maps over the value of the producer widget.
Right now every stream contains a value for the frequency with it.
Let's merge two streams together and invoke the instrument on the
single stream. The result should be the same:

~~~haskell
> instr x = return $ fades 0.1 0.5 * osc x
> dac $ hlift2 (\p1 p2 -> mul 0.25 $ sched (instr . sig) (withDur 1 $ p1 <> p2)) n1 n2
~~~

#### Box

With boxes we can just show the user some message.

~~~haskell
box :: String -> Display
~~~

Let's say something to the user.

~~~haskell
> gmsg = box "Two buttons. Here we are."
> dac $ gmsg >>= \msg ->
          mapGuiSource (\g -> ver [msg, g]) $
          hlift2 (\p1 p2 -> mul 0.25 $ sched (instr . sig) (withDur 1 $ p1 <> p2)) n1 n2
~~~

The function `mapGuiSource` - maps over Gui value of the source:

~~~haskell
mapGuiSource :: (Gui -> Gui) -> Source a -> Source a
~~~

#### Radio-buttons

Radio buttons let the user select a value from the set of choices.

~~~haskell
radioButton :: Arg a => String -> [(String, a)] -> Int -> Source (Evt a)
~~~

Let's redefine our previous example:

~~~haskell
> ns = radioButton "two notes" [("330", 330 :: D), ("440", 440)] 0
> dac $ lift1 (\p -> mul 0.25 $ sched (instr . sig) (withDur 2 p)) ns
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

### Making reusable source

The applicative style lets us create reusable combos of widgets.

Let's make a reusable widget for a Moog low-pass filter.
It's a producer or source. It's going to produce a
transformation `Sig -> Sig`:

~~~haskell
import Csound.Base

mlpWidget :: Source (Sig -> Sig)
mlpWidget = vlift2 mlp cfq q
  where
    cfq = slider "center frequency" (expSpan 100 5000)  2000
    q   = slider "resonance"        (linSpan 0.01 0.9)  0.5
~~~

Let's save this definition in the file and load it in ghci.
Now we can use it as a custom widget:

~~~haskell
dac $ lift1 (\f -> mul 0.5 $ f $ saw 220) mlpWidget
~~~

Notice that a widget can produce a function as a value!

Let's define another widget for saw-oscillator:

~~~haskell
sawWidget :: Source Sig
sawWidget = vlift2 (\a c -> a * saw c) amp cps
  where
    amp = slider "amplitude" (linSpan 0 1) 0.5
    cps = slider "frequency" (expSpan 50 10000) 220
~~~

Now let's use them together:

~~~haskell
dac $ vlift2 (\wave filt -> filt wave) sawWidget mlpWidget
~~~

### Low-level representation of widgets

Note that any widget is made with just handful of functions:

~~~haskell
type Output a = a -> SE ()
type Input a = a
type Display = SE Gui

sink    :: SE (Gui, Output a) -> Sink a
source  :: SE (Gui, Input a) -> Source a
display :: SE Gui -> Display
sinkSource :: SE (Gui, Output a, Input a) -> SinkSource a
~~~

Thy specify low level behaviour of the widget.

* sink - widget that consumes the value. So it has visual representation
   and function to consume the value.

* source - widget that produces value. So it has  visual  representation
   and holds the value.

* display - has only visuals

* sinkSource - combines all behaviours into one.

Let's see how one of the widgets from the previous example can be made
with low level functions. Let's look at the moog filter widget:

~~~haskell
import Csound.Base

mlpWidget :: Source (Sig -> Sig)
mlpWidget = source $ do
	(gcfq, cfq) <- slider "center frequency" (expSpan 100 5000)  2000
	(gq,   q)   <- slider "resonance"        (linSpan 0.01 0.9)  0.5
	return (ver [gcfq, gq], mlp cfq q)
~~~

Now we can apply it to the value with low level functions:

~~~haskell
> dac $ do {
	(g, filt) <- mlpWidget;
	panel g;
	return $ mul 0.5 $ filt $ saw 220
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

#### Listening for signals

It's often happens that OSC-messages encode a continuous signal. The signal controls
some parameter of the synth. In this case we can use a handy function:

~~~haskell
listenOscVal :: (OscVal a) => OscRef -> String -> a -> SE a
listenOscVal oscRef address initValue
~~~

The `OscVal` signnifies all sorts of tuples of `Str`'s and `Sig`'s.
We listen on the specific port and address. The last argument is an initial value for the signal (or tuple of signals).
When new value comes the output signal is set to that value. It's like sample and hold function
only messages for signal update come from the OSC-channel.

There are two useful aliases for this function. They read signals and pairsof signals:

~~~haskell
listenOscSig  :: (Tuple a, OscVal a) => OscRef -> String -> Sig  -> SE Sig
listenOscSig2 :: (Tuple a, OscVal a) => OscRef -> String -> Sig2 -> SE Sig2
~~~


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
(like Ardour, Cubase, Ableton or BitWig).

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

* => [Scores](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ScoresTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)
