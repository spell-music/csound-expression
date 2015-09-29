Events
================================


Events
=============================

We have learned how to trigger an instrument with the score.
Now we are going to learn how to do it with an event stream.
The model for event streams is heavily inspired with 
functional reactive programming (FRP) though it's not a FRP
model in the strict sense, because our signals are discrete
and not continuous as FRP requires. But nevertheless it's useful
to know the basics of FRP to learn the construction of event streams.

Introduction to FRP
------------------------------------

FRP is a novel approach for description of interactive systems.
It introduces two main concepts: behaviors and event streams. 
A behavior can be though as continuous signal of some value.
It represents the changes in the life of the value. 
What's interesting is that it describes the whole 
life of the value. An event stream contains a value
that may happen sometimes. For example if we have a computer mouse.
The position of the cursor is a behavior that contains two values 
(X and Y) and an event stream is a stream of all clicks
for the mouse's buttons.

In the traditional callback based approach we have
some instrument to register a callback function for the mouse clicks.
The function accepts an event that carries the information about
which button was pressed and what is the position of the mouse. 
When something happens we can update some mutable variables.

With FRP we can manipulate event streams as if they are values.
We can map over the values that are contained in the events.
We can merge two event streams together. We can accumulate
some value based on upcoming events. And we can convert the 
event streams to behaviors. The simplest function that
comes into mind is creation of step-wise constant function.
When something happens on the event stream we hold the value
until the next event fires and updates the value.

~~~
stepper :: a -> Event a -> Behavior a
stepper initVal events
~~~

We have an initial value. It lasts while nothing has happened.

More complicated function is a switch function:

~~~
switch :: Behavior b -> (a -> Behavior b) -> Event a -> Behavior b
switch initVal behaviorProducer events
~~~

The switch applies some behavior constructor to the value of event
when something happens. The resulting behavior lasts until
the next event happens. Then we apply the function again and so on.

With this approach we can build complex behaviors from simple ones.
The key feature is that a single value can contain a whole event stream!
It removes the need for mutable variables. we use mutable
values with callbacks when we want to communicate the changes of
the value from one callback to another. If we want to use the results of
a callback in the rest of the program. 

That's how we can count the clicks of the mouse:

~~~
> showOnScreen $ stepper 0 $ accum 0 (+ 1) $ filter isLeftClick $ mouseClicks
~~~

It's an imaginary code but it shows the idea. The ides is 
that we can take the stream of all mouse clicks. Then we can filter it
so that we get only clicks for the left button. Then we can accumulate
a value over the event stream and in the last function we convert
the stream of counter into the continuous signal and show it on the screen.

The callback based solution can look like this (again it's an imaginary imperative
code written in Haskell):

~~~
counter <- newIORef 0
screen <- newScreen

Mouse.registerCallback $ \evt -> do
    if isLeftClick evt then do
        modifyIORef (+1) counter
        pushValuetoScreen screen =<< readIORef counter
    else do
        return ()
~~~

Triggering instruments with event streams
-----------------------------------

Let's trigger an instrument with event stream.
There is a function:

~~~
sched :: (Arg a, Sigs b) => (a -> SE b) -> Evt (Sco a) -> b
~~~

It takes in an instrument and an event stream of scores. 
Every event contains a score. We have a simple instrument:

~~~
> let bam _ = mul (fades 0.01 0.3) $ pink
~~~

It plays a pink noise. It takes no arguments but the
`sched` function requires an instrument to be a function
so we created an "empty" argument.
Let's trigger it with the stream:

~~~
> dac $ sched bam $ withDur 0.1 $ metro 2
~~~

The `metro` creates an event stream of ticks that
happen with given frequency. We have set the frequency to 2 
per second. The function `withDur` creates an event stream of scores 
out of event stream of values. We can set the duration of every event.
The final function `sched` applies an instrument to an event stream.
We get the signal as a result.

Let's create an instrument with a parameter. We are going
to produce a filtered pink noise:

~~~
> let bam x = mul (fades 0.01 0.3) $ at (mlp (2500 * sig x) 0.1) $ pink
~~~

The parameter is responsible for the center frequency. 
The example introduces an instrument that is not parametrized
with an amplitude or frequency but still it can produce
a musical result. Let's create a sound:

~~~
> dac $ sched bam $ withDur 0.1 $ cycleE [1, 0.5, 0.5, 0.25, 1, 0.5, 0.8, 0.65] $ metro 4
~~~

The function `cycleE` substitutes a values of the event stream with 
repeating values that are taken from the given list. When something
happens it takes a next value from the list and puts it to the 
event stream when it reaches the last value in the list it starts 
from the first value and so on. With the example we create a drum pattern.

Also we can create an arpeggio:

~~~
> let instr x = return $ mul (fades 0.01 0.1) $ tri $ sig x
> let notes = fmap (* 220) [1, 5/4, 1, 3/2, 5/4, 2, 3/2, 10/4, 2, 3, 10/4, 4]
> dac $ mul 0.5 $ sched instr $ withDur 0.1 $ cycleE notes $ metro 8
~~~

Let's add a couple effects. We add a delay (`echo`) and low pass filter (`mlp`):

~~~
> dac $ mul 0.25 $ at (mlp 3500 0.1) $ echo 0.25 0.5 
    $ sched instr $ withDur 0.1 $ cycleE notes $ metro 8
~~~

We can recieve the events from the user. Let's create a button:

~~~
> let btn = button "play"
~~~

The button produces an event stream of clicks:

~~~ 
> :t btn
btn :: Source (Evt Unit)
~~~

The `Unit` is Csound value that signifies no value or empty tuple.
It has to be defined for implementation reasons. We can not just use Haskell empty tuple.

Let's trigger an instrument:

~~~
> dac $ lift1 (sched instr . withDur 0.1 . fmap (const 440)) btn
~~~

The fun part of it is that an instrument can contain signals that were 
created with event streams! Let's abstract away our arpeggios in an instrument:

~~~
> let arpInstr _ = mul (fadeOut 1) $ at (mlp 3500 0.1) $ echo 0.25 0.5 $ mul 0.25 
    $ sched instr $ withDur 0.1 $ cycleE notes $ metro 8
> dac $ lift1 (sched arpInstr . withDur 1) btn
~~~

There are functions that play an instrument until something happens 
with another event stream:

~~~
schedUntil :: (Arg a, Sigs b) => (a -> SE b) -> Evt a -> Evt c -> b
~~~

Let's create another button for stopping an instrument. 
We are going to play the `arpInstr` until we press another button.

~~~
> let stop = button "stop"
> dac $ hlift2 (schedUntil arpInstr) btn stop
~~~

We can create an event stream of keyboard presses. 
There are handy functions:

~~~
charOn, charOff :: Char -> Evt Unit
~~~

The function takes in a symbolic representation of key and produces
an event stream of clicks/ Let's rewrite previous example:

~~~
> dac $ (schedUntil arpInstr) (charOn 'a') (charOff 'a')
~~~

Try to press the key `a`. We should focus on the Csounds window.

There is a more generic function `keyIn`:

~~~
> :t keyIn
keyIn :: KeyEvt -> Evt Unit
> :i KeyEvt
data KeyEvt = Press Key | Release Key
~~~

And type `Key` contains all special keys. We can find the complete
description in the documentation.

There are functions to listen for midi event streams:

~~~
midiKeyOn, midiKeyOff :: MidiChn -> D -> SE (Evt D)
> :i MidiChn
data MidiChn = ChnAll | Chn Int | Pgm (Maybe Int) Int
~~~

We are going to study them later.

Main functions for event streams
-----------------------------------------------------

Let's study the main functions for construction of event streams.


### Monoid

Event stream is a `Monoid`. The `mempty` is an event stream
that has no events and `mappend` combines to event streams
into a single event stream that contains events from both streams.
Reminder: `mconcat` is a version of `mappend` that is defined
on lists.

We can create an intricate drum pattern: 

~~~
> let bam _ = mul (fades 0.01 0.05) $ pink
> dac $ sched bam $ withDur 0.1 $ mconcat [metro 2, metro 1.5, metro $ 3/7]
~~~

Try to exclude values from the list or include your own and see what happens.

### Functor

an event stream is a functor.
We can transform the events of an event stream with a function.
We can map over events with `fmap`:

~~~
fmap :: (a -> b) -> Evt a -> Evt b
~~~

The function `withDur` that turns values to scores is
defined with `fmap`:

~~~
withDur :: D -> Evt a -> Evt (Sco a)
withDur dur = fmap (str dt . temp)
~~~

There is another useful function `devt`. It substitutes
any value in the stream with the given value:

~~~
devt :: a -> Evt b -> Evt a
devt a = fmap (const a)
~~~

We can create pitched beats:

~~~
> let oscInstr x = return $ mul (fades 0.01 0.1) $ osc $ sig x
> dac $ sched oscInstr $ withDur 0.1 $ mconcat 
    [devt 440 $ metro 2, devt 660 $ metro 1.5, devt 220 $ metro 0.5]
~~~

### Picking values from the lists

We already familiar with th function `cycleE` it
cycles over the values in the list. Another useful
function is `oneOf` it picks a value at random from the list:

~~~
> dac $ mlp 2500 0.1 $ sched oscInstr $ withDur 0.1 $ 
    oneOf (fmap (* 220) [1, 9/8, 5/4, 3/2, 2]) $ metro 8
~~~

The type signatures:

~~~
cycleE, oneOf :: [a] -> Evt b -> Evt a
~~~

We can also set the frequencies of repetition for the values in the list:

~~~
type Rnds a = [(D, a)]

freqOf :: (Tuple a, Arg a) => Rnds a -> Evt b -> Evt a
~~~

The type `Rnds` is a list of pairs. They are values augmented with probabilities.
The sum of probabilities should be equal to 1.

The most generic function is:

~~~
listAt :: (Tuple a, Arg a) => [a] -> Evt D -> Evt a
~~~

It picks values from the list by the event stream of indices.

### Accumulation of values

We can create a simple accumulation of values. 

The simple function `iterateE` applies a function
over and over when something happens on the event stream:

~~~
iterateE :: Tuple a => a -> (a -> a) -> Evt b -> Evt a
~~~

Let's listen to the midi notes:

~~~
> dac $ sched oscInstr $ withDur 0.2 $ fmap cpsmidinn $ iterateE 30 (+1) $ metro 4
~~~

The function `cpsmidinn` trn an integer number of midi key to frequency.

The function `iterateE` doesn't take into account the value of events.
We can run counter that takes values from the event stream:

~~~
appendE :: Tuple a => a -> (a -> a -> a) -> Evt a -> Evt a
~~~

The function `appendE` takes in an initial value and a function
to apply to the current value and the value of the event. 
When event happens the function is applied and result is stored
as the state. The current value is put into the output stream.
We can create a simple synth with two buttons. 
Left button is for going down the scale and the right button
is for going up the scale:

~~~
> let btnDown = button "down"
> let btnUp   = button "up"
> dac $ hlift2 (\down up -> mlp 1500 0.1 $ saw $ cpsmidinn $ evtToSig 60 
    $ appendE 60 (+) $ mconcat [devt 1 up, devt (-1) down]) 
    btnDown btnUp  
~~~

It's interesting to note how an instrument is controlled with
an event stream. We don't trigger any instrument. We convert
the event stream to signal. The signal controls the pitch of the filtered saw.

The function `evtToSig` converts an event stream of numbers to a signal:

~~~
evtToSig :: D -> Evt D -> Sig
evtToSig initVal evt
~~~

Let's unwind this expressin. First we transform the event streams
for buttons so that each button produces 1's or -1's and we merge
two streams in the single stream:

~~~
mconcat [devt 1 up, devt (-1) down]
~~~

Then we create a running sum. So that when user presses up
the value goes up and when the user presses down we subtract the 1.

~~~
appendE 60 (+) $ previousExpression
~~~

Then we convert event stream to signal and convert numbers to pitches:

~~~
cpsmidinn $ evtToSig 0 $ previousExpression
~~~

At the last expression we apply the pitch to filtered saw and send the output to speakers:

~~~
mlp 1500 0.1 $ saw $ previousExpression
~~~

The whole expression is wrapped in the `hlift2` so that
we can read the values from UI-widgets and stack the widgets
horizontally.

There are more generic functions for accumulating state:

~~~
accumE  :: Tuple s => s -> (a -> s ->    (b, s)) -> Evt a -> Evt b
accumSE :: Tuple s => s -> (a -> s -> SE (b, s)) -> Evt a -> Evt b
~~~

They accumulate state in pure expressions and on expressions with side effects.


### Filtering event streams

We can skip some events if we don't like them.
We can do it with function:

~~~
filterE :: (a -> BoolD) -> Evt a -> Evt a
~~~

The first argument is a predicate, if it's true for
the given event it is put in the output otherwise it's left out.

We can also skip events at random:

~~~
randSkip :: D -> Evt a -> Evt a
~~~

The first argument is the probability of skip.

There are many more functions we can check them out in the docs (see module `Csound.Control.Evt`).


Signal segments
------------------------------------------------------

The signal segments lets us schedule signals with event streams.
They are defined in the module `Csound.Air.Seg`.
A signal segment can be constructed from a single signal or a tuple of signals:

~~~haskell
toSeg :: a -> Seg a
~~~

It plays the signal indefinitely. We can limit the duration of the segment
with static length measured in seconds:

~~~haskell
constLim :: D -> Seg a -> Seg a
~~~

or with an event stream:

~~~haskell
type Tick = Evt Unit

slim :: Tick -> Seg a -> Seg a
~~~

The signal is played until something happens on the given event stream.
When segment is limited we can loop over it:

~~~haskell
sloop :: Seg a -> Seg a
~~~

It plays the segment and the replays it again when it comes to an end.

If we several limited signals we can play them in sequence:

~~~haskell
sflow :: [Seg a] -> Seg a
~~~

When the first signal stops the next one comes into play and
when it stops the next one is turned on.

Also we can play segments at the same time:

~~~
spar :: [Seg a] -> Seg a
~~~

The length of the result equals to the longest length among all input segments.

We can delay the segment with an event stream or a static length:

~~~haskell
sdel     :: Tick -> Seg a -> Seg a
constDel :: D    -> Seg a -> Seg a
~~~

There is a handy shortcut for playing nothing for the given amount of time:

~~~haskell
srest     :: Num a => Tick -> Seg a
constRest :: Num a => D    -> Seg a
~~~

To listen the segment we need to convert it to signal:

~~~
runSeg :: Sigs a => Seg a -> a
~~~

That's it. With signal segments we can easily schedule the signals with 
event streams. 

Let's create a button and turn the signal on when it's pressed:

~~~haskell
> dac $ lift1 (\x -> runSeg $ sdel x $ toSeg $ osc 440) (button "start")
~~~

Let's create a second button that can turn off the signal.

~~~haskell
> dac $ hlift2 (\x y -> runSeg $ sdel x $ slim y $ toSeg $ osc 440) 
    (button "start") 
    (button "stop")
~~~

When signal stops the program exits. We can repeat the process by looping:

~~~haskell
> dac $ hlift2 (\x y -> runSeg $ sloop $ sdel x $ slim y $ toSeg $ osc 440) 
    (button "start") 
    (button "stop")
~~~

Let's play several signals one after another with `sflow`:

~~~haskell
> dac $ hlift2 (\x y -> runSeg $ sloop $ slim y 
    $ sdel x $ sloop $ sflow $ fmap (slim x . toSeg . osc) [220, 330, 440]) 
    (button "start") 
    (button "stop")
~~~


Warning: Note that signal release is not working with signal segments.

Samplers
---------------------------------

There are handy functions to trigger signals that are based on signal segments.
We can look at the module `Csound.Air.Sampler` to find them.

The functions trigger the signals with event streams, keyboard presses and midi messages.
Let's look at the functions for keyboard (the rest functions are roughly the same).

There are several patterns of (re)triggering.

* `Trig` -- triggers a note and plays it while the same key is not pressed again

    ~~~haskell
    charTrig :: Sigs a => String -> String -> a -> SE a
    charTrig ons offs asig = ...
    ~~~

    It accepts a string of keys to turn on the signal and the 
    string of keys to turn it off.

    Let's try it out:

    ~~~haskell
    > dac $ at (mlp 500 0.1) $ charTrig "q" "a" $ saw 110
    ~~~

    Try to hit `q` and `a` keys.

* `Tap` -- is usefull optimization for `Trig` it plays the note only for 
        a given static amount of time (it's good for short drum sounds)
        `Tap` has the same arguments but the turn off string is substituted
        with a note's length in seconds (it comes first):

    ~~~haskell
    charTap :: Sigs a => D -> String -> a -> SE a
    ~~~

* `Push` -- plays a signal while the key is pressed.

    ~~~haskell
    charPush :: Sigs a => Char -> a -> SE a
    ~~~

    Let's create a simple note:

    ~~~haskell
    > dac $ at (mlp 500 0.1) $ charPush 'q' $ saw 110
    ~~~

    Let's create a couple of notes:

    ~~~haskell
    > dac $ at (mlp 500 0.1) $ sum [charPush 'q' $ saw 110, charPush 'w' $ saw (110 * 9 / 8)]
    ~~~ 

    Note that only one key (de)press can be registered at the moment. 
    It's current limitation of the library. It's not so for midi events.

* `Toggle` -- uses the same key to turn the signal on/off.

    ~~~haskell
    > dac $ at (mlp 500 0.1) $ charPush 'q' $ saw 110
    ~~~ 


* `Group` -- creates a mini mono-synth. It's give a list of pairs
        of keys an signals. When key is pressed the corresponding 
        signal starts playing. When the next key is pressed 
        the previous is turned off and the current is turned on.

    ~~~haskell
    charGroup :: Sigs a => [(Char, a)] -> SE a
    ~~~

There are many more functions. You can find them in the module `Csound.Air.Sampler`.

### Turning keyboard to DJ-console

Let's create a mini mix board for a DJ.
The first thing we need is a cool dance drone:

~~~haskell
> let snd1 a b = mul 1.5 $ mlp (400 + 500 * uosc 0.25) 0.1 $ mul (sqrSeq [1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.5] b) $ saw a
~~~

Let's trigger it with keyboard!

~~~haskell
> dac $ charTrig "q" "a" (snd1 110 8)
~~~

Try to press `q` and `a`  keys to get the beat going.
Let's create another signal. It's intended to be high pitched pulses.

~~~haskell
> let snd2 a b = mul 0.75 $ mul (usqr (b / 4) * sqrSeq [1, 0.5] b) $ osc a
~~~

Let's try it out. Try to press `w`, `e`, `r` keys.

~~~haskell
> dac $ mul 0.5 $ sum [charPush 'w' $ snd2 440 4, charPush 'e' $ snd2 330 4, charPush 'r' $ snd2 660 8]
~~~

Note that only one keyboard event can be recognized. So if you press or depress
several keys only one is going to take effect. It's a limitation of 
current implementation. It's not so with midi events. Let's join the results:

~~~haskell
> let pulses = mul 0.5 $ sum [charPush 'w' $ snd2 440 4, charPush 'e' $ snd2 330 4, charPush 'r' $ snd2 660 8]
> let beat = mul 0.5 $ sum [charTrig "q" "a" (snd1 110 8), charTrig "t" "g" $ snd1 220 4]
~~~

Let's create some drum sounds:

~~~haskell
> let snd3 = osc (110 * linseg [1, 0.2, 0])
> let snd4 = mul 3 $ hp 300 10 $ osc (110 * linseg [1, 0.2, 0])
> let drums = sum [charTrig "z" "" snd3, charTrig "x" "" snd4]
~~~

Let's rave along.

~~~haskell
> dac $ sum [pulses, mul 0.5 beat, mul 1.2 drums]
~~~


----------------------------------------------------

* <= [Scores](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ScoresTutorial.md)

* => [Real-world instruments show case](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/Patches.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)
