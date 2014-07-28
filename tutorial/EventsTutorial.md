Events
================================

We can trigger instruments with midi. But what
if we want to trigger the instrument with some repetetive
pattern of notes. Or what if we want the instrument
play some notes at random from the list of given notes.

All these tasks can be solved with event streams.
The event stream is something that can trigger the 
given procedure when new event happens:

~~~
type Bam a = a -> SE ()
data Evt a = Evt { runEvt :: Bam a -> SE () }
~~~

The type `Bam` is a procedure. It takes a value of some type and 
then bams (or booms) with it. So if we have the event stream
we can execute some callback on it. Let's look at the simple event
streams:

~~~
metroE :: Sig -> Evt Unit
~~~

The `Unit` is a csound-value for an empty tuple `()`. 
We have to introduce the special value for implementation reasons.
The function `metroE`  produces the event stream of units
that happen all the time with given frequency. If we want the 
event to happen twice a second we can type in:

~~~
> metroE 2 
~~~


An event stream is a `Functor`. We can map it with the function `fmap`.
Also there are usefull functions:

~~~
filterE :: (a -> BoolD) -> Evt a -> Evt a
appendE :: Tuple a => a -> (a -> a -> a) -> Evt a -> Evt a
~~~

We can filter the events and we can accumulate some value uppon events.

Also the event stream is a monoid. An empty stream is a unit and
the merge of two event streams is `mappend`. So with `mappen` we
can merge two streams together like this:

~~~
> metroE 0.5 <> metroE 10
~~~


Triggering the instruments with events
-------------------------------------------------------------

If we have an event stream we can trigger an instrument with it.
There are functions:

~~~
trig  :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, D, a) -> b
sched :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, a) -> b
~~~
The function `trig` invokes an instrument `a -> SE b` when 
the event happens. The note is a triple `(D, D, a)`. It's
`(delayTime, durationTime, instrumentArgument)`. The function 
`sched` is just like `trig` but delay time is set to zero
for all events. So that we need only a pair in place of the triple.

It's usefull to know one another function:

~~~
withDur :: D -> Evt a -> Evt (D, a)
withDur dt = fmap (dt,)
~~~

It populates the events with constant time duration.
It can be useful with function `sched`.

Let's create a steady pulse of 440's:

~~~
> let instr _ = return $ fades 0.1 0.1 * osc 440
> dac $ mul 0.5 $ sched instr $ withDur 0.5 $ metroE 1
~~~

We can make it more often:

~~~
> let instr _ = return $ fades 0.01 0.01 * osc 440
> dac $ sched instr $ withDur 0.1 $ metroE 5
~~~

Functions for event stream
--------------------------------------------------------

Right now our instrument can play only single note,
but let's make it a bit more interesting:

~~~
> let instr x = return $ fades 0.01 0.01 * osc (sig x)
~~~

We can play a  note a t a given pitch.

There are plenty of event transformers that can make 
our pulse of sines more interesting. Let's explore
some of them.

The `cycleE` can repeat the values in the list.
Let's play an A major chord all the time:

~~~
> dac $ sched instr $ withDur 0.3 $ cycleE (fmap (440 * ) [1, 5/4, 3/2, 2]) $ metroE 2
~~~

With `oneOf` we can play values from the list at random:

~~~
> dac $ sched instr $ withDur 0.3 $ oneOf (fmap (440 * ) [1, 5/4, 3/2, 2]) $ metroE 2
~~~

With `freqOf` we can specify the frequencies of the individual events.
The total sum of the frequencies should be equal to 1. All frequencies
should be positive.

~~~
type Rnds a = [(D, a)]

freqOf :: (Tuple a, Arg a) => Rnds a -> Evt b -> Evt a
~~~

The `devt` is a simple function that can help to construct
the event streams of constant values
 
~~~ 
devt :: D -> Evt a -> Evt D
devt d = fmap (const d)
~~~

The function `every` is usefull for drum-patterns.

~~~
every :: (Tuple a, Arg a) => Int -> [Int] -> Evt a -> Evt a
every tickToSkip repeatTicks
~~~

The first argument specifies the number of ticks to skip.
The second argument specifies a pettern of repetitions.
It's a list of integers. The integer is a number of ticks
in the period. When period starts it triggers the single event
then it skips the number of events minus one. Then the next period starts.

Let's create a simple metronome. We've got a one note for main bit
and three same notes for other bits:

~~~
> let t = metroE 3
> let e1 = devt 440 $ every 0 [4] t
> let e2 = devt 220 $ every 1 [1,1,1] t
> let es = e1 <> e2
> dac $ sched instr $ withDur 0.1 $ es
~~~

Let's create a metronome for a more complicated bit.
It's an indian bit called Deepchandi. There are 14 tick's
in the bit. They are arranged in sequence 3-4-3-4. 
We are going to use the 440 Hz for main bit. The 330 Hz for
the start of every period. The 220 for secondary beats.

~~~
> let t = metroE 3
> let e1 = devt 440 $ every 0 [14] t
> let e2 = devt 330 $ every 3 [4,3,4] t
> let e3 = devt 220 $ every 1 [1,2,1,1,1] t
> let es = e1 <> e2 <> e3
> dac $ sched instr $ withDur 0.1 $ es
~~~

For the e3 we don't have to specify all 14 beats. Since
the period contains two periods which are the same.

It's an easy way of combining loops or creating music
that repeats in strange patterns.


There are many more event stream functions you can 
check them out in the 
[docs](http://hackage.haskell.org/package/csound-expression-3.3.2/docs/Csound-Control-Evt.html).


Plural events
----------------------------------------

Every event can contain a list of events. There are
plural versions of the instrument-triggering functions:

~~~
trigs    :: (Arg a, Sigs b) => (a -> SE b) -> Evt [(D, D, a)] -> b
scheds   :: (Arg a, Sigs b) => (a -> SE b) -> Evt [(D, a)] -> b
withDurs :: D -> Evt [a] -> Evt [(D, a)]
~~~

That's how we can play the sequence of major chords:

~~~
> let ch x = take 8 $ zipWith (\dt a -> (double dt, 0.2, a))  [0, 0.2 ..]  $ fmap (x * ) (cycle [1, 5/4, 3/2, 5/4])
> let es = oneOf [220::D, 5 * 220 / 4, 330] $ metroE (1/2.5)
> dac $ trigs instr $ fmap ch $ es
~~~
This sound is very primitive. But it's here just for illustration.
We can plug in the cool sample in place of the pure sine.


GUIs
-------------------------------------------

Lot's of GUI's a producing event streams. They are buttons, toggles, counters, radio buttons.
You can check them out in the chapter on 'User interaction'.
