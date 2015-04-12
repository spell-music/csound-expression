Signal segments
=====================================

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

Try to press 'q' and `a`  keys to get the beat going.
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

Let's the rave along.

~~~haskell
> dac $ sum [pulses, mul 0.5 beat, mul 1.2 drums]
~~~

----------------------------------------------------

* <= [Scores](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ScoresTutorial.md)

* => [Widgets for live performances](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/LiveWidgetsTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)
