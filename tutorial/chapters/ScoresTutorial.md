
Scores
=============================

The type for Score comes from another library `temporal-media`.
The score is a bunch of notes for an instrument to be played.
Every note has a start time, duration (both in seconds) and 
some arguments for the instrument. The arguments can carry
information about volume and pitch or some timbral parameters. 

We can invoke an instrument with functions:

~~~
sco :: (Arg a, Sigs b) => (a -> SE b) -> Sco a -> Sco (Mix b)
mix :: Sigs a => Sco (Mix a) -> a
~~~

With `sco` we convert a score of notes to score of unmixed signals.
With `mix` we can mix the score of signals to a single signal.

We can notice the two type classes. 

* The `Arg` is for arguments.
  It contains the primitive types `D` (numbers), `Str` (strings)
  and `Tab` (tables). Also the argument can contain tuples 
  of afore mentioned primitive types.

* The `Sigs` is for tuples of signals. It can be `Sig`, `Sig2`, `Sig4` and so on.


That's how we can play a single note for one second:

~~~
> let instr x = return $ osc $ sig x
> dac $ mix $ sco instr (temp 440)
~~~

The function `temp` creates a note that starts right away 
and lasts for one second. The argument of the function becomes
the argument for the instrument to play.

Why do we need two functions? Isn't it better to convert
the score of notes to signal? The answer to this question
lies in the fact that when we have scores of signals we can 
combine them together. We can construct scores that contain
signals from different instruments:

~~~
> let oscInstr x = return $ osc $ sig x
> let sawInstr x = return $ saw $ sig x
> dac $ mix $ mel [sco oscInstr (temp 440), rest 1, sco sawInstr (temp 440)]
~~~

We have created two instruments for pure sine and saw-tooth. 
Then we create a couple of notes (`temp`), apply the instruments to
them (`sco`) and play them one after another (`mel`). We have put a one second
rest between the notes. So the mix contains a signals
from two different instruments.


Main functions
----------------------------------

The main strength of the type `Sco` is that we can build complex 
scores out of simple primitives. Let's repeat our simple notes four times (`loopBy`)
and play it four times faster (`str`):

~~~
> dac $ mix $ str 0.25 $ loopBy 4 $ mel [sco oscInstr (temp 440), rest 1, sco sawInstr (temp 440), rest 1]
~~~

Let's study the most important functions for composition 
(the complete list can be found in the docs for `temporal-media` package, on Hackage). 

### Primitive functions

Let's start with primitive functions:

~~~
temp :: a -> Sco a
rest :: D -> Sco a
~~~

The `temp` creates a single note that starts right away and lasts for one second.
The function `rest` creates a pause that lasts for the given amount of time.

### Functions for sequential and parallel composition

The next functions can group lists of scores. 
If we play notes one after another we can get a melody (`mel`).
If we play notes at the same time we can get a harmony (`har`).
So there are two functions:

~~~
mel, har :: [Sco a] -> Sco a
~~~

Let's play a major chord. First we play it in line and then we form a chord:

~~~
> let notes = fmap temp $ fmap (220 * ) [1, 5/4, 3/2, 2]
> let q = mel [mel notes, har notes]
> dac $ mix $ sco oscInstr q
~~~

We can hear the buzz in the last chord. It's caused by clipping.
All signals for `dac` should have the amplitude less or equal than 1.
We can scale the last chord by amplitude with the function `eff`:

~~~
eff :: (Sigs b, Sigs a) => (a -> SE b) -> Sco (Mix a) -> Sco (Mix b)
~~~

The `eff` applies an effect to the scores of signals.

~~~
dac $ mix $ mel [sco oscInstr (mel notes), eff (return . mul 0.2) $ sco oscInstr $ har notes]
~~~

The cool part of it is that we can treat a block of notes as a single value.
We can give it a name, process it with a function or produce it with the function.
It's impossible with plain Csound.

### Time to delay

We can delay a bunch of notes with function:

~~~
del :: D -> Sco a -> Sco a
~~~

It takes a time to delay and a score. Let's play a note after two seconds delay:

~~~
> dac $ mix $ sco oscInstr $ del 2 $ temp 440
~~~

### Speed up or slow down

We can speed  up or slow down the notes playback with function `str` (short for stretch).
It stretches the length of notes in time domain. Let's play our previous example four times faster:

~~~
> dac $ mix $ str 0.25 $ mel [sco oscInstr (mel notes), eff (return . mul 0.2) $ sco oscInstr $ har notes]
~~~

### Loops

We can repeat a score several times with function `loopBy`:

~~~
loopBy :: Int -> Sco a -> Sco a
~~~

### Functor

Needless to say that Sco is a functor. We can map the notes with `fmap`:

~~~
fmap :: (a -> b) -> Sco a -> Sco b
~~~

Twinkle twinkle little star
----------------------------------------

Let's create a simple melody and play it with a sine instrument.
We are going to play a twinkle twinkle little star song.
For this tune we have two kind of bars. The first bar
contains two notes that are played twice. In the second type
of bar one note is played twice and then another is held.
We've got two patterns:

~~~
> let p1 a b = mel $ fmap temp [a, a, b, b]
> let p2 a b = mel [mel $ fmap temp [a, a], str 2 $ temp b]
~~~

Alsow we have a third pattern. It's more higher level. 
If we study the song we can see that we always play a first 
pattern and then we play a second one. So let's create a function for it:

~~~
> let p3 a b c d = mel [p1 a b, p2 c d]
~~~

Let's add an amplitude envelope to the instrument:

~~~
let oscInstr x = return $ mul (linsegr [0, 0.03, 1, 0.2, 0] 0.1 0) $ osc $ sig x
~~~

Let's also define a synonym for rendering function:

~~~
> let run = dac . mix . sco oscInstr . fmap cpspch
~~~

The `cpspch` is csound function that converts numeric values (encoded
in Csound) to frequencies. The value `8.00` is a C1, the `8.01` is D#1
the value `8.02` is D1, the `9.00` is C2, and so on. The `8.12` is the
same as `9.00`.

Let's listen for the first phrase:

~~~
> run $ str 0.25 $ p3 8.00 8.07 8.09 8.07
~~~

And then goes the second phrase:

~~~
> run $ str 0.25 $ mel [p3 8.00 8.07 8.09 8.07, p3 8.05 8.04 8.02 8.00]
~~~
We can notice that the third and fourt phrases are the same. And in the last
two phrases we are going to repeat first two phrases. Let's give a name to phrases.
And combine them in the tune:

~~~
> let ph1 = p3 8.00 8.07 8.09 8.07
> let ph2 = p3 8.05 8.04 8.02 8.00
> let ph3 = p3 8.07 8.05 8.04 8.02

> let ph12 = mel [ph1, ph2]
> let ph33 = loopBy 2 ph3
> let ph   = mel [ph12, ph33, ph12]

> run $ str 0.25 ph
~~~

With this approach we can better see the structure of the song.

Let's add chords to the tune. The song is based on three chords: C, F, G7.
Let's create a function to play a chord:

~~~
> let ch a b c = mel [temp a, har [temp b, temp c]]
> let chC = ch 7.00 7.04 7.07
> let chF = ch 7.00 7.05 7.09
> let chG = ch 7.02 7.05 7.07
~~~

The structure of the chords is the same as the structure of the tune:

~~~
> let ch1 = mel [chC, chC, chF, chC]
> let ch2 = loopBy 2 $ mel [chG, chC]
> let ch3 = loopBy 2 $ mel [chC, chG]

> let ch12 = mel [ch1, ch2]
> let ch33 = mel [ch3, ch3]

> let ch = mel [ch12, ch33, ch12]
~~~

We can play the tune with chords. Let's play it twice:

~~~
> run $ str 0.25 $ loopBy 2 $ har [ch, ph]
~~~ 

Here it is! But what about clipping? Some signals are above the 1 in amplitude.
We can easily solve this problem by scaling thae output signal. But here 
we are going to take another approach. We are going to introduce another parameter
for the instrument. The instrument was defined for frequencies. Now it's going
to get in the amplitudes also:

~~~
> let oscInstr (amp, cps) = 
  return $ mul (sig amp * linsegr [0, 0.03, 1, 0.2, 0] 0.1 0) $ osc $ sig cps
~~~

We have to update the `run` function also:

~~~
> let run = dac . mix . sco oscInstr . fmap (\(a, b) -> (a, cpspch b))
~~~

We transform not the whole argument with `cpspch` but 
only the second value in the tuple. We have the scores of frequencies.
Let's transform them in the scores of pairs! We assume that chords are 
quieter than the melody:

~~~
> run $ str 0.25 $ loopBy 2 $ har [fmap (\x -> (0.4, x)) ch,  fmap (\x -> (0.6, x)) ph]
~~~

Main classes for composition
------------------------------------

I have simplified a bit the types for functions.
For example, If we try to query the type in the ghci:

~~~
> :t mel
mel :: Compose a => [a] -> a
~~~

Or for `del`:

~~~
> :t del
del :: Delay a => DurOf a -> a -> a
~~~

The main functions belong to the type class. They are not
defined for `Sco` lone. There is an implementation for `del`,
`mel`, `har`, `rest`, etc. But later we are going to meet some other
types which we can compose with the same functions. We are going
to compose with samples (pieces of audio) and signal segments 
(signals that are limited with event streams).

The only functions that was defined on `Sco` is `temp`:

~~~
:t temp
temp :: Num t => a -> Track t a
~~~

We can see that is defined for `Track`s but the `Sco` is 
a special case for `Track`:

~~~
type Sco a = Track D a
~~~

### Compose

Let's review the main classes. We can `Compose` things:

~~~
:i Compose
class Compose a where
  mel :: [a] -> a
  har :: [a] -> a
  (+:+) :: a -> a -> a
  (=:=) :: a -> a -> a
    -- Defined in ‘Temporal.Class’
~~~

We can see our good friends `mel` and `har` alongside
with corresponding binary equivalents `(+:+)` and `(=:=)`.

There is a function that is based on this class:

~~~
> :t loopBy
loopBy :: Compose a => Int -> a -> a
~~~

It's defined as

~~~
loopBy n a = mel $ replicate n a
~~~

### Delay

We can delay things:

~~~
> :i Delay
class Delay a where
  del :: DurOf a -> a -> a
    -- Defined in ‘Temporal.Class’
~~~

The `DurOf` is a type family. If you don't know what type family
is here is the description. Type family is a function defined
on types. It means that for any type that is instance of `DurOf`
there is a corresponding type that signifies it's duration.

The duration for `Sco` is a constant number `D`.

So the function for delaying is:

~~~
del :: Delay a => DurOf a -> a -> a
~~~

### Stretch

We can stretch things:

~~~
> :i Stretch
class Stretch a where
  str :: DurOf a -> a -> a
~~~

### Rest

We can create pauses:

~~~
> :i Rest
class Compose a => Rest a where
  rest :: DurOf a -> a
    -- Defined in ‘Temporal.Class’  
~~~

### Loops

We can create an infinite loop:

~~~
class Loop a where
  loop :: a -> a
    -- Defined in ‘Temporal.Class’
~~~

### Limit

We can limit the length:

~~~
:i Limit
class Limit a where
  lim :: DurOf a -> a -> a
~~~

This function is not defined for `Sco`.

----------------------------------------------------

* <= [User interaction](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/UserInteractionTutorial.md)

* => [Events](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/EventsTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)
