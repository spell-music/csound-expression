csound-sampler
===============================

A csound-sampler is an easy to use sampler based on csound-expression library.

We can load and play audio files. We can play them back in loops,
in reverse, at random segments with different pitch, apply 
any effects available in Csound. We can arrange them in sequences. 
We can easily create patterns of audio snippets.

How to install
---------------------------------

The library is available on Hackage. So we can install it with cabal-install:

~~~
> cabal update
> cabal install csound-sampler
~~~

Also we need to install the [Csound](http://www.csounds.com) compiler. It's software synth. It's going
to be our audio engine. When it's properly installed it should be possible to run the csound
at the command line. Open up your terminal and type in `csound`. On windows sometimes csound
complains on missing `python27.dll`. If it has happened with you download the dll from the web
and drop it in the folder `C:\Windows\system32`.

Let's review the main functions of the library.


How to load files and sounds
---------------------------------

Let me introduce you to `Sam` (Sam nods). He is the main guy in
the library. He can sing samples for you. All samples are in stereo.

We can listen to the audio file:

~~~{.haskell}
module Main where

import Csound.Base
import Csound.Sam

audio = wav "samples/song.wav"

bpm = 120 * 4

main = dac $ runSam bpm audio
~~~

Let's load the module in the ghci and invoke the main function.

~~~
ghci Main
> main
... and the Sam sings ...
~~~

Press `Ctrl+C` to stop the program. Note that it's the best way to work
with csound libs. We can create module with common functions and imports then 
we load it in the ghci and we can start messing around with samples right in the 
interpreter!

We load the lossless audio files with function `wav`. 
If our audio file is mono we should use the function `wav1`.

The function `wav` creates a sample out of file name:

~~~{.haskell}
wav :: String -> Sam
~~~

To hear the sample we should run the `Sam`. 

~~~{.haskell}
runSam :: D -> Sam -> SE (Sig, Sig)
runSam bpm sample = ...
~~~

The first argument of the `runSam` is the Beats Per Minute measure.
It's the tempo of the sample playback.
When the sample is converted to stereo signal we can hear the result
with function `dac`. It's a standard function form csound-expression library.

Playing loops
------------------------------------

Ok, we can hear a sample. How can we loop it?
There is a function

~~~{.haskell}
loop :: Sam -> Sam
~~~

To make things more easy let's create a couple of shortcuts:

~~~{.haskell}
module Main where

import Csound.Base
import Csound.Sam

run = dac . runSam (120 * 4)

song = wav "samples/song.wav"
beat = wav "samples/beat.wav"
~~~

Let's save it as Main.hs. From nowon we are going to load the 
module `Main.hs` in the interpreter. So let's loop over beat:

~~~{.haskell}
ghci Main.hs
> run $ loop beat
~~~

Let's add the voice on top of it:

~~~{.haskell}
> run $ loop beat + song
~~~

The `Sam` behaves just like a simple number. We can add samples or
take a mean of samples:

~~~{.haskell}
> run $ mean [loop beat, song]
~~~

Changing the volume of the sample
--------------------------------------------

But the beat is too loud we can not hear the voice properly.
Let's fix that:

~~~{.haskell}
> run $ mean [mul 0.5 $ loop beat, song]
~~~

The function `mul` comes with library `csound-expression`. 
It happens that the `Sam` is the instance of `SigSpace`:
We can use the the function `mapSig` to apply any signal transforms to it:

~~~{.haskell}
mapSig :: (SigSapce a) => (Sig -> Sig) -> a -> a
~~~

The `mul` is just a multiplication by a signal.

~~~{.haskell}
mul :: (SigSpace a) => Sig -> a -> a
mul k = mapSig (* k)
~~~

Playing parts of the sample
------------------------------------------------

What if we don't want to hear the whole song but only 
8 beats of it. We can use `lim`:

~~~{.haskell}
> run $ mean [mul 0.5 $ loop beat, lim 8 song]
~~~

Applying envelopes
-----------------------------------------------

We can hear only a part of the song. But now we can hear
a nasty clipping. The sound jumps at the and of the sample.

We can fix it with envelope:

~~~{.haskell}
> run $ mean [mul 0.5 $ loop beat, linEnv 1 1 $ lim 8 song]
~~~

The `linEnv` takes rise and decay times in BPM and applies 
a trapezoid envelope to the sample. 

There are many more envelopes to explore:

~~~{.haskell}
-- | Exponential trapezoid
expEnv :: D -> D -> Sam -> Sam
expEnv rise dec = ...

-- | Parabolic envelope
hatEnv :: Sam -> Sam

-- | Linear rise and decay envelopes
riseEnv, decEnv :: Sam -> Sam

-- | Exponential rise and decay envelopes
eriseEnv, edecEnv :: Sam -> Sam
~~~ 


Playing samples in reverse
--------------------------------------------

It's cool to reverse audio. The sound becomes mystic and SigurRos'y.
We can play audio files in reverse:

~~~{.haskell}
> let revSong = wavr "samples/song.wav"
run revSong
~~~

Notice the suffix `r` in the function `wavr`.


Playing one sample after another
---------------------------------------------

Let's play song in two modes. the first time forth
and then backwards:

~~~{.haskell}
> let songLoop = let env = (linEnv 1 1 . lim 8) in loop $ flow [env song, env revSong]
> let beatLoop = mul 0.5 $ loop beat
> run $ mean [beatLoop, songLoop]
~~~

Notice the function `flow`. It plays a list of samples in sequence.
If we want to put some silence between the song samples, we can use 
the function `rest`. It creates a silent sample:

~~~{.haskell}
> flow [env song, rest 4, env revSong]
~~~

Delaying the samples
---------------------------------------------

We want beat's to enter the song first and then after 4 beats delay
comes the voice:

~~~{.haskell}
> run $ mean [beatLoop, del 4 songLoop]
~~~

We can use the function `del`.

Playing samples at random
--------------------------------------------

What if we want to make our voice track more alive.
We can introduce randomness in the choice of the sample:

~~~{.haskell}
pick :: Sig -> [Sam] -> Sam
pick period sample = 
~~~

The function `pick` plays one sample from the list with the given period:

~~~{.haskell}
> pick 8 [env song, rest 8, env revSong]
~~~

That's how we can play song back and forth with random playback modes.
There is a function

~~~{.haskell}
pickBy :: Sig -> [(D, Sam)] -> Sam
~~~

The `pickBy` plays samples with given frequencies of occurrence. 
The sum of the frequencies should be equal to 1. 


Playing patterns samples
------------------------------

We can play sample in the loop. But what's about more complex patterns?
we can create them with function `pat` (short for pattern):

~~~{.haskell}
pat :: [D] -> Sam -> Sam
~~~

The first argument is the list of time length for sequence of loops.
It's the drum-like pattern:

~~~{.haskell}
> pat [3, 3, 2] beat
~~~

It means play the sample `beat` in the loop. The loop spans for 8 beats and
it contains three segments. The length of each segments is given in the list. 

The pat plays the whole sample. When samples overlap it mixes them together.
If we want to play just a parts of the sample we can use the function `rep` (short for repeat).
With it we can create complex drum patterns out of simple samples:

~~~{.haskell}
> rep [3, 3, 1, 2, 1] beat
~~~


Changing the tempo
-----------------------------

We can speed up or slow down the sample playback with

~~~{.haskell}
str, wide :: D -> Sam -> Sam

str  speedUpRate  = ...
wide slowDownRate = ...
~~~

It doesn't changes the rate of playback. It changes the BPM measure.
The looping or limiting functions will respond to the changes.

Changing the pitch and panning
------------------------------------

We can change the pitch of the sample with function:

~~~{.haskell}
atPch :: D -> Sam -> Sam
~~~

It lowers (if negative) or heightens the pitch in semitones:

~~~{.haskell}
> let songLoop = atPch 2 $ loop song
~~~

We can change the pan with the function

~~~{.haskell}
atPan :: Sig -> Sam -> Sam
~~~

The first argument is the panning level. The zero is all left and the one is all right.
We can easily create the spinning pan:

~~~{.haskell}
> let songLoop = atPan (uosc 0.1) $ loop song
~~~

Playing segments of the audio file
-----------------------------------------

What if we like just one specific spot in the audio file
and we want to loop only over it. we can read the segment
with function:

~~~{.haskell}
seg :: D -> D -> String -> Sam
seg startTime endTime fileName = sample
~~~

The times are measured in seconds. To play the segment in reverse
we can use the function `segr`. There are mono variants: `seg1` and `segr1`.


Playing random segments of the audio file
-----------------------------------------

We can create complex sound out of the simple one if we play
segments of it at random:

~~~{.haskell}
rndSeg :: D -> D -> D -> String -> Sam
rndSeg segLength startTime endTime fileName = sample
~~~

The first argument is the length of the segment. 
If we want to read segments from the entire audio file
we can use the function:

~~~{.haskell}
rndWav :: D -> String -> Sam
rndWav segLength fileName = sample
~~~

Applying effects
--------------------------------

The type `Sam` is a synonym for generic type:

~~~{.haskell}
type Sig2 = (Sig, Sig)

type Sam = Sampler Sig2
~~~

The `Sampler` is applicative and function. We can easily apply
an effect with `fmap`:

~~~{.haskell}
> dac $ fmap magicCave2 $ loop song
~~~

We applied a reverb (`magicCave2 :: Sig2 -> Sig2`). It's taken from the 
library csound-expression. 

If our effect produces side effects we can use one of the lifting functions:

~~~{.haskell}
liftSam :: Sample (SE a) -> Sample a
bindSam :: (Sig2 -> SE Sig2) -> Sam -> Sam
~~~

If we want to now the current BPM we can use functions:

~~~{.haskell}
mapBpm :: (Bpm -> Sig2 -> Sig2) -> Sam -> Sam
bindBpm :: (Bpm -> Sig2 -> SE Sig2) -> Sam -> Sam
~~~

And many more
----------------------------------

There are many other functions. We can find them all in the docs.
Happy sampling!
