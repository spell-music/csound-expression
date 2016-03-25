Granular synthesis
====================================

Granular synthesis is good for creation of atmospheric ambient textures.
We can take a plain violin note in the sustain phase and turn it into 
wonderful soundscape.

The Csound contains a set of functions for granular synthesis.
Unfortunately they are very hard to use due to large number of arguments.
This module attempts to set most of the arguments with sensible defaults.
So that a novice could start to use it. The defaults are implemented with
the help of the class `Default`. It's a standard way to implement defaults 
in the Haskell. The class `Defaults` defines a single constant called `def`.
With `def` we can get the default value for the given type.
Several csound opcodes are reimplemented so that first argument contains
secondary parameters. The type for parameters always has the instance for the 
class `Default`. The original csound opcodes are defined in the end of the module
with prefix `csd`.

Also many granular synthesis opcodes expect the sound file as input. 
There are predefined versions of the opcodes that take in the file names 
instead of tables with sampled sound. They have suffix `Snd` for stereo and `Snd1` for mono files.

For example, that's how we can use the @granule@ opcode:

~~~haskell
> dac $ granuleSnd1 spec [1, 2, 3] grainSize "fox.wav"
~~~

No need to set all 22 parameters.

The four functions are reimplemented in this way: `sndwarp`, `syncgrain`, `partikkel`, `granule`.

The most often used arguments are:

* Scale factors for tempo and pitch: `TempoSig` or `speed` and `PitchSig`. Ranges in 0 to 1.

* Grain size is the size of produced grains in seconds. Good range is 0.005 to 0.01 or even 0.1.
    The higher the value the more it sounds like the original sound.

* Grain rate. It's the speed of grain production in Hz. If it's in audio range
	we can no longer perceive the original pitch of the file. Then the pitch is determined
 	with grain rate value.

* Grain gap. It's the gap in samples between the grains. Good values are 1 to 100.

* Grain window function. For the sound to be a grain it have to be enveloped
	with grain window (some sort of bell shaped envelope). We can use half-sine for this purpose
	(and it's so in most of the defaults) or we can use a table in the `GEN20` family. In the library
	they implemented as window tables see the table constructors with prefix `win`.

Usual order of arguments is: `GrainRate`, `GrainSize`, `TempoSig`, `PitchSig`, file `table` or `name`, 
`poniter` to the table.

Let's study some examples. We assume that there is a file `"fox.wav"` in the current directory.
 
~~~haskell
file = "fox.wav"
~~~

We assume that it contains a long note in the sustain phase. It varies but not so much.

Grainy
--------------------

The simplest granular function is `grainy`. Grainy is based on the function `partikkel`.
It's the most basic version of it. Here is the signature:

~~~haskell
grainy :: GrainRate -> GrainSize -> TempoSig -> PitchSig -> String -> Sig2
~~~

As we can see it takes the grain rate and size, scaling factors for tempo and pitch,
file name. It produces the stereo signal. It expects the stereo file as input 
(for mono files there is a function `grainy1`).

Let's see how the grain rate nd grain size affect the sound:

~~~
> dac $ grainy 200 (linseg [0.1, 5, 0.01]) 1 1 file
> dac $ grainy (linseg [200, 5, 10]) 0.1 1 1 file
~~~

In the first function we change the grain size. And in the second example 
we change the grain rate.

Sndwarp
-------------------------

we can change the tempo and pitch of the sound with `sndwarp`.
Also we can add a special grainy noise if we change the secondary parameters.
secondary parameters are defined in the structure `SndwarpSpec` (short for sndwarp specification).
If we are too lazy to care for the parameters we can supply the default value.
The `SndwarpSpec` is instance of `Default` so we can use the constant `def`.

~~~haskell
sndwarp :: SndwarpSpec -> TempoSig -> PitchSig -> Tab -> Sig
~~~

Let's create a drone sound. We can create the drone if we 
lower the pitch down an octave. Then we can read a small portion 
of the file (just half a second). We can control the read position
with special function `ptrSndwarpSnd`

~~~haskell
ptrSndwarpSnd :: SndwarpSpec -> PitchSig -> String -> Pointer -> Sig2
~~~

It takes in not only a file but also a pointer to the reading position (in seconds). 
we can create a slow motion of the playhead with function `linseg`:

~~~haskell
> dac $ ptrSndwarpSnd def 0.5 w2 (linseg [0, 10, 0.5, 10, 0.25])
~~~

Let's create a more involved example. Let's create continuous sound.
We are going to trigger long notes so that the next one starts
just several seconds before the current one is stopped.
Each note is going to play an audio file with `sndwarp`
that scales the pitch with random notes from the given scale.

Let's create an instrument:

~~~haskell
instr dt file n = do
	a <- random 1.5 (lengthSnd file - 1.5)	
	b <- random (-1) 1
	iwin <- random 0.4 1
	let a1 = a
	    a2 = a + b
	    spec = def { sndwarpWinSize = iwin, sndwarpRandw = iwin / 3 }
	return $ mul (0.5 * env) $ at (mlp (12000 * env) 0.5) 
		$ ptrSndwarpSnd spec (sig n) file (linseg [a1, dt, a2])
	where 
		env = linseg [0, 0.2 * dt, 1, 0.4 * dt, 1, 0.4 * dt, 0]
~~~

The instrument takes in a note duration, filename and the pitch scaling factor.
It creates a short interval to read from and reads the file. The grains
are scaled by pitch. 

Let's trigger the instrument:

~~~haskell
grainOcean :: D -> [D] -> String -> Sig2
grainOcean dt scale file = at largeHall2 $ mul 0.5 
	$ sched (instr dt file) $ withDur dt 
	$ oneOf scale $ metroE (recip $ sig $ dt * 0.8)
~~~

Here we create a stream of events with a period that is slightly shorter
than the total length of the note (so that there is an intersection of the notes).

~~~haskell
metroE (recip $ sig $ dt * 0.8)
~~~

Then we pick pitches at random from the given scale (`oneOf`):

~~~haskell
oneOf scale $ metroE (recip $ sig $ dt * 0.8)
~~~

Then we trigger the instrument and add a reverb:

~~~haskell
at largeHall2 $ mul 0.5 
	$ sched (instr dt file) $ withDur dt 
	$ oneOf scale $ metroE (recip $ sig $ dt * 0.8)
~~~

Let's invoke the function:

~~~
> dac $ grainOcean 16 [1, 9/8, 6/5, 3/2, 2, 0.5, 3/4, (3/2) * (5/4), 6/2] "fox.wav"
~~~

Granule
--------------------------------

With granule we can create a clouds of grains. 
We can supply a list of pitch scaling factors so
that the resulting sound plays a chord:

~~~haskell
type ConstPitchSig = D
type GrainSize = Sig

granuleSnd :: GranuleSpec -> [ConstPitchSig] -> GrainSize -> String -> Sig2
~~~

The second argument is a chord of pitches.

Let's study an example:

~~~haskell
> dac $ granuleSnd def [1, 3/2, 2, 0.5] 0.2 "fox.wav"
~~~

Syncgrain
-----------------------------------

The `syncgrain` implements synchronous granular synthesis.
The grains are created not at random but with some law.
The `syncgrain` can dramatically change the sound:

~~~haskell
syncgrainSnd :: SyncgrainSpec -> GrainSize -> TempoSig -> PitchSig -> String -> Sig2
~~~

Here is an example

~~~haskell
> dac $ smallHall2 $ syncgrainSnd def 0.01 (1.5) 0.3 "fox.wav"
~~~

Let's study the three parameters. we are going to change them with knobs:

~~~haskell
> dac $ mul 0.5 $ hlift3 (\a b c -> smallHall2 $ 
	syncgrainSnd def (0.2 * a) (-2 + 4 * b) (-2 + 4 * c) file)
	(uknob 0.7)	(uknob 0.7)	(uknob 0.7)
~~~

There are many more functions to study. Take a look at the module `Csound.Air.Granular`.


----------------------------------------------------

* <= [Padsynth algorithm](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/Padsynth.md)

* => [Arguments modulation](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ModArg.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)