
Custom temperament
==================================

The temperament in music is the set of exact values in Hz that
we assign to notes. In modern western music tradition
the main temperament is equal temperament. 

Equal temperament divides octave on 12 equal parts (in logarithmic scale)
so that each interval with the same number of notes in between is 
should sound the same no matter from where you place the root of the interval. 
For instance a C major triad should
sound the same as F# major triad. The sound is different in pitch but not 
in quality or relationships of the notes. It gives a huge advantage for 
transposition. If you want to sing along with the song but the scale
is not good for your voice you can easily transpose the scale and it 
should sound the same.

But it brings some disadvantages too. The main strength of the equal temperament
can become it's main weakness. All major thirds are the same and all minor seconds 
are the same. In fact all same intervals produce the same sound in all scales.
It can wipe away all the colors from the music. The Bach, Chopin, Beethoven
and all composers from the Romanticism era used different temperaments. 
So when we listen Chopin on the modern piano we listen to the music that is not quite the same 
as Chopin intended it to be. 

They used temperaments that have many slightly different triads. It gives the specific
colors to the scales and it makes the scale divergence within the composition more profound. 
Change in scale is not just a trasnposition it can affect the mood of the piece.

Ethnic music enjoys the variety of temperaments. In the Indian classical music
octave is divided in 22 notes (or shruties). The musician picks up 5 to 9 notes from 
the raw material of 22 shruties and each combination can create different mood. 
For Indian music different scales have not only different sharps and flats
but the quality of the note's flatness can be different from scale to scale. 
For example there can be three different F#.

By default all midi playing utilities use equal temperament.
But we can alter this behavior. 

Patches
--------------------------

The most common way to play patches is to use the function `atMidi`. 
It plays the patch with equal temperament. If you have a real midi device
you can use the `dac` in place of `vdac`:

~~~haskell
> ghci
> :m +Csound.Base Csound.Patch
> vdac $ atMidi vibraphone1
~~~

To change the temperament we can use the function `atMidiTemp`
that accepts the temperament as the first argument:

~~~haskell
> vdac $ atMidiTemp youngTemp1 vibraphone1
~~~

We can try out an ancient Pythagorean tuning:

~~~haskell
> vdac $ atMidiTemp pythagorTemp1 vibraphone1
~~~

We have several predefined temperaments to try out:
`equalTemp`, `pythagorTemp`, `meantoneTemp`, `justTemp`, `werckmeisterTemp`,
`youngTemp`, `youngTemp1`, `youngTemp2`.

Temperament
---------------------------

Temperament is defined with the base note and the set of relationships
for the notes of the scale. The temperament (`Temp`) can be created
with function `genTemp`:

~~~
genTemp :: Double -> Double -> Double -> [Double] -> Temp
genTemp mainInterval baseHz baseMidiKey cents
~~~

Let's look at the arguments:

* `mainInterval` - The frequency range covered before repeating the grade ratios, for example 2 for one octave, 1.5 for a fifth etcetera.

* `baseHz` - The base frequency of the scale in cycles per second.

* `baseMidiKey` - The integer index of the scale to which to assign `baseHz` unmodified.

* `cents` - the list of ratios for each note of the temperament in cents.

So here is the definition for equal temperament:

~~~haskell
equalTemp  = genTemp 2 261.63 60 equalCents
equalCents = [0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200]
~~~

The list should include the first note from the next octave (scale's main interval).

There are utility functions that simplify the definition of the temperament:

~~~haskell
baseC :: [Double] -> Temp
baseC cents 
~~~

The `baseC` creates a temperament with octave interval and modern C as the base note of the temperament.
We can rewrite the previous definition as:

~~~
equalTemp = baseC equalCents
~~~

There are other useful functions

~~~haskell
stdTemp, barTemp :: [Double] -> Temp
~~~

The function `stdTemp` creates a scale so that 9nth note is modern concert A (440 Hz).
The `barTemp` creates a temperament with baroque concert A (415 Hz).
There are predefined lists of cents for several western temperaments:
`equalCents`, `pythagorCents`, `meantoneCents`, `werckmeisterCents`,
`youngCents`, `youngCents1`, `youngCents2`.
We can use them as an example to define our own temperaments.

Midi instruments
--------------------------

Let's invoke a simple virtual midi instrument:

~~~haskell
> vdac $ midi $ onMsg $ \cps -> 0.5 * fades 0.01 0.1 * tri cps
~~~

The `onMsg` function takes in a function of type `Sig -> Sig`
and converts it to midi function of the type `Msg -> SE Sig`
We can change the temperament with function `onMsg'`

~~~haskell
> vdac $ midi $ onMsg' justTemp $ \cps -> 0.4 * fades 0.01 0.1 * tri cps
~~~

The `onMsg` takes in a temperament as the first argument.
Behind the scenes the function `onMsg` invokes the function `ampCps`.
it extracts the amplitude and frequency from the midi message.
To change the temperament we can use the the function `ampCps'`.
it accepts the temperament as the first argument:

~~~haskell
ampCps' :: Temp -> Msg -> (D, D)
ampCps temp msg = (amplitude, frequency)
~~~

The `ampCps'` uses the function `cpsmidi'` to extract frequency with custom temperament:

~~~
cpsmidi' :: Temp -> Msg -> D
~~~

Patches
--------------------------

With patches we can use the functions `atMidiTemp` (for polyphonic synths) and
`atMonoTemp` (for monophonic synths). Let's lookt at a couple of examples:

~~~haskell
> vdac $ atMidiTemp youngTemp1 dreamPad
~~~

~~~haskell
> vdac $ atMonoTemp justTemp nightPadm
~~~

Sound fonts
--------------------------

Also we can use custom temperaments with sound fonts.

~~~haskell
> vdac $ sfTemp meantoneTemp (Sf "/path/to/soundfont/jRhodes3.sf2" 0 0) 0.2
~~~

Temperament as a note's parameter
-------------------------

It's worth to note that we can pass the temperament as the instrument's argument.
It can be used inside the scores or with event streams.
The `Temp` type is an instance of the typeclass `Arg`.


* <= [Sound fonts](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SoundFontsTutorial.md)

* => [Samples](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SamplesTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)
