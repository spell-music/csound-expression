Padsynth algorithm
-----------------------------------

Padsynth is an interesting technique to make you timbre alive.
It's created by Paul Nasca in his famous synthesizer ZynAddSubFX.
It was ported to Csound by Michael Gogins. It requires at least Csound 6.05.

The main idea lies in the notion that all cool sounds are inharmonic.
They can be harmonic but there are tiny fluctuations and digressions 
from the ideal shape. The human ear catches those tiny fluctuations and 
this what can make a difference between dull digital sound and warm analog sound.

The main idea is to add continuous sidebands to each harmonic, then we
can apply invere fourier transform to the spectrum and get inharmonic wave.
Then we can write the audio wave to table and play it back with an oscillator. 
It's going to be periodic. But the period of repetition is quite large (about 5-10 seconds).

So in place of single harmonics we get Gaussian curves with peaks at the given harmonics:

![Image](http://www.paulnasca.com/_/rsrc/1347380628020/algorithms-created-by-me/padsynth_steps.png.1347380627793.png?height=261&width=400)

The algorithm is described by the author [here](http://www.paulnasca.com/algorithms-created-by-me).

Also you can read the Csound [docs](http://csound.github.io/docs/manual/GENpadsynth.html).

The Csound provides only GEN routine to create the PADsynth long ftables.
But the algorithm is so much useful that I've decided to supply many more
functions to make it easy to create beautiful PADsynth-instruments. 
There are predefined patches that use the algorithm.

Let's start with simplest functions and then we can dive deeper.

## PADsynth standard audio waves

There are versions of standard audio waves: pure sine, triangle, square, sawtooth
that are enriched by padsynth algorithm. They have the same prefix `bw`:

~~~haskell
type PadsynthBandwidth = Double

bwOsc, bwTri, bwSqr, bwSaw :: PadsynthBandwidth -> Sig -> SE Sig
~~~

The good values for padsynth bandwidth are 0 to 120. When it increases it creates chorus like effect.
You can hear the difference between pure sine and sine with bandwidth:

~~~
> dac $ osc 220
> dac $ bwOsc 15 220
> dac $ bwOsc 45 220
> dac $ bwOsc 85 220
~~~

Let's listen to the saw filtered with moog-like low pass filter:

~~~haskell
dac $ at (mlp (2500 * linseg [0, 3, 1, 4, 0]) 0.2) $ bwSaw 45 220
~~~

Let's modulate the filter with LFO:

~~~haskell
dac $ at (mlp (1500 * utri (2 + 2 * usaw 1)) 0.2) $ bwSaw 65 70
~~~

Let's try the same algorithm but with different bandwidth:

~~~haskell
dac $ at (mlp (1500 * utri (2 + 2 * usaw 1)) 0.2) $ bwSqr 2 70
~~~

There is an oscillator with given list harmonics:

~~~haskell
bwOscBy :: PadsynthBandwidth -> [Double] -> Sig -> Sig
~~~

### Stereo waves

The padsynth algorithm can become more alive and natural when we use
separate oscillators for each channel. There are "stereo"-versions for
most of padsynth-related functions. So there are stereo oscillators:

~~~haskell
bwOsc2, bwTri2, bwSqr2, bwSaw2 :: PadsynthBandwidth -> Sig -> SE Sig

bwOscby2 :: PadsynthBandwidth -> [Double] -> Sig -> SE Sig2
~~~

The signals in each channel have different phase. The phase is random for each note.

## PADsynth oscillators

There is a generic PADsynth-oscillator:

~~~haskell
padsynthOsc :: PadsynthSpec -> Sig -> SE Sig
~~~

It takes in padsynth initialization parameters and produces an oscillator.
Let's look at those parameters:

~~~haskell
-- | Padsynth parameters.
--
-- see for details: <http://csound.github.io/docs/manual/GENpadsynth.html>
data PadsynthSpec = PadsynthSpec 
    { padsynthFundamental     :: Double
    , padsynthBandwidth       :: Double    
    , padsynthPartialScale    :: Double
    , padsynthHarmonicStretch :: Double
    , padsynthShape           :: PadsynthShape
    , padsynthShapeParameter  :: Double
    , padsynthHarmonics       :: [Double]
    } deriving (Show, Eq)

data PadsynthShape = GaussShape | SquareShape | ExpShape
~~~

Wow! Lots of parameters. 

* Fundamental -- is the frequency of the note that is stored in the table.

* Bandwidth -- is the bandwidth of harmonic. How wide we should spread the harmonics.

* PartialScale -- Is the ratio with which we increase the bandwidth for each subsequent harmonic. 
    There is a notion that for the sound to sound natural the bandwidth should become bigger 
    when we go from lower harmonics to higher. This parameter declares 

* HarmonicStretch -- ratio of stretch of the overtones

* Shape -- shape of the single harmonic (gaussian, square or exponential)

* ShapeParameter -- shape parameter of the curve.

* Harmonics -- list of relative amplitudes of the partials

There seems to be too many parameters to set! But there is a handy function to set
reasonable defaults:

~~~haskell
defPadsynthSpec :: Double -> [Double] -> PadsynthSpec
~~~

It requires only bandwidth and harmonics. Also you can modify some parameters like this:

~~~haskell
> (defPArameters 45 [1, 0.5, 0.1]) { padsynthPartialScale  = 2.3  }
~~~

Let's listen to the sound of some harmonics:

~~~haskell
> let wave cps = padsynthOsc (defPadsynthSpec 25 [1, 0.5, 0, 0.2]) cps
> dac $ at (mlp (150 + 2500 * uosc 0.25) 0.1) $ wave $ constSeq [110, 137, 165, 220] 6
~~~

We modify the center frequency of moogladder low-pass filter with LFO. The frequency is created with running sequence of four notes.

It's useful to be able to assign different harmonic content to different
regions of frequencies. We can do it with :

~~~haskell
padsynthOscMultiCps :: [(Double, PadsynthSpec)] -> D -> SE Sig
padsynthOscMultiCps specs frequency = ...
~~~

The list of pairs contains thresholds for frequencies and padsynth specifications. 
The given padsynth specification is going to be applied to all notes
with frequencies that are below the given threshold and above of the threshold of
the previous element in the list.

There is a function that can apply different padsynth specs according to the value of the amplitude.

~~~haskell
padsynthOscMultiVol :: [(Double, PadsynthSpec)] -> (D, Sig) -> SE Sig
padsynthOscMultiVol specs (amplitude, frequency) = ...
~~~

There are stereo versions of the padsynth oscillators:

~~~haskell
padsynthOsc2 :: PadsynthSpec -> Sig -> SE Sig2

padsynthOscMultiCps2 :: [(Double, PadsynthSpec)] -> D -> SE Sig2

padsynthOscMultiVol2 :: [(Double, PadsynthSpec)] -> (D, Sig) -> SE Sig2
~~~

## Low level PADsynth table generator

If the default oscillators are not good for you and you want to implement
your own you may beed to create the padsynth ftable first.
It's not that hard to do if we understand the `PadsynthSpec` data type (see prev section).

We can create a table with a following function:

~~~haskell
padsynth :: PadsynthSpec -> Tab
~~~

## PADsynth instruments

The package `csound-catalog` contains many predefined instruments that are based 
on padsynth algorithm. They take in a spectrum of Sharc instrument 
and create a padsynth instrument with it:

~~~haskell
psOrganSharc :: SharcInstr -> Patch2
psPianoSharc :: SharcInstr -> Patch2
psPadSharc :: SharcInstr -> Patch2
psSoftPadSharc :: SharcInstr -> Patch2
~~~

There are about 30 predefined sharc instruments. The sharc instrument contains
spectrum of some orchestral instrument. You can find the full list of sharc instruments in
the module Csound.Patch (Section Sharc instruments > Concrete instruments)

Let's listen to some of them (recall that  we need to import the `Csound.Patch` module to use the predefined patches):

~~~haskell
> :m +Csound.Patch
> vdac $ atMidi $ psSoftPadSharc shAltoFlute
> vdac $ atMidi $ psOrganSharc shCello
> vdac $ atMidi $ psPiano shTrumpetC
~~~

The timbre of an instrument can be altered by changing the bandwidth of padsynth.
There are special versions of aforementioned functions that allows to alter
specific parameters (The function name stays the same but it's followed by `'`). 

~~~haskell
data PadSharcSpec = PadSharcSpec {
        padSharcBandwidth :: Double,
        padSharcSize      :: Int
    }

psPadSharc' :: PadSharcSpec -> SharcInstr -> Patch2
~~~

The type `PadSharcSpec` is defined in the module `Csound.Catalog.Wave` (see SHARC section). 
It contains two parameters:

* Bandwidth -- bandwidth for padsynth ftables

* Size -- number of frequency regions (1 to 40)

The size determines how many tables are going to be used. The default is 15.

There is an instance of `Default` class for `PadSharcSpec`:

~~~haskell
instance Default PadSharcSpec where
    def = PadSharcSpec 15 8
~~~

So if we want to alter only bandwidth we can do it like this:

~~~haskell
vdac $ atMidi $ psSoftPadSharc' (def { padSharcBandwidth = 56 }) shAltoFlute
~~~

There are many more functions they are related to altering reverb effect for the instruments
and the number of frequency regions. We can increase the number of regions if we use the suffix `Hifi`:

~~~haskell
vdac $ atMidi $ psLargeOrganSharcHifi shAltoFlute
~~~

### Deep pads

The padsynth algorithm is super cool for creation of pads. There are predefined functions that
create great pads. They have vedic names: 

~~~haskell
vibhu, rishi, agni, prakriti, rajas, avatara, bhumi :: PadsynthBandwidth -> Patch2
~~~

The only argument is the bandwidth for underlying tables.

You can try them out:

~~~haskell
> dac $ atMidi $ vibhu 35
> dac $ atMidi $ vibhu 0.6
~~~

You can switch `dac` to `vdac` if you don't have the real hardware midi device
attached to your computer.

### Pads with crossfades

There are cool instruments that allow to morph between several timbres.
Right now they are defined only for pads. They have got suffix `Cfd` 
for morphing of two timbres and `Cfd4` for morphing four timbres:

~~~haskell
psPadSharcCfd :: Sig -> SharcInstr -> SharcInstr -> Patch2
psPadSharcCfd cfdLevel instr1 instr2 = ...

psPadSharcCfd4 :: Sig -> Sig -> SharcInstr -> SharcInstr -> SharcInstr -> SharcInstr -> Patch2
psPadSharcCfd4 cfdLevelX cfdLevelY instr1 instr2 instr3 instr4 = ...
~~~

The `cfdLevel` lies in the interval `(0, 1)`. The `0` produces only first instrument and
the `1` produces only second instrument. So we have the mixture of two timbres.
Also we can create the mixture of four signals. But in this case we have two levels:
`cfdLevelX` and `cfdLevelY`. We can imagine that timbres lie at the corners of the rectangle.
The levels define the coordinates of the point that lies inside the rectangle. 
The output timbre is produced with bilinear interpolation of timbres that lie at the corners of the rectangle.
The values for levels lie at the interval `(0, 1)`. The `0` means left corner (or bottom) and `1`
stands for right corner (or top corner).

Let's create a simple crossfade pad:

~~~haskell
vdac $ atMidi $ psPadSharcCfd (uosc 0.25) shAltoFlute shCello
~~~

reminder: the `uosc` produces unipolar sine wave with given frequency.

There are many more functions. They have different prefixes:

~~~haskell
psSoftPadSharcCfd, psDeepPadSharcCfd, psDeepSoftPadSharcCfd, ...
~~~

See the full list at the module `Csound.Patch`.

Also there are deep pads with corssfades:

~~~haskell
vedicPadCfd :: Sig -> SharcInstr -> SharcInstr -> PadsynthBandwidth -> Patch2
vedicPadCfd cfdLevel instr1 instr2 bandwidth = ...

vedicPadCfd4 :: Sig -> Sig -> SharcInstr -> SharcInstr -> SharcInstr -> SharcInstr -> PadsynthBandwidth -> Patch2
vedicPadCfd4 cfdLevelX cfdLevelY instr1 instr2 instr3 instr4 bandwidth = ...
~~~

They are particularly useful to test timbres with different values for bandwidth (it's the last input argument). 
Good values lie at the interval `(0.01, 130)`.

--------------------------------------------------------

* <= [Widgets for live performances](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/LiveWidgetsTutorial.md)

* => [Granular synthesis](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/GranularSynthesisTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)
