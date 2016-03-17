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

* Funcdamental -- is the frequency of the note that is stored in the table.

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
> (defPArameters 45 [1, 0.5, 0.1]) { padsynthShape = SquareShape }
~~~

Let's listen to the sound of some harmonics:

~~~haskell
> let wave cps = padsynthOsc (defPadsynthSpec 25 [1, 0.5, 0, 0.2]) cps
> dac $ at (mlp (500 + 1500 * uosc 0.25) 0.1) $ wave $ constSeq [110, 137, 165, 220] 6
~~~

We modify the center frequency of lowpass filter with LFO. The frequency is created with running sequence of four notes.

## Low level PADsynth table generator

Table generator

## PADsynth instruments



