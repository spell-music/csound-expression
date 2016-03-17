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

Pure sine, triangle, square, sawtooth.

## PADsynth oscillators

oscillators

## Low level PADsynth table generator

Table generator

## PADsynth instruments



