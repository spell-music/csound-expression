
SoundFonts
===========================

With soundfonts it's very easy to turn your computer in synthesizer.
A sound font encodes the timbre of the instrument with samples. 
There are many free soundfonts with good level of quality.

## Midi

We can read soundfonts that are encoded in sf2 format. 
The function `sfMsg` can turn sound font file in midi instrument:

~~~{.haskell}
> let sf = Sf "rhodes.sf2" 0 0
> vdac $ midi $ sfMsg sf 0.5
~~~

We play the file `"rhodes.sf2"` at the bank `0` 
with program `0`. The sound font can contain many instruments. 
They are identified with the pair of integers: bank and program number.
The second argument of the function `sfMsg` is sustain value of the instrument
in seconds.

The funciton `sfMsg` reads the samples with linear interpolation.
We can improve the quality with cubic interpolation if we 
use the function `sfMsg3`.

## Non-midi

We are not constrained to midi-frequencies. We can read samples at any frequency
with function `sfCps`:

~~~{.haskell}
> let sf = Sf "rhodes.sf2" 0 0
> dac $ sfCps sf 0 0.5 440
~~~

The arguments are:

* a soundfont preset. 

* sustain. 

* the amplitude (it ranges in the iterval [0, 1])

* the frequency in Hz.

We can find a lot of soundfonts in the net. Some links to start:

* [Hammer sound](http://www.hammersound.net/)

* [Rhodes](http://learjeff.net/sf/sf.html)

* [And many more](http://www.synthfont.com/links_to_soundfonts.html)

----------------------------------------------------

* <= [Events](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/EventsTutorial.md)

* => [Scores](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SamplesTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)

