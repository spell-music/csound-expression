Release todo
============================

* update docs for Patches

* implement Iain FXs, update docs (almost done, need good EQ and compressor)

----------------------------------------
-- Done

* synth character for Patches (change filters)  maybe it's worth to add attack and decay?

* all GENs are implemented (except 15)

------------------------------
-- future releases

* arpeggiator (note that we already have tabQueue opcodes, it can simplify the implementation)

* make granular oscillators (based on SHARC and partikkel)

* add filters with adjectives for character names

* casio oscillators

* nord drums

* freeverb

* What about moving to 48 KHz? (look out for the bug in sampler (for loading samples that have different sampling rate than the global settings))

* Global signals for patches

* Podolski oscillator

* try to emulate some instruments from commercial Virtual-analog synths

------------------------------
-- next release (super hard problems)

* global tempo

* note arpegiator

* thinking on more modular approach (creation of streams of notes instead of callback functions)
   arpeggiator as anote transformer

* thinking on delay + sampler bug 

* thinking on moving from Hash nums to Andy Gil CSE or maybe there are better alternatives?

* redesign the state of the renderer