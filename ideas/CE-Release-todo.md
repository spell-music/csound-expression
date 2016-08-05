Release todo
============================

* make granular oscillators (based on SHARC and partikkel)

* add filters with adjectives for character names

* add SE wrappers to table read and write opcodes
   add special funs for read-only tables.

   What about Tab of tabs in this case ??

* casio oscillators

----------------------------------------
-- Done

* complete Hard and Soft sync 

* add mul' for SE

* complete partikkel helper function to read from samples
    think hard on interface how to mke it easy  (what namings to choose)

    * based on samples

    * maybe other cases:  (I think it's better for the next release)


------------------------------
-- next release

* What about moving to 48 KHz? (look out for the bug in sampler (for loading samples that have different sampling rate than the global settings))

* Global signals for patches

* Podolski oscillator

* try to emulate some instruments from commercial Virtual-analog synths

------------------------------
-- next release (super hard problems)

* thinking on cabbage UI. To get Csound instruments work with DAW.

* adding parameters for patches. Synth characters. Set of sub-synth tools to create the patch (waves filters envelopes)

* global tempo

* note arpegiator

* thinking on more modular approach (creation of streams of notes instead of callback functions)
   arpeggiator as anote transformer

* thinking on delay + sampler bug

* thinking on moving from Hash nums to Andy Gil CSE or maybe there are better alternatives?

* redesign the state of the renderer