

Let's make music with text! We can use [Csound](www.csounds.com) to describe our music. Csound has 
so [many](http://www.csounds.com/manual/html/PartOpcodesOverview.html) fantastic sound generators.
It's very efficient. But sometimes Csound is too low level. So many details: integer identifiers for instruments 
and arrays, should I use control rate or audio rate signals, lack of abstractions, no nested expressions and it has limited set of types. 
This library embeds Csound in Haskell. We can use powerful Csound's primitives and glue them
together with Haskell abstractions. You can find a shord overview of the Csound and how it
fits to the Haskell picture at the docs for the module `Csound.Base` then you can look at 
the directory examples (it's int the archive with source code).

Key principles
-------------------------

* Keep it simple and compact.

* Try to hide low level csound's wiring as much as we can (no ids for ftables, instruments, global variables).

* Don't describe the whole csound in all it's generality 
    but give the user some handy tools to play with sound.

* No distinction between audio and control rates on the type level. 
    Derive all rates from the context. If the user plugs signal to 
    an opcode that expects an audio rate signal the argument is converted to the right rate.
  
* Watch out for side-effects. There is a special type called `SE`. It functions as `IO` in Haskell.     

* Less typing, more music. Use short names for all types. Make library 
    so that all expressions can be built without type annotations. 
    Make it simple for the compiler to derive all types. Don't use complex type classes.

* Make low level opcode definitions simple. Let user define his own opcodes (if they are missing).

* Ensure that output signal is limited by amplitude. Csound can produce 
    signals with HUGE amplitudes. Little typo can damage your ears 
    and your speakers. In generated code all signals are clipped 
    by 0dbfs value. 0dbfs is set to 1. Just as in Pure Data. 
    So 1 is absolute maximum value for amplitude. 

* No dependency on Score-generation libraries. Score (or list of events) 
    is represented with type class. You can use your favorite Score-generation 
    library if you provide an instance for the CsdSco type class. Currently 
    there is support for temporal-music-notation library (see temporal-csound package).

* Remove score/instrument barrier. Let instrument play a score within a note 
    and trigger other instruments. 

* Set Csound flags with meaningful (well-typed) values. 
        Derive as much as you can from the context.

For the future
--------------------------
        
* Make composable guis. Just plug the slider in the opcode and see it on the screen. Interactive instruments should be easy to make.

* Timing of events. User can set the beat rate and align events by beat events.

* Optimization of the memory allocation (liveness analysis).


Installation guide
===========================

Requirements (for Csound and Haskell users):

*   [GHC](www.haskell.org/ghc) - haskell compiler. This library uses GHC-specific features.

*   [cabal-install](www.haskell.org/cabal) to install haskell packages.

*   [Csound](www.csounds.com) compiler (version 5.13 or higher). You must get it installed on your system.
    Since we are going to generate the csound code we need to compile it to sound somehow.
    We can find out how to install the Csound [here](www.csounds.com). 
    To test whether csound is installed open the command line and type:

~~~
> csound
~~~

It should print a long message with version and available flags and libraries.

* Good library for composition. Right now only one is supported (see temporal-csound package on hackage).
    It brings together temporal-music-notation and csound-expression packages.
    It's used to make the process of score-writing more convenient.


Acknowledgements 
=============================

I'd like to mention those who supported me a lot with their music and ideas:

* music: entertainment for the braindead, ann's'annat & alizbar, toe, iamthemorning, atoms for piece / radiohead, 
    loscil, boards of canada, Hozan Yamamoto, Tony Scott and Shinichi Yuize. 

* ideas: Conal Elliott, Oleg Kiselyov, Paul Hudak, Gabriel Gonzalez, Rich Hickey and Csound's community.

