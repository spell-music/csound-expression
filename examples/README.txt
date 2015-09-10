Here you can try the csound-expression out. 

Requirements:

*   GHC - haskell compiler. This library uses GHC-specific features.

    www.haskell.org/ghc

*   cabal-install (to install haskell packages).
  
    www.haskell.org/cabal

*   Csound compiler (version 5.13 or higher). You must get it installed on your system
    since we are going to generate the csound code we need to compile it to sound somehow.
    We can find out how to install the Csound on www.csounds.com.

    To test whether csound is installed open the command line and type:

    > csound

    It should print a long message with version and available flags and libraries.

*   csound-expression and csound-sampler packages. Also the Tibetan example requires the `random` package.

    > cabal install csound-expression 
    > cabal install csound-sampler
    > cabal install random


When  everything is installed examples should work as executable programs.
So you can type in the command line:

> runhaskell AnExample.hs 

and get the sound going out of your speakers. 

Also you can load the file into ghci and play with parts of the music
in interactive mode.

------------------------------------------

Lots of examples can be found at the repository: csound-bits

https://github.com/spell-music/csound-bits

