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

*   temporal-csound package. It brings together temporal-music-notation and csound-expression packages.
    It's used to make the process of score-writing more convenient.

    > cabal install temporal-csound


When  everything is installed examples should work as executable programs.
So you can type in the command line:

> runhaskell AnExample.hs 

and get the sound or csound-file. 

