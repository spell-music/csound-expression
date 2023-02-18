Introduction
===============================

In this tutorial we are going to build step by step
a system for live performances analag to abeltone live.
We are using Haskell programming language and the library csound-expression-typed-core.
With it we can launch samples synchronized on bpm,
control them with MIDI and arrange them in sequences 
for live performances

We will go beyond the live capabilities and 
play with randmoness and fancy audio tricks to make performance
more fun and cool.

This tutorial should be read in linear fashion. 
In each chapter we build on top of what we have built in previous ones.

## Install and setup

We are going to work with library `csound-core` and compiler `csound`.
The Csound is an audio programming language. It's super powerful and
great for our needs. It is a language and compiler.  The compiler converts
Csound code to audio file or real-time performance. The csound is available
on all platforms. Install Csound for your system. See the guide on the [csound site](https://csound.com/download.html).

The `csound-core` provides core tools to write csound programs from Haskell.
we can install it from Hackage or with stack from sources. 

After everything is installed we can open terminal and check that csound works:

```
csound --version
```

Also we can check that csound-core can be loaded to terminal and used:

```
> ghci

> import Csound.Core
> dac $ 0.5 * osc 220   -- play pure tone
Ctrl + C - to stop audio
```

## Contents

* Play a single sample

* Play lives with notes




