
Introduction
===============================================

Csound-expression is a framework for creation of computer music.
It's a Haskell library to make Csound much more friendly.
It generates Csound files out of Haskell code.


[Csound](http://www.csounds.com/) is an audio programming language. It is really awesome. 
It features unlimited polyphony, hundreds of synth-units 
including FM, granular synth, frequency domain transforms and many more.
Midi and OSC control, compatible with JACK. With JACK it's easy to use with your DAW
of choice. It can run on mobile devices and even in the web browser. 
It has the support for GUI-widgets.

But Csound is clumsy. It's an old  C-style language. We can boost it with 
functional programming. The Csound-expression gives you eloquence of Haskell 
combined with power of Csound.

With the help of the library we can create our instruments on the fly. 
A few lines in the interpreter is enough to get the cool sound going
out of your speakers. Some of the features of the library are heavily inspired
by reactive programming. We can invoke the instruments with event streams.
Event streams can be combined in the manner of reactive programming. 
The GUI-widgets are producing the event streams as a control messages. 
Moreover with Haskell we get all standard types and functions like 
lists, maps, trees. It's a great way to organize code and data.

Let's look at how we can create computer music with Haskell. 
If you are a Csounder that stumbled upon this page and got interested then
it's better to learn some Haskell. The basic level is enough to use 
the library. I recommend the book [Learn you a Haskell for a Great Good](http://learnyouahaskell.com/) by Miran Lipovaca.
It's a great book with an elephant on the cover. It's a wonderful introduction to the
wisdom and miracles of the Haskell.

Installation guide
-----------------------------------

Let's install everything. The first thing we need is a 
[csound compiler](http://www.csounds.com/resources/downloads/).
When it's installed properly we can type in the terminal:

~~~
> csound
~~~

It will print the long message. Ubuntu/Debian users can install the Csound with `apt-get`:

~~~
> sudo apt-get install csound csound-gui
~~~

The next thing is a working Haskell environment with `ghc` and `cabal-install`
It can be installed with [Haskell Platform](http://www.haskell.org/platform/).
If it works to install the `csound-expression` we can type in the terminal:

### Installing with cabal

~~~
> cabal update
> cabal install csound-expression --lib
~~~

Let's have a break and take a cup of tea. The library contains 
a lot of modules to install. 

### Installing with Stack (recommended)

I prefer to use the library with stack. 
My usual workflow is to try ideas in the ghci and to save the expressions that sound cool in the file.
To setup ghci with the most recent library we can clone the github repo.
And create an alias in `.bashrc` file to load with dependency:

```
alias cei='stack exec ghci --stack-yaml /home/anton/dev/hs/csound/csound-expression/stack.yaml -- -XOverloadedStrings -ghci-script /home/anton/dev/hs/csound/csound-expression/scripts/default.ghci'
``` 

It launches the ghci interpreter and loads the needed default modules to the interpreter session.
See file `scripts/default.ghci` for defaults.

The first sound
---------------------------------------------------------

Let's start the `ghci` and load the main module `Csound.Base`. It exports
all modules:

~~~haskell
> ghci
Prelude> :m +Csound.Base
Prelude Csound.Base> 
~~~

We can play a sine wave with 440 Hz:

~~~haskell
> dac $ osc 440
~~~

Pressing Ctrl+C stops the sound. The expression `osc 440` makes the sine wave and
the function `dac` makes a file `tmp.csd` in the current directory invokes the `csound`
on it and sends the output to speakers.

**WARNING**: the library works best within `ghci`. The real-time sound rendering 
function `dac` spawns a child process in the background which may continue 
to execute after you stop the main process that runs the program. 
It's not so in vim but it happens in the Sublime Editor and when you 
invoke `runhaskell`. So the best is to write you program in the 
separate file and then load it in the `ghci` and invoke the function 
`main` (which runs the sound rendering with the function `dac` or
another sound rendering function).


Key principles
----------------------------------------------------

Here is an overview of the features:

* Keep it simple and compact.

* Try to hide low level Csound's wiring as much as we can (no ids for ftables, instruments, global variables).

* Don't describe the whole Csound in all it's generality 
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

* Composable GUIs. Interactive instruments should be easy to make.


Acknowledgements 
------------------------------------------------------

I'd like to mention those who supported me a lot with their music and ideas:

* **music**: [entertainment for the braindead](http://entertainmentforthebraindead.com/), 
   [three pandas and the moon](https://soundcloud.com/three-pandas-and-the-moon), 
   [odno no](http://odnono.ru/), [ann's'annat & alizbar](http://www.alizbar-harp.com/), 
   [toe](http://toe.st/), [iamthemorning](http://iamthemorning.com/), [atoms for piece / radiohead](http://www.radiohead.com/deadairspace/), 
    [loscil](http://www.loscil.ca/), [boards of canada](http://boardsofcanada.com/vinyl-reissues/), [Hozan Yamamoto, Tony Scott and Shinichi Yuize](http://en.wikipedia.org/wiki/Music_for_Zen_Meditation). 

* **ideas**: [Conal Elliott](http://conal.net/), [Oleg Kiselyov](http://okmij.org/ftp/), 
    [Paul Hudak](http://haskell.cs.yale.edu/people/paul-hudak/), [Gabriel Gonzalez](http://www.haskellforall.com/), 
    [Rich Hickey](http://thechangelog.com/rich-hickeys-greatest-hits/) and Csound's community.

* Thanks a lot to all who patiently answered my questions and provided skillful solutions, encouragement and ideas:

    Iain McCurdy, Victor Lazarini, Rory Walsh, Steven Yi, John Ffitch, Joachim Heintz, Peter Burgess, Dr. Richard Boulanger, Michael Gogins, Oeyvind Brandtsegg,
    Richard Dobson, Partev Barr Sarkissian, Dave Phillips, Guillermo Senna, Art Hunkins, 
    Ben McAllister, Michael Rhoades, Brian Merchant, Gleb Rogozinsky, Eugene Cherny, Wolf Peuker,Hlöðver Sigurðsson, Aaron Krister Johnson, Andy Fillebrown and friends)

    tell me if I forgot to mention you :)


----------------------------------------------------

* <= [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)

* => [Basic types](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/BasicTypesTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)
