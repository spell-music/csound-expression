Rendering Csound files
==========================================

We know how to play the sound live. We can use the function `dac`
for it. Also we know how to use virtual midi-device. We can use `vdac` for it.
But there are many more ways to render the Csound file. Let's study them.
The functions that we are going to look at live in the 
module [Csound.IO](http://hackage.haskell.org/package/csound-expression-3.3.2/docs/Csound-IO.html).

Producing the Csound code
---------------------------------------------------

The csound-expression library at its core is a Csound file
generator. The most basic thing it can do is to make
as String that contains the Csound code.

~~~haskell
renderCsd :: RenderCsd a => a -> IO String
~~~

It takes something renderable and produces a `String`.
We can write the String to the file with function:

~~~haskell
writeCsd :: RenderCsd a => String -> a -> IO ()
writeCsd fileName csd = ...
~~~

These functions are useful if we want to use the Csound code
without Haskell. For instance we can take it on some computer 
that doesn't have the Haskell installed on it and run it with Csound. 
It can be used on mobile devices inside of the other programs
with Csound API. We can send it to our friend by mail. So that
he can render it at home and hear the music.

Saving the output to sound-file
------------------------------------------------------

We can write the output to wav-file or aiff-file with function:

~~~haskell
writeSnd :: RenderCsd a => String -> a -> IO ()
writeSnd fileName csd = ...
~~~

Let's write a 10 seconds of concert A (440 Hz). We can use it
for tuning:

~~~haskell
> writeSnd "A.wav" $ setDur 10 $ osc 440
~~~

The audio is going to be rendered off-line. The good thing
about off-line rendering is that it can happen much faster.
It depends on the complexity of the sound units. It's not limited
with real-time constraints. So we can render a file with 30 minutes
very quickly.

Playing live
------------------------------------------------------

We have already seen these functions. You can guess them:

~~~haskell
dac  :: RenderCsd a => a -> IO ()
vdac :: RenderCsd a => a -> IO ()
~~~

The `dac` is for sending the sound to sound card and
the `vdac` is for playing with virtual midi device.  
If you have the real midi-controller you can use it
with `dac` function. Just use the plain `midi`-function
and everything should work out of the box.

Playing the sound with player 
--------------------------------------------------------

We can render the file to sound file and play it with sound player.
Right now only Linux players are supported:

~~~haskell
mplayer, totem :: RenderCsd a => a -> IO ()
~~~

Render-able types
----------------------------------------------------------

It's time to take a closer look at the arguments of the functions.
What does type class `RenderCsd` mean? 

We have seen how we can play a mono and stereo signals with it.
But can we do anything else? Yes, we can.

We can render the signals or tuples of signals.

~~~haskell
Sig, (Sig, Sig), (Sig, Sig, Sig, Sig)
~~~

They can be wrapped in the type `SE` (they can contain side effects)

~~~haskell
SE Sig, SE (Sig, Sig), SE (Sig, Sig, Sig, Sig)
~~~

We can listen on the sound card ports for input signals.
Yes, we can use the Csound as a sound-effect. Then we render
a function:

~~~haskell
(Sigs a, Sigs b) => RenderCsd (a -> b)
(Sigs a, Sigs b) => RenderCsd (a -> SE b)
~~~

We can render a procedure:

~~~haskell
SE ()
~~~

In this case we are using Csound to do something useful
but without making any noise about it. Maybe we are going
to manipulate some sound-files or receive Midi-messages 
and silently print them on the screen.

Options
----------------------------------------------------

We doesn't care much about sound rates for the output or 
what sound card to use or what size does internal sound buffers have.
But if we do? 

Can we alter the sample rate? The default is 44100. It's good enough
for real-time performance. If we want to produce the high quality audio
we need to alter the defaults. That's where the `Options` are handy.

If we look at the module [Csound.IO](http://hackage.haskell.org/package/csound-expression-3.3.2/docs/Csound-IO.html)
we shortly notice that there are duplicate functions that ends with `By`

~~~haskell
dacBy 		:: RenderCsd a => Options -> a -> IO ()
writeCsdBy 	:: RenderCsd a => Options -> String -> a -> IO ()
writeSndBy 	:: RenderCsd a => Options -> String -> a -> IO ()
...
~~~

They take in one more argument. It's `Options` 
([Csound.Options](http://hackage.haskell.org/package/csound-expression-3.3.2/docs/Csound-Options.html)). 
With `Options` we can do a lot of fine tuning. 
we can alter audio sample rate, alter the default size for
functional tables, assign settings for JACK-instruments and so on.

That's how we can alter the sound-rates:

~~~haskell
> let opt = setRates 96000 64
> writeSndBy opt result
~~~

The sound rates contain two integers. The former is the result audio rate
and the latter is for the length of the single audio array. The latter is called
blackSize (it's ksmps in Csound). It affects the rate of control signals. 
We produce the audio signals in frames of the `blockSize` length.
When we are done with one frame we can listen for the control signals
and then apply them in the production of the next frame.

The cool thing about `Options` is that it's a `Monoid`.
We can use the default `Options` and alter only the things
we need to alter without the need to redefine the other things.
Let's see how we can combine different settings:

~~~haskell
> let opt = setRates 96000 64 <> def { tabFi = coarseFi 15 }
> writeSndBy opt result
~~~

We combine the two options with Monoid's `mappend` function. 
The first option is for rate and the second set's higher degree
of fidelity for functional tables. It affects the default table size.
By default it's 13th degree of 2. But we have set it to 15.

----------------------------------------------------

* <= [Basic types](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/BasicTypesTutorial.md)

* => [Basics of sound synthesis](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SynthTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)