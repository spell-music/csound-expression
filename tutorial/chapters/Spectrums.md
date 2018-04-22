Spectrums (Spec)
=================================================

We can extract a spectrum from the signal. It's an advanced type.
The simplest function to create a spectrum is:

~~~{.haskell}
toSpec   :: Sig -> Spec
fromSpec :: Spec -> Sig
mapSpec  :: (Spec -> Spec) -> Sig -> Sig
~~~

With `Spec` we can apply spectral transformations to signal.
we can create a vocoder effect with it for instance or scale a pitch
or crossfade between several timbres.

We can interpolate between several signals:

~~~{.haskell}
cfdSpec :: Sig -> Spec -> Spec -> Spec
cfdSpec4 :: Sig -> Sig -> Spec -> Spec -> Spec -> Spec -> Spec
cfdsSpec :: [Sig] -> [Spec] -> Spec
~~~

To scale the pitch there are handy shortcuts:

~~~{.haskell}
scaleSpec :: Sig -> Sig -> Sig
scalePitch :: Sig -> Sig -> Sig
~~~

`scaleSpec` scales the frequency of the signal in Hz ratios
but `scalePitch` does it in semitones.

If we have a spectrum we can process it with many functions from the
module [Spectral processing](http://hackage.haskell.org/package/csound-expression-opcodes-0.0.0/docs/Csound-Typed-Opcode-SpectralProcessing.html).

-----------------------------------------------

* <= [Argument modifiers](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ModArg.md)

* => [Arrays](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/Arrays.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)
