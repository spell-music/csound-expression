Widgets for live performances
=====================================

Since the version 4.2.0 there are many widgets tageted
at real-time performance. They should make it easy to 
mix and process audio live. 

It's assumed that the library `csound-sampler` is installed.

Playing samples
-------------------------

We can start and stop samples with function `sim`.

~~~haskell
module Main where

import Csound.Base
import Csound.Sam

a1 = infSig1 $ osc 220
a2 = infSig1 $ osc 330

main = dac $ do
	(g, sam) <- sim 4 [("220", a1), ("330", a2)]
	panel g
	mul 0.5 $ runSam 120 sam
~~~

For simplicity we use pure sine waves but we can use samples 
with cool sounds instead. 

The first argument for `sim` (it's 4 in the example above)
is responsible for syncronization. The samples are started only
on every n'th beat. 

we can toggle between samples with the function `tog`.
The example is the same but write `tog` in place of `sim`.
With `tog` only one sample is going to be played.

The widget `live` resembles the session view of the Ableton.
the samples are arranged in matrix. We can start all samples
in the row by the single click of the mouse and we can toggle samples
within each column. Let's look at the example:

~~~haskell
module Main where

import Csound.Base
import Csound.Sam

b1 = infSig1 $ sqr 220
b2 = infSig1 $ sqr 330
b3 = infSig1 $ sqr 440

c1 = infSig1 $ tri 220
c2 = infSig1 $ tri 330
c3 = infSig1 $ tri 440

main = dac $ do
	(g, sam) <- live 4 ["triangle", "square"] 
		[ c1, b1
		, c2, b3
		, c3, b3]
	panel g
	mul 0.3 $ runSam 120 sam
~~~

the function `live` takes in the number of beats for syncronization,
the names for columns and the list of samples. The number of columns
in the matrix is defined with the length of list of the column names.


Using mixer
-------------------------

We can mix several stereo signals together with the widget mixer.

~~~haskell
mixer :: [(String, SE Sig2)] -> Source Sig2
~~~

Mixer takes in the list of pairs. The first element of the pair
is the name of the instrument and the second element is the actual signal.

Let's balance the sound of the chord:

~~~haskell
main = dac $ do
	(g, res) <- mixer $ fmap (\x -> mixMono (show x) (osc $ sig $ int x)) [220, 330, 440]
	win "mixer" (600, 300) g
	return $ mul 0.5 $ res
~~~

Note the function `win`. It constructs the window with the given name, size and content.
The function `mixMono` is usefull for mixing mono signals.

We can use mixer with functions `sim` and `tog`:

~~~haskell
a1 = infSig1 $ osc 220
a2 = infSig1 $ osc 330

run = runSam 120

main = dac $ do
	(g1, sam1) <- tog 4 [("220", a1), ("330", a2)]
	(g2, sam2) <- sim 4 [("220", a1), ("330", a2)]
	(g3, res)  <- mixer [("tog", run sam1), ("sim", run sam2)]
	win "main" (600, 400) $ ver [sca 0.6 $ hor [g1, g2], g3]
	return res
~~~

Processing signals
---------------------------------

There are many widgets to process stereo signals.
The sound processing function is a function of the type:

~~~haskell
type FxFun = Sig2 -> SE Sig2
~~~

To be truly interesting the sund processing function
should depend on parameters which control the behavior of
the effect:

~~~haskell
Sig -> Sig -> ... -> Sig -> FxFun
~~~

We can create a visual representation of this type
with `fxBox`.

~~~haskell
fxBox :: FxUI a => String -> a -> Bool -> [(String, Double)] -> Source FxFun
fxBox name fx isOn args = ...
~~~

It expects the name of the widget, the sound processing function 
the flag that turns on the widget (is it active at the start time)
and the list of arguments. The result contains the widget and fx-function.

The class `FxUI` contains the functions like:

~~~haskell
Sig2 -> SE Sig2
Sig -> Sig2 -> SE Sig2
Sig -> Sig -> Sig2 -> SE Sig2
...

Sig2 -> Sig2
Sig -> Sig2 -> Sig2
Sig -> Sig -> Sig2 -> Sig2
...
~~~

I hope that you've got the pattern. The arguments are turned into
sliders. There are many predefined widgets that implement typical
effects (reverbs, distortion, chorus, flanger etc).

~~~haskell
main = dac $ do
	(gui, fx) <- fxHor 
		[ uiFilter False 0.5 0.5 0.5
		, uiChorus False 0.5 0.5 0.5 0.5		
		, uiPhaser False 0.5 0.5 0.5 0.5 0.5		
		, uiReverb True  0.5 0.5
		, uiGain   True  0.5 
		]
	win "main" (900, 400) gui
	fx $ fromMono $ saw 110
~~~

We can group the fx-widgets with functions `fxHor`, `fxVer` and `fxSca`.
They group widgets horizontaly, verticaly and scale the widgets.
There are many more widgets to consider you can find them in the module 
`Csound.Air.Live`.

----------------------------------------------------

* <= [Signal segments](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/SignalSegmentsTutorial.md)

* => [Granular synthesis](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/GranularSynthesisTutorial.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)

