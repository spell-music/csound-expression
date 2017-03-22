Creating plugins with Cabbage
==================================================

![Cabbage logo](http://cabbageaudio.com/images/cabbage_transparent.svg)

(since version 5.1)

WARNING: Right now Cabbage is not stable on my environment (Ubuntu) so it's hard for me to test things out. It's hard to use. But when it will improve we are going to have 
the set of tools to create cabbage instruments.

The Cabbage is a cool program that let's you run Csound instruments and effects as VST-plugins.
Moreover it let's you run Csound on Android. It defines it's own way to define UI-widgets in Csound.
Cabbage is very easy to learn, the Haskell implementation faithfully represents the cabbage.
So for better understending you should get the taste of cabbage on the official [site](http://cabbageaudio.com/docs/introduction/).
Do have a look at the [docs](http://cabbageaudio.com/docs/introduction/) or watch the [video tutorials](http://cabbageaudio.com/tutorials/).
But take the light view on Csound stuff since we have the haskell way of doing this just
get acquinted with the cabage way of UI-declaration.

The native cabbage declaration is a list of widget declarations.
Each declaration takes it's own line. The first goes the name of the widget
and then on the same line we can write the properties of the widget:

~~~xtml
<Cabbage>
form size(100, 100), pluginid("plugin")
button bounds(10, 10, 80, 80), channel("button-id"), text("Click Me"), colour:0(150, 30, 0), colour:1(30, 150, 12)
</Cabbage>
~~~

With markup language enclosed in `Cabbage` tag we define the UI. And in the audio engine
code we can read the values from channels. We define the name of the channel with `channel` property.

To use the cabbage we need to import it separately. It's supposed to be imported qualified
to aviod name-clashes with csound-functions:

~~~Haskell
import Csound.Base
import qualified Csound.Cabbage as C
~~~

The Haskell EDSL for Cabbage is inspired with `blaze-html` library. 
We represent the lists of widgets and properties with monads (scary word).
It means that we can use the next line and identation  as delimiter for widgets and properties:

~~~Haskell
import Csound.Base
import qualified Csound.Cabbage as C

ui = do
	C.cabbage $ do
		C.form $ do
			C.size 100 100
			C.pluginid "plugin"
		C.button $ do
			C.bounds 10 10 80 80
			C.channel "button"
			C.text1 "Click me"
			C.colour0 (C.Rgb 150 30 0)
			C.colour1 (C.Rgb 30 150 12)	
	res <- chnCtrlGet "button"	
	return res

main = dac $ do
	btn <- ui
	return $ btn * osc 220
~~~

So in Haskell the properties are delimited by indentation. There are some differences
that are copuled with Haskell restrictions on names and type-system:

* Notice that in Haskell the function can not take variable number of arguments
	so we use `text1` for `text` with one argument and `text2` for text with to arguments.

* We can not use colon in the identifiers so `colour:0` becomes just `colour0`.

* The haskell has strict types. But in Cabbage there are two ways to represent colours.
	We can pass strings (web-hash codes) and we can pass triplets (RGB-values). 
	To emulate this behaviour in Haskell there is a special type with two cases:

	~~~Haskell
	data Col = Hash String | Rgb Int Int Int
	~~~

We use the `chnCtrlGet` to get the control signal from the button with named channel.

If you are accustomed to Cabbage way of writing properties you can use the function `sequence_`:

~~~Haskell
C.form $ sequence_ [C.size 100 100, C.pluginid "plugin"]
~~~

What makes Haskell embedding really great is that it's not a spearate block of specific markdown.
It's a code. 

And we can abstract away the common blocks of code: 

~~~Haskell
colors = do
	C.colour0 (C.Rgb 150 30 0)
	C.colour1 (C.Rgb 30 150 12)	

C.cabbage $ do
	C.form $ do
		C.size 200 100
		C.pluginid "plugin"
	C.button $ do
		C.bounds 10 10 80 80
		C.channel "button1"
		C.text1 "Hi"
		colors
	C.button $ do
		C.bounds 110 10 80 80
		C.channel "button2"
		C.text1 "Bye"
		colors		
~~~

We can write functions to avoid duplication:

~~~Haskell
colors = do
	C.colour0 (C.Rgb 150 30 0)
	C.colour1 (C.Rgb 30 150 12)	

mkButton name id (x, y) = C.button $ do
		C.bounds x y 80 80
		C.channel id
		C.text1 name
		colors

C.cabbage $ do
	C.form $ do
		C.size 200 100
		C.pluginid "plugin"
	mkButton "Hi"  "button1" (10, 10)
	mkButton "Bye" "button2" (110, 10)
~~~

We can share the variables between audio-engine and markup.
It can be useful to store the names for channels:

~~~Haskell
ui = do
	C.cabbage $ do
		C.form $ do
			C.size 200 100
			C.pluginid "plugin"
		mkButton "Hi"  btn1 (10, 10)
		mkButton "Bye" btn2 (110, 10)
	b1 <- chnCtrlGet btn1
	b2 <- chnCtrlGet btn2
	return (b1, b2)
	where
		btn1 = "button1"
		btn2 = "button2"
~~~

After the csound file is rendered we can load it to cabbage and then use it
as VST or AU plugin or load it to the Cabbage App on android. 
We can find out how to do it o the official [web-site](http://cabbageaudio.com/docs/exporting/). 


--------------------------------------------------

* <= [Csound API. Using generated code with another languages](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/CsoundAPI.md)

* => [Imperative instruments](https://github.com/anton-k/csound-expression/blob/master/tutorial/chapters/ImperativeInstruments.md)

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)
