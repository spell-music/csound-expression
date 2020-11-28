{-# Language GeneralizedNewtypeDeriving  #-}
module Csound.Typed.Gui.Cabbage.Cabbage(    
    Cab, CabProp, Col(..), runCab, 
    
    -- * Widgets
    button, filebutton, infobutton, checkbox, combobox, csoundoutput, encoder, gentable, 
    hrange, vrange, form, groupbox, image, keyboard, label, hslider, vslider,
    rslider, soundfiler, signaldisplay, textbox, texteditor, xypad,

    -- * Properties
    bounds, channel, text1, text2, value, colour, colour0, colour1, backgroundcolour, textcolour, trackercolour, outlinecolour, 
    fontcolour, fontcolour0, fontcolour1, latched, identchannel, rotate, alpha, visible, caption, widgetarray, popuptext, 
    active, svgfile, populate, mode, file, shape, corners, channeltype, align, sliderincr, max, min, textbox', trackerthickness,
    linethickness, range, range2, size, pluginid, guirefresh, plant, child, show, middlec, keywidth, scrollbars, fontstyle,
    scrubberpos, zoom, displaytype, updaterate, wrap

) where

import Prelude hiding (show, min, max)

import Data.Maybe
import Control.Monad.Trans.Writer.Strict
import Control.Applicative

import Csound.Typed.Gui.Cabbage.CabbageLang	

type Cab = Cab' ()
type CabProp = CabProp' ()

-- | The Cab is a monad for Cabbage markup language. 
-- The markup description can be constructed in the same way as blaze-html markup.
newtype Cab' a = Cab' { unCab' :: Writer [Line] a }
	deriving (Functor, Applicative, Monad)

runCab :: Cab -> [Line]
runCab = snd . runWriter . unCab'

newtype CabProp' a = CabProp' { unCabProp' :: Writer [Property] a }
    deriving (Functor, Applicative, Monad)

runCabProp :: CabProp -> [Property]
runCabProp = snd . runWriter . unCabProp'

---------------------------------------
-- widgets

widget :: String -> CabProp -> Cab
widget name props = Cab' $ tell [Line name $ runCabProp props]

---------------------------------------

button, filebutton, infobutton, checkbox, combobox, csoundoutput, encoder, gentable, 
    hrange, vrange, form, groupbox, image, keyboard, label, hslider, vslider,
    rslider, soundfiler, signaldisplay, textbox, texteditor, xypad :: CabProp -> Cab

button 			= widget "button"
filebutton 		= widget "filebutton"
infobutton 		= widget "infobutton"
checkbox 		= widget "checkbox"
combobox 		= widget "combobox"
csoundoutput	= widget "csoundoutput"
encoder 		= widget "encoder"
gentable 		= widget "gentable"
hrange 			= widget "hrange"
vrange 			= widget "vrange"
form 			= widget "form"
groupbox 		= widget "groupbox"
image 			= widget "image"
keyboard 		= widget "keyboard"
label 			= widget "label"
hslider 		= widget "hslider"
vslider 		= widget "vslider"
rslider 		= widget "rslider"
soundfiler 		= widget "soundfiler"
signaldisplay	= widget "signaldisplay"
textbox 		= widget "textbox"
texteditor 		= widget "texteditor"
xypad 			= widget "xypad"
	
---------------------------------------
-- properties

mkProperty :: String -> [Arg] -> CabProp
mkProperty name args = CabProp' $ tell [Property name args]

data Col = Hash String | Rgb Int Int Int

colProp x = case x of
	Hash a -> [StringArg a]
	Rgb r g b -> fmap IntArg [r, g, b]

boolProp x = IntArg $ if x then 1 else 0	

bounds :: Int -> Int -> Int -> Int -> CabProp
bounds x y w h = mkProperty "bounds" (fmap IntArg [x, y, w, h])

channel :: String -> CabProp
channel name = mkProperty "channel" [StringArg name]

text1 :: String -> CabProp
text1 name = mkProperty "text" [StringArg name]

text2 :: String -> String -> CabProp
text2 name1 name2 = mkProperty "text" [StringArg name1, StringArg name2]

value :: Float -> CabProp
value x = mkProperty "value" [FloatArg x]

colour :: Col -> CabProp
colour col = mkProperty "colour" (colProp col)

colour0 :: Col -> CabProp
colour0 col = mkProperty "colour:0" (colProp col)

colour1 :: Col -> CabProp
colour1 col = mkProperty "colour:1" (colProp col)

backgroundcolour :: Col -> CabProp
backgroundcolour col = mkProperty "backgroundcolour" (colProp col)

textcolour :: Col -> CabProp
textcolour col = mkProperty "textcolour" (colProp col)

trackercolour :: Col -> CabProp
trackercolour col = mkProperty "trackercolour" (colProp col)

outlinecolour :: Col -> CabProp
outlinecolour col = mkProperty "outlinecolour" (colProp col)

fontcolour :: Col -> CabProp
fontcolour col = mkProperty "fontcolour" (colProp col)

fontcolour0 :: Col -> CabProp
fontcolour0 col = mkProperty "fontcolour:0" (colProp col)

fontcolour1 :: Col -> CabProp
fontcolour1 col = mkProperty "fontcolour:1" (colProp col)

latched :: Bool -> CabProp
latched b = mkProperty "latched" [boolProp b]

identchannel :: String -> CabProp
identchannel s = mkProperty "identchannel" [StringArg s]

rotate :: Float -> Float -> Float -> CabProp
rotate radians pivotx pivoty = mkProperty "rotate" $ fmap FloatArg [radians, pivotx, pivoty]

alpha :: Float -> CabProp
alpha a = mkProperty "alpha" [FloatArg a]

visible :: Bool -> CabProp
visible a = mkProperty "visible" [boolProp a]

caption :: String -> CabProp
caption a = mkProperty "caption" [StringArg a]

widgetarray :: String -> Int -> CabProp
widgetarray name n = mkProperty "widgetarray" [StringArg name, IntArg n]

popuptext :: String -> CabProp
popuptext a = mkProperty "popuptext" [StringArg a]

active :: Bool -> CabProp
active a = mkProperty "active" [boolProp a]

svgfile :: String -> String -> CabProp
svgfile ty file = mkProperty "svgfile" (fmap StringArg [ty, file])

populate :: String -> String -> CabProp
populate filetype dir = mkProperty "populate" (fmap StringArg [filetype, dir])

mode :: String -> CabProp
mode a = mkProperty "mode" [StringArg a]

file :: String -> CabProp
file a = mkProperty "file" [StringArg a]

shape :: String -> CabProp
shape a = mkProperty "shape" [StringArg a]

corners :: Float -> CabProp
corners a = mkProperty "corners" [FloatArg a]

channeltype :: String -> CabProp
channeltype a = mkProperty "channeltype" [StringArg a]

align :: String -> CabProp
align a = mkProperty "align" [StringArg a]

sliderincr :: Float -> CabProp
sliderincr a = mkProperty "sliderincr" [FloatArg a]

max :: Float -> CabProp
max a = mkProperty "max" [FloatArg a]

min :: Float -> CabProp
min a = mkProperty "min" [FloatArg a]

textbox' :: Bool -> CabProp
textbox' a = mkProperty "textbox" [boolProp a]

trackerthickness :: Float -> CabProp
trackerthickness a = mkProperty "trackerthickness" [FloatArg a]

linethickness :: Float -> CabProp
linethickness a = mkProperty "linethickness" [FloatArg a]

range :: Float -> Float -> (Float, Float) -> CabProp
range min max value = range2 min max value Nothing Nothing

range2 :: Float -> Float -> (Float, Float) -> Maybe Float -> Maybe Float -> CabProp
range2 min max value mskew mincr = mkProperty "range" $ catMaybes [Just $ FloatArg min, Just $ FloatArg max, Just $ (uncurry ColonArg) value, fmap FloatArg mskew, fmap FloatArg mincr]

size :: Int -> Int -> CabProp
size w h = mkProperty "size" (fmap IntArg [w, h])

pluginid :: String -> CabProp
pluginid a = mkProperty "pluginid" [StringArg a]

guirefresh :: Int -> CabProp
guirefresh a = mkProperty "guirefresh" [IntArg a]

plant :: String -> CabProp
plant a = mkProperty "plant" [StringArg a]

child :: Bool -> CabProp
child a = mkProperty "child" [boolProp a]

show :: Bool -> CabProp
show a = mkProperty "show" [boolProp a]

middlec :: Int -> CabProp
middlec a = mkProperty "middlec" [IntArg a]

keywidth :: Int -> CabProp
keywidth a = mkProperty "keywidth" [IntArg a]

scrollbars :: Bool -> CabProp
scrollbars a = mkProperty "scrollbars" [boolProp a]

fontstyle :: String -> CabProp
fontstyle a = mkProperty "fontstyle" [StringArg a]

scrubberpos :: Int -> CabProp
scrubberpos a = mkProperty "scrubberpos" [IntArg a]

zoom :: Float -> CabProp
zoom a = mkProperty "zoom" [FloatArg a]

displaytype :: String -> CabProp
displaytype a = mkProperty "displaytype" [StringArg a]

updaterate :: Int -> CabProp
updaterate a = mkProperty "updaterate" [IntArg a]

wrap :: Bool -> CabProp
wrap a = mkProperty "wrap" [boolProp a]
