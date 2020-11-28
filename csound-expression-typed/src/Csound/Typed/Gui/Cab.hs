module Csound.Typed.Gui.Cab(
    Cab, CabProp, Col(..), cabbage,

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

import Csound.Typed.Gui.Cabbage.Cabbage

import qualified Csound.Typed.GlobalState as G
import Csound.Typed.GlobalState(SE)

cabbage :: Cab -> SE ()
cabbage = G.geToSe . G.cabbage
