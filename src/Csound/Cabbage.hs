-- | The Cab is a monad for Cabbage markup language. 
-- The markup description can be constructed in the same way as blaze-html markup.
--
-- We use monadic equencing for sequencing of markup elements.
--
-- An example:
--
-- > import Csound.Base
-- > import qualified Csound.Cabbage as C
-- > 
-- > ui = do
-- >     C.cabbage $ do
-- >         C.form $ do
-- >            C.size 100 100
-- >            C.pluginid "plugin"
-- >        C.button $ do
-- >            C.bounds 10 10 80 80
-- >            C.channel "button"
-- >            C.text1 "Click me"
-- >             C.colour0 (C.Rgb 150 30 0)
-- >             C.colour1 (C.Rgb 30 150 12) 
-- >     res <- chnCtrlGet "button"  
-- >     return res
-- > 
-- > main = dac $ do
-- >    btn <- ui
-- >    return $ btn * osc 220
--
-- We can read a complete tutorial on how to create Cabbage plugins at the guide:
-- <https://github.com/spell-music/csound-expression/blob/master/tutorial/chapters/CabbageTutorial.md>
module Csound.Cabbage(
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
import Csound.Typed.Gui.Cab
