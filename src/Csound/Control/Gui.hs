-- | GUI (Graphical User Interface) elements are handy to change 
-- the parameters of the sound in real time. It includes sliders, 
-- knobs, rollers, buttons and other widgets. 
--
-- A GUI element consists of two parts. They are view (how it looks)
-- and logic (what's going on with it). For example a slider can be
-- horizontal or vertical or green or yellow or small or big. It's the view 
-- of the slider. And a slider can produce a continuous signal within the 
-- given interval. It's a logic of the slider. 
--
-- Let's talk about the view. The view is divided on two parts:
--
-- * where element is placed or Layout. 
--
-- * all other  properties or just Properties. 
--
-- The layout is defined with very simple functions. There are vertical and horizontal grouping 
-- of the elements. We can scale the element within the group and include an empty
-- space in the group. Everything is aligned (see "Csound.Gui.Layout"). 
-- Other properties include colors, fonts (size and type), borders, specific properties 
-- of the widgets (see "Csound.Gui.Props").
--
-- Let's consider the logic. The logic consists of three parts:
--
-- * what is consumed ('Csound.Gui.Output')
--
-- * what is produced ('Csound.Gui.Input')
--
-- * what's going on inside ('Csound.Gui.Inner')
--
-- A widget can react on values, produce values or do something useful. 
-- There are special types of widgets:
--
-- * 'Csound.Gui.Source'  - they produce values only
--
-- * 'Csound.Gui.Sink'    - they consume values only
--
-- * 'Csound.Gui.Display' - something is going on inside them (for example, it can show a "hello world" message)
-- 
--
-- Widgets can be simple and compound. Simple widgets are primitive elements
-- (sliders, knobs, rollers, buttons). We have a special constructors that 
-- produce simple widgets (see "Csound.Gui.Widget"). Compound widgets glue together
-- several widgets. That is the view contains several elements and all of them 
-- involved in the logic of the widget.
--
--
module Csound.Control.Gui (
    -- * Gui
    Gui, 
    Widget, Input, Output, Inner,
    Sink, Source, Display, SinkSource,
    widget, sink, source, display, sinkSource,
    mapSource, mapGuiSource, 
    mhor, mver, msca,

    -- * Panels
    panel, win, panels, panelBy,
    keyPanel, keyWin, keyPanels, keyPanelBy,

    -- * Re-exports
    module Csound.Control.Gui.Layout,
    module Csound.Control.Gui.Props,
    module Csound.Control.Gui.Widget
) where

import Csound.Typed

import Csound.Typed.Gui

import Csound.Control.Gui.Layout
import Csound.Control.Gui.Props
import Csound.Control.Gui.Widget

-- | Creates a window with the given name, size and content
--
-- > win name (width, height) gui
win :: String -> (Int, Int) -> Gui -> SE ()
win name (x, y) = panelBy name (Just $ Rect 0 0 x y)

keyWin :: String -> (Int, Int) -> Gui -> SE ()
keyWin name (x, y) = keyPanelBy name (Just $ Rect 0 0 x y)

