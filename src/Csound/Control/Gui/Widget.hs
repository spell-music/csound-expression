-- | Primitive GUI elements.
--
-- There is a convention that constructors take only parameters that 
-- specify the logic of the widget. The view is set for GUI-elements with 
-- other functions.
module Csound.Control.Gui.Widget (
    -- * Common properties 
    ValDiap(..), ValStep, ValScaleType(..), ValSpan(..),
    linSpan, expSpan, uspan, bspan, uspanExp,
    -- * Valuators
    count, countSig, joy, 
    knob, KnobType(..), setKnobType,
    roller, 
    slider, sliderBank, SliderType(..), setSliderType,
    numeric, TextType(..), setTextType,

    -- * Other widgets
    box, BoxType(..), setBoxType,
    button, ButtonType(..), setButtonType, 
    toggle, butBank, buttonSig, toggleSig, butBankSig,
    butBank1, butBankSig1,
    value, meter
) where

import Csound.Typed.Gui

