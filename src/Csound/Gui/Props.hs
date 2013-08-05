module Csound.Gui.Props (
    -- * Properties
    props,
    Prop(..), BorderType(..), BoxType(..), Color,
    Rect(..), Span(..), Diap(..), ScaleType(..),
    FontType(..), Emphasis(..), 
    Material(..),
    SliderType(..), TextType(..), ButtonType(..),
    Orient(..), KnobType(..),

    -- * Setters
    setBorder, setLabel, setMaterial, setBoxType,
    setColor1, setColor2, setColors, setTextColor,
    setFontSize, setFontType, setEmphasis,
    setSliderType, setTextType, setButtonType,
    setOrient, setKnobType
) where

import Csound.BoxModel(Rect(..))
import Csound.Exp.Gui

