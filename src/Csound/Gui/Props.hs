-- | Properties that specify the appearance of the GUI elements. 
-- The specification is inspired by CSS. All properties
-- are set in the cascade manner. For example, if you want to change the font type
-- for all elements you should set this property only for the top-most GUI element.
-- If the property is set on the lower level it wins versus property that is set on the 
-- higher level. 
module Csound.Gui.Props (
    -- * Properties
    props, forceProps,
    Prop(..), BorderType(..), Color,
    Rect(..), FontType(..), Emphasis(..), 
    Material(..), Orient(..), 

    -- * Setters
    -- | Handy short-cuts for the function @props@.
    setBorder, setLabel, setMaterial, 
    setColor1, setColor2, setColors, setTextColor,
    setFontSize, setFontType, setEmphasis, setOrient
) where

import Csound.Exp.Gui

