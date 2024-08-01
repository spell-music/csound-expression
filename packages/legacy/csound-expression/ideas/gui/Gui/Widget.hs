{- | Primitive GUI elements.

There is a convention that constructors take only parameters that
specify the logic of the widget. The view is set for GUI-elements with
other functions.
-}
module Csound.Gui.Widget (
  -- * Common properties
  ValDiap (..),
  ValStep,
  ValScaleType (..),
  ValSpan (..),

  -- * Valuators
  count,
  countSig,
  joy,
  knob,
  KnobType (..),
  setKnobType,
  roller,
  slider,
  SliderType (..),
  setSliderType,
  text,
  TextType (..),
  setTextType,

  -- * Other widgets
  box,
  BoxType (..),
  setBoxType,
  button,
  ButtonType (..),
  setButtonType,
  toggle,
  butBank,
  buttonSig,
  toggleSig,
  butBankSig,
  value,
  meter,
) where

import Csound.Exp.Gui
import Csound.Exp.Widget
