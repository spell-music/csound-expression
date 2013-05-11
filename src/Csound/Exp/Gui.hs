module Csound.Exp.Gui where

data Gui 
    = Comp [Gui] 
    | Prim Int Label Elem

data Elem = Slider | Btn | Radio | Text

type Label = String


