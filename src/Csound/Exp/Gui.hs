module Csound.Exp.Gui where

data GuiNode = GuiNode
    { guiNodeElem   :: Gui
    , guiNodeId     :: Int }

data Gui
    = Comp [Gui] 
    | Prim Label Elem

data Elem = Slider | Btn | Radio | Text

type Label = String


