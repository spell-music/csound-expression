module Csound.Exp.Gui where

newtype GuiHandle = GuiHandle { unGuiHandle :: Int }

data Win = Win 
    { winTitle :: String 
    , winGui   :: Gui }

data GuiNode = GuiNode
    { guiNodeElem   :: Gui
    , guiNodeId     :: GuiHandle }

data Gui
    = Comp [Gui] 
    | Prim Label Elem

data Elem = Slider | Btn | Radio | Text

type Label = String


