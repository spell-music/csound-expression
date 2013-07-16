module Csound.Exp.Widget where

import Csound.Exp.Gui
import Csound.Exp.Wrapper
import Csound.Exp.SE
import Csound.Exp.GE
import Csound.Exp.Ref(newGuiRef)
import Csound.Exp.Event

runWins :: [Win] -> GE ()
runWins = mapM_ runWin

runWin :: Win -> GE ()
runWin w = saveGuiRoot w

runFl :: Gui -> GE ()
runFl g = runWin (Win "" g)

type Reader a = SE a
type Writer a = a -> SE ()
type Inner    = SE ()

noWrite :: Writer ()
noWrite = return 

noRead :: Reader ()
noRead  = return ()

noInner :: Inner
noInner = return ()

newtype Widget a b = Widget { unWidget :: GE (GuiNode, Writer a, Reader b, Inner) }

type Sink   a = Widget a ()
type Source a = Widget () a
type Display  = Widget () ()

widget :: Widget a b -> GE (Gui, Writer a, Reader b)
widget a = do 
    (gui, writer, reader, inner) <- unWidget a
    appendToGui gui inner
    return (guiNodeElem gui, writer, reader)

mkWidgetWith :: GE (Gui, Writer a, Reader b, Inner) -> Widget a b
mkWidgetWith elems = Widget $ do
    n <- newGuiHandle 
    (gui, writer, reader, inner) <- elems
    return (GuiNode gui n, writer, reader, inner)

sink :: Widget a b -> GE (Gui, Writer a)
sink = fmap (\(gui, writer, _) -> (gui, writer)) . widget

source :: Widget a b -> GE (Gui, Reader b)
source = fmap (\(gui, _, reader) -> (gui, reader)) . widget

display :: Widget a b -> GE Gui
display = fmap (\(gui, _, _) -> gui) . widget

mkDisplayWith :: GE (Gui, Inner) -> Display 
mkDisplayWith = mkWidgetWith . fmap (\(gui, inner) -> (gui, noWrite, noRead, inner))
    
mkWidget :: GE (Gui, Writer a, Reader b) -> Widget a b
mkWidget = mkWidgetWith . fmap (\(a, b, c) -> (a, b, c, noInner))

mkSink :: GE (Gui, Writer a) -> Sink a
mkSink = mkWidget . fmap (\(gui, writer) -> (gui, writer, noRead))

mkSource :: GE (Gui, Reader b) -> Source b
mkSource = mkWidget . fmap (\(gui, reader) -> (gui, noWrite, reader))
    
mkDisplay :: GE Gui -> Display
mkDisplay = mkWidget . fmap (\gui -> (gui, noWrite, noRead))

-----------------------------------------------------------------------------  
-- primitive elements

slider :: Label -> Widget Sig Sig
slider label = Widget $ do
    (handle, reader, writer) <- newGuiRef
    return (GuiNode (Prim label Slider) handle, writer, reader, noInner)

btn :: Label -> Source (Evt ()) 
btn label = Widget $ do
    (handle, reader, _) <- newGuiRef
    return (GuiNode (Prim label Btn) handle, noWrite, fmap sigToEvt reader, noInner)

text :: String -> Display 
text name = mkDisplay $ return $ Prim name Text


