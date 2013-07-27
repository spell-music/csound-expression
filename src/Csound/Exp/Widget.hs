module Csound.Exp.Widget where

import Csound.Exp.Gui
import Csound.Exp.Wrapper
import Csound.Exp.SE
import Csound.Exp.GE
import Csound.Exp.Event
import Csound.Render.Channel(flSetVal, flPrintk2, changed)

runWins :: [Win] -> GE ()
runWins = mapM_ runWin

runWin :: Win -> GE ()
runWin w = saveGuiRoot w

runFl :: Gui -> GE ()
runFl g = runWin (Win "" (Rect 50 50 300 300) g)

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
    return (GuiVar $ guiNodeHandle gui, writer, reader)

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
    (var, handle) <- newGuiVar
    return (GuiNode (sliderElem [var, guiHandleToVar handle] label) handle, setVal handle, readVar var, noInner)

btn :: Label -> Source (Evt ()) 
btn label = Widget $ do
    (var, handle) <- newGuiVar
    return (GuiNode (btnElem [var, guiHandleToVar handle] label) handle, noWrite, fmap sigToEvt $ readVar var, noInner)

text :: String -> Sink Sig 
text name = Widget $ do
    (_, handle) <- newGuiVar
    return $ (GuiNode (textElem [guiHandleToVar handle] name) handle, printk2 handle, noRead, noInner)

-- writers

refHandle :: GuiHandle -> SE D
refHandle h = readVar (guiHandleToVar h)

setVal :: GuiHandle -> Sig -> SE ()
setVal handle val = flSetVal (changed [val]) val =<< refHandle handle

printk2 :: GuiHandle -> Sig -> SE ()
printk2 handle val = flPrintk2 val =<< refHandle handle

