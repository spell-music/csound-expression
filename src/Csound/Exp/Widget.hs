module Csound.Exp.Widget where

import Control.Applicative

import Csound.Exp.Gui
import Csound.Exp.Wrapper
import Csound.Exp.SE
import Csound.Exp.GE
import Csound.Exp.Ref(newGuiRef)
import Csound.Exp.Event

import Csound.Opcode(idur, linseg)

runWins :: [(String, Gui)] -> GE ()
runWins = undefined

runFl :: Gui -> GE ()
runFl = undefined

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
    n <- newGuiId 
    (gui, writer, reader, inner) <- elems
    return (GuiNode gui n, writer, reader, inner)

sink :: Widget a b -> GE (Gui, Writer a)
sink = fmap (\(gui, writer, _) -> (gui, writer)) . widget

source :: Widget a b -> GE (Gui, Reader b)
source = fmap (\(gui, _, reader) -> (gui, reader)) . widget

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
slider label = mkWidget $ do
    (reader, writer) <- newGuiRef
    return (Prim label Slider, writer, reader)

btn :: Label -> Source (Evt ()) 
btn label = mkSource $ do
    (reader, _) <- newGuiRef
    return (Prim label Btn, fmap sigToEvt reader)

text :: String -> Display 
text name = mkDisplay $ return $ Prim name Text

------------------------------------------------------------------------------------

linenWidget :: Source Sig
linenWidget = mkSource $ do
    (g1, r1) <- source $ slider "rise time"
    (g2, r2) <- source $ slider "decay time"
    let out = liftA2 fun r1 r2        
    return (Comp [g1, g2], out)
    where fun a b = linseg [0, ir a, 1, idur - ir a - ir b, 1, ir b, 0]

adder :: Display
adder = mkDisplayWith $ do
    (ga, ina)   <- source $ slider "a"
    (gb, inb)   <- source $ slider "b"
    (gres, res) <- sink   $ slider "res"
    return (Comp [ga, gb, gres], 
            res =<< liftA2 (+) ina inb)

