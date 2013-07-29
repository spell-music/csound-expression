module Csound.Exp.Widget where

import Control.Applicative(liftA2)

import Csound.BoxModel(Rect(..))

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

instance Functor (Widget a) where
    fmap f (Widget x) = Widget $ fmap 
        (\(gui, writer, reader, inner) -> (gui, writer, fmap f reader, inner)) x

type Sink   a = Widget a ()
type Source a = Widget () a
type Display  = Widget () ()

widget :: Widget a b -> GE (Gui, Writer a, Reader b)
widget a = do 
    (gui, writer, reader, inner) <- unWidget a
    appendToGui gui inner
    return (fromGuiHandle $ guiNodeHandle gui, writer, reader)

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

singleOut :: Elem -> Source Sig 
singleOut el = Widget $ do
    (var, handle) <- newGuiVar
    return ( GuiNode (fromElem [var, guiHandleToVar handle] el) handle
           , noWrite
           , readVar var
           , noInner )

count :: Diap -> Step -> Maybe Step -> Double -> Source Sig
count diap step1 mStep2 v0 = singleOut $ Count diap step1 mStep2 v0

joy :: Span -> Span -> (Double, Double) -> Source (Sig, Sig)
joy sp1 sp2 v0 = Widget $ do
    (var1, handle1) <- newGuiVar
    (var2, handle2) <- newGuiVar
    let outs = [var1, var2, guiHandleToVar handle1, guiHandleToVar handle2]
    return ( GuiNode (fromElem outs (Joy sp1 sp2 v0)) handle1
           , noWrite
           , liftA2 (,) (readVar var1) (readVar var2)
           , noInner)

knob :: Span -> Double -> Source Sig
knob sp v0 = singleOut $ Knob sp v0

roller :: Span -> Step -> Double -> Source Sig
roller sp step v0 = singleOut $ Roller sp step v0

slider :: Span -> Double -> Source Sig
slider sp v0 = singleOut $ Slider sp v0

text :: Diap -> Step -> Double -> Source Sig
text diap step v0 = singleOut $ Text diap step v0

-- write slider

writeSlider :: Span -> Double -> Widget Sig Sig
writeSlider sp v0 = Widget $ do
    (var, handle) <- newGuiVar
    return ( GuiNode (fromElem [var, guiHandleToVar handle] (Slider sp v0)) handle
           , setVal handle
           , readVar var
           , noInner )    

box :: String -> Display
box label = Widget $ do
    (_, handle) <- newGuiVar
    return $ ( GuiNode (fromElem [guiHandleToVar handle] (Box label)) handle
             , noWrite
             , noRead
             , noInner )

button :: Source (Evt ())
button = fmap sigToEvt buttonSig

buttonSig :: Source Sig
buttonSig = singleOut Button

butBank :: Int -> Int -> Source (Evt D)
butBank xn yn = fmap snaps $ butBankSig xn yn

butBankSig :: Int -> Int -> Source Sig 
butBankSig xn yn = singleOut $ ButBank xn yn

value :: Double -> Sink Sig 
value v = Widget $ do
    (_, handle) <- newGuiVar
    return $ ( GuiNode (fromElem [guiHandleToVar handle] (Value v)) handle
             , printk2 handle
             , noRead
             , noInner )


-- writers

refHandle :: GuiHandle -> SE D
refHandle h = readVar (guiHandleToVar h)

setVal :: GuiHandle -> Sig -> SE ()
setVal handle val = flSetVal (changed [val]) val =<< refHandle handle

printk2 :: GuiHandle -> Sig -> SE ()
printk2 handle val = flPrintk2 val =<< refHandle handle

