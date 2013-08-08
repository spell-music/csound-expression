module Csound.Exp.Widget(
    -- * Panels
    panel, tabs, panels, panelBy, tabsBy,

    -- * Types
    Input, Output, Inner,
    noInput, noOutput, noInner,
    Widget, widget, Source, source, Sink, sink, Display, display,

    -- * Widgets
    count, countSig, joy, knob, roller, slider, text, meter, box,
    button, buttonSig, butBank, butBankSig, toggle, toggleSig,
    value    
) where

import Control.Applicative(liftA2)

import Csound.Exp.Gui
import Csound.Exp.Wrapper
import Csound.Exp.SE
import Csound.Exp.GE(GE, saveGuiRoot, appendToGui, newGuiVar, newGuiHandle, guiHandleToVar)
import Csound.Exp.Event
import Csound.Render.Channel(flSetVal, flPrintk2, changed)

-- | Renders a list of panels.
panels :: [Gui] -> GE ()
panels = mapM_ panel

-- | Renders the GUI elements on the window. Rectangle is calculated
-- automatically.
panel :: Gui -> GE ()
panel = saveGuiRoot . Single . Win "" Nothing 

-- | Renders the GUI elements with tabs. Rectangles are calculated
-- automatically.
tabs :: [(String, Gui)] -> GE ()
tabs = saveGuiRoot . Tabs "" Nothing . fmap (\(title, gui) -> Win title Nothing gui)

-- | Renders the GUI elements on the window. We can specify the window title
-- and rectangle of the window.
panelBy :: String -> Maybe Rect -> Gui -> GE ()
panelBy title mrect gui = saveGuiRoot $ Single $ Win title mrect gui

-- | Renders the GUI elements with tabs. We can specify the window title and
-- rectangles for all tabs and for the main window.
tabsBy :: String -> Maybe Rect -> [(String, Maybe Rect, Gui)] -> GE ()
tabsBy title mrect guis = saveGuiRoot $ Tabs title mrect $ fmap (\(a, b, c) -> Win a b c) guis

-- | Widgets that produce something has inputs.
type Input  a = SE a

-- | Widgets that consume something has outputs.
type Output a = a -> SE ()

-- | Widgets that just do something inside them or have an inner state.
type Inner    = SE ()

-- | A value for widgets that consume nothing.
noOutput :: Output ()
noOutput = return 

-- | A value for widgets that produce nothing.
noInput :: Input ()
noInput  = return ()

-- | A value for stateless widgets.
noInner :: Inner
noInner = return ()

-- | A widget consists of visible element (Gui), value consumer (Output) 
-- and producer (Input) and an inner state (Inner).
type Widget a b = GE (Gui, Output a, Input b, Inner)

-- | A consumer of the values.
type Sink   a = GE (Gui, Output a)

-- | A producer of the values.
type Source a = GE (Gui, Input a)

-- | A static element. We can only look at it.
type Display  = GE Gui

-- | A handy function for transforming the value of producers.
mapSource :: (a -> b) -> Source a -> Source b
mapSource f = fmap $ \(gui, input) -> (gui, fmap f input) 

-- | A widget constructor.
widget :: Gui -> Output a -> Input b -> Inner -> Widget a b
widget gui output input inner = do     
    handle <- newGuiHandle
    appendToGui (GuiNode gui handle) inner
    return (fromGuiHandle handle, output, input, inner)

-- | A producer constructor.
source :: Gui -> Input a -> Source a
source gui input = fmap select $ widget gui noOutput input noInner
    where select (g, _, i, _) = (g, i)

-- | A consumer constructor.
sink :: Gui -> Output a -> Sink a
sink gui output = fmap select $ widget gui output noInput noInner
    where select (g, o, _, _) = (g, o)

-- | A display constructor.
display :: Gui -> Display 
display gui = fmap select $ widget gui noOutput noInput noInner
    where select (g, _, _, _) = g

-----------------------------------------------------------------------------  
-- primitive elements

singleOut :: Maybe Double -> Elem -> Source Sig 
singleOut v0 el = do
    (var, handle) <- newGuiVar
    let handleVar = guiHandleToVar handle
        inits = maybe [] (return . InitMe handleVar) v0
        gui = fromElem [var, handleVar] inits el
    appendToGui (GuiNode gui handle) noInner
    return (fromGuiHandle handle, readVar var)

singleIn :: (GuiHandle -> Output Sig) -> Maybe Double -> Elem -> Sink Sig 
singleIn output v0 el = do
    (_, handle) <- newGuiVar
    let handleVar = guiHandleToVar handle        
        inits = maybe [] (return . InitMe handleVar) v0
        gui = fromElem [handleVar] inits el
    appendToGui (GuiNode gui handle) noInner
    return (fromGuiHandle handle, output handle)

-- | A variance on the function 'Csound.Gui.Widget.count', but it produces 
-- a signal of piecewise constant function. 
countSig :: ValDiap -> ValStep -> Maybe ValStep -> Double -> Source Sig
countSig diap step1 mValStep2 v0 = singleOut (Just v0) $ Count diap step1 mValStep2

-- | Allows the user to increase/decrease a value with mouse 
-- clicks on a corresponding arrow button. Output is an event stream that contains 
-- values when counter changes.
-- 
-- > count diapason fineValStep maybeCoarseValStep initValue 
-- 
-- doc: http://www.csounds.com/manual/html/FLcount.html
count :: ValDiap -> ValStep -> Maybe ValStep -> Double -> Source (Evt D)
count diap step1 mValStep2 v0 = mapSource snaps $ countSig diap step1 mValStep2 v0

-- | It is a squared area that allows the user to modify two output values 
-- at the same time. It acts like a joystick. 
-- 
-- > joy valueSpanX valueSpanY (initX, initY) 
--
-- doc: <http://www.csounds.com/manual/html/FLjoy.html>
joy :: ValSpan -> ValSpan -> (Double, Double) -> Source (Sig, Sig)
joy sp1 sp2 (x, y) = do
    (var1, handle1) <- newGuiVar
    (var2, handle2) <- newGuiVar
    let handleVar1 = guiHandleToVar handle1
        handleVar2 = guiHandleToVar handle2
        outs  = [var1, var2, handleVar1, handleVar2]
        inits = [InitMe handleVar1 x, InitMe handleVar2 y]
        gui   = fromElem outs inits (Joy sp1 sp2)
    appendToGui (GuiNode gui handle1) noInner
    return ( fromGuiHandle handle1, liftA2 (,) (readVar var1) (readVar var2))

-- | A FLTK widget opcode that creates a knob.
--
-- > knob valueSpan initValue
--
-- doc: <http://www.csounds.com/manual/html/FLknob.html>
knob :: ValSpan -> Double -> Source Sig
knob sp v0 = singleOut (Just v0) $ Knob sp

-- | FLroller is a sort of knob, but put transversally. 
--
-- > roller valueSpan step initVal
--
-- doc: <http://www.csounds.com/manual/html/FLroller.html>
roller :: ValSpan -> ValStep -> Double -> Source Sig
roller sp step v0 = singleOut (Just v0) $ Roller sp step

-- | FLslider puts a slider into the corresponding container.
--
-- > slider valueSpan initVal 
--
-- doc: <http://www.csounds.com/manual/html/FLslider.html>
slider :: ValSpan -> Double -> Source Sig
slider sp v0 = singleOut (Just v0) $ Slider sp

-- | FLtext allows the user to modify a parameter value by directly typing 
-- it into a text field.
--
-- > text diapason step initValue 
--
-- doc: <http://www.csounds.com/manual/html/FLtext.html>
text :: ValDiap -> ValStep -> Double -> Source Sig
text diap step v0 = singleOut (Just v0) $ Text diap step 

-- | A FLTK widget that displays text inside of a box.
--
-- > box text
--
-- doc: <http://www.csounds.com/manual/html/FLbox.html>
box :: String -> Display
box label = do
    (_, handle) <- newGuiVar
    let gui = fromElem [guiHandleToVar handle] [] (Box label)
    appendToGui (GuiNode gui handle) noInner
    return $ fromGuiHandle handle

-- | A FLTK widget opcode that creates a button. 
--
-- > button text
-- 
-- doc: <http://www.csounds.com/manual/html/FLbutton.html>
button :: Source (Evt ())
button = mapSource sigToEvt buttonSig

-- | A variance on the function 'Csound.Gui.Widget.button', but it produces 
-- a signal of piecewise constant function. 
buttonSig :: Source Sig
buttonSig = singleOut Nothing Button

-- | A FLTK widget opcode that creates a toggle button.
--
-- > button text
-- 
-- doc: <http://www.csounds.com/manual/html/FLbutton.html>
toggle :: Source (Evt D)
toggle = mapSource snaps toggleSig

-- | A variance on the function 'Csound.Gui.Widget.toggle', but it produces 
-- a signal of piecewise constant function. 
toggleSig :: Source Sig
toggleSig = singleOut Nothing Toggle

-- | A FLTK widget opcode that creates a bank of buttons.
-- 
-- > butBank xNumOfButtons yNumOfButtons
-- 
-- doc: <http://www.csounds.com/manual/html/FLbutBank.html>
butBank :: Int -> Int -> Source (Evt D)
butBank xn yn = mapSource snaps $ butBankSig xn yn

-- | A variance on the function 'Csound.Gui.Widget.butBank', but it produces 
-- a signal of piecewise constant function. 
butBankSig :: Int -> Int -> Source Sig 
butBankSig xn yn = singleOut Nothing $ ButBank xn yn

-- | FLvalue shows current the value of a valuator in a text field.
--
-- > value initVal
--
-- doc: <http://www.csounds.com/manual/html/FLvalue.html>
value :: Double -> Sink Sig 
value v = singleIn printk2 (Just v) Value

-- | A slider that serves as indicator. It consumes values instead of producing.
--
-- > meter valueSpan initValue
meter :: ValSpan -> Double -> Sink Sig
meter sp v = singleIn setVal (Just v) (Slider sp)

-- Outputs

refHandle :: GuiHandle -> SE D
refHandle h = readVar (guiHandleToVar h)

setVal :: GuiHandle -> Sig -> SE ()
setVal handle val = flSetVal (changed [val]) val =<< refHandle handle

printk2 :: GuiHandle -> Sig -> SE ()
printk2 handle val = flPrintk2 val =<< refHandle handle

