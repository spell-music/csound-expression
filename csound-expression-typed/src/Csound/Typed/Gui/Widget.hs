{-# Language DeriveFunctor #-}
module Csound.Typed.Gui.Widget(
    -- * Panels
    panel, keyPanel, tabs, keyTabs, panels,
    keyPanels, panelBy, keyPanelBy, tabsBy, keyTabsBy,

    -- * Types
    Input(..), Output(..), Inner(..),
    noInput, noOutput, noInner,
    Widget, widget, Source, source, Sink, sink, Display, display, SinkSource, sinkSource, sourceSlice, sinkSlice,
    mapSource, mapGuiSource, mhor, mver, msca,

    -- * Widgets
    count, countSig, joy, knob, roller, slider, sliderBank, numeric, meter, box,
    button, butBank, butBankSig, butBank1, butBankSig1, toggle, toggleSig,
    setNumeric,
    setToggle, setToggleSig, setKnob, setSlider,
    -- * Transformers
    setTitle,
    -- * Keyboard
    KeyEvt(..), Key(..), keyIn
) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class

import Data.Monoid
import Data.Boolean

import Csound.Dynamic hiding (int, when1)
import qualified Csound.Typed.GlobalState.Elements as C
import qualified Csound.Typed.GlobalState.Opcodes as C

import Csound.Typed.Gui.Gui
import Csound.Typed.GlobalState
import Csound.Typed.Types hiding (whens)
import Csound.Typed.InnerOpcodes

-- | Renders a list of panels.
panels :: [Gui] -> SE ()
panels = mapM_ panel

-- | Renders a list of panels. Panels are sensitive to keyboard events.
keyPanels :: [Gui] -> SE ()
keyPanels = mapM_ keyPanel

-- | Renders the GUI elements on the window. Rectangle is calculated
-- automatically (window doesn't listens for keyboard events).
panel :: Gui -> SE ()
panel = genPanel False

-- | Renders the GUI elements on the window. Rectangle is calculated
-- automatically (window listens for keyboard events).
keyPanel :: Gui -> SE ()
keyPanel = genPanel True

genPanel :: Bool -> Gui -> SE ()
genPanel isKeybd g = geToSe $ saveGuiRoot $ Single (Win "" Nothing g) isKeybd

-- | Renders the GUI elements with tabs. Rectangles are calculated
-- automatically.
tabs :: [(String, Gui)] -> SE ()
tabs = genTabs False

-- | Renders the GUI elements with tabs. Rectangles are calculated
-- automatically.
keyTabs :: [(String, Gui)] -> SE ()
keyTabs = genTabs True

genTabs :: Bool -> [(String, Gui)] -> SE ()
genTabs isKey xs = geToSe $ saveGuiRoot $ Tabs "" Nothing (fmap (\(title, gui) -> Win title Nothing gui) xs) isKey

-- | Renders the GUI elements on the window. We can specify the window title
-- and rectangle of the window.
panelBy :: String -> Maybe Rect -> Gui -> SE ()
panelBy = genPanelBy False

-- | Renders the GUI elements on the window. We can specify the window title
-- and rectangle of the window. Panesls are sensitive to keyboard events.
keyPanelBy :: String -> Maybe Rect -> Gui -> SE ()
keyPanelBy = genPanelBy True

genPanelBy :: Bool -> String -> Maybe Rect -> Gui -> SE ()
genPanelBy isKeybd title mrect gui = geToSe $ saveGuiRoot $ Single (Win title mrect gui) isKeybd

-- | Renders the GUI elements with tabs. We can specify the window title and
-- rectangles for all tabs and for the main window.
tabsBy :: String -> Maybe Rect -> [(String, Maybe Rect, Gui)] -> SE ()
tabsBy = genTabsBy False

-- | Renders the GUI elements with tabs. We can specify the window title and
-- rectangles for all tabs and for the main window. Tabs are sensitive to keyboard events.
keyTabsBy :: String -> Maybe Rect -> [(String, Maybe Rect, Gui)] -> SE ()
keyTabsBy = genTabsBy True

genTabsBy :: Bool -> String -> Maybe Rect -> [(String, Maybe Rect, Gui)] -> SE ()
genTabsBy isKeybd title mrect gui = geToSe $ saveGuiRoot $ Tabs title mrect (fmap (\(a, b, c) -> Win a b c) gui) isKeybd

-- | Widgets that produce something has inputs.
type Input  a = a

-- | Widgets that consume something has outputs.
type Output a = a -> SE ()

-- | Widgets that just do something inside them or have an inner state.
type Inner    = SE ()

-- | A value for widgets that consume nothing.
noOutput :: Output ()
noOutput = return

-- | A value for widgets that produce nothing.
noInput :: Input ()
noInput  = ()

-- | A value for stateless widgets.
noInner :: Inner
noInner = return ()

-- | A widget consists of visible element (Gui), value consumer (Output)
-- and producer (Input) and an inner state (Inner).
type Widget a b = SE (Gui, Output a, Input b, Inner)

-- | A consumer of the values.
type Sink   a = SE (Gui, Output a)

-- | A producer of the values.
type Source a = SE (Gui, Input a)

type SinkSource a = SE (Gui, Output a, Input a)

-- | A static element. We can only look at it.
type Display  = SE Gui

-- | A handy function for transforming the value of producers.
mapSource :: (a -> b) -> Source a -> Source b
mapSource f x = fmap (second f) x

-- | A handy function for transforming the GUIs of producers.
mapGuiSource :: (Gui -> Gui) -> Source a -> Source a
mapGuiSource f x = fmap (\(gui, ins) -> (f gui, ins)) x

mGroup :: Monoid a => ([Gui] -> Gui) -> [Source a] -> Source a
mGroup guiGroup as = do
    (gs, fs) <- fmap unzip $ sequence as
    return (guiGroup gs, mconcat fs)

-- | Horizontal grouping of widgets that can produce monoidal values.
mhor :: Monoid a => [Source a] -> Source a
mhor = mGroup hor

-- | Vertical grouping of widgets that can produce monoidal values.
mver :: Monoid a => [Source a] -> Source a
mver = mGroup ver

-- | Scaling of widgets that can produce values.
msca :: Double -> Source a -> Source a
msca d = mapGuiSource (sca d)

-- | A widget constructor.
widget :: SE (Gui, Output a, Input b, Inner) -> Widget a b
widget x = go =<< x
    where
        go :: (Gui, Output a, Input b, Inner) -> Widget a b
        go (gui, outs, ins, inner) = geToSe $ do
            handle <- newGuiHandle
            appendToGui (GuiNode gui handle) (unSE inner)
            return (fromGuiHandle handle, outs, ins, inner)

-- | A producer constructor.
source :: SE (Gui, Input a) -> Source a
source x = fmap select $ widget $ fmap append x
    where
        select (g, _, i, _) = (g, i)
        append (g, i) = (g, noOutput, i, noInner)

-- | A consumer constructor.
sink :: SE (Gui, Output a) -> Sink a
sink x = fmap select $ widget $ fmap append x
    where
        select (g, o, _, _) = (g, o)
        append (g, o) = (g, o, noInput, noInner)

sinkSource :: SE (Gui, Output a, Input a) -> SinkSource a
sinkSource x = fmap select $ widget $ fmap append x
    where
        select (g, o, i, _) = (g, o, i)
        append (g, o, i) = (g, o, i, noInner)

-- | A display constructor.
display :: SE Gui -> Display
display x = fmap select $ widget $ fmap append x
    where
        select (g, _, _, _) = g
        append g = (g, noOutput, noInput, noInner)

-----------------------------------------------------------------------------
-- primitive elements

-- | Appends a title to a group of widgets.
setTitle :: String -> Gui -> SE Gui
setTitle name g
    | null name = return g
    | otherwise = do
        gTitle <- box name
        return $ ver [sca 0.01 gTitle, g]

setSourceTitle :: String -> Source a -> Source a
setSourceTitle name ma = source $ do
    (gui, val) <- ma
    newGui <- setTitle name gui
    return (newGui, val)

setLabelSource :: String -> Source a -> Source a
setLabelSource a
    | null a    = id
    | otherwise = fmap (first $ setLabel a)

setLabelSink :: String -> Sink a -> Sink a
setLabelSink a
    | null a    = id
    | otherwise = fmap (first $ setLabel a)

setLabelSnkSource :: String -> SinkSource a -> SinkSource a
setLabelSnkSource a
    | null a    = id
    | otherwise = fmap (\(x, y, z) -> (setLabel a x, y, z))

singleOut :: Maybe Double -> Elem -> Source Sig
singleOut v0 el = geToSe $ do
    (var, handle) <- newGuiVar
    let handleVar = guiHandleToVar handle
        inits = maybe [] (return . InitMe handleVar) v0
        gui = fromElem [var, handleVar] inits el
    appendToGui (GuiNode gui handle) (unSE noInner)
    return (fromGuiHandle handle, readSig var)

singleIn :: (GuiHandle -> Output Sig) -> Maybe Double -> Elem -> Sink Sig
singleIn outs v0 el = geToSe $ do
    (var, handle) <- newGuiVar
    let handleVar = guiHandleToVar handle
        inits = maybe [] (return . InitMe handleVar) v0
        gui = fromElem [var, handleVar] inits el
    appendToGui (GuiNode gui handle) (unSE noInner)
    return (fromGuiHandle handle, outs handle)

singleInOut :: (GuiHandle -> Output Sig) -> Maybe Double -> Elem -> SinkSource Sig
singleInOut outs v0 el = geToSe $ do
    (var, handle) <- newGuiVar
    let handleVar = guiHandleToVar handle
        inits = maybe [] (return . InitMe handleVar) v0
        gui = fromElem [var, handleVar] inits el
    appendToGui (GuiNode gui handle) (unSE noInner)
    return (fromGuiHandle handle, outs handle, readSig var)

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
joy sp1 sp2 (x, y) = geToSe $ do
    (var1, handle1) <- newGuiVar
    (var2, handle2) <- newGuiVar
    let handleVar1 = guiHandleToVar handle1
        handleVar2 = guiHandleToVar handle2
        outs  = [var1, var2, handleVar1, handleVar2]
        inits = [InitMe handleVar1 x, InitMe handleVar2 y]
        gui   = fromElem outs inits (Joy sp1 sp2)
    appendToGui (GuiNode gui handle1) (unSE noInner)
    return ( fromGuiHandle handle1, (readSig var1, readSig var2))

-- | A FLTK widget opcode that creates a knob.
--
-- > knob valueSpan initValue
--
-- doc: <http://www.csounds.com/manual/html/FLknob.html>
knob :: String -> ValSpan -> Double -> Source Sig
knob name sp v0 = setLabelSource name $ singleOut (Just v0) $ Knob sp

-- | FLroller is a sort of knob, but put transversally.
--
-- > roller valueSpan step initVal
--
-- doc: <http://www.csounds.com/manual/html/FLroller.html>
roller :: String -> ValSpan -> ValStep -> Double -> Source Sig
roller name sp step v0 = setLabelSource name $ singleOut (Just v0) $ Roller sp step

-- | FLslider puts a slider into the corresponding container.
--
-- > slider valueSpan initVal
--
-- doc: <http://www.csounds.com/manual/html/FLslider.html>
slider :: String -> ValSpan -> Double -> Source Sig
slider name sp v0 = setLabelSource name $ singleOut (Just v0) $ Slider sp

-- | Constructs a list of linear unit sliders (ranges in [0, 1]). It takes a list
-- of init values.
sliderBank :: String -> [Double] -> Source [Sig]
sliderBank name ds = source $ do
    (gs, vs) <- fmap unzip $ zipWithM (\n d -> slider (show n) uspan d) [(1::Int) ..] ds
    gui <- setTitle name  $ hor gs
    return (gui, vs)

-- | numeric (originally FLtext in the Csound) allows the user to modify
-- a parameter value by directly typing it into a text field.
--
-- > numeric diapason step initValue
--
-- doc: <http://www.csounds.com/manual/html/FLtext.html>
numeric :: String -> ValDiap -> ValStep -> Double -> Source Sig
numeric name diap step v0 = setLabelSource name $ singleOut (Just v0) $ Text diap step

-- | A FLTK widget that displays text inside of a box.
-- If the text is longer than 255 characters the text
-- is split on several parts (Csound limitations).
--
-- > box text
--
-- doc: <http://www.csounds.com/manual/html/FLbox.html>
box :: String -> Display
box label
    | length label < lim = rawBox label
    | otherwise          = fmap (padding 0 . ver) $ mapM rawBox $ parts lim label
    where
        parts n xs
            | length xs < n = [xs]
            | otherwise     = a : parts n b
            where (a, b) = splitAt n xs
        lim = 255

rawBox :: String -> Display
rawBox label = geToSe $ do
    (_, handle) <- newGuiVar
    let gui = fromElem [guiHandleToVar handle] [] (Box label)
    appendToGui (GuiNode gui handle) (unSE noInner)
    return $ fromGuiHandle handle

-- | A FLTK widget opcode that creates a button.
--
-- > button text
--
-- doc: <http://www.csounds.com/manual/html/FLbutton.html>
button :: String -> Source (Evt Unit)
button name = setLabelSource name $ source $ do
    flag <- geToSe $ onGlobals $ C.newPersistentGlobalVar Kr 0
    flagChanged <- geToSe $ onGlobals $ C.newPersistentGlobalVar Kr 0
    instrId <- geToSe $ saveInstr $ instr flag
    geToSe $ (saveAlwaysOnInstr =<< ) $ saveInstr $ instrCh flag flagChanged
    (g, _) <- singleOut Nothing (Button instrId)
    val <- fmap fromGE $ fromDep $ readVar flagChanged
    return (g, sigToEvt val)
    where
        instr ref = SE $ do
            val <- readVar ref
            whens Kr
                [ (val ==* 0, writeVar ref 1)
                ] (writeVar ref 0)
            turnoff

        instrCh ref refCh = SE $ do
            val <- readVar ref
            writeVar refCh (C.changed val)

-- | A FLTK widget opcode that creates a toggle button.
--
-- > button text
--
-- doc: <http://www.csounds.com/manual/html/FLbutton.html>
toggle :: String -> Bool -> Source (Evt D)
toggle name initVal = mapSource snaps $ toggleSig name initVal

-- | A variance on the function 'Csound.Gui.Widget.toggle', but it produces
-- a signal of piecewise constant function.
toggleSig :: String -> Bool -> Source Sig
toggleSig name initVal = setLabelSource name $ singleOut (initToggle initVal) Toggle

initToggle :: Bool -> Maybe Double
initToggle a = if a then (Just 1) else Nothing

-- | A FLTK widget opcode that creates a bank of buttons.
-- Result is (x, y) coordinate of the triggered button.
--
-- > butBank xNumOfButtons yNumOfButtons
--
-- doc: <http://www.csounds.com/manual/html/FLbutBank.html>
butBank :: String -> Int -> Int -> (Int, Int) -> Source (Evt (D, D))
butBank name xn yn inits = mapSource (fmap split2 . snaps) $ butBankSig1 name xn yn inits
    where
        split2 a = (floor' $ a / y, mod' a x)
        x = int xn
        y = int yn

-- | A variance on the function 'Csound.Gui.Widget.butBank', but it produces
-- a signal of piecewise constant function.
-- Result is (x, y) coordinate of the triggered button.
butBankSig :: String -> Int -> Int -> (Int, Int) -> Source (Sig, Sig)
butBankSig name xn yn inits = mapSource split2 $ butBankSig1 name xn yn inits
    where
        split2 a = (floor' $ a / y, mod' a x)
        x = sig $ int xn
        y = sig $ int yn

-- | A FLTK widget opcode that creates a bank of buttons.
--
-- > butBank xNumOfButtons yNumOfButtons
--
-- doc: <http://www.csounds.com/manual/html/FLbutBank.html>
butBank1 :: String -> Int -> Int -> (Int, Int) -> Source (Evt D)
butBank1 name xn yn inits = mapSource snaps $ butBankSig1 name xn yn inits

butBankSig1 :: String -> Int -> Int -> (Int, Int) -> Source Sig
butBankSig1 name xn yn (x0, y0) = setSourceTitle name $ singleOut (Just n) $ ButBank xn yn
    where n = fromIntegral $ y0 + x0 * yn

-- |  FLtext that is sink shows current the value of a valuator in a text field.
setNumeric :: String -> ValDiap -> ValStep -> Double -> Sink Sig
setNumeric name diap step v0 = setLabelSink name $ singleIn printk2 (Just v0) $ Text diap step

-- | A slider that serves as indicator. It consumes values instead of producing.
--
-- > meter valueSpan initValue
meter :: String -> ValSpan -> Double -> Sink Sig
meter name sp v = setLabelSink name $ singleIn setVal (Just v) (Slider sp)

-------------------------------------------------------------
-- writeable widgets

setToggleSig :: String -> Bool -> SinkSource Sig
setToggleSig name initVal = setLabelSnkSource name $ singleInOut setVal (initToggle initVal) Toggle

setToggle :: String -> Bool -> SinkSource (Evt D)
setToggle name initVal = sinkSource $ do
    (g, outs, ins) <- setToggleSig name initVal
    let evtOuts a = outs =<< stepper 0 (fmap sig a)
    return (g, evtOuts, snaps ins)

setKnob :: String -> ValSpan -> Double -> SinkSource Sig
setKnob name sp v0 = setLabelSnkSource name $ singleInOut setVal' (Just v0) $ Knob sp

setSlider :: String -> ValSpan -> Double -> SinkSource Sig
setSlider name sp v0 = setLabelSnkSource name $ singleInOut setVal' (Just v0) $ Slider sp

-------------------------------------------------------------
-- keyboard

-- | The stream of keyboard press/release events.
keyIn :: KeyEvt -> Evt Unit
keyIn evt = boolToEvt $ asig ==* 1
    where asig = Sig $ fmap readOnlyVar $ listenKeyEvt evt

-- Outputs

readD :: Var -> SE D
readD v = fmap (D . return) $ SE $ readVar v

readSig :: Var -> Sig
readSig v = Sig $ return $ readOnlyVar v


refHandle :: GuiHandle -> SE D
refHandle h = readD (guiHandleToVar h)

setVal :: GuiHandle -> Sig -> SE ()
setVal handle val = flSetVal (changed [val]) val =<< refHandle handle

printk2 :: GuiHandle -> Sig -> SE ()
printk2 handle val = flPrintk2 val =<< refHandle handle

setVal' :: GuiHandle -> Sig -> SE ()
setVal' handle val = flSetVal 1 val =<< refHandle handle


-------------------------------------------------------------
-- set gui value

flSetVal :: Sig -> Sig -> D -> SE ()
flSetVal trig val handle = SE $ (depT_ =<<) $ lift $ f <$> toGE trig <*> toGE val <*> toGE handle
    where f a b c = opcs "FLsetVal" [(Xr, [Kr, Kr, Ir])] [a, b, c]

flPrintk2 :: Sig -> D -> SE ()
flPrintk2 val handle = SE $ (depT_ =<<) $ lift $ f <$> toGE val <*> toGE handle
    where f a b = opcs "FLprintk2" [(Xr, [Kr, Ir])] [a, b]

-----------------------------------------------------

sourceSlice :: SinkSource a -> Source a
sourceSlice = fmap (\(gui, _, a) -> (gui, a))

sinkSlice :: SinkSource a -> Sink a
sinkSlice = fmap (\(gui, a, _) -> (gui, a))

