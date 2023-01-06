-- | Primitive GUI elements.
--
-- There is a convention that constructors take only parameters that
-- specify the logic of the widget. The view is set for GUI-elements with
-- other functions.
module Csound.Control.Gui.Widget (
    -- * Common properties
    ValDiap(..), ValStep, ValScaleType(..), ValSpan(..),
    linSpan, expSpan, uspan, bspan, uspanExp,
    -- * Valuators
    count, countSig, joy,
    knob, KnobType(..), setKnobType,
    roller,
    slider, sliderBank, SliderType(..), setSliderType,
    numeric, TextType(..), setTextType,

    -- * Other widgets
    box, BoxType(..), setBoxType,
    button, ButtonType(..), setButtonType,
    toggle, butBank, toggleSig, butBankSig,
    butBank1, butBankSig1,
    radioButton, matrixButton, funnyRadio, funnyMatrix,
    setNumeric, meter,
    setKnob, setSlider,
    setToggle, setToggleSig,
    -- * Transformers
    setTitle,
    -- * Keyboard
    KeyEvt(..), Key(..), keyIn, charOn, charOff, strOn, strOff,

    -- * Easy to use widgets
    uknob, xknob, uslider, xslider, ujoy,
    hradio, vradio, hradioSig, vradioSig,

    -- * Number selectors
    -- | Widgets for sample and hold functions
    hnumbers, vnumbers,

    -- * Range widgets
    Range,
    rangeKnob, rangeSlider, rangeKnobSig, rangeSliderSig,
    rangeJoy, rangeJoy2, rangeJoySig,

    -- * The 2D matrix of widgets
    knobPad, togglePad, buttonPad, genPad,

    -- * External control

    -- | The widgets can be controlled with external signals/event streams
    button', toggle', toggleSig', knob', slider', uknob', uslider',
    hradio', vradio', hradioSig', vradioSig'
) where

import Prelude hiding (span, reads)

import Control.Monad

import Data.List(transpose)
import Data.Boolean

import Csound.Typed.Gui hiding (widget, height, width)
import Csound.Typed.Types
import Csound.Control.SE
import Csound.Control.Evt(listAt, Tick, snaps2, dropE, devt, loadbang, evtToSig)
import Csound.Typed.Opcode(changed)
import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------
-- aux widgets

readMatrix :: Int -> Int -> [a] -> [a]
readMatrix xn yn as = transp $ take (xn * yn) $ as ++ repeat (head as)
    where
        transp xs = concat $ transpose $ parts yn xn xs
        parts x y qs
            | x == 0    = []
            | otherwise = a : parts (x - 1) y b
            where (a, b) = splitAt y qs

-- | A radio button. It takes a list of values with labels.
radioButton :: Arg a => Text -> [(Text, a)] -> Int -> Source (Evt a)
radioButton title as initVal = source $ do
    (g, ind) <- butBank1 "" 1 (length as) (0, initVal)
    gnames   <- mapM box names
    let val = listAt vals $ fmap sig ind
    gui <- setTitle title $ padding 0 $ hor [sca 0.15 g, ver gnames]
    return (gui, val)
    where (names, vals) = unzip as

-- | A matrix of values.
matrixButton :: Arg a => Text -> Int -> Int -> [a] -> (Int, Int) -> Source (Evt a)
matrixButton name xn yn vals initVal = source $ do
    (gui, ind) <- butBank1 name xn yn initVal
    let val = listAt allVals $ fmap sig ind
    return (gui, val)
    where allVals = readMatrix xn yn vals

-- | Radio button that returns functions. Useful for picking a waveform or type of filter.
funnyRadio :: Tuple b => Text -> [(Text, a -> b)] -> Int -> Source (a -> b)
funnyRadio name as initVal = source $ do
    (gui, ind) <- radioButton name (zip names (fmap int [0 ..])) initVal
    contInd <- stepper (sig $ int initVal) $ fmap sig ind
    let instr x = guardedTuple (
                zipWith (\n f -> (contInd ==* (sig $ int n), f x)) [0 ..] funs
            ) (head funs x)
    return (gui, instr)
    where (names, funs) = unzip as

-- | Matrix of functional values.
funnyMatrix :: Tuple b => Text -> Int -> Int -> [(a -> b)] -> (Int, Int) -> Source (a -> b)
funnyMatrix name xn yn funs initVal@(x0, y0) = source $ do
    (gui, ind) <- butBank1 name xn yn initVal
    contInd <- stepper flattenInitVal $ fmap sig ind
    let instr x = guardedTuple (
                zipWith (\n f -> (contInd ==* (sig $ int n), f x)) [0 ..] allFuns
            ) (head allFuns x)
    return (gui, instr)
    where
        allFuns = readMatrix xn yn funs
        flattenInitVal = sig $ int $ y0 + x0 * yn


-- | Shortcut for press 'CharKey' events.
charOn :: Char -> Evt Unit
charOn  = keyIn . Press   . CharKey

-- | Shortcut for release 'CharKey' events.
charOff :: Char -> Evt Unit
charOff = keyIn . Release . CharKey

-- | Creates an event in the output stream when one of the chars is pressed.
strOn :: String -> Tick
strOn a = mconcat $ fmap charOn a

-- | Creates an event in the output stream when one of the chars is depressed.
strOff :: String -> Tick
strOff a = mconcat $ fmap charOff a

-- | Unipolar linear slider. The value belongs to the interval [0, 1].
-- The argument is for initial value.
uslider :: Double -> Source Sig
uslider = slider "" (linSpan 0 1)

-- | Unipolar linear knob. The value belongs to the interval [0, 1].
-- The argument is for initial value.
uknob :: Double -> Source Sig
uknob = knob "" (linSpan 0 1)

-- | Exponential slider (usefull for exploring frequencies or decibels).
--
-- > xknob (min, max) initVal
--
-- The value belongs to the interval [min, max].
-- The last argument is for initial value.
xslider :: Range Double -> Double -> Source Sig
xslider (a, b) initVal = slider "" (expSpan a b) initVal

-- | Exponential knob (usefull for exploring frequencies or decibels).
--
-- > xknob (min, max) initVal
--
-- The value belongs to the interval [min, max].
-- The last argument is for initial value.
xknob :: Range Double -> Double -> Source Sig
xknob (a, b) initVal = knob "" (expSpan a b) initVal

-- | Unit linear joystick.
ujoy :: (Double, Double) -> Source (Sig, Sig)
ujoy = joy (linSpan 0 1) (linSpan 0 1)

---------------------------------------------------------------
-- sample and hold

-- | The sample and hold widget. You can pick a value from the list of doubles.
-- The original value is a head of the list (the first element).
-- The visual grouping is horizontal (notice the prefix @h@).
-- It's common to use it with function @selector@.
hnumbers :: [Double] -> Source Sig
hnumbers = genNumbers hor

-- | The sample and hold widget. You can pick a value from the list of doubles.
-- The original value is a head of the list (the first element).
-- The visual grouping is vertical (notice the prefix @v@).
-- It's common to use it with function @selector@.
vnumbers :: [Double] -> Source Sig
vnumbers = genNumbers ver

genNumbers :: ([Gui] -> Gui) -> [Double] -> Source Sig
genNumbers gx as@(d:_) = source $ do
    ref <- newGlobalCtrlRef (sig $ double d)
    (gs, evts) <- fmap unzip $ mapM (button . Text.pack . show) as
    zipWithM_ (\x e -> runEvt e $ \_ -> writeRef ref (sig $ double x)) as evts
    res <- readRef ref
    return (gx gs, res)
genNumbers _ [] = error "Not implemented for empty list"


-------------------------------------------------------------------
-- 2D matrix of widgets

-- | The matrix of unipolar knobs.
--
-- > knobPad columnNum rowNum names initVals
--
-- It takes in the dimensions of matrix, the names (we can leave it empty
-- if names are not important) and list of init values.
-- It returns a function that takes in indices and produces the signal in
-- the corresponding cell.
knobPad :: Int -> Int -> [Text] -> [Double] -> Source (Int -> Int -> Sig)
knobPad = genPad mkKnob 0.5
    where mkKnob name = knob name uspan

-- | The matrix of toggle buttons.
--
-- > togglePad columnNum rowNum names initVals
--
-- It takes in the dimensions of matrix, the names (we can leave it empty
-- if names are not important) and list of init values (on/off booleans).
-- It returns a function that takes in indices and produces the event stream in
-- the corresponding cell.
togglePad :: Int -> Int -> [Text] -> [Bool] -> Source (Int -> Int -> Evt D)
togglePad = genPad toggle False

-- | The matrix of buttons.
--
-- > buttonPad columnNum rowNum names
--
-- It takes in the dimensions of matrix, the names (we can leave it empty
-- if names are not important).
-- It returns a function that takes in indices and produces the event stream in
-- the corresponding cell.
buttonPad :: Int -> Int -> [Text] -> Source (Int -> Int -> Evt Unit)
buttonPad width height names = genPad mkButton False width height names []
    where mkButton name _ = button name

-- | A generic constructor for matrixes of sound source widgets.
-- It takes the constructor of the widget, a default initial value,
-- the dimensions of the matrix, the list of names and the list of initial values.
-- It produces the function that maps indices to corresponding values.
genPad :: (Text -> a -> Source b) -> a -> Int -> Int -> [Text] -> [a] -> Source (Int -> Int -> b)
genPad mk initVal width height names as = source $ do
    (gui, vals) <- fmap reGroupCol $ mapM mkRow inits
    let f x y = (vals !! y) !! x
    return $ (gui, f)
    where
        mkRow xs = fmap reGroupRow $ mapM (uncurry mk) xs

        inits = split height width $ zip (names ++ repeat "") (as ++ repeat initVal)

        split m n xs = case m of
            0 -> []
            a -> (take n xs) : split (a - 1) n (drop n xs)

        reGroupCol = reGroup ver
        reGroupRow = reGroup hor

        reGroup f bs = (f xs, ys)
            where (xs, ys) = unzip bs


-- | Horizontal radio group.
hradio :: [Text] -> Int -> Source (Evt D)
hradio = radioGroup hor

-- | Vertical radio group.
vradio :: [Text] -> Int -> Source (Evt D)
vradio = radioGroup ver

-- | Horizontal radio group.
hradioSig :: [Text] -> Int -> Source Sig
hradioSig = radioGroupSig hor

-- | Vertical radio group.
vradioSig :: [Text] -> Int -> Source Sig
vradioSig = radioGroupSig ver

radioGroup :: ([Gui] -> Gui) -> [Text] -> Int -> Source (Evt D)
radioGroup gcat names initVal = mapSource snaps $ radioGroupSig gcat names initVal

radioGroupSig  :: ([Gui] -> Gui) -> [Text] -> Int -> Source Sig
radioGroupSig gcat names initVal = source $ do
    (guis, writes, reads) <- fmap unzip3 $ mapM (\(i, tag) -> flip setToggleSig (i == initVal) tag) $ zip [0 ..] names
    curRef <- newGlobalCtrlRef (sig $ int initVal)
    current <- readRef curRef
    zipWithM_ (\w i -> w $ ifB (current ==* i) 1 0) writes ids
    zipWithM_ (\r i -> runEvt (snaps r) $ \x -> do
        when1 (sig x ==* 1) $ do
            writeRef curRef i
        when1 (sig x ==* 0 &&* current ==* i) $ do
           writeRef curRef i
        ) reads ids

    res <- readRef curRef
    return (gcat guis, res)
    where
        ids = fmap (sig . int) [0 .. length names - 1]



-- | Pair of minimum and maximum values.
type Range a = (a, a)

-- | Creates a knob that outputs only integers in the given range.
-- It produces a signal of integer values.
--
-- > rangeKnobSig (min, max) initVal
rangeKnobSig :: Range Int -> Int -> Source Sig
rangeKnobSig = rangeSig1 uknob

-- | Creates a slider that outputs only integers in the given range.
-- It produces a signal of integer values.
--
-- > rangeSliderSig (min, max) initVal
rangeSliderSig :: Range Int -> Int -> Source Sig
rangeSliderSig = rangeSig1 uslider

-- | Creates a knob that outputs only integers in the given range.
-- It produces an event stream of integer values. It can be used with
-- list access functions @listAt@, @atTuple@, @atArg@.
--
-- > rangeKnob needInit (min, max) initVal
--
-- The first argument is a boolean. If it's true than the initial value
-- is put in the output stream. If it\s False the initial value is skipped.
rangeKnob :: Bool -> Range Int -> Int -> Source (Evt D)
rangeKnob = rangeEvt1 uknob

-- | Creates a slider that outputs only integers in the given range.
-- It produces an event stream of integer values. It can be used with
-- list access functions @listAt@, @atTuple@, @atArg@.
--
-- > rangeSlider needInit (min, max) initVal
--
-- The first argument is a boolean. If it's true than the initial value
-- is put in the output stream. If it\s False the initial value is skipped.
rangeSlider :: Bool -> Range Int -> Int -> Source (Evt D)
rangeSlider = rangeEvt1 uslider

rangeSig1 :: (Double -> Source Sig) -> Range Int -> Int -> Source Sig
rangeSig1 widget range initVal = mapSource (fromRelative range) $ widget $ toRelativeInitVal range initVal

rangeEvt1 :: (Double -> Source Sig) -> Bool -> Range Int -> Int -> Source (Evt D)
rangeEvt1 widget isInit range initVal = mapSource (addInit . snaps) $ rangeSig1 widget range initVal
    where
        addInit
            | isInit    = ((devt (int initVal) loadbang) `mappend` )
            | otherwise = id

-- | 2d range range slider. Outputs a pair of event streams.
-- Each stream  contains changes in the given direction (Ox or Oy).
--
-- > rangeJoy needsInit rangeX rangeY (initX, initY)
--
-- The first argument is a boolean. If it's true than the initial value
-- is put in the output stream. If it\s False the initial value is skipped.
rangeJoy :: Bool -> Range Int -> Range Int -> (Int, Int) -> Source (Evt D, Evt D)
rangeJoy isInit rangeX rangeY initVals = mapSource (addInit . f) $ rangeJoySig rangeX rangeY initVals
    where
        f (x, y) = (snaps x, snaps y)
        addInit
            | isInit    = id
            | otherwise = \(a, b) -> (dropE 1 a, dropE 1 b)

-- | 2d range range slider. It produces a single event stream.
-- The event fires when any signal changes.
--
-- > rangeJoy2 needsInit rangeX rangeY (initX, initY)
--
-- The first argument is a boolean. If it's true than the initial value
-- is put in the output stream. If it\s False the initial value is skipped.
rangeJoy2 :: Bool -> Range Int -> Range Int -> (Int, Int) -> Source (Evt (D, D))
rangeJoy2 isInit rangeX rangeY initVals = mapSource (addInit . snaps2) $ rangeJoySig rangeX rangeY initVals
    where
        addInit
            | isInit    = id
            | otherwise = dropE 1

-- | 2d range range slider. It produces the pair of integer signals
rangeJoySig :: Range Int -> Range Int -> (Int, Int) -> Source (Sig, Sig)
rangeJoySig rangeX rangeY (initValX, initValY) = mapSource f $
    ujoy (toRelativeInitVal rangeX initValX, toRelativeInitVal rangeY initValY)
    where f (x, y) = (fromRelative rangeX x, fromRelative rangeY y)

toRelativeInitVal :: Range Int -> Int -> Double
toRelativeInitVal (kmin, kmax) initVal = (fromIntegral $ initVal - kmin) / (fromIntegral $ (kmax - 1) - kmin)

fromRelative :: Range Int -> Sig -> Sig
fromRelative (kmin, kmax) = floor' . uon (f kmin) (f kmax - 0.01)
    where f = sig . int


------------------------------------------------------------
-- external control of widgets

-- | It's like simple @button@, but it can be controlled with external control.
-- The first argument is for external control.
button' :: Tick -> Text -> Source Tick
button' ctrl name = mapSource (mappend ctrl) $ button name

-- | It's like simple @toggle@, but it can be controlled with external control.
-- The first argument is for external control.
toggle' :: Evt D -> Text -> Bool -> Source (Evt D)
toggle' ctrl name initVal = source $ do
    (gui, output, input) <- setToggle name initVal
    output ctrl
    return $ (gui, mappend ctrl input)

toggleSig' :: Sig -> Text -> Bool -> Source Sig
toggleSig' ctrl name initVal =
    ctrlSig (if initVal then 1 else 0) ctrl $ setToggleSig name initVal

-- | It's like simple @uknob@, but it can be controlled with external control.
-- The first argument is for external control.
uknob' :: Sig -> Double -> Source Sig
uknob' ctrl initVal = ctrlSig (double initVal) ctrl $ setKnob "" uspan initVal

-- | It's like simple @uslider@, but it can be controlled with external control.
-- The first argument is for external control.
uslider' :: Sig -> Double -> Source Sig
uslider' ctrl initVal = ctrlSig (double initVal) ctrl $ setSlider "" uspan initVal

-- | It's like simple @knob@, but it can be controlled with external control.
-- The first argument is for external control.
knob' :: Sig -> Text -> ValSpan -> Double -> Source Sig
knob' ctrl name span initVal = ctrlSig (double initVal) ctrl $ setKnob name span initVal

-- | It's like simple @slider@, but it can be controlled with external control.
-- The first argument is for external control.
slider' :: Sig -> Text -> ValSpan -> Double -> Source Sig
slider' ctrl name span initVal = ctrlSig (double initVal) ctrl $ setSlider name span initVal

-- | It's like simple @hradioSig@, but it can be controlled with external control.
-- The first argument is for external control.
hradioSig' :: Sig -> [Text] -> Int -> Source Sig
hradioSig' = radioGroupSig' hor

-- | It's like simple @vradioSig@, but it can be controlled with external control.
-- The first argument is for external control.
vradioSig' :: Sig -> [Text] -> Int -> Source Sig
vradioSig' = radioGroupSig' ver

-- | It's like simple @hradio@, but it can be controlled with external control.
-- The first argument is for external control.
hradio' :: Evt D -> [Text] -> Int -> Source (Evt D)
hradio' = radioGroup' hor

-- | It's like simple @vradio@, but it can be controlled with external control.
-- The first argument is for external control.
vradio' :: Evt D -> [Text] -> Int -> Source (Evt D)
vradio' = radioGroup' ver

radioGroup'  :: ([Gui] -> Gui) -> Evt D -> [Text] -> Int -> Source (Evt D)
radioGroup' gcat ctrl names initVal =  mapSource snaps $ radioGroupSig' gcat (evtToSig (int initVal) ctrl) names initVal

radioGroupSig'  :: ([Gui] -> Gui) -> Sig -> [Text] -> Int -> Source Sig
radioGroupSig' gcat ctrl names initVal = source $ do
    (guis, writes, reads) <- fmap unzip3 $ mapM (\(i, tag) -> flip setToggleSig (i == initVal) tag) $ zip [0 ..] names
    curRef <- newGlobalCtrlRef (sig $ int initVal)

    when1 (changed [ctrl] ==* 1) $ writeRef curRef ctrl

    current <- readRef curRef
    zipWithM_ (\w i -> w $ ifB (current ==* i) 1 0) writes ids
    zipWithM_ (\r i -> runEvt (snaps r) $ \x -> do
        when1 (sig x ==* 1) $ do
            writeRef curRef i
        when1 (sig x ==* 0 &&* current ==* i) $ do
           writeRef curRef i
        ) reads ids

    res <- readRef curRef
    return (gcat guis, res)
    where
        ids = fmap (sig . int) [0 .. length names - 1]


ctrlSig :: D -> Sig -> SinkSource Sig -> Source Sig
ctrlSig initVal ctrl v = source $ do
    (gui, output, input) <- v
    ref <- newGlobalCtrlRef (sig initVal)
    when1 (changed [ctrl] ==* 1) $ writeRef ref ctrl
    when1 (changed [input] ==* 1) $ writeRef ref input
    res <- readRef ref
    output res
    return (gui, res)

