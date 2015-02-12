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
    setToggle, setToggleSig,
    -- * Transformers
    setTitle,
    -- * Keyboard
    KeyEvt(..), Key(..), keyIn, charOn, charOff,

    -- * Easy to use widgets
    uknob, xknob, uslider, xslider, ujoy, 
    hradio, vradio, hradioSig, vradioSig,

    -- * Number selectors
    -- | Widgets for sample and hold functions
    hnumbers, vnumbers,

    -- * The 2D matrix of widgets
    knobPad, togglePad, buttonPad, genPad
) where

import Control.Monad

import Data.List(transpose)
import Data.Boolean

import Csound.Typed.Gui
import Csound.Typed.Types
import Csound.Control.SE
import Csound.Control.Evt(listAt)

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
radioButton :: Arg a => String -> [(String, a)] -> Int -> Source (Evt a)
radioButton title as initVal = source $ do
    (g, ind) <- butBank1 "" 1 (length as) (0, initVal)
    gnames   <- mapM box names
    let val = listAt vals ind    
    gui <- setTitle title $ padding 0 $ hor [sca 0.15 g, ver gnames]
    return (gui, val)
    where (names, vals) = unzip as

-- | A matrix of values.
matrixButton :: Arg a => String -> Int -> Int -> [a] -> (Int, Int) -> Source (Evt a)
matrixButton name xn yn vals initVal = source $ do
    (gui, ind) <- butBank1 name xn yn initVal
    let val = listAt allVals ind
    return (gui, val)
    where allVals = readMatrix xn yn vals

-- | Radio button that returns functions. Useful for picking a waveform or type of filter.
funnyRadio :: Tuple b => String -> [(String, a -> b)] -> Int -> Source (a -> b)
funnyRadio name as initVal = source $ do
    (gui, ind) <- radioButton name (zip names (fmap int [0 ..])) initVal
    contInd <- stepper (sig $ int initVal) $ fmap sig ind
    let instr x = guardedTuple (
                zipWith (\n f -> (contInd ==* (sig $ int n), f x)) [0 ..] funs
            ) (head funs x)
    return (gui, instr)
    where (names, funs) = unzip as

-- | Matrix of functional values.
funnyMatrix :: Tuple b => String -> Int -> Int -> [(a -> b)] -> (Int, Int) -> Source (a -> b)
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
-- > xknob min max initVal
--
-- The value belongs to the interval [min, max].
-- The last argument is for initial value.
xslider :: Double -> Double -> Double -> Source Sig
xslider a b initVal = slider "" (expSpan a b) initVal

-- | Exponential knob (usefull for exploring frequencies or decibels). 
--
-- > xknob min max initVal
--
-- The value belongs to the interval [min, max].
-- The last argument is for initial value.
xknob :: Double -> Double -> Double -> Source Sig
xknob a b initVal = knob "" (expSpan a b) initVal

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
genNumbers gx as@(d:ds) = source $ do
    ref <- newGlobalSERef (sig $ double d)
    (gs, evts) <- fmap unzip $ mapM (button . show) as
    zipWithM_ (\x e -> runEvt e $ \_ -> writeSERef ref (sig $ double x)) as evts 
    res <- readSERef ref
    return (gx gs, res)


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
knobPad :: Int -> Int -> [String] -> [Double] -> Source (Int -> Int -> Sig)
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
togglePad :: Int -> Int -> [String] -> [Bool] -> Source (Int -> Int -> Evt D)
togglePad = genPad toggle False

-- | The matrix of buttons.
--
-- > buttonPad columnNum rowNum names
--
-- It takes in the dimensions of matrix, the names (we can leave it empty 
-- if names are not important).
-- It returns a function that takes in indices and produces the event stream in
-- the corresponding cell.
buttonPad :: Int -> Int -> [String] -> Source (Int -> Int -> Evt Unit)
buttonPad width height names = genPad mkButton False width height names []
    where mkButton name _ = button name

-- | A generic constructor for matrixes of sound source widgets.
-- It takes the constructor of the widget, a default initial value,
-- the dimensions of the matrix, the list of names and the list of initial values.
-- It produces the function that maps indices to corresponding values.
genPad :: (String -> a -> Source b) -> a -> Int -> Int -> [String] -> [a] -> Source (Int -> Int -> b)
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

        reGroup f as = (f xs, ys)
            where (xs, ys) = unzip as


-- | Horizontal radio group.
hradio :: [String] -> Int -> Source (Evt D)
hradio = radioGroup hor

-- | Vertical radio group.
vradio :: [String] -> Int -> Source (Evt D)
vradio = radioGroup ver

-- | Horizontal radio group.
hradioSig :: [String] -> Int -> Source Sig
hradioSig = radioGroupSig hor

-- | Vertical radio group.
vradioSig :: [String] -> Int -> Source Sig
vradioSig = radioGroupSig ver

radioGroup :: ([Gui] -> Gui) -> [String] -> Int -> Source (Evt D)
radioGroup gcat names initVal = mapSource snaps $ radioGroupSig gcat names initVal

radioGroupSig  :: ([Gui] -> Gui) -> [String] -> Int -> Source Sig
radioGroupSig gcat names initVal = source $ do
    (guis, writes, reads) <- fmap unzip3 $ mapM (\(i, tag) -> flip setToggleSig (i == initVal) tag) $ zip [0 ..] names
    curRef <- newGlobalSERef (sig $ int initVal)
    current <- readSERef curRef    
    zipWithM_ (\w i -> w $ ifB (current ==* i) 1 0) writes ids
    zipWithM_ (\r i -> runEvt (snaps r) $ \x -> do              
        when1 (sig x ==* 1) $ do
            writeSERef curRef i
        when1 (sig x ==* 0 &&* current ==* i) $ do
           writeSERef curRef i    
        ) reads ids   

    res <- readSERef curRef
    return (gcat guis, res)
    where        
        ids = fmap (sig . int) [0 .. length names - 1]