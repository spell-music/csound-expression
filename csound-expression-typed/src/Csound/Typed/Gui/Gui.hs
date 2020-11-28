module Csound.Typed.Gui.Gui (
    Panel(..), Win(..), GuiNode(..), GuiHandle(..), Gui(..),
    Elem(..), InitMe(..),
    restoreTree, guiMap, mapGuiOnPanel, fromElem, fromGuiHandle,
    panelIsKeybdSensitive, defText,
    guiStmt,

    -- * Layout
    hor, ver, space, sca, horSca, verSca,
    padding, margin, ScaleFactor, resizeGui,
    -- * Props
    props, forceProps,
    Prop(..), BorderType(..), Color,
    Rect(..), FontType(..), Emphasis(..),
    Material(..), Orient(..), LabelType(..),
    -- ** Setters
    -- | Handy short-cuts for the function @props@.
    setBorder, setLabel, setMaterial, setLabelType,
    setColor1, setColor2, setColors, setTextColor,
    setFontSize, setFontType, setEmphasis, setOrient,

    -- * Widgets
    ValDiap(..), ValStep, ValScaleType(..), ValSpan(..),
    linSpan, expSpan, uspan, bspan, uspanExp,
    KnobType(..), setKnobType,
    SliderType(..), setSliderType,
    TextType(..), setTextType,
    BoxType(..), setBoxType,
    ButtonType(..), setButtonType
) where

import Prelude hiding(elem, span)

import Control.Applicative(Alternative(..))
import Data.Default
import Data.Char(toLower)
import Data.Maybe(isNothing)
import Data.Monoid

import Data.Colour
import Data.Colour.Names(white, gray)
import Data.Colour.SRGB

import qualified Data.IntMap as IM
import Text.PrettyPrint.Leijen(Doc)

import Csound.Dynamic(DepT, depT_, Var(..), VarType(..), Rate(..), noRate, MainExp(..), InstrId(..))

import qualified Text.PrettyPrint.Leijen as P(int, double, vcat, hcat, hsep, punctuate, comma, empty, text, char, (<+>))
import qualified Csound.Typed.Gui.BoxModel as Box
import Csound.Typed.Gui.BoxModel(Rect(..))
import Csound.Typed.Constants(infiniteDur)

import Csound.Typed.Gui.Types
import Csound.Typed.Gui.Pretty

newtype GuiHandle = GuiHandle { unGuiHandle :: Int }

-- | A visual representation of the GUI-element.
newtype Gui = Gui { unGui :: LowGui }

type LowGui = Box.Scene Props ElemWithOuts

data Panel
    = Single
        { singleContent :: Win
        , singleIsKeybdSensitive :: Bool }
    | Tabs
        { tabsTitle     :: String
        , tabsRect      :: Maybe Rect
        , tabsContent   :: [Win]
        , tabsIsKeybdSensitive :: Bool }

panelIsKeybdSensitive :: Panel -> Bool
panelIsKeybdSensitive x = case x of
    Single _ res -> res
    Tabs _ _ _ res -> res

data Win = Win
    { winTitle :: String
    , winRect  :: Maybe Rect
    , winGui   :: Gui }

data GuiNode = GuiNode
    { guiNodeElem   :: Gui
    , guiNodeHandle :: GuiHandle }

data ElemWithOuts = ElemWithOuts
    { elemOuts      :: [Var]
    , elemInits     :: [InitMe]
    , elemContent   :: Elem }

data InitMe = InitMe
    { initHandle :: Var
    , initValue  :: Double }

data Elem
    = GuiVar GuiHandle

    -- valuators
    | Count  ValDiap ValStep (Maybe ValStep)
    | Joy    ValSpan ValSpan
    | Knob   ValSpan
    | Roller ValSpan ValStep
    | Slider ValSpan
    | Text   ValDiap ValStep

    -- other widgets
    | Box String
    | ButBank Int Int
    | Button InstrId
    | Toggle
    | Value
    | Vkeybd

type ElemOuts = [Var]

defText :: String -> Gui
defText str = Gui $ Box.Prim (ElemWithOuts [Var LocalVar Ir "keybd"] [] $ Box str)

fromElem :: ElemOuts -> [InitMe] -> Elem -> Gui
fromElem outs inits el = Gui $ Box.prim (ElemWithOuts outs inits el)

fromGuiHandle :: GuiHandle -> Gui
fromGuiHandle = Gui . Box.prim . ElemWithOuts [] [] . GuiVar

mapGuiOnPanel :: (Gui -> Gui) -> Panel -> Panel
mapGuiOnPanel f x = case x of
    Single w isKey            -> Single (mapWin w) isKey
    Tabs title rect ws  isKey -> Tabs title rect (fmap mapWin ws) isKey
    where mapWin a = a{ winGui = f $ winGui a  }

onLowGuis :: ([LowGui] -> LowGui) -> ([Gui] -> Gui)
onLowGuis f = Gui . f . fmap unGui

onLowGui1 :: (LowGui -> LowGui) -> (Gui -> Gui)
onLowGui1 f = Gui . f . unGui

-- | Horizontal groupping of the elements. All elements are
-- placed in the stright horizontal line and aligned by Y-coordinate
-- and height.
hor :: [Gui] -> Gui
hor = onLowGuis Box.hor

-- | Vertical groupping of the elements. All elements are
-- placed in the stright vertical line and aligned by X-coordinate
-- and width.
ver :: [Gui] -> Gui
ver = onLowGuis Box.ver

-- | An empty space.
space :: Gui
space = Gui Box.space

-- | Scales an element within the group. It depends on the type
-- of the alignment (horizontal or vertical) which side of the bounding
-- box is scaled. If it's a horizontal group then the width is scaled
-- and height is scaled otherwise.
--
-- Every element in the group has a scaling factor. By
-- default it equals to one. During rendering all scaling factors are summed
-- and divided on the sum of all factors. So that factors become weights
-- or proportions. This process is called normalization.
-- Scaling one element affects not only this element but
-- all other elements in the group!
--
-- An example:
--
-- One element is twice as large as the other two:
--
-- > hor [a, b, sca 2 c]
--
-- Why is it so? Let's look at the hidden scaling factors:
--
-- > hor [sca 1 a, sca 1 b, sca 2 c]
--
-- During rendering we scale all the scaling fators so that
-- total sum equals to one:
--
-- > hor [sca 0.25 a, sca 0.25 b, sca 0.5 c]
sca :: Double -> Gui -> Gui
sca d = onLowGui1 (Box.sca d)

-- | Weighted horizontal grouping.
-- It takes a list of scaling factors and elements.
horSca :: [(Double, Gui)] -> Gui
horSca ps = hor $ fmap (uncurry sca) ps

-- | Weighted vertical grouping.
-- It takes a list of scaling factors and elements.
verSca :: [(Double, Gui)] -> Gui
verSca ps = ver $ fmap (uncurry sca) ps

-- | Sets the padding of the element. How much empty space
-- to reserve outside the element.
padding :: Int -> Gui -> Gui
padding n = onLowGui1 (Box.padding n)

-- | Sets the margin of the element. How much empty space
-- to reserve between the elements within the group. It affects
-- only compound elements.
margin :: Int -> Gui -> Gui
margin n = onLowGui1 (Box.margin n)

-- | Sets the properties for a GUI element.
props :: [Prop] -> Gui -> Gui
props ps = onLowGui1 (Box.appendContext $ def { otherProps = ps })

-- | Rescales the default sizes for the UI elements.
resizeGui :: ScaleFactor -> Gui -> Gui
resizeGui factorXY = onLowGui1 (Box.appendContext $ def { propsScaleFactor = Just factorXY })

-- | Sets the properties for a GUI element on all levels.
forceProps :: [Prop] -> Gui -> Gui
forceProps = error "forceProps: TODO"

setBorder :: BorderType -> Gui -> Gui
setBorder a = onLowGui1 (Box.appendContext $ def { propsBorder = Just a })

type GuiMap = IM.IntMap Gui

guiMap :: [GuiNode] -> GuiMap
guiMap = IM.fromList . fmap (\(GuiNode elem (GuiHandle n)) -> (n, elem))

restoreTree :: GuiMap -> Gui -> Gui
restoreTree m x = Gui $ (unGui x) >>= rec
    where rec elem = case elemContent elem of
            GuiVar h -> unGui $ restoreTree m $ m IM.! unGuiHandle h
            _        -> return elem


guiStmt :: Monad m => ScaleFactor -> [Panel] -> DepT m ()
guiStmt defaultScaleUI panels = depT_ $ noRate (phi defaultScaleUI)
    where phi scaleUI
            | null panels = EmptyExp
            | otherwise   = Verbatim $ show $ P.vcat [P.vcat $ fmap (drawGui scaleUI) panels, P.text "FLrun"]

drawGui :: ScaleFactor -> Panel -> Doc
drawGui defaultScaleUI x = case x of
    Single w    isKeybd -> panel isKeybd boundingRect $ drawWin (withWinMargin boundingRect) w
    Tabs _ _ ws isKeybd -> panel isKeybd tabPanelRect $ case ws of
        [] -> P.empty
        _  -> onTabs mainTabRect $ P.vcat $ fmap (uncurry $ drawTab shift) tabsRs
    where boundingRect = panelRect defaultScaleUI (fmap fst tabsRs) x
          tabsRs = tabsRects defaultScaleUI x
          (mainTabRect, shift) = mainTabRectAndShift boundingRect

          tabPanelRect = Rect
            { px = 100
            , py = 100
            , width = width mainTabRect + 20
            , height = height mainTabRect + 20
            }

          panel = onPanel (panelTitle x)

          onPanel title isKeybdSensitive rect body = P.vcat
            -- panel with default position no border and capture of keyboard events
            [ ppProc "FLpanel" [ P.text $ show title, P.int $ width rect, P.int $ height rect, P.int (-1), P.int (-1), P.int 0
                               , P.int $ if isKeybdSensitive then 1 else 0 ]
            , body
            , ppProc "FLpanelEnd" []]

          onTabs rect body = P.vcat
            [ ppProc "FLtabs" $ rectToFrame rect
            , body
            , ppProc "FLtabsEnd" []]


panelTitle :: Panel -> String
panelTitle x = case x of
    Single w _       -> winTitle w
    Tabs title _ _ _ -> title

panelRect :: ScaleFactor -> [Rect] -> Panel -> Rect
panelRect defaultScaleUI rs x = case x of
    Single w _       -> winBoundingRect defaultScaleUI w
    Tabs _ mrect _ _ -> case rs of
        [] -> Box.zeroRect
        _  -> maybe (foldr boundingRect (head rs) rs) id mrect
    where boundingRect a b = Rect { px = x1, py = y1, width = x2 - x1, height = y2 - y1 }
              where x1 = min (px a) (px b)
                    y1 = min (py a) (py b)
                    x2 = max (px a + width a) (px b + width b)
                    y2 = max (py a + height a) (py b + height b)

mainTabRectAndShift :: Rect -> (Rect, (Int, Int))
mainTabRectAndShift r = (res, (dx, dy))
    where res = Rect
            { px     = 5
            , py     = 5
            , width  = px r + width r + 10
            , height = py r + height r + yBox 15 2 + 10
            }
          dx = 10
          dy = yBox 15 2 + 10



tabsRects :: ScaleFactor -> Panel -> [(Rect, Win)]
tabsRects defaultScaleUI x = case x of
    Single _ _    -> []
    Tabs _ _ ws _ -> zip (fmap (winBoundingRect defaultScaleUI) ws) ws

winBoundingRect :: ScaleFactor -> Win -> Rect
winBoundingRect defaultScaleUI w = maybe (shiftBy 50 $ bestRect defaultScaleUI $ winGui w) id $ winRect w
    where shiftBy n r = r { px = n + px r, py = n + py r }

drawTab :: (Int, Int) -> Rect -> Win -> Doc
drawTab shift r w = group (winTitle w) r $ drawWin (withRelWinMargin $ shiftRect shift r) w
    where group title rect body = P.vcat
            [ ppProc "FLgroup" $ (P.text $ show title) : rectToFrame rect
            , body
            , ppProc "FLgroupEnd" []]

          shiftRect (dx, dy) rect = rect
            { px = dx + px rect
            , py = dy + py rect }

rectToFrame :: Rect -> [Doc]
rectToFrame rect = fmap P.int [width rect, height rect, px rect, py rect]

drawWin :: Rect -> Win -> Doc
drawWin rect w = renderAbsScene $ Box.draw rect $ unGui $ winGui w
    where
        renderAbsScene = Box.cascade drawPrim P.empty P.vcat onCtx setProps def
            where
                setProps ps = appEndo $ mconcat $ fmap (Endo . setPropCtx) (otherProps ps)

                onCtx r ps res = maybe res (\borderType -> drawBorder borderType r res) (propsBorder ps)

drawBorder :: BorderType -> Rect -> Doc -> Doc
drawBorder borderType rect a = P.vcat
    [ ppProc "FLgroup" $ ((P.text $ show "") : frame) ++ [borderAsInt borderType]
    , a
    , ppProc "FLgroupEnd" []]
    where borderAsInt = P.int . fromEnum
          frame = rectToFrame rect

drawPrim :: PropCtx -> Rect -> ElemWithOuts -> Doc
drawPrim ctx rect elem = P.vcat
    [ drawElemDef ctx rect elem
    , drawAppearance ctx elem
    , drawInitVal elem ]

drawAppearance :: PropCtx -> ElemWithOuts -> Doc
drawAppearance ctx el = maybe P.empty (flip flSetAll ctx)
    $ getPropHandle $ elemOuts el

drawInitVal :: ElemWithOuts -> Doc
drawInitVal = P.vcat . fmap flSetVal_i . elemInits

drawElemDef :: PropCtx -> Rect -> ElemWithOuts -> Doc
drawElemDef ctx rectWithoutLabel el = case elemContent el of
    -- valuators
    Count  diap step1 step2 -> drawCount diap step1 step2
    Joy    span1 span2      -> drawJoy span1 span2
    Knob   span             -> drawKnob span
    Roller span step        -> drawRoller span step
    Slider span             -> drawSlider span
    Text   diap step        -> drawText diap step

    -- other widgets
    Box label               -> drawBox label
    ButBank xn yn           -> drawButBank xn yn
    Button instrId          -> drawButton instrId
    Toggle                  -> drawToggle
    Value                   -> drawValue
    Vkeybd                  -> drawVkeybd

    -- error
    GuiVar guiHandle        -> orphanGuiVar guiHandle
    where
        rect = clearSpaceForLabel $ rectWithoutLabel
        clearSpaceForLabel a
            | label == ""   = a
            | otherwise     = a { height = max 20 $ height a - yLabelBox (getIntFontSize ctx) }
            where label = getLabel ctx

        f = fWithLabel (getLabel ctx)
        fWithLabel label name args = ppMoOpc (fmap ppVar $ elemOuts el) name ((P.text $ show $ label) : args)
        fNoLabel name args = ppMoOpc (fmap ppVar $ elemOuts el) name args
        frame = frameBy rect
        frameWithoutLabel = frameBy rectWithoutLabel
        frameBy x = fmap P.int [width x, height x, px x, py x]
        noDisp = P.int (-1)
        noOpc  = P.int (-1)
        onOpc instrId xs = P.int 0 : P.int (instrIdCeil instrId) : fmap P.double xs
        drawSpan (ValSpan diap scale) = [imin diap, imax diap, getScale scale]

        imin = P.double . valDiapMin
        imax = P.double . valDiapMax

        -----------------------------------------------------------------------
        -- valuators

        -- FLcount
        drawCount diap step1 mValStep2 = f "FLcount" $
            [ imin diap, imax diap
            , P.double step1, P.double step2
            , P.int itype ]
            ++ frame ++ [noOpc]
            where (step2, itype) = case mValStep2 of
                    -- type 1 FLcount with 2 steps
                    Just n  -> (n, 1)
                    -- type 2 FLcount with a single step
                    Nothing -> (step1, 2)

        -- FLjoy
        drawJoy (ValSpan dX sX) (ValSpan dY sY) = f "FLjoy" $
            [ imin dX, imax dX, imin dY, imax dY
            , getScale sX, getScale sY
            , noDisp, noDisp
            ] ++ frame

        -- FLknob
        drawKnob span = f "FLknob" $
            drawSpan span ++ [getKnobType ctx, noDisp]
            ++ fmap P.int knobFrame ++ getKnobCursorSize ctx
            where
                knobFrame
                    | w < h     = [w, x, y + d]
                    | otherwise = [h, x + d, y]
                h = height rect
                w = width rect
                x = px rect
                y = py rect
                d = div (abs $ h - w) 2

        -- FLroller
        drawRoller (ValSpan d s) step = f "FLroller" $
            [ imin d, imax d, P.double step
            , getScale s, getRollerType (getDefOrient rect) ctx, noDisp
            ] ++ frame

        -- FLslider
        drawSlider span = f "FLslider" $
            drawSpan span
            ++ [getSliderType (getDefOrient rect) ctx, noDisp]
            ++ frame

        -- FLtext
        drawText diap step = f "FLtext" $
            [imin diap, imax diap, P.double step, getTextType ctx] ++ frame

        -----------------------------------------------------------------------
        -- other widgets

        -- FLbox
        drawBox label = fWithLabel label "FLbox" $
            [ getBoxType ctx, getFontType ctx, getFontSize ctx ] ++ frameWithoutLabel

        -- FLbutBank
        drawButBank xn yn = fNoLabel "FLbutBank" $
            [getButtonBankType ctx, P.int xn, P.int yn] ++ frameWithoutLabel ++ [noOpc]

        -- FLbutton's
        drawButton instrId = f "FLbutton" $ [P.int 1, P.int 0, getButtonType ctx] ++ frameWithoutLabel ++ (onOpc instrId [0, infiniteDur])

        drawToggle = f "FLbutton" $ [P.int 1, P.int 0, getToggleType ctx] ++ frameWithoutLabel ++ [noOpc]

        -- FLvalue
        drawValue = f "FLvalue" frame

        -- FLvkeybd
        drawVkeybd = fWithLabel "" "FLvkeybd" frame

-----------------------------------------------------------------
-- handy shortcuts

setProp :: Prop -> Gui -> Gui
setProp p = props [p]

setLabel :: String -> Gui -> Gui
setLabel = setProp . SetLabel

setLabelType :: LabelType -> Gui -> Gui
setLabelType = setProp . SetLabelType

setMaterial :: Material -> Gui -> Gui
setMaterial = setProp . SetMaterial

setBoxType :: BoxType -> Gui -> Gui
setBoxType = setProp . SetBoxType

setColor1 :: Color -> Gui -> Gui
setColor1 = setProp . SetColor1

setColor2 :: Color -> Gui -> Gui
setColor2 = setProp . SetColor2

setColors :: Color -> Color -> Gui -> Gui
setColors primary secondary = setColor1 primary . setColor2 secondary

setTextColor :: Color -> Gui -> Gui
setTextColor = setProp . SetTextColor

setFontSize :: Int -> Gui -> Gui
setFontSize = setProp . SetFontSize

setFontType :: FontType -> Gui -> Gui
setFontType = setProp . SetFontType

setEmphasis :: Emphasis -> Gui -> Gui
setEmphasis = setProp . SetEmphasis

setSliderType :: SliderType -> Gui -> Gui
setSliderType = setProp . SetSliderType

setTextType :: TextType -> Gui -> Gui
setTextType = setProp . SetTextType

setButtonType :: ButtonType -> Gui -> Gui
setButtonType = setProp . SetButtonType

setOrient :: Orient -> Gui -> Gui
setOrient = setProp . SetOrient

setKnobType :: KnobType -> Gui -> Gui
setKnobType = setProp . SetKnobType

------------------------------------------------------------------
-- best rectangles for the elements
--

winMargin :: Int
winMargin = 10

appendWinMargin :: Rect -> Rect
appendWinMargin r = r
    { width  = 2 * winMargin + width r
    , height = 2 * winMargin + height r
    }

withWinMargin :: Rect -> Rect
withWinMargin r = r
    { px = winMargin
    , py = winMargin
    , height = height r - 2 * winMargin
    , width  = width  r - 2 * winMargin
    }

withRelWinMargin :: Rect -> Rect
withRelWinMargin r = r
    { px = winMargin + px r
    , py = winMargin + py r
    , height = height r - 2 * winMargin
    , width  = width  r - 2 * winMargin
    }

bestRect :: ScaleFactor -> Gui -> Rect
bestRect defaultScaleUI
    = appendWinMargin . Box.boundingRect
    . mapWithOrientAndScale defaultScaleUI (\curOrient curScaleFactor x -> uncurry noShiftRect $ bestElemSizesRescaled curScaleFactor $ bestElemSizes curOrient $ elemContent x)
    . unGui
    where noShiftRect w h = Rect { px = 0, py = 0, width = w, height = h }

mapWithOrientAndScale :: ScaleFactor -> (Orient -> ScaleFactor -> a -> b) -> Box.Scene Props a -> Box.Scene Props b
mapWithOrientAndScale defaultScaleUI f = iter Hor defaultScaleUI
    where
        iter curOrient curScale x = case x of
            Box.Prim a          -> Box.Prim $ f curOrient curScale a
            Box.Space           -> Box.Space
            Box.Scale d a       -> Box.Scale d $ iter curOrient curScale a
            Box.Hor offs as     -> Box.Hor offs $ fmap (iter Hor curScale) as
            Box.Ver offs as     -> Box.Ver offs $ fmap (iter Ver curScale) as
            Box.Context ctx a   -> case propsScaleFactor ctx of
                    Nothing -> Box.Context ctx $ iter curOrient curScale a
                    Just newScale -> Box.Context ctx $ iter curOrient (mulFactors curScale newScale) a
            where
                mulFactors (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)

bestElemSizesRescaled :: ScaleFactor -> (Int, Int) -> (Int, Int)
bestElemSizesRescaled (scaleX, scaleY) (sizeX, sizeY) = (mul scaleX sizeX, mul scaleY sizeY)
    where mul d n = round $ d * fromIntegral n

bestElemSizes :: Orient -> Elem -> (Int, Int)
bestElemSizes orient x = case x of
    -- valuators
    Count   _ _ _   -> (120, 30)
    Joy     _ _     -> (200, 200)
    Knob    _       -> (80, 80)
    Roller  _ _     -> inVer (150, 30)
    Slider  _       -> inVer (150, 25)
    Text    _ _     -> (100, 35)

    -- other widgets
    Box     label    ->
        let symbolsPerLine = 60
            numOfLines = succ $ div (length label) symbolsPerLine
        in  (xBox 15 symbolsPerLine, yBox 15 numOfLines)

    ButBank xn yn   -> (xn * 70, yn * 35)
    Button _        -> (75, 35)
    Toggle          -> (75, 35)
    Value           -> (80, 35)
    Vkeybd          -> (1080, 240)

    -- error
    GuiVar h        -> orphanGuiVar h
    where inVer (a, b) = case orient of
            Ver -> (a, b)
            Hor -> (b, a)

------------------------------------------------------------
-- FLbox font coefficients

xBox, yBox :: Int -> Int -> Int

xBox fontSize xn = round $ fromIntegral fontSize * (0.6 :: Double) * fromIntegral (1 + xn)

yBox fontSize yn = (fontSize + 12) * (1 + yn)

yLabelBox :: Int -> Int
yLabelBox fontSize = fontSize - 5

------------------------------------------------------------
-- set properties

flSetAll :: Var -> PropCtx -> Doc
flSetAll handle ctx = P.vcat $ fmap (\f -> f handle ctx)
    [ flSetColor, flSetColor2, flSetTextColor
    , flSetTextSize, flSetTextType, flSetFont ]

flSetColor, flSetColor2, flSetTextColor, flSetTextSize, flSetTextType,
    flSetFont :: Var -> PropCtx -> Doc

flSetProp :: String
    -> (PropCtx -> Maybe a)
    -> (PropCtx -> Doc)
    -> Var -> PropCtx -> Doc
flSetProp name isDef select handle ctx
    | isNothing $ isDef ctx = P.empty
    | otherwise             = ppProc name [select ctx, ppVar handle]

flSetColor        = flSetProp "FLsetColor"        ctxColor1       getColor1
flSetColor2       = flSetProp "FLsetColor2"       ctxColor2       getColor2
flSetTextColor    = flSetProp "FLsetTextColor"    ctxTextColor    getTextColor
flSetTextSize     = flSetProp "FLsetTextSize"     (const $ Just (15 :: Int)) getFontSize
flSetTextType     = flSetProp "FLsetTextType"     ctxLabelType    getLabelType
flSetFont         = flSetProp "FLsetFont"         ctxFontType     getFontType

flSetVal_i :: InitMe -> Doc
flSetVal_i (InitMe handle v0) = ppProc "FLsetVal_i" [P.double v0, ppVar handle]

------------------------------------------------------------
-- extract handle.Hor

getPropHandle :: [Var] -> Maybe Var
getPropHandle xs = case xs of
    [] -> Nothing
    _  -> Just (last xs)

------------------------------------------------------------
-- error messages

orphanGuiVar :: GuiHandle -> a
orphanGuiVar (GuiHandle n) = error $ "orphan GuiHandle: " ++ show n
