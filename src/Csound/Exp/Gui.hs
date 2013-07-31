module Csound.Exp.Gui where

import Prelude hiding(elem, span)

import Control.Applicative((<|>))
import Data.Default
import Data.Maybe(isNothing)
import Data.Monoid

import Data.Colour
import Data.Colour.Names(white, gray)
import Data.Colour.SRGB

import qualified Data.IntMap as IM
import qualified Csound.Render.Pretty as P
import Csound.Render.Pretty(Doc, int, double, vcat, hcat, punctuate, comma)

import Csound.Exp hiding (P)
import qualified Csound.BoxModel as Box
import Csound.BoxModel(Rect(..))

newtype GuiHandle = GuiHandle { unGuiHandle :: Int }

type Color = Colour Double

data Orient = Hor | Ver

data Span = Span 
    { spanDiap  :: Diap
    , spanScale :: ScaleType }

data Diap = Diap 
    { diapMin   :: Double 
    , diapMax   :: Double }

data ScaleType = Linear | Exponential

type Step = Double

data FontType       = Helvetica | Courier | Times | Symbol | Screen | Dingbats
data Emphasis       = NoEmphasis | Italic | Bold | BoldItalic
data KnobType       = ThreeD (Maybe Int) | Pie | Clock | Flat
data SliderType     = Fill | Engraved | Nice
data TextType       = NormalText | NoDrag | NoEdit
data Material       = NoPlastic | Plastic
data LabelType      = NormalLabel | NoLabel | SymbolLabel 
                    | ShadowLabel | EngravedLabel | EmbossedLabel

data BoxType    
    = FlatBox | UpBox | DownBox | ThinUpBox | ThinDownBox 
    | EngravedBox | EmbossedBox | BorderBox | ShadowBox
    | Roundedbox | RoundedShadowBox | RoundedFlatBox
    | RoundedUpBox | RoundedDownBox | DiamondUpBox 
    | DiamondDownBox | OvalBox | OvalShadowBox | OvalFlatBox
    deriving (Enum)

data BorderType
    = NoBorder | DownBoxBorder | UpBoxBorder | EngravedBorder 
    | EmbossedBorder | BlackLine | ThinDown | ThinUp
    deriving (Enum)

data ButtonType = NormalButton | LightButton | CheckButton | RoundButton

defFontSize :: Int
defFontSize = 15

instance Default FontType       where def = Courier
instance Default Emphasis       where def = NoEmphasis
instance Default SliderType     where def = Fill
instance Default KnobType       where def = Flat
instance Default TextType       where def = NormalText
instance Default ButtonType     where def = NormalButton
instance Default BoxType        where def = FlatBox
instance Default Material       where def = Plastic
instance Default LabelType      where def = NormalLabel

data InitMe = InitMe 
    { initHandle :: Var
    , initValue  :: Double }

data Elem 
    = GuiVar GuiHandle 
    
    -- valuators
    | Count  Diap Step (Maybe Step)
    | Joy    Span Span
    | Knob   Span
    | Roller Span Step
    | Slider Span
    | Text   Diap Step

    -- other widgets  
    | Box String
    | ButBank Int Int
    | Button
    | Toggle 
    | Value
    | Vkeybd

data Props = Props 
    { propsBorder   :: Maybe BorderType
    , otherProps    :: [Prop] }

instance Monoid Props where
    mempty = Props Nothing []
    mappend a b = Props { propsBorder = (propsBorder a) <|> (propsBorder b)
                        , otherProps  = mappend (otherProps a) (otherProps b) }
    
data Prop
    = SetLabel String
    | SetMaterial Material
    | SetBoxType BoxType
    | SetColor1 Color | SetColor2 Color | SetTextColor Color
    | SetFontSize Int | SetFontType FontType | SetEmphasis Emphasis
    | SetSliderType SliderType    
    | SetTextType TextType
    | SetButtonType ButtonType 
    | SetOrient Orient
    | SetKnobType KnobType
    | SetLabelType LabelType

newtype Gui = Gui { unGui :: LowGui }

type LowGui = Box.Scene Props ElemWithOuts

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

type ElemOuts = [Var]

fromElem :: ElemOuts -> [InitMe] -> Elem -> Gui
fromElem outs inits el = Gui $ Box.prim (ElemWithOuts outs inits el)

fromGuiHandle :: GuiHandle -> Gui
fromGuiHandle = Gui . Box.prim . ElemWithOuts [] [] . GuiVar 

onLowGuis :: ([LowGui] -> LowGui) -> ([Gui] -> Gui)
onLowGuis f = Gui . f . fmap unGui

onLowGui1 :: (LowGui -> LowGui) -> (Gui -> Gui)
onLowGui1 f = Gui . f . unGui 

hor :: [Gui] -> Gui
hor = onLowGuis Box.hor 

ver :: [Gui] -> Gui
ver = onLowGuis Box.ver

space :: Gui
space = Gui Box.space

sca :: Double -> Gui -> Gui
sca d = onLowGui1 (Box.sca d)

padding :: Int -> Gui -> Gui
padding n = onLowGui1 (Box.padding n)

margin :: Int -> Gui -> Gui
margin n = onLowGui1 (Box.margin n)

props :: [Prop] -> Gui -> Gui
props ps = onLowGui1 (Box.appendContext (Props Nothing ps))

setBorder :: BorderType -> Gui -> Gui
setBorder a = onLowGui1 (Box.appendContext (Props (Just a) []))

type GuiMap = IM.IntMap Gui

guiMap :: [GuiNode] -> GuiMap
guiMap = IM.fromList . fmap (\(GuiNode elem (GuiHandle n)) -> (n, elem))

restoreTree :: GuiMap -> Gui -> Gui
restoreTree m x = Gui $ (unGui x) >>= rec
    where rec elem = case elemContent elem of
            GuiVar h -> unGui $ restoreTree m $ m IM.! unGuiHandle h
            _        -> return elem

drawGui :: Win -> P.Doc
drawGui w = onPanel (winTitle w) panelRect $ 
    renderAbsScene $ Box.draw (withZeroOffset $ panelRect) $ unGui $ winGui w
    where
        panelRect = maybe (shiftBy 50 $ bestRect $ winGui w) id $ winRect w
            where shiftBy n r = r { px = n + px r, py = n + py r }

        withZeroOffset r = r { px = 0, py = 0 }

        renderAbsScene = Box.cascade drawPrim P.empty P.vcat onCtx setProps def
            where
                setProps ps = appEndo $ mconcat $ fmap (Endo . setPropCtx) (otherProps ps)
                
                onCtx r ps res = maybe res (\borderType -> drawBorder borderType r res) (propsBorder ps)
                
        drawBorder borderType rect a = vcat 
            [ P.ppProc "FLgroup" $ ((P.text $ show "") : frame) ++ [borderAsInt borderType]
            , a
            , P.ppProc "FLgroupEnd" []]
            where borderAsInt = int . fromEnum
                  frame = fmap int [width rect, height rect, px rect, py rect]                             

        onPanel title rect body = P.vcat [
            P.ppProc "FLpanel" [P.text $ show title, P.int $ width rect, P.int $ height rect],
            body,
            P.ppProc "FLpanelEnd" []]
    
drawPrim :: PropCtx -> Rect -> ElemWithOuts -> Doc
drawPrim ctx rect elem = vcat 
    [ drawElemDef ctx rect elem
    , drawAppearance ctx elem
    , drawInitVal elem ]

drawAppearance :: PropCtx -> ElemWithOuts -> Doc
drawAppearance ctx el = maybe P.empty (flip flSetAll ctx)
    $ getPropHandle $ elemOuts el

drawInitVal :: ElemWithOuts -> Doc
drawInitVal = vcat . fmap flSetVal_i . elemInits 

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
    Box text                -> drawBox text 
    ButBank xn yn           -> drawButBank xn yn 
    Button                  -> drawButton 
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
        fWithLabel label name args = P.ppMoOpc (fmap P.ppVar $ elemOuts el) name ((P.text $ show $ label) : args)
        fNoLabel name args = P.ppMoOpc (fmap P.ppVar $ elemOuts el) name args
        frame = frameBy rect
        frameWithoutLabel = frameBy rectWithoutLabel
        frameBy x = fmap int [width x, height x, px x, py x]       
        noDisp = int (-1)
        noOpc  = int (-1)
        drawSpan (Span diap scale) = [imin diap, imax diap, getScale scale]
   
        imin = double . diapMin
        imax = double . diapMax

        -----------------------------------------------------------------------
        -- valuators

        -- FLcount
        drawCount diap step1 mStep2 = f "FLcount" $
            [ imin diap, imax diap
            , double step1, double step2
            , int itype ] 
            ++ frame ++ [noOpc]
            where (step2, itype) = case mStep2 of
                    -- type 1 FLcount with 2 steps
                    Just n  -> (n, 1)
                    -- type 2 FLcount with a single step
                    Nothing -> (step1, 2)

        -- FLjoy
        drawJoy (Span dX sX) (Span dY sY) = f "FLjoy" $
            [ imin dX, imax dX, imin dY, imax dY
            , getScale sX, getScale sY
            , noDisp, noDisp 
            ] ++ frame

        -- FLknob
        drawKnob span = f "FLknob" $ 
            drawSpan span ++ [getKnobType ctx, noDisp] 
            ++ fmap int knobFrame ++ getKnobCursorSize ctx            
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
        drawRoller (Span d s) step = f "FLroller" $
            [ imin d, imax d, double step
            , getScale s, getRollerType (getDefOrient rect) ctx, noDisp
            ] ++ frame

        -- FLslider
        drawSlider span = f "FLslider" $ 
            drawSpan span 
            ++ [getSliderType (getDefOrient rect) ctx, noDisp] 
            ++ frame

        -- FLtext
        drawText diap step = f "FLtext" $ 
            [imin diap, imax diap, double step, getTextType ctx] ++ frame

        -----------------------------------------------------------------------
        -- other widgets
        
        -- FLbox
        drawBox text = fWithLabel text "FLbox" $
            [ getBoxType ctx, getFontType ctx, getFontSize ctx ] ++ frameWithoutLabel
        
        -- FLbutBank
        drawButBank xn yn = fNoLabel "FLbutBank" $ 
            [getButtonType ctx, int xn, int yn] ++ frameWithoutLabel ++ [noOpc] 

        -- FLbutton's
        drawButton = f "FLbutton" $ [int 1, int 0, getButtonType ctx] ++ frameWithoutLabel ++ [noOpc]
        
        drawToggle = f "FLbutton" $ [int 1, int 0, getToggleType ctx] ++ frameWithoutLabel ++ [noOpc]

        -- FLvalue
        drawValue = f "FLvalue" frame 

        -- FLvkeybd
        drawVkeybd = fWithLabel "" "FLvkeybd" frame  

-----------------------------------------------------------
-- cascading context, here we group properties by type

data PropCtx = PropCtx 
    { ctxLabel        :: Maybe String
    , ctxMaterial     :: Maybe Material
    , ctxLabelType    :: Maybe LabelType
    , ctxBoxType      :: Maybe BoxType
    , ctxColor1       :: Maybe Color
    , ctxColor2       :: Maybe Color
    , ctxTextColor    :: Maybe Color
    , ctxFontSize     :: Maybe Int
    , ctxFontType     :: Maybe FontType
    , ctxEmphasis     :: Maybe Emphasis
    , ctxOrient       :: Maybe Orient
    , ctxSliderType   :: Maybe SliderType
    , ctxButtonType   :: Maybe ButtonType
    , ctxTextType     :: Maybe TextType
    , ctxKnobType     :: Maybe KnobType }
    
instance Default PropCtx where
    def = PropCtx Nothing Nothing Nothing Nothing Nothing Nothing
                  Nothing Nothing Nothing Nothing Nothing Nothing
                  Nothing Nothing Nothing 
   
setPropCtx :: Prop -> PropCtx -> PropCtx 
setPropCtx p x = case p of
            SetLabel        a -> x { ctxLabel  = Just a }
            SetMaterial     a -> x { ctxMaterial = Just a }
            SetLabelType    a -> x { ctxLabelType = Just a }
            SetBoxType      a -> x { ctxBoxType = Just a }
            SetColor1       a -> x { ctxColor1 = Just a }
            SetColor2       a -> x { ctxColor2 = Just a }
            SetTextColor    a -> x { ctxTextColor = Just a }
            SetFontSize     a -> x { ctxFontSize = Just a }
            SetFontType     a -> x { ctxFontType = Just a }
            SetEmphasis     a -> x { ctxEmphasis = Just a }
            SetOrient       a -> x { ctxOrient = Just a }
            SetSliderType   a -> x { ctxSliderType = Just a }
            SetButtonType   a -> x { ctxButtonType = Just a }
            SetTextType     a -> x { ctxTextType = Just a }
            SetKnobType     a -> x { ctxKnobType = Just a } 

getLabel :: PropCtx -> String
getLabel = maybe "" id . ctxLabel

------------------------------------------------------------------
-- Converting readable properties to integer codes

maybeDef :: Default a => Maybe a -> a
maybeDef = maybe def id

intProp :: Default a => (PropCtx -> Maybe a) -> (a -> Int) -> (PropCtx -> Doc)
intProp select convert = int . convert . maybeDef . select 

getScale :: ScaleType -> Doc
getScale x = int $ case x of
    Linear      -> 0
    Exponential -> -1

getLabelType :: PropCtx -> Doc
getLabelType = intProp ctxLabelType $ \x -> case x of
    NormalLabel     -> 0
    NoLabel         -> 1
    SymbolLabel     -> 2
    ShadowLabel     -> 3
    EngravedLabel   -> 4
    EmbossedLabel   -> 5

getDefOrient :: Rect -> Orient
getDefOrient r 
    | height r < width r    = Hor
    | otherwise             = Ver

getOrient :: Orient -> PropCtx -> Orient
getOrient defOrient = maybe defOrient id . ctxOrient

getKnobType :: PropCtx -> Doc
getKnobType = intProp ctxKnobType $ \x -> case x of
    Flat        -> 4
    Pie         -> 2
    Clock       -> 3
    ThreeD _    -> 1
  
getKnobCursorSize :: PropCtx -> [Doc]
getKnobCursorSize ctx = case maybeDef $ ctxKnobType ctx of
    ThreeD (Just n) -> [int n]
    _               -> []

getRollerType :: Orient -> PropCtx -> Doc
getRollerType defOrient ctx = int $ case getOrient defOrient ctx of
    Hor -> 1
    Ver -> 2
    
getSliderType :: Orient -> PropCtx -> Doc
getSliderType defOrient ctx = int $ appMaterial ctx $ 
    case (getOrient defOrient ctx, maybeDef $ ctxSliderType ctx) of
        (Hor, Fill)         -> 1
        (Ver, Fill)         -> 2
        (Hor, Engraved)     -> 3
        (Ver, Engraved)     -> 4
        (Hor, Nice)         -> 5
        (Ver, Nice)         -> 6

getTextType :: PropCtx -> Doc
getTextType = intProp ctxTextType $ \x -> case x of  
    NormalText  -> 1
    NoDrag      -> 2
    NoEdit      -> 3

getBoxType :: PropCtx -> Doc
getBoxType = intProp ctxBoxType $ succ . fromEnum

getFontSize :: PropCtx -> Doc 
getFontSize  = int . getIntFontSize 

getIntFontSize :: PropCtx -> Int
getIntFontSize ctx = maybe defFontSize id $ ctxFontSize ctx

getFontType :: PropCtx -> Doc
getFontType ctx = int $ 
    case (maybeDef $ ctxFontType ctx, maybeDef $ ctxEmphasis ctx) of
        (Helvetica, NoEmphasis)         -> 1
        (Helvetica, Bold)               -> 2
        (Helvetica, Italic)             -> 3
        (Helvetica, BoldItalic)         -> 4
        (Courier, NoEmphasis)           -> 5
        (Courier, Bold)                 -> 6
        (Courier, Italic)               -> 7
        (Courier, BoldItalic)           -> 8
        (Times, NoEmphasis)             -> 9
        (Times, Bold)                   -> 10
        (Times, Italic)                 -> 11
        (Times, BoldItalic)             -> 12
        (Symbol, _)                     -> 13
        (Screen, Bold)                  -> 15
        (Screen, _)                     -> 14
        (Dingbats, _)                   -> 16
       
getButtonType :: PropCtx -> Doc
getButtonType ctx = int $ appMaterial ctx 1
       
getToggleType :: PropCtx -> Doc
getToggleType ctx = ($ ctx) $ intProp ctxButtonType $ \x -> 
    reactOnNoPlasticForRoundBug $ appMaterial ctx $ case x of
        NormalButton    -> 2
        LightButton     -> 2
        CheckButton     -> 3
        RoundButton     -> 4   
    where reactOnNoPlasticForRoundBug x = case x of
            24 -> 4
            _  -> x


appMaterial :: PropCtx -> Int -> Int
appMaterial ctx = case maybeDef $ ctxMaterial ctx of
    Plastic   -> (+ 20)
    NoPlastic -> id
  
getColor1, getColor2, getTextColor :: PropCtx -> Doc

getColor1       = genGetColor gray  ctxColor1
getColor2       = genGetColor white ctxColor2
getTextColor    = genGetColor black ctxTextColor

genGetColor :: Color -> (PropCtx -> Maybe Color) -> PropCtx -> Doc
genGetColor defColor select ctx = colorToDoc $ maybe defColor id $ select ctx
    where colorToDoc col = hcat $ punctuate comma 
            $ fmap (channelToDoc col) [channelRed, channelGreen, channelBlue]        
          channelToDoc col chn = int $ fromEnum $ chn $ toSRGB24 $ col  

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

bestRect :: Gui -> Rect
bestRect 
    = Box.boundingRect 
    . mapWithOrient (\curOrient x -> uncurry noShiftRect $ bestElemSizes curOrient $ elemContent x)
    . unGui
    where noShiftRect w h = Rect { px = 0, py = 0, width = w, height = h }

mapWithOrient :: (Orient -> a -> b) -> Box.Scene ctx a -> Box.Scene ctx b
mapWithOrient f = iter Hor
    where 
        iter curOrient x = case x of
            Box.Prim a          -> Box.Prim $ f curOrient a
            Box.Space           -> Box.Space
            Box.Scale d a       -> Box.Scale d $ iter curOrient a
            Box.Hor offs as     -> Box.Hor offs $ fmap (iter Hor) as
            Box.Ver offs as     -> Box.Ver offs $ fmap (iter Ver) as
            Box.Context ctx a   -> Box.Context ctx $ iter curOrient a
            
bestElemSizes :: Orient -> Elem -> (Int, Int)
bestElemSizes orient x = case x of
    -- valuators
    Count   _ _ _   -> (150, 35)
    Joy     _ _     -> (350, 350)  
    Knob    _       -> (200, 200)
    Roller  _ _     -> inVer (300, 35)
    Slider  _       -> inVer (500, 35)
    Text    _ _     -> (150, 35)

    -- other widgets  
    Box     text    -> 
        let symbolsPerLine = 60
            numOfLines = succ $ div (length text) symbolsPerLine
        in  (xBox 15 symbolsPerLine, yBox 15 numOfLines)            

    ButBank xn yn   -> (xn * 80, yn * 35)
    Button          -> (80, 35) 
    Toggle          -> (80, 35) 
    Value           -> (100, 35)
    Vkeybd          -> (1280, 240)
    
    -- error
    GuiVar h        -> orphanGuiVar h
    where inVer (a, b) = case orient of
            Ver -> (a, b)
            Hor -> (b, a)

------------------------------------------------------------
-- FLbox font coefficients

xBox, yBox :: Int -> Int -> Int

xBox fontSize xn = round $ fromIntegral fontSize * (0.4 :: Double) * fromIntegral (1 + xn)

yBox fontSize yn = (fontSize + 8) * (1 + yn)

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
    | otherwise             = P.ppProc name [select ctx, P.ppVar handle]    

flSetColor        = flSetProp "FLsetColor"        ctxColor1       getColor1
flSetColor2       = flSetProp "FLsetColor2"       ctxColor2       getColor2
flSetTextColor    = flSetProp "FLsetTextColor"    ctxTextColor    getTextColor
flSetTextSize     = flSetProp "FLsetTextSize"     (const $ Just (15 :: Int)) getFontSize
flSetTextType     = flSetProp "FLsetTextType"     ctxLabelType    getLabelType
flSetFont         = flSetProp "FLsetFont"         ctxFontType     getFontType

flSetVal_i :: InitMe -> Doc
flSetVal_i (InitMe handle v0) = P.ppProc "FLsetVal_i" [double v0, P.ppVar handle]

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



